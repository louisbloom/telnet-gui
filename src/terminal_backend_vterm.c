/* VTerm backend implementation - libvterm-based terminal emulation */

#include "terminal_backend.h"
#include "term_cell.h"
#include "input_area.h"
#include "telnet.h"
#include "dynamic_buffer.h"
#include "ansi_sequences.h"
#include "lisp.h"
#include <vterm.h>
#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define OUTPUT_BUFFER_INITIAL_SIZE 4096
#define SCROLLBACK_MAX_LINES 10000

/* libvterm version string (detected at compile-time via CMake) */
#ifndef LIBVTERM_VERSION
#define LIBVTERM_VERSION "unknown"
#endif

/* Scrollback line storage */
typedef struct {
    VTermScreenCell *cells; /* Array of cells for this line */
    int cols;               /* Number of columns when line was stored */
    bool continuation;      /* Whether this line is a continuation of previous line */
} ScrollbackLine;

/* VTerm backend state - all vterm-specific data */
typedef struct {
    VTerm *vterm;
    VTermScreen *screen;
    int rows, cols;
    int scrolling_rows;
    int input_row;
    int needs_redraw;
    VTermScreenCallbacks callbacks;

    /* Output buffer for telnet */
    char *output_buffer;
    size_t output_buffer_size;
    size_t output_buffer_len;
    size_t output_buffer_echoed;
    int echoing_locally;

    /* Output callback (for telnet) */
    void (*output_callback_fn)(const char *s, size_t len, void *user);
    void *output_callback_user;

    /* Render buffer for building ANSI sequences (reused across calls) */
    DynamicBuffer *render_buffer;

    /* Normalize buffer for LF to CRLF conversion (reused across calls) */
    DynamicBuffer *normalize_buffer;

    /* Scrollback */
    ScrollbackLine *scrollback;
    int scrollback_size;
    int scrollback_start;
    int scrollback_capacity;
    int max_scrollback_lines;
    int viewport_offset;
    int scroll_locked; /* Whether user has scrolled back (prevents auto-scroll on new data) */

    /* Cursor tracking */
    int cursor_row; /* Cursor position anywhere in vterm (including input area) */
    int cursor_col;
    int cursor_visible;

    /* Terminal content cursor tracking (scrolling region only, excludes input area) */
    int terminal_cursor_row; /* Cursor position within terminal content (for echoes) */
    int terminal_cursor_col;
    int ignore_cursor_tracking; /* Set to 1 during resize/render to prevent corruption */
} VTermBackendState;

/* Helper: Convert logical scrollback index to physical circular buffer index */
static inline int scrollback_physical_index(VTermBackendState *state, int logical_index) {
    return (state->scrollback_start + logical_index) % state->scrollback_capacity;
}

/* Conversion functions between VTermScreenCell and TermCell */

/* Helper type for generic color structure */
typedef struct {
    uint8_t type;
    union {
        uint8_t idx;
        struct {
            uint8_t r, g, b;
        } rgb;
    } color;
} GenericColor;

/* Helper: Convert VTerm color to TermCell color */
static void vterm_color_to_term_color(const VTermColor *vcolor, GenericColor *tcolor) {
    /* Check actual color type first (RGB or INDEXED) before checking DEFAULT flags.
     * VTerm color flags are bitwise, so INDEXED and DEFAULT_FG can both be set.
     * We prioritize explicit color values over DEFAULT metadata. */

    if (VTERM_COLOR_IS_RGB(vcolor)) {
        tcolor->type = TERM_COLOR_RGB;
        tcolor->color.rgb.r = vcolor->rgb.red;
        tcolor->color.rgb.g = vcolor->rgb.green;
        tcolor->color.rgb.b = vcolor->rgb.blue;
    } else if (VTERM_COLOR_IS_INDEXED(vcolor)) {
        tcolor->type = TERM_COLOR_INDEXED;
        tcolor->color.idx = vcolor->indexed.idx;
    } else if (VTERM_COLOR_IS_DEFAULT_FG(vcolor) || VTERM_COLOR_IS_DEFAULT_BG(vcolor)) {
        tcolor->type = TERM_COLOR_DEFAULT;
    } else {
        tcolor->type = TERM_COLOR_DEFAULT;
    }
}

/* Convert VTermScreenCell to generic TermCell */
static void vterm_cell_to_term_cell(const VTermScreenCell *vcell, TermCell *tcell) {
    memcpy(tcell->chars, vcell->chars, sizeof(tcell->chars));

    tcell->width = vcell->width;

    tcell->attrs.bold = vcell->attrs.bold;
    tcell->attrs.italic = vcell->attrs.italic;
    tcell->attrs.underline = vcell->attrs.underline;
    tcell->attrs.reverse = vcell->attrs.reverse;
    tcell->attrs.strike = vcell->attrs.strike;
    tcell->attrs.blink = vcell->attrs.blink;

    vterm_color_to_term_color(&vcell->fg, (GenericColor *)&tcell->fg);
    vterm_color_to_term_color(&vcell->bg, (GenericColor *)&tcell->bg);
}

/* VTerm callbacks */

static int damage(VTermRect rect, void *user) {
    (void)rect;
    VTermBackendState *state = (VTermBackendState *)user;
    if (!state)
        return 0;
    state->needs_redraw = 1;
    return 1;
}

static int screen_movecursor(VTermPos pos, VTermPos oldpos, int visible, void *user) {
    (void)oldpos;
    VTermBackendState *state = (VTermBackendState *)user;
    if (!state)
        return 0;

    /* Track cursor position and visibility for rendering */
    state->cursor_row = pos.row;
    state->cursor_col = pos.col;
    state->cursor_visible = visible;

    /* Track terminal content cursor (scrolling region only) */
    /* Skip update during internal operations (resize, render) to prevent corruption */
    if (!state->ignore_cursor_tracking && pos.row < state->scrolling_rows) {
        state->terminal_cursor_row = pos.row;
        state->terminal_cursor_col = pos.col;
    }
    /* If flag is set or cursor in input area, terminal_cursor stays at last known position */

    state->needs_redraw = 1;
    return 1;
}

static int settermprop(VTermProp prop, VTermValue *val, void *user) {
    (void)prop;
    (void)val;
    (void)user;
    return 1;
}

static int bell(void *user) {
    (void)user;
    return 1;
}

/* Helper: Evict oldest line from scrollback */
static void evict_oldest_scrollback_line(VTermBackendState *state) {
    if (!state || state->scrollback_size == 0)
        return;

    int oldest_index = state->scrollback_start;
    free(state->scrollback[oldest_index].cells);
    state->scrollback[oldest_index].cells = NULL;

    state->scrollback_start = (state->scrollback_start + 1) % state->scrollback_capacity;
    state->scrollback_size--;

    if (state->viewport_offset > state->scrollback_size)
        state->viewport_offset = state->scrollback_size;
}

static int sb_pushline(int cols, const VTermScreenCell *cells, void *user) {
    VTermBackendState *state = (VTermBackendState *)user;
    if (!state || !cells)
        return 0;

    int effective_max = (state->max_scrollback_lines > 0) ? state->max_scrollback_lines : SCROLLBACK_MAX_LINES;

    if (state->scrollback_size >= effective_max) {
        evict_oldest_scrollback_line(state);
    }

    if (state->scrollback_size >= state->scrollback_capacity) {
        int new_capacity = state->scrollback_capacity == 0 ? 100 : state->scrollback_capacity * 2;
        if (new_capacity > effective_max)
            new_capacity = effective_max;

        ScrollbackLine *new_scrollback = malloc(new_capacity * sizeof(ScrollbackLine));
        if (!new_scrollback)
            return 0;

        for (int i = 0; i < state->scrollback_size; i++) {
            new_scrollback[i] = state->scrollback[scrollback_physical_index(state, i)];
        }

        for (int i = state->scrollback_size; i < new_capacity; i++) {
            new_scrollback[i].cells = NULL;
            new_scrollback[i].cols = 0;
        }

        free(state->scrollback);
        state->scrollback = new_scrollback;
        state->scrollback_capacity = new_capacity;
        state->scrollback_start = 0;
    }

    int physical_index = scrollback_physical_index(state, state->scrollback_size);
    ScrollbackLine *line = &state->scrollback[physical_index];
    line->cells = malloc(cols * sizeof(VTermScreenCell));
    if (!line->cells)
        return 0;

    memcpy(line->cells, cells, cols * sizeof(VTermScreenCell));
    line->cols = cols;
    line->continuation = false;

    state->scrollback_size++;

    /* If scroll-locked, adjust viewport to keep viewing the same content */
    if (state->scroll_locked && state->viewport_offset > 0) {
        state->viewport_offset++;
    }

    return 1;
}

#if HAVE_VTERM_PUSHLINE4
static int sb_pushline4(int cols, const VTermScreenCell *cells, bool continuation, void *user) {
    VTermBackendState *state = (VTermBackendState *)user;
    if (!state || !cells)
        return 0;

    int effective_max = (state->max_scrollback_lines > 0) ? state->max_scrollback_lines : SCROLLBACK_MAX_LINES;

    if (state->scrollback_size >= effective_max) {
        evict_oldest_scrollback_line(state);
    }

    if (state->scrollback_size >= state->scrollback_capacity) {
        int new_capacity = state->scrollback_capacity == 0 ? 100 : state->scrollback_capacity * 2;
        if (new_capacity > effective_max)
            new_capacity = effective_max;

        ScrollbackLine *new_scrollback = malloc(new_capacity * sizeof(ScrollbackLine));
        if (!new_scrollback)
            return 0;

        for (int i = 0; i < state->scrollback_size; i++) {
            new_scrollback[i] = state->scrollback[scrollback_physical_index(state, i)];
        }

        for (int i = state->scrollback_size; i < new_capacity; i++) {
            new_scrollback[i].cells = NULL;
            new_scrollback[i].cols = 0;
        }

        free(state->scrollback);
        state->scrollback = new_scrollback;
        state->scrollback_capacity = new_capacity;
        state->scrollback_start = 0;
    }

    int physical_index = scrollback_physical_index(state, state->scrollback_size);
    ScrollbackLine *line = &state->scrollback[physical_index];
    line->cells = malloc(cols * sizeof(VTermScreenCell));
    if (!line->cells)
        return 0;

    memcpy(line->cells, cells, cols * sizeof(VTermScreenCell));
    line->cols = cols;
    line->continuation = continuation;

    state->scrollback_size++;

    /* If scroll-locked, adjust viewport to keep viewing the same content */
    if (state->scroll_locked && state->viewport_offset > 0) {
        state->viewport_offset++;
    }

    return 1;
}
#endif /* HAVE_VTERM_PUSHLINE4 */

static int sb_popline(int cols, VTermScreenCell *cells, void *user) {
    VTermBackendState *state = (VTermBackendState *)user;
    if (!state || !cells || state->scrollback_size == 0)
        return 0;

    int physical_index = scrollback_physical_index(state, state->scrollback_size - 1);
    ScrollbackLine *line = &state->scrollback[physical_index];
    int copy_cols = line->cols < cols ? line->cols : cols;

    memcpy(cells, line->cells, copy_cols * sizeof(VTermScreenCell));
    if (copy_cols < cols) {
        VTermScreenCell empty = {0};
        for (int i = copy_cols; i < cols; i++)
            cells[i] = empty;
    }

    free(line->cells);
    line->cells = NULL;
    state->scrollback_size--;

    return 1;
}

static int sb_clear(void *user) {
    VTermBackendState *state = (VTermBackendState *)user;
    if (!state)
        return 0;

    for (int i = 0; i < state->scrollback_size; i++) {
        int physical_index = scrollback_physical_index(state, i);
        free(state->scrollback[physical_index].cells);
        state->scrollback[physical_index].cells = NULL;
    }
    free(state->scrollback);
    state->scrollback = NULL;
    state->scrollback_size = 0;
    state->scrollback_start = 0;
    state->scrollback_capacity = 0;
    state->viewport_offset = 0;

    return 1;
}

static int vterm_resize_callback(int rows, int cols, void *user) {
    VTermBackendState *state = (VTermBackendState *)user;
    if (!state)
        return 0;
    state->rows = rows;
    state->cols = cols;
    state->needs_redraw = 1;
    return 1;
}

/* Output callback */
static void output_callback(const char *s, size_t len, void *user) {
    VTermBackendState *state = (VTermBackendState *)user;
    if (!state || !s || len == 0)
        return;

    if (state->echoing_locally)
        return;

    while (state->output_buffer_len + len > state->output_buffer_size) {
        size_t new_size = state->output_buffer_size * 2;
        char *new_buffer = realloc(state->output_buffer, new_size);
        if (!new_buffer)
            return;
        state->output_buffer = new_buffer;
        state->output_buffer_size = new_size;
    }

    memcpy(state->output_buffer + state->output_buffer_len, s, len);
    state->output_buffer_len += len;

    /* Call external output callback if set */
    if (state->output_callback_fn) {
        state->output_callback_fn(s, len, state->output_callback_user);
    }
}

/* Backend interface implementation */

static void *vterm_create(int rows, int cols) {
    VTermBackendState *state = (VTermBackendState *)malloc(sizeof(VTermBackendState));
    if (!state)
        return NULL;

    vterm_check_version(0, 3);

    int total_rows = rows + 2;
    state->vterm = vterm_new(total_rows, cols);
    if (!state->vterm) {
        free(state);
        return NULL;
    }

    vterm_set_utf8(state->vterm, 1);

    state->output_buffer_size = OUTPUT_BUFFER_INITIAL_SIZE;
    state->output_buffer = malloc(state->output_buffer_size);
    if (!state->output_buffer) {
        vterm_free(state->vterm);
        free(state);
        return NULL;
    }
    state->output_buffer_len = 0;
    state->output_buffer_echoed = 0;
    state->echoing_locally = 0;
    state->output_callback_fn = NULL;
    state->output_callback_user = NULL;

    state->render_buffer = dynamic_buffer_create(4096);
    if (!state->render_buffer) {
        free(state->output_buffer);
        vterm_free(state->vterm);
        free(state);
        return NULL;
    }

    state->normalize_buffer = dynamic_buffer_create(1024);
    if (!state->normalize_buffer) {
        dynamic_buffer_destroy(state->render_buffer);
        free(state->output_buffer);
        vterm_free(state->vterm);
        free(state);
        return NULL;
    }

    vterm_output_set_callback(state->vterm, output_callback, state);

    state->screen = vterm_obtain_screen(state->vterm);
    if (!state->screen) {
        dynamic_buffer_destroy(state->normalize_buffer);
        dynamic_buffer_destroy(state->render_buffer);
        free(state->output_buffer);
        vterm_free(state->vterm);
        free(state);
        return NULL;
    }

    state->rows = total_rows;
    state->cols = cols;
    state->scrolling_rows = rows;
    state->input_row = rows;
    state->needs_redraw = 1;

    state->scrollback = NULL;
    state->scrollback_size = 0;
    state->scrollback_start = 0;
    state->scrollback_capacity = 0;
    state->max_scrollback_lines = 0;
    state->viewport_offset = 0;
    state->scroll_locked = 0;

    /* Initialize cursor tracking */
    state->cursor_row = 0;
    state->cursor_col = 0;
    state->cursor_visible = 1;

    /* Initialize terminal content cursor tracking */
    state->terminal_cursor_row = 0;
    state->terminal_cursor_col = 0;
    state->ignore_cursor_tracking = 0;

    memset(&state->callbacks, 0, sizeof(state->callbacks));
    state->callbacks.damage = damage;
    state->callbacks.movecursor = screen_movecursor;
    state->callbacks.settermprop = settermprop;
    state->callbacks.bell = bell;
    state->callbacks.resize = vterm_resize_callback;
    state->callbacks.sb_pushline = sb_pushline;
    state->callbacks.sb_popline = sb_popline;
    state->callbacks.sb_clear = sb_clear;
#if HAVE_VTERM_PUSHLINE4
    state->callbacks.sb_pushline4 = sb_pushline4;
#endif

    vterm_screen_set_callbacks(state->screen, &state->callbacks, state);

#if HAVE_VTERM_PUSHLINE4
    vterm_screen_callbacks_has_pushline4(state->screen);
    fprintf(stderr, "Using libvterm with pushline4 (text reflow enabled)\n");
#else
    fprintf(stderr, "Using legacy libvterm (text reflow disabled)\n");
#endif

#if HAVE_VTERM_REFLOW
    vterm_screen_enable_reflow(state->screen, 1);
#endif

    vterm_screen_reset(state->screen, 1);

    char seq[32];
    ansi_format_scroll_region(seq, sizeof(seq), 1, rows);
    vterm_input_write(state->vterm, seq, strlen(seq));
    vterm_screen_flush_damage(state->screen);

    return state;
}

static void vterm_destroy(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state)
        return;

    if (state->output_buffer)
        free(state->output_buffer);

    if (state->render_buffer)
        dynamic_buffer_destroy(state->render_buffer);

    if (state->normalize_buffer)
        dynamic_buffer_destroy(state->normalize_buffer);

    if (state->scrollback) {
        for (int i = 0; i < state->scrollback_size; i++) {
            int physical_index = scrollback_physical_index(state, i);
            free(state->scrollback[physical_index].cells);
        }
        free(state->scrollback);
    }

    if (state->vterm)
        vterm_free(state->vterm);

    free(state);
}

static void vterm_feed_data(void *vstate, const char *data, size_t len) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state || !data)
        return;

    /* Set echoing_locally flag to prevent output callback from buffering when feeding data for display */
    /* This prevents echoed text from being sent to telnet */
    state->echoing_locally = 1;

    /* Normalize LF to CRLF to keep cursor at column 0 on new lines */
    size_t max_out = len * 2;
    dynamic_buffer_clear(state->normalize_buffer);
    if (dynamic_buffer_ensure_size(state->normalize_buffer, max_out) < 0) {
        state->echoing_locally = 0;
        return;
    }

    for (size_t i = 0; i < len; i++) {
        char c = data[i];
        if (c == '\n') {
            char prev = (i == 0) ? '\0' : data[i - 1];
            if (prev != '\r') {
                dynamic_buffer_append(state->normalize_buffer, "\r", 1);
            }
        }
        dynamic_buffer_append(state->normalize_buffer, &c, 1);
    }

    vterm_input_write(state->vterm, dynamic_buffer_data(state->normalize_buffer),
                      dynamic_buffer_len(state->normalize_buffer));

    /* Flush damage - this may trigger output, so keep echoing_locally set */
    vterm_screen_flush_damage(state->screen);

    /* Clear echoing_locally flag after all processing is complete */
    state->echoing_locally = 0;
}

static int vterm_get_cell_at(void *vstate, int row, int col, TermCell *cell) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state || !cell)
        return 0;

    int viewport_offset = state->viewport_offset;

    if (viewport_offset == 0) {
        VTermPos pos = {row, col};
        VTermScreenCell vcell;
        int result = vterm_screen_get_cell(state->screen, pos, &vcell);
        if (result) {
            vterm_cell_to_term_cell(&vcell, cell);
        }
        return result;
    } else {
        if (row < viewport_offset) {
            int scrollback_index = state->scrollback_size - viewport_offset + row;
            if (scrollback_index >= 0 && scrollback_index < state->scrollback_size) {
                int physical_index = scrollback_physical_index(state, scrollback_index);
                ScrollbackLine *line = &state->scrollback[physical_index];
                if (col < line->cols) {
                    vterm_cell_to_term_cell(&line->cells[col], cell);
                    return 1;
                } else {
                    memset(cell, 0, sizeof(TermCell));
                    return 1;
                }
            } else {
                memset(cell, 0, sizeof(TermCell));
                return 1;
            }
        } else {
            int screen_row = row - viewport_offset;
            if (screen_row >= 0 && screen_row < state->rows) {
                VTermPos pos = {screen_row, col};
                VTermScreenCell vcell;
                int result = vterm_screen_get_cell(state->screen, pos, &vcell);
                if (result) {
                    vterm_cell_to_term_cell(&vcell, cell);
                }
                return result;
            } else {
                memset(cell, 0, sizeof(TermCell));
                return 1;
            }
        }
    }
}

static int vterm_get_cell_at_scrollback_index(void *vstate, int scrollback_index, int col, TermCell *cell) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state || !cell)
        return 0;

    if (scrollback_index < 0) {
        memset(cell, 0, sizeof(TermCell));
        return 0;
    } else if (scrollback_index < state->scrollback_size) {
        int physical_index = scrollback_physical_index(state, scrollback_index);
        ScrollbackLine *line = &state->scrollback[physical_index];
        if (col < line->cols) {
            vterm_cell_to_term_cell(&line->cells[col], cell);
            return 1;
        } else {
            memset(cell, 0, sizeof(TermCell));
            return 1;
        }
    } else {
        int screen_row = scrollback_index - state->scrollback_size;
        if (screen_row >= 0 && screen_row < state->rows) {
            VTermPos pos = {screen_row, col};
            VTermScreenCell vcell;
            int result = vterm_screen_get_cell(state->screen, pos, &vcell);
            if (result) {
                vterm_cell_to_term_cell(&vcell, cell);
            }
            return result;
        } else {
            memset(cell, 0, sizeof(TermCell));
            return 0;
        }
    }
}

static void vterm_resize(void *vstate, int rows, int cols, int input_visible_rows) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state)
        return;

    /* Disable cursor tracking and save terminal cursor position FIRST */
    /* Must be before ANY vterm_input_write calls that could trigger screen_movecursor */
    state->ignore_cursor_tracking = 1;
    int saved_cursor_row = state->terminal_cursor_row;
    int saved_cursor_col = state->terminal_cursor_col;

    int old_scrolling_rows = state->scrolling_rows;
    int old_cols = state->cols;
    int old_input_row = state->input_row;
    int old_total_rows = state->rows;
    int total_rows = rows + 2 + input_visible_rows; /* top divider + bottom divider + variable input rows */

    /* If columns decreased, clear entire old input area (all divider and input rows) BEFORE resizing */
    /* This prevents old divider/content from being pushed into scroll area during resize */
    if (cols < old_cols) {
        char pos_seq[32];
        /* Clear entire old input area (divider + input rows) to remove all old divider content */
        /* Old input area starts at old_input_row + 1 (top divider) and goes to old_total_rows */
        for (int row = old_input_row + 1; row <= old_total_rows; row++) {
            /* Clear entire row from column 1 to end */
            ansi_format_cursor_pos(pos_seq, sizeof(pos_seq), row, 1);
            vterm_input_write(state->vterm, pos_seq, strlen(pos_seq));
            vterm_input_write(state->vterm, ANSI_ERASE_TO_EOL, strlen(ANSI_ERASE_TO_EOL));
        }
        /* Also clear all scroll area rows beyond new column count to remove any old content */
        for (int row = 1; row <= old_scrolling_rows; row++) {
            ansi_format_cursor_pos(pos_seq, sizeof(pos_seq), row, cols + 1);
            vterm_input_write(state->vterm, pos_seq, strlen(pos_seq));
            vterm_input_write(state->vterm, ANSI_ERASE_TO_EOL, strlen(ANSI_ERASE_TO_EOL));
        }
    }

    /* If scrolling region is shrinking (input area growing), scroll content up first */
    if (rows < old_scrolling_rows) {
        int lines_to_scroll = old_scrolling_rows - rows;
        /* Position cursor at bottom of current scrolling region */
        char pos_seq[32];
        ansi_format_cursor_pos(pos_seq, sizeof(pos_seq), old_scrolling_rows, 1);
        vterm_input_write(state->vterm, pos_seq, strlen(pos_seq));
        /* Insert newlines to scroll content up into scrollback */
        for (int i = 0; i < lines_to_scroll; i++) {
            vterm_input_write(state->vterm, "\n", 1);
        }
        /* Adjust saved cursor position for the scroll - content moved up */
        saved_cursor_row -= lines_to_scroll;
        if (saved_cursor_row < 0)
            saved_cursor_row = 0;
    }

    state->rows = total_rows;
    state->cols = cols;
    state->scrolling_rows = rows;
    state->input_row = rows;

    vterm_set_size(state->vterm, total_rows, cols);

    /* After resize, clear any remaining cells beyond new column count on all rows */
    if (cols < old_cols) {
        char pos_seq[32];
        /* Clear from column (cols+1) to end of line for all rows (scroll area + input area) */
        for (int row = 1; row <= total_rows; row++) {
            ansi_format_cursor_pos(pos_seq, sizeof(pos_seq), row, cols + 1);
            vterm_input_write(state->vterm, pos_seq, strlen(pos_seq));
            vterm_input_write(state->vterm, ANSI_ERASE_TO_EOL, strlen(ANSI_ERASE_TO_EOL));
        }
    }

    /* Clear old input area if terminal grew (input area shrank) */
    if (rows > old_scrolling_rows) {
        char clear_seq[64];
        /* Position at old separator row and clear to end of screen */
        int old_separator_row = old_scrolling_rows + 1; /* 1-indexed */
        ansi_format_cursor_pos(clear_seq, sizeof(clear_seq), old_separator_row, 1);
        vterm_input_write(state->vterm, clear_seq, strlen(clear_seq));
        vterm_input_write(state->vterm, ANSI_ERASE_TO_EOS, strlen(ANSI_ERASE_TO_EOS));
    }

    char seq[32];
    ansi_format_scroll_region(seq, sizeof(seq), 1, rows);
    vterm_input_write(state->vterm, seq, strlen(seq));
    /* DECSTBM moves cursor to (1,1) as documented side effect */

    /* Restore cursor to terminal content position */
    ansi_format_cursor_pos(seq, sizeof(seq), saved_cursor_row + 1, saved_cursor_col + 1);
    vterm_input_write(state->vterm, seq, strlen(seq));

    /* Re-enable cursor tracking and manually set terminal_cursor */
    state->ignore_cursor_tracking = 0;
    state->terminal_cursor_row = saved_cursor_row;
    state->terminal_cursor_col = saved_cursor_col;

    /* Don't flush damage here - let the main render loop handle it to avoid blocking during reflow */
    /* vterm_screen_flush_damage(state->screen); */

    VTermRect full_rect = {0, 0, total_rows, cols};
    damage(full_rect, state);
    state->needs_redraw = 1;
}

static void vterm_backend_get_size(void *vstate, int *rows, int *cols) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state || !rows || !cols)
        return;
    *rows = state->scrolling_rows; /* Return only scrolling area, not total rows */
    *cols = state->cols;
}

static void vterm_scroll_up(void *vstate, int lines) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state || lines <= 0)
        return;

    int max_offset = state->scrollback_size;
    int new_offset = state->viewport_offset + lines;
    if (new_offset > max_offset)
        new_offset = max_offset;

    if (new_offset != state->viewport_offset) {
        state->viewport_offset = new_offset;
        state->scroll_locked = 1; /* User scrolled back, lock scroll */
        state->needs_redraw = 1;
    }
}

static void vterm_scroll_down(void *vstate, int lines) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state || lines <= 0)
        return;

    int new_offset = state->viewport_offset - lines;
    if (new_offset < 0)
        new_offset = 0;

    if (new_offset != state->viewport_offset) {
        state->viewport_offset = new_offset;
        if (new_offset == 0) {
            state->scroll_locked = 0; /* Back at bottom, unlock scroll */
        }
        state->needs_redraw = 1;
    }
}

static void vterm_scroll_to_bottom(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state)
        return;

    state->scroll_locked = 0; /* Explicit scroll to bottom unlocks */
    if (state->viewport_offset != 0) {
        state->viewport_offset = 0;
        state->needs_redraw = 1;
    }
}

static int vterm_is_scroll_locked(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    return state ? state->scroll_locked : 0;
}

static int vterm_get_viewport_offset(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    return state ? state->viewport_offset : 0;
}

static int vterm_get_scrollback_size(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    return state ? state->scrollback_size : 0;
}

static int vterm_get_max_scrollback_lines(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    return state ? state->max_scrollback_lines : 0;
}

static void vterm_set_max_scrollback_lines(void *vstate, int max_lines) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state)
        return;

    if (max_lines < 0)
        max_lines = 0;

    state->max_scrollback_lines = max_lines;

    int effective_max = (max_lines > 0) ? max_lines : SCROLLBACK_MAX_LINES;
    while (state->scrollback_size > effective_max) {
        evict_oldest_scrollback_line(state);
    }
}

static int vterm_needs_redraw(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    return state ? state->needs_redraw : 0;
}

static void vterm_mark_drawn(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (state)
        state->needs_redraw = 0;
}

static void vterm_request_redraw(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (state)
        state->needs_redraw = 1;
}

static void vterm_get_cursor_info(void *vstate, int *row, int *col, int *visible) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state)
        return;
    if (row)
        *row = state->cursor_row;
    if (col)
        *col = state->cursor_col;
    if (visible)
        *visible = state->cursor_visible;
}

static void vterm_set_output_callback(void *vstate, void (*callback)(const char *s, size_t len, void *user),
                                      void *user) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state)
        return;
    state->output_callback_fn = callback;
    state->output_callback_user = user;
}

static void vterm_echo_local(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state || state->output_buffer_len <= state->output_buffer_echoed)
        return;

    state->echoing_locally = 1;

    /* Echo new bytes, expanding LF to CRLF for proper cursor return */
    size_t new_bytes = state->output_buffer_len - state->output_buffer_echoed;
    const char *src = state->output_buffer + state->output_buffer_echoed;

    /* Worst-case buffer: every byte is '\n' -> doubles size */
    size_t max_out = new_bytes * 2;
    dynamic_buffer_clear(state->normalize_buffer);
    if (dynamic_buffer_ensure_size(state->normalize_buffer, max_out) < 0) {
        state->echoing_locally = 0;
        return;
    }

    for (size_t i = 0; i < new_bytes; i++) {
        char c = src[i];
        int needs_cr = (c == '\n');
        /* Insert CR if this LF is not already preceded by CR (consider previous byte overall) */
        if (needs_cr) {
            char prev =
                (i == 0)
                    ? ((state->output_buffer_echoed > 0) ? state->output_buffer[state->output_buffer_echoed - 1] : '\0')
                    : src[i - 1];
            if (prev != '\r') {
                dynamic_buffer_append(state->normalize_buffer, "\r", 1);
            }
        }
        dynamic_buffer_append(state->normalize_buffer, &c, 1);
    }

    /* Position cursor at terminal content cursor before echoing */
    /* This ensures echo appears in the scrolling region, not input area */
    char pos_seq[32];
    ansi_format_cursor_pos(pos_seq, sizeof(pos_seq), state->terminal_cursor_row + 1, state->terminal_cursor_col + 1);
    vterm_input_write(state->vterm, pos_seq, strlen(pos_seq));

    vterm_input_write(state->vterm, dynamic_buffer_data(state->normalize_buffer),
                      dynamic_buffer_len(state->normalize_buffer));
    vterm_screen_flush_damage(state->screen);

    state->output_buffer_echoed = state->output_buffer_len;
    state->echoing_locally = 0;
}

static void vterm_clear_output_buffer(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state)
        return;
    state->output_buffer_len = 0;
    state->output_buffer_echoed = 0;
}

static void vterm_send_buffer(void *vstate, void *telnet) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state || !telnet || state->output_buffer_len == 0)
        return;

    /* Send buffered data to telnet server */
    Telnet *t = (Telnet *)telnet;
    telnet_send(t, state->output_buffer, state->output_buffer_len);

    /* Clear buffer */
    state->output_buffer_len = 0;
    state->output_buffer_echoed = 0;
}

/* Helper: Get byte range for a specific visual row */
static void get_visual_row_byte_range(const char *buffer, int length, int cols, int target_visual_row, int *out_start,
                                      int *out_end) {
    if (cols <= 0)
        cols = 80;

    int current_row = 0;
    int current_col = 0;
    int row_start = -1;

    /* If looking for row 0, it starts at position 0 */
    if (target_visual_row == 0) {
        row_start = 0;
    }

    for (int i = 0; i <= length; i++) {
        /* End of buffer */
        if (i == length) {
            if (current_row == target_visual_row) {
                /* We're on the target row at end of buffer */
                if (row_start == -1)
                    row_start = i;
                *out_start = row_start;
                *out_end = length;
            } else if (row_start >= 0) {
                /* We were on target row but now past it */
                *out_start = row_start;
                *out_end = i;
            } else {
                /* Never reached target row */
                *out_start = length;
                *out_end = length;
            }
            return;
        }

        /* Process character to update position */
        if (buffer[i] == '\n') {
            /* If we're on target row and encounter newline, end the row here (exclude newline) */
            if (current_row == target_visual_row && row_start >= 0) {
                *out_start = row_start;
                *out_end = i; /* Exclude the newline */
                return;
            }
            current_row++;
            current_col = 0;
            /* After processing newline, check if we've entered target row */
            if (current_row == target_visual_row && row_start == -1) {
                row_start = i + 1; /* Start after the newline */
            }
        } else {
            /* Check if we've entered the target row */
            if (current_row == target_visual_row && row_start == -1) {
                row_start = i;
            }

            current_col++;
            if (current_col >= cols) {
                /* Line wrapped - if we're on target row, end it here */
                if (current_row == target_visual_row && row_start >= 0) {
                    *out_start = row_start;
                    *out_end = i + 1; /* Include current character */
                    return;
                }
                current_row++;
                current_col = 0;
                /* After wrapping, check if we've entered target row */
                if (current_row == target_visual_row && row_start == -1) {
                    row_start = i + 1; /* Start after wrapped character */
                }
            }
        }
    }

    /* Shouldn't reach here, but handle it */
    *out_start = row_start >= 0 ? row_start : 0;
    *out_end = length;
}

static void vterm_render_input_area(void *vstate, void *input_area_ptr, int input_row, int cols, int connected) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    InputArea *input_area = (InputArea *)input_area_ptr;
    if (!state || !input_area)
        return;

    /* Reuse state's render buffer (clear it first) */
    DynamicBuffer *buf = state->render_buffer;
    dynamic_buffer_clear(buf);

    /* Note: We don't use DECSC/DECRC here because DECSTBM below resets cursor to (1,1)
     * and DECRC doesn't reliably restore after that. Instead, we explicitly position
     * cursor at terminal_cursor_row/col at the end. */

    /* Get divider colors based on connection status from Lisp config */
    int divider_r, divider_g, divider_b;
    if (connected) {
        lisp_x_get_divider_connected_color(&divider_r, &divider_g, &divider_b);
    } else {
        lisp_x_get_divider_disconnected_color(&divider_r, &divider_g, &divider_b);
    }

    /* Get terminal background color for divider background */
    int term_bg_r, term_bg_g, term_bg_b;
    lisp_x_get_terminal_bg_color(&term_bg_r, &term_bg_g, &term_bg_b);

    /* Use vterm's scrolling region width (state->cols) if available, otherwise use cols param */
    int actual_cols = (state->cols > 0) ? state->cols : cols;

    /* Get input area state for multi-row rendering */
    const char *text = input_area_get_text(input_area);
    int text_len = input_area_get_length(input_area);
    int visible_rows = input_area_get_visible_rows(input_area);

    /* Calculate row positions (1-indexed) */
    int top_divider_row = input_row + 1;
    int input_text_start_row = input_row + 2;
    int bottom_divider_row = input_text_start_row + visible_rows;

    /* Clear all divider and input rows completely before redrawing */
    /* This is critical when columns decrease - old divider cells beyond new column count must be cleared */
    char cursor_buf[16];

    /* Clear all divider and input rows completely */
    for (int row = top_divider_row; row <= bottom_divider_row; row++) {
        /* Clear entire row from column 1 to end of line */
        ansi_format_cursor_pos(cursor_buf, sizeof(cursor_buf), row, 1);
        dynamic_buffer_append_str(buf, cursor_buf);
        dynamic_buffer_append_str(buf, ANSI_ERASE_TO_EOL);
    }

    /* Also clear from bottom divider to end of screen to remove any old content below */
    if (bottom_divider_row < state->rows) {
        ansi_format_cursor_pos(cursor_buf, sizeof(cursor_buf), bottom_divider_row + 1, 1);
        dynamic_buffer_append_str(buf, cursor_buf);
        dynamic_buffer_append_str(buf, ANSI_ERASE_TO_EOS);
    }

    /* Draw top divider - colored box drawing character */
    /* Position cursor at top divider row, column 1 */
    ansi_format_cursor_pos(cursor_buf, sizeof(cursor_buf), top_divider_row, 1);
    dynamic_buffer_append_str(buf, cursor_buf);

    char color_buf[32];
    ansi_format_fg_color_rgb(color_buf, sizeof(color_buf), divider_r, divider_g, divider_b);
    dynamic_buffer_append_str(buf, color_buf);
    ansi_format_bg_color_rgb(color_buf, sizeof(color_buf), term_bg_r, term_bg_g, term_bg_b);
    dynamic_buffer_append_str(buf, color_buf);

    /* Draw box drawing character (U+2500 = horizontal line) for each column */
    /* UTF-8 encoding: 0xE2 0x94 0x80 */
    char box_char[4] = {(char)0xE2, (char)0x94, (char)0x80, '\0'};
    for (int i = 0; i < actual_cols; i++) {
        dynamic_buffer_append(buf, box_char, 3);
    }

    /* Add [EVAL] indicator if in eval mode */
    if (input_area_get_mode(input_area) == INPUT_AREA_MODE_EVAL) {
        const char *eval_text = " [EVAL] ";
        int eval_len = 8;

        /* Only show if wide enough */
        if (actual_cols >= 10) {
            int indicator_col = actual_cols - eval_len + 1;
            ansi_format_cursor_pos(cursor_buf, sizeof(cursor_buf), top_divider_row, indicator_col);
            dynamic_buffer_append_str(buf, cursor_buf);

            /* Same colors as divider */
            ansi_format_fg_color_rgb(color_buf, sizeof(color_buf), divider_r, divider_g, divider_b);
            dynamic_buffer_append_str(buf, color_buf);
            ansi_format_bg_color_rgb(color_buf, sizeof(color_buf), term_bg_r, term_bg_g, term_bg_b);
            dynamic_buffer_append_str(buf, color_buf);

            dynamic_buffer_append_str(buf, eval_text);
        }
    }

    /* Get input area colors - use default terminal colors */
    int fg_r, fg_g, fg_b, bg_r, bg_g, bg_b;
    lisp_x_get_terminal_fg_color(&fg_r, &fg_g, &fg_b);
    lisp_x_get_terminal_bg_color(&bg_r, &bg_g, &bg_b);

    /* Get input area state for multi-row rendering */
    int scroll_offset = input_area->scroll_offset;
    int sel_start = 0, sel_end = 0;
    int has_sel = input_area_has_selection(input_area);
    if (has_sel)
        input_area_get_selection_range(input_area, &sel_start, &sel_end);

    /* Render each visible row */
    for (int vis_row = 0; vis_row < visible_rows; vis_row++) {
        int visual_row = scroll_offset + vis_row;
        /* Position cursor at start of this absolute row */
        int absolute_row = input_text_start_row + vis_row;
        ansi_format_cursor_pos(cursor_buf, sizeof(cursor_buf), absolute_row, 1);
        dynamic_buffer_append_str(buf, cursor_buf);

        /* Set colors for this row AFTER positioning */
        ansi_format_fg_color_rgb(color_buf, sizeof(color_buf), fg_r, fg_g, fg_b);
        dynamic_buffer_append_str(buf, color_buf);
        ansi_format_bg_color_rgb(color_buf, sizeof(color_buf), bg_r, bg_g, bg_b);
        dynamic_buffer_append_str(buf, color_buf);

        /* Erase line to fill with current background color */
        dynamic_buffer_append_str(buf, "\x1b[2K"); /* EL2 - Erase entire line */

        /* Get byte range for this visual row */
        int row_start, row_end;
        get_visual_row_byte_range(text, text_len, actual_cols, visual_row, &row_start, &row_end);

        /* Render characters in this row with selection highlighting */
        for (int i = row_start; i < row_end; i++) {
            if (has_sel && i == sel_start) {
                dynamic_buffer_append_str(buf, ANSI_SGR_REVERSE);
            }
            if (has_sel && i == sel_end) {
                /* Turn off reverse video after selection */
                dynamic_buffer_append_str(buf, ANSI_SGR_REVERSE_OFF);
            }

            /* Skip newline characters (they mark end of line, don't render) */
            if (text[i] != '\n') {
                dynamic_buffer_append(buf, &text[i], 1);
            }
        }
        /* Turn off selection if it extends past this row */
        if (has_sel && sel_end > row_start && sel_end <= row_end) {
            dynamic_buffer_append_str(buf, ANSI_SGR_REVERSE_OFF);
        }
    }

    /* Draw bottom divider - colored box drawing character */
    ansi_format_cursor_pos(cursor_buf, sizeof(cursor_buf), bottom_divider_row, 1);
    dynamic_buffer_append_str(buf, cursor_buf);
    ansi_format_fg_color_rgb(color_buf, sizeof(color_buf), divider_r, divider_g, divider_b);
    dynamic_buffer_append_str(buf, color_buf);
    ansi_format_bg_color_rgb(color_buf, sizeof(color_buf), term_bg_r, term_bg_g, term_bg_b);
    dynamic_buffer_append_str(buf, color_buf);

    /* Draw box drawing character (U+2500 = horizontal line) for each column */
    for (int i = 0; i < actual_cols; i++) {
        dynamic_buffer_append(buf, box_char, 3);
    }

    /* Reset colors after drawing divider to prevent green color from persisting */
    dynamic_buffer_append_str(buf, SGR_RESET);

    /* Restore scrolling region */
    char scroll_buf[24];
    ansi_format_scroll_region(scroll_buf, sizeof(scroll_buf), 1, state->scrolling_rows);
    dynamic_buffer_append_str(buf, scroll_buf);

    /* Explicitly restore cursor to terminal content position */
    /* DECSTBM above moves cursor to (1,1), so we must reposition explicitly */
    char pos_buf[32];
    ansi_format_cursor_pos(pos_buf, sizeof(pos_buf), state->terminal_cursor_row + 1, state->terminal_cursor_col + 1);
    dynamic_buffer_append_str(buf, pos_buf);

    /* Set flags to prevent output callback from buffering and cursor tracking corruption */
    state->echoing_locally = 1;
    state->ignore_cursor_tracking = 1;

    /* Send all escape sequences to vterm */
    vterm_input_write(state->vterm, dynamic_buffer_data(buf), dynamic_buffer_len(buf));
    /* Flush damage to ensure all rows are processed */
    vterm_screen_flush_damage(state->screen);
    state->needs_redraw = 1;

    /* Clear flags after rendering */
    state->echoing_locally = 0;
    state->ignore_cursor_tracking = 0;
}

static const char *vterm_get_version(void) {
    return LIBVTERM_VERSION;
}

static void *vterm_get_vterm(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    return state ? state->vterm : NULL;
}

static void *vterm_get_vterm_screen(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    return state ? state->screen : NULL;
}

/* Backend registration */
const TerminalBackend terminal_backend_vterm = {
    .create = vterm_create,
    .destroy = vterm_destroy,
    .feed_data = vterm_feed_data,
    .get_cell_at = vterm_get_cell_at,
    .get_cell_at_scrollback_index = vterm_get_cell_at_scrollback_index,
    .resize = vterm_resize,
    .get_size = vterm_backend_get_size,
    .scroll_up = vterm_scroll_up,
    .scroll_down = vterm_scroll_down,
    .scroll_to_bottom = vterm_scroll_to_bottom,
    .is_scroll_locked = vterm_is_scroll_locked,
    .get_viewport_offset = vterm_get_viewport_offset,
    .get_scrollback_size = vterm_get_scrollback_size,
    .get_max_scrollback_lines = vterm_get_max_scrollback_lines,
    .set_max_scrollback_lines = vterm_set_max_scrollback_lines,
    .needs_redraw = vterm_needs_redraw,
    .mark_drawn = vterm_mark_drawn,
    .request_redraw = vterm_request_redraw,
    .get_cursor_info = vterm_get_cursor_info,
    .set_output_callback = vterm_set_output_callback,
    .echo_local = vterm_echo_local,
    .send_buffer = vterm_send_buffer,
    .clear_output_buffer = vterm_clear_output_buffer,
    .render_input_area = vterm_render_input_area,
    .get_version = vterm_get_version,
    .get_vterm = vterm_get_vterm,
    .get_vterm_screen = vterm_get_vterm_screen,
};
