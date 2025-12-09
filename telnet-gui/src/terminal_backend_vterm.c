/* VTerm backend implementation - libvterm-based terminal emulation */

#include "terminal_backend.h"
#include "term_cell.h"
#include "input_area.h"
#include "telnet.h"
#include "dynamic_buffer.h"
#include <vterm.h>
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

    /* Scrollback */
    ScrollbackLine *scrollback;
    int scrollback_size;
    int scrollback_start;
    int scrollback_capacity;
    int max_scrollback_lines;
    int viewport_offset;

    /* Cursor tracking */
    int cursor_row;
    int cursor_col;
    int cursor_visible;
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
    if (VTERM_COLOR_IS_DEFAULT_FG(vcolor) || VTERM_COLOR_IS_DEFAULT_BG(vcolor)) {
        tcolor->type = TERM_COLOR_DEFAULT;
    } else if (VTERM_COLOR_IS_INDEXED(vcolor)) {
        tcolor->type = TERM_COLOR_INDEXED;
        tcolor->color.idx = vcolor->indexed.idx;
    } else if (VTERM_COLOR_IS_RGB(vcolor)) {
        tcolor->type = TERM_COLOR_RGB;
        tcolor->color.rgb.r = vcolor->rgb.red;
        tcolor->color.rgb.g = vcolor->rgb.green;
        tcolor->color.rgb.b = vcolor->rgb.blue;
    } else {
        tcolor->type = TERM_COLOR_DEFAULT;
    }
}

/* Convert VTermScreenCell to generic TermCell */
static void vterm_cell_to_term_cell(const VTermScreenCell *vcell, TermCell *tcell) {
    memcpy(tcell->chars, vcell->chars, sizeof(tcell->chars));

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

    vterm_output_set_callback(state->vterm, output_callback, state);

    state->screen = vterm_obtain_screen(state->vterm);
    if (!state->screen) {
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

    /* Initialize cursor tracking */
    state->cursor_row = 0;
    state->cursor_col = 0;
    state->cursor_visible = 1;

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
    snprintf(seq, sizeof(seq), "\033[%d;%dr", 1, rows);
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
    vterm_input_write(state->vterm, data, len);
    vterm_screen_flush_damage(state->screen);
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

static void vterm_resize(void *vstate, int rows, int cols) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state)
        return;

    int total_rows = rows + 2;
    state->rows = total_rows;
    state->cols = cols;
    state->scrolling_rows = rows;
    state->input_row = rows;

    vterm_set_size(state->vterm, total_rows, cols);

    char seq[32];
    snprintf(seq, sizeof(seq), "\033[%d;%dr", 1, rows);
    vterm_input_write(state->vterm, seq, strlen(seq));
    vterm_screen_flush_damage(state->screen);

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
        state->needs_redraw = 1;
    }
}

static void vterm_scroll_to_bottom(void *vstate) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    if (!state)
        return;

    if (state->viewport_offset != 0) {
        state->viewport_offset = 0;
        state->needs_redraw = 1;
    }
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

    size_t new_bytes = state->output_buffer_len - state->output_buffer_echoed;
    vterm_input_write(state->vterm, state->output_buffer + state->output_buffer_echoed, new_bytes);
    vterm_screen_flush_damage(state->screen);

    state->output_buffer_echoed = state->output_buffer_len;
    state->echoing_locally = 0;
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

static void vterm_render_input_area(void *vstate, void *input_area_ptr, int input_row, int cols) {
    VTermBackendState *state = (VTermBackendState *)vstate;
    InputArea *input_area = (InputArea *)input_area_ptr;
    if (!state || !input_area)
        return;

    /* Reuse state's render buffer (clear it first) */
    DynamicBuffer *buf = state->render_buffer;
    dynamic_buffer_clear(buf);

    /* Save cursor position so telnet input can restore it for correct echo positioning */
    dynamic_buffer_append_str(buf, "\0337"); /* DECSC - Save Cursor */

    int separator_row_1indexed = input_row + 1;
    int input_text_row_1indexed = separator_row_1indexed + 1;

    /* Position cursor and clear line for separator */
    dynamic_buffer_append_printf(buf, "\033[%d;1H\033[K", separator_row_1indexed);

    /* Draw separator line - reverse video */
    dynamic_buffer_append_str(buf, "\033[7m");

    /* Use vterm's scrolling region width (state->cols) if available, otherwise use cols param */
    int actual_cols = (state->cols > 0) ? state->cols : cols;

    /* Draw box drawing character (U+2500 = horizontal line) for each column */
    /* UTF-8 encoding: 0xE2 0x94 0x80 */
    char box_char[4] = {(char)0xE2, (char)0x94, (char)0x80, '\0'};
    for (int i = 0; i < actual_cols; i++) {
        dynamic_buffer_append(buf, box_char, 3);
    }

    /* Normal video */
    dynamic_buffer_append_str(buf, "\033[27m");

    /* Position cursor for input text and clear line */
    dynamic_buffer_append_printf(buf, "\033[%d;1H\033[K", input_text_row_1indexed);

    /* Render input text with selection highlighting */
    const char *text = input_area_get_text(input_area);
    int text_len = input_area_get_length(input_area);
    int sel_start = 0, sel_end = 0;
    int has_sel = input_area_has_selection(input_area);
    if (has_sel)
        input_area_get_selection_range(input_area, &sel_start, &sel_end);

    for (int i = 0; i < text_len; i++) {
        if (has_sel && i == sel_start) {
            dynamic_buffer_append_str(buf, "\033[7m");
        }
        if (has_sel && i == sel_end) {
            dynamic_buffer_append_str(buf, "\033[27m");
        }
        dynamic_buffer_append(buf, &text[i], 1);
    }
    if (has_sel && sel_end >= text_len) {
        dynamic_buffer_append_str(buf, "\033[27m");
    }

    /* Re-establish scrolling region to ensure input area rows stay fixed */
    dynamic_buffer_append_printf(buf, "\033[1;%dr", state->scrolling_rows);

    /* Restore cursor position so telnet input echoes at the correct location */
    dynamic_buffer_append_str(buf, "\0338"); /* DECRC - Restore Cursor */

    /* Send all escape sequences to vterm */
    vterm_input_write(state->vterm, dynamic_buffer_data(buf), dynamic_buffer_len(buf));
    /* Note: Don't flush damage here - let the main render loop handle it to avoid blocking */
    state->needs_redraw = 1;
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
    .render_input_area = vterm_render_input_area,
    .get_version = vterm_get_version,
    .get_vterm = vterm_get_vterm,
    .get_vterm_screen = vterm_get_vterm_screen,
};
