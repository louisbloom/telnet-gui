/* Terminal emulation using libvterm */

#include "terminal.h"
#include "telnet.h"
#include <stdlib.h>
#include <string.h>

#define OUTPUT_BUFFER_INITIAL_SIZE 4096
#define SCROLLBACK_MAX_LINES 10000

/* Scrollback line storage */
typedef struct {
    VTermScreenCell *cells; /* Array of cells for this line */
    int cols;               /* Number of columns when line was stored */
    bool continuation;      /* Whether this line is a continuation of previous line */
} ScrollbackLine;

struct Terminal {
    VTerm *vterm;
    VTermScreen *screen;
    int rows, cols;
    int needs_redraw;
    VTermScreenCallbacks callbacks; /* Keep callbacks in struct so they don't go out of scope */
    Telnet *telnet;                 /* Telnet connection for sending output */
    char *output_buffer;            /* Buffer for line input before sending */
    size_t output_buffer_size;      /* Allocated size of output buffer */
    size_t output_buffer_len;       /* Current length of data in buffer */
    size_t output_buffer_echoed;    /* Number of bytes already echoed locally */
    int echoing_locally;            /* Flag to prevent output callback during local echo */
    /* Scrollback support */
    ScrollbackLine *scrollback; /* Array of scrollback lines */
    int scrollback_size;        /* Current number of lines in scrollback */
    int scrollback_capacity;    /* Maximum capacity of scrollback buffer */
    int viewport_offset;        /* Number of lines scrolled up (0 = showing current screen) */
};

static int damage(VTermRect rect, void *user) {
    (void)rect;
    Terminal *term = (Terminal *)user;
    if (!term)
        return 0;
    term->needs_redraw = 1;
    return 1;
}

/* Screen layer movecursor callback */
static int screen_movecursor(VTermPos pos, VTermPos oldpos, int visible, void *user) {
    (void)pos;
    (void)oldpos;
    (void)visible;
    Terminal *term = (Terminal *)user;
    if (!term)
        return 0;
    term->needs_redraw = 1;
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

/* Scrollback callbacks */
static int sb_pushline(int cols, const VTermScreenCell *cells, void *user) {
    Terminal *term = (Terminal *)user;
    if (!term || !cells)
        return 0;

    /* Grow scrollback buffer if needed */
    if (term->scrollback_size >= term->scrollback_capacity) {
        int new_capacity = term->scrollback_capacity == 0 ? 100 : term->scrollback_capacity * 2;
        if (new_capacity > SCROLLBACK_MAX_LINES)
            new_capacity = SCROLLBACK_MAX_LINES;
        if (new_capacity <= term->scrollback_capacity)
            return 0; /* Already at max capacity */

        ScrollbackLine *new_scrollback = realloc(term->scrollback, new_capacity * sizeof(ScrollbackLine));
        if (!new_scrollback)
            return 0; /* Out of memory */
        term->scrollback = new_scrollback;
        term->scrollback_capacity = new_capacity;
    }

    /* Allocate cells for this line */
    ScrollbackLine *line = &term->scrollback[term->scrollback_size];
    line->cells = malloc(cols * sizeof(VTermScreenCell));
    if (!line->cells)
        return 0; /* Out of memory */

    /* Copy cells */
    memcpy(line->cells, cells, cols * sizeof(VTermScreenCell));
    line->cols = cols;
    line->continuation = false; /* Will be set by sb_pushline4 if available */

    term->scrollback_size++;
    return 1;
}

static int sb_pushline4(int cols, const VTermScreenCell *cells, bool continuation, void *user) {
    Terminal *term = (Terminal *)user;
    if (!term || !cells)
        return 0;

    /* Grow scrollback buffer if needed */
    if (term->scrollback_size >= term->scrollback_capacity) {
        int new_capacity = term->scrollback_capacity == 0 ? 100 : term->scrollback_capacity * 2;
        if (new_capacity > SCROLLBACK_MAX_LINES)
            new_capacity = SCROLLBACK_MAX_LINES;
        if (new_capacity <= term->scrollback_capacity)
            return 0; /* Already at max capacity */

        ScrollbackLine *new_scrollback = realloc(term->scrollback, new_capacity * sizeof(ScrollbackLine));
        if (!new_scrollback)
            return 0; /* Out of memory */
        term->scrollback = new_scrollback;
        term->scrollback_capacity = new_capacity;
    }

    /* Allocate cells for this line */
    ScrollbackLine *line = &term->scrollback[term->scrollback_size];
    line->cells = malloc(cols * sizeof(VTermScreenCell));
    if (!line->cells)
        return 0; /* Out of memory */

    /* Copy cells */
    memcpy(line->cells, cells, cols * sizeof(VTermScreenCell));
    line->cols = cols;
    line->continuation = continuation;

    term->scrollback_size++;
    return 1;
}

static int sb_popline(int cols, VTermScreenCell *cells, void *user) {
    Terminal *term = (Terminal *)user;
    if (!term || !cells || term->scrollback_size == 0)
        return 0;

    /* Get the last line from scrollback */
    ScrollbackLine *line = &term->scrollback[term->scrollback_size - 1];
    int copy_cols = line->cols < cols ? line->cols : cols;

    /* Copy cells */
    memcpy(cells, line->cells, copy_cols * sizeof(VTermScreenCell));
    /* Fill remaining columns with empty cells if needed */
    if (copy_cols < cols) {
        VTermScreenCell empty = {0};
        for (int i = copy_cols; i < cols; i++)
            cells[i] = empty;
    }

    /* Free and remove line */
    free(line->cells);
    term->scrollback_size--;

    return 1;
}

static int sb_clear(void *user) {
    Terminal *term = (Terminal *)user;
    if (!term)
        return 0;

    /* Free all scrollback lines */
    for (int i = 0; i < term->scrollback_size; i++) {
        free(term->scrollback[i].cells);
    }
    free(term->scrollback);
    term->scrollback = NULL;
    term->scrollback_size = 0;
    term->scrollback_capacity = 0;
    term->viewport_offset = 0; /* Reset viewport when clearing scrollback */

    return 1;
}

static int resize(int rows, int cols, void *user) {
    Terminal *term = (Terminal *)user;
    if (!term)
        return 0;
    term->rows = rows;
    term->cols = cols;
    term->needs_redraw = 1; /* Ensure redraw is triggered after resize */
    return 1;
}

/* Output callback - buffers data from libvterm for telnet transmission */
static void output_callback(const char *s, size_t len, void *user) {
    Terminal *term = (Terminal *)user;
    if (!term || !s || len == 0)
        return;

    /* Skip buffering if we're doing local echo (to avoid double buffering) */
    if (term->echoing_locally)
        return;

    /* Grow buffer if needed */
    while (term->output_buffer_len + len > term->output_buffer_size) {
        size_t new_size = term->output_buffer_size * 2;
        char *new_buffer = realloc(term->output_buffer, new_size);
        if (!new_buffer)
            return; /* Out of memory */
        term->output_buffer = new_buffer;
        term->output_buffer_size = new_size;
    }

    /* Append data to buffer */
    memcpy(term->output_buffer + term->output_buffer_len, s, len);
    term->output_buffer_len += len;
}

Terminal *terminal_create(int rows, int cols) {
    Terminal *term = (Terminal *)malloc(sizeof(Terminal));
    if (!term)
        return NULL;

    /* Check libvterm version */
    vterm_check_version(0, 3);

    term->vterm = vterm_new(rows, cols);
    if (!term->vterm) {
        free(term);
        return NULL;
    }

    /* Set UTF-8 mode BEFORE getting screen */
    vterm_set_utf8(term->vterm, 1);

    /* Initialize output buffer before setting callback */
    term->output_buffer_size = OUTPUT_BUFFER_INITIAL_SIZE;
    term->output_buffer = malloc(term->output_buffer_size);
    if (!term->output_buffer) {
        vterm_free(term->vterm);
        free(term);
        return NULL;
    }
    term->output_buffer_len = 0;

    /* Set output callback BEFORE getting screen (follows libvterm demo/main.c) */
    vterm_output_set_callback(term->vterm, output_callback, term);

    term->screen = vterm_obtain_screen(term->vterm);
    if (!term->screen) {
        free(term->output_buffer);
        vterm_free(term->vterm);
        free(term);
        return NULL;
    }

    term->rows = rows;
    term->cols = cols;
    term->needs_redraw = 1;
    term->telnet = NULL;
    term->output_buffer_echoed = 0;
    term->echoing_locally = 0;
    /* Initialize scrollback */
    term->scrollback = NULL;
    term->scrollback_size = 0;
    term->scrollback_capacity = 0;
    term->viewport_offset = 0;

    /* Set up screen callbacks - screen layer automatically sets up state callbacks */
    /* Keep callbacks in Terminal struct so they persist */
    /* Initialize to zero first, then set callbacks (follows demo/main.c exactly) */
    memset(&term->callbacks, 0, sizeof(term->callbacks));
    term->callbacks.damage = damage;
    term->callbacks.movecursor = screen_movecursor;
    term->callbacks.settermprop = settermprop;
    term->callbacks.bell = bell;
    term->callbacks.resize = resize;
    term->callbacks.sb_pushline = sb_pushline;
    term->callbacks.sb_popline = sb_popline;
    term->callbacks.sb_clear = sb_clear;
    term->callbacks.sb_pushline4 = sb_pushline4;

    /* Set callbacks FIRST (like demo/main.c) */
    vterm_screen_set_callbacks(term->screen, &term->callbacks, term);

    /* Enable pushline4 callback support */
    vterm_screen_callbacks_has_pushline4(term->screen);

    /* Enable text reflow so text wraps when terminal is resized */
    vterm_screen_enable_reflow(term->screen, 1);

    /* Reset screen (hard reset) - this calls vterm_state_reset internally */
    /* State reset initializes encoding arrays using UTF-8 mode */
    vterm_screen_reset(term->screen, 1);

    return term;
}

void terminal_destroy(Terminal *term) {
    if (!term)
        return;
    if (term->output_buffer)
        free(term->output_buffer);
    /* Free scrollback */
    if (term->scrollback) {
        for (int i = 0; i < term->scrollback_size; i++) {
            free(term->scrollback[i].cells);
        }
        free(term->scrollback);
    }
    if (term->vterm)
        vterm_free(term->vterm);
    free(term);
}

void terminal_feed_data(Terminal *term, const char *data, size_t len) {
    if (!term || !data)
        return;
    vterm_input_write(term->vterm, data, len);
    /* Flush screen damage to trigger callbacks (demo/main.c does this) */
    vterm_screen_flush_damage(term->screen);
}

/* Echo local input to screen for immediate feedback */
void terminal_echo_local(Terminal *term) {
    if (!term || term->output_buffer_len <= term->output_buffer_echoed)
        return;

    /* Disable output callback during local echo to avoid double buffering */
    term->echoing_locally = 1;

    /* Feed only newly added bytes back to libvterm for rendering */
    size_t new_bytes = term->output_buffer_len - term->output_buffer_echoed;
    vterm_input_write(term->vterm, term->output_buffer + term->output_buffer_echoed, new_bytes);
    vterm_screen_flush_damage(term->screen);

    /* Update echoed counter */
    term->output_buffer_echoed = term->output_buffer_len;

    /* Re-enable output callback */
    term->echoing_locally = 0;
}

VTerm *terminal_get_vterm(Terminal *term) {
    return term ? term->vterm : NULL;
}

VTermScreen *terminal_get_screen(Terminal *term) {
    return term ? term->screen : NULL;
}

void terminal_resize(Terminal *term, int rows, int cols) {
    if (!term)
        return;
    term->rows = rows;
    term->cols = cols;
    vterm_set_size(term->vterm, rows, cols);
    /* Flush damage to trigger libvterm's text reflow and damage callbacks */
    vterm_screen_flush_damage(term->screen);
    /* Force full screen damage to ensure entire screen is redrawn */
    VTermRect full_rect = {0, 0, rows, cols};
    damage(full_rect, term);
    term->needs_redraw = 1;
}

void terminal_get_size(Terminal *term, int *rows, int *cols) {
    if (!term || !rows || !cols)
        return;
    *rows = term->rows;
    *cols = term->cols;
}

int terminal_needs_redraw(Terminal *term) {
    if (!term)
        return 0;
    return term->needs_redraw;
}

void terminal_mark_drawn(Terminal *term) {
    if (term)
        term->needs_redraw = 0;
}

void terminal_set_telnet(Terminal *term, void *telnet) {
    if (term)
        term->telnet = (Telnet *)telnet;
}

void terminal_send_buffer(Terminal *term) {
    if (!term || !term->telnet || term->output_buffer_len == 0)
        return;

    /* Send buffered data to telnet server */
    telnet_send(term->telnet, term->output_buffer, term->output_buffer_len);

    /* Clear buffer */
    term->output_buffer_len = 0;
    term->output_buffer_echoed = 0;
}

/* Scroll viewport up by N lines */
void terminal_scroll_up(Terminal *term, int lines) {
    if (!term || lines <= 0)
        return;

    /* Calculate maximum scrollable lines */
    int max_offset = term->scrollback_size;
    int new_offset = term->viewport_offset + lines;
    if (new_offset > max_offset)
        new_offset = max_offset;

    if (new_offset != term->viewport_offset) {
        term->viewport_offset = new_offset;
        term->needs_redraw = 1;
    }
}

/* Scroll viewport down by N lines */
void terminal_scroll_down(Terminal *term, int lines) {
    if (!term || lines <= 0)
        return;

    int new_offset = term->viewport_offset - lines;
    if (new_offset < 0)
        new_offset = 0;

    if (new_offset != term->viewport_offset) {
        term->viewport_offset = new_offset;
        term->needs_redraw = 1;
    }
}

/* Scroll to bottom (show current screen) */
void terminal_scroll_to_bottom(Terminal *term) {
    if (!term)
        return;

    if (term->viewport_offset != 0) {
        term->viewport_offset = 0;
        term->needs_redraw = 1;
    }
}

/* Get viewport offset */
int terminal_get_viewport_offset(Terminal *term) {
    return term ? term->viewport_offset : 0;
}

/* Get scrollback size */
int terminal_get_scrollback_size(Terminal *term) {
    return term ? term->scrollback_size : 0;
}

/* Get cell at position considering viewport offset */
int terminal_get_cell_at(Terminal *term, int row, int col, VTermScreenCell *cell) {
    if (!term || !cell)
        return 0;

    int viewport_offset = term->viewport_offset;
    int rows, cols;
    terminal_get_size(term, &rows, &cols);

    if (viewport_offset == 0) {
        /* Normal mode: get from screen */
        VTermPos pos = {row, col};
        return vterm_screen_get_cell(term->screen, pos, cell);
    } else {
        /* Scrolled mode: get from scrollback or screen */
        /* viewport_offset is how many lines we've scrolled up from current screen */
        /* If viewport_offset <= row, we're showing scrollback */
        /* If viewport_offset > row, we're showing screen */
        if (row < viewport_offset) {
            /* Get from scrollback */
            int scrollback_index = term->scrollback_size - viewport_offset + row;
            if (scrollback_index >= 0 && scrollback_index < term->scrollback_size) {
                ScrollbackLine *line = &term->scrollback[scrollback_index];
                if (col < line->cols) {
                    *cell = line->cells[col];
                    return 1;
                } else {
                    /* Empty cell if beyond line width */
                    memset(cell, 0, sizeof(VTermScreenCell));
                    return 1;
                }
            } else {
                /* Empty cell if beyond scrollback */
                memset(cell, 0, sizeof(VTermScreenCell));
                return 1;
            }
        } else {
            /* Get from screen */
            int screen_row = row - viewport_offset;
            if (screen_row >= 0 && screen_row < rows) {
                VTermPos pos = {screen_row, col};
                return vterm_screen_get_cell(term->screen, pos, cell);
            } else {
                /* Empty cell */
                memset(cell, 0, sizeof(VTermScreenCell));
                return 1;
            }
        }
    }
}
