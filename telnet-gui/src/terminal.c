/* Terminal emulation using libvterm */

#include "terminal.h"
#include "telnet.h"
#include <stdlib.h>
#include <string.h>

#define OUTPUT_BUFFER_INITIAL_SIZE 4096

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

static int resize(int rows, int cols, void *user) {
    Terminal *term = (Terminal *)user;
    if (!term)
        return 0;
    term->rows = rows;
    term->cols = cols;
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

    /* Set up screen callbacks - screen layer automatically sets up state callbacks */
    /* Keep callbacks in Terminal struct so they persist */
    /* Initialize to zero first, then set callbacks (follows demo/main.c exactly) */
    memset(&term->callbacks, 0, sizeof(term->callbacks));
    term->callbacks.damage = damage;
    term->callbacks.movecursor = screen_movecursor;
    term->callbacks.settermprop = settermprop;
    term->callbacks.bell = bell;
    term->callbacks.resize = resize;

    /* Set callbacks FIRST (like demo/main.c) */
    vterm_screen_set_callbacks(term->screen, &term->callbacks, term);

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
