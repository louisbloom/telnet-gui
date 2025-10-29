/* Terminal emulation using libvterm */

#include "terminal.h"
#include <stdlib.h>
#include <string.h>

struct Terminal {
    VTerm *vterm;
    VTermScreen *screen;
    int rows, cols;
    int needs_redraw;
    VTermScreenCallbacks callbacks; /* Keep callbacks in struct so they don't go out of scope */
};

static int damage(VTermRect rect, void *user) {
    Terminal *term = (Terminal *)user;
    if (!term)
        return 0;
    term->needs_redraw = 1;
    return 1;
}

/* Screen layer movecursor callback */
static int screen_movecursor(VTermPos pos, VTermPos oldpos, int visible, void *user) {
    Terminal *term = (Terminal *)user;
    if (!term)
        return 0;
    term->needs_redraw = 1;
    return 1;
}

/* State layer movecursor callback - same signature */
static int state_movecursor(VTermPos pos, VTermPos oldpos, int visible, void *user) {
    Terminal *term = (Terminal *)user;
    if (!term)
        return 0;
    term->needs_redraw = 1;
    return 1;
}

static int settermprop(VTermProp prop, VTermValue *val, void *user) {
    return 1;
}

static int bell(void *user) {
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

static int putglyph(VTermGlyphInfo *info, VTermPos pos, void *user) {
    Terminal *term = (Terminal *)user;
    if (!term)
        return 0;
    term->needs_redraw = 1;
    return 1;
}

static int scrollrect(VTermRect rect, int downward, int rightward, void *user) {
    Terminal *term = (Terminal *)user;
    if (!term)
        return 0;
    term->needs_redraw = 1;
    return 1;
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

    term->screen = vterm_obtain_screen(term->vterm);
    if (!term->screen) {
        vterm_free(term->vterm);
        free(term);
        return NULL;
    }

    term->rows = rows;
    term->cols = cols;
    term->needs_redraw = 1;

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
