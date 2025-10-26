/* Terminal emulation using libvterm */

#include "terminal.h"
#include <stdlib.h>
#include <string.h>

struct Terminal {
    VTerm *vterm;
    VTermScreen *screen;
    int rows, cols;
    int needs_redraw;
};

static int damage(VTermRect rect, void *user) {
    Terminal *term = (Terminal *)user;
    term->needs_redraw = 1;
    return 1;
}

static int movecursor(VTermPos pos, VTermPos oldpos, int visible, void *user) {
    Terminal *term = (Terminal *)user;
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
    term->rows = rows;
    term->cols = cols;
    return 1;
}

Terminal *terminal_create(int rows, int cols) {
    Terminal *term = (Terminal *)malloc(sizeof(Terminal));
    if (!term)
        return NULL;

    term->vterm = vterm_new(rows, cols);
    if (!term->vterm) {
        free(term);
        return NULL;
    }

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

    /* Set up callbacks */
    VTermScreenCallbacks callbacks = {0};
    callbacks.damage = damage;
    callbacks.movecursor = movecursor;
    callbacks.settermprop = settermprop;
    callbacks.bell = bell;
    callbacks.resize = resize;

    vterm_screen_set_callbacks(term->screen, &callbacks, term);

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
