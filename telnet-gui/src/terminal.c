/* Terminal emulation - backend delegation */

#include "terminal.h"
#include "terminal_backend.h"
#include "telnet.h"
#include "input_area.h"
#include "term_cell.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Terminal {
    const TerminalBackend *backend; /* Backend vtable */
    void *backend_state;            /* Opaque backend state */

    /* Terminal-level state (not in backend) */
    Telnet *telnet; /* Telnet connection for sending output */
};

/* Factory function - uses vterm backend by default */
Terminal *terminal_create(int rows, int cols) {
    return terminal_create_with_backend(rows, cols, "vterm");
}

/* Factory function with backend selection */
Terminal *terminal_create_with_backend(int rows, int cols, const char *backend_name) {
    Terminal *term = (Terminal *)malloc(sizeof(Terminal));
    if (!term)
        return NULL;

    /* Select backend */
    if (strcmp(backend_name, "vterm") == 0) {
        term->backend = &terminal_backend_vterm;
    } else {
        fprintf(stderr, "Unknown backend: %s\n", backend_name);
        free(term);
        return NULL;
    }

    /* Initialize backend */
    term->backend_state = term->backend->create(rows, cols);
    if (!term->backend_state) {
        free(term);
        return NULL;
    }

    /* Initialize terminal-level state */
    term->telnet = NULL;

    return term;
}

void terminal_destroy(Terminal *term) {
    if (!term)
        return;
    if (term->backend && term->backend->destroy)
        term->backend->destroy(term->backend_state);
    free(term);
}

void terminal_feed_data(Terminal *term, const char *data, size_t len) {
    if (!term || !term->backend || !term->backend->feed_data)
        return;
    term->backend->feed_data(term->backend_state, data, len);
}

void terminal_echo_local(Terminal *term) {
    if (!term || !term->backend || !term->backend->echo_local)
        return;
    term->backend->echo_local(term->backend_state);
}

void terminal_resize(Terminal *term, int rows, int cols) {
    if (!term || !term->backend || !term->backend->resize)
        return;
    term->backend->resize(term->backend_state, rows, cols);
}

void terminal_get_size(Terminal *term, int *rows, int *cols) {
    if (!term || !term->backend || !term->backend->get_size)
        return;
    term->backend->get_size(term->backend_state, rows, cols);
}

int terminal_needs_redraw(Terminal *term) {
    if (!term || !term->backend || !term->backend->needs_redraw)
        return 0;
    return term->backend->needs_redraw(term->backend_state);
}

void terminal_mark_drawn(Terminal *term) {
    if (!term || !term->backend || !term->backend->mark_drawn)
        return;
    term->backend->mark_drawn(term->backend_state);
}

void terminal_request_redraw(Terminal *term) {
    if (!term || !term->backend || !term->backend->request_redraw)
        return;
    term->backend->request_redraw(term->backend_state);
}

void terminal_set_telnet(Terminal *term, void *telnet) {
    if (term)
        term->telnet = (Telnet *)telnet;
}

void terminal_send_buffer(Terminal *term) {
    if (!term || !term->backend || !term->backend->send_buffer)
        return;
    term->backend->send_buffer(term->backend_state, term->telnet);
}

void terminal_scroll_up(Terminal *term, int lines) {
    if (!term || !term->backend || !term->backend->scroll_up)
        return;
    term->backend->scroll_up(term->backend_state, lines);
}

void terminal_scroll_down(Terminal *term, int lines) {
    if (!term || !term->backend || !term->backend->scroll_down)
        return;
    term->backend->scroll_down(term->backend_state, lines);
}

void terminal_scroll_to_bottom(Terminal *term) {
    if (!term || !term->backend || !term->backend->scroll_to_bottom)
        return;
    term->backend->scroll_to_bottom(term->backend_state);
}

int terminal_get_viewport_offset(Terminal *term) {
    if (!term || !term->backend || !term->backend->get_viewport_offset)
        return 0;
    return term->backend->get_viewport_offset(term->backend_state);
}

int terminal_get_scrollback_size(Terminal *term) {
    if (!term || !term->backend || !term->backend->get_scrollback_size)
        return 0;
    return term->backend->get_scrollback_size(term->backend_state);
}

int terminal_get_max_scrollback_lines(Terminal *term) {
    if (!term || !term->backend || !term->backend->get_max_scrollback_lines)
        return 0;
    return term->backend->get_max_scrollback_lines(term->backend_state);
}

const char *terminal_get_libvterm_version(void) {
    /* Delegate to vterm backend for now */
    return terminal_backend_vterm.get_version();
}

void terminal_set_max_scrollback_lines(Terminal *term, int max_lines) {
    if (!term || !term->backend || !term->backend->set_max_scrollback_lines)
        return;
    term->backend->set_max_scrollback_lines(term->backend_state, max_lines);
}

int terminal_get_cell_at(Terminal *term, int row, int col, TermCell *cell) {
    if (!term || !term->backend || !term->backend->get_cell_at)
        return 0;
    return term->backend->get_cell_at(term->backend_state, row, col, cell);
}

int terminal_get_cell_at_scrollback_index(Terminal *term, int scrollback_index, int col, TermCell *cell) {
    if (!term || !term->backend || !term->backend->get_cell_at_scrollback_index)
        return 0;
    return term->backend->get_cell_at_scrollback_index(term->backend_state, scrollback_index, col, cell);
}

void terminal_render_input_area(Terminal *term, InputArea *input_area) {
    if (!term || !term->backend || !term->backend->render_input_area)
        return;

    int rows, cols;
    terminal_get_size(term, &rows, &cols);
    /* Input row starts right after scrolling region (rows is 0-indexed, so row 'rows' is outside scrolling region) */
    int input_row = rows;

    term->backend->render_input_area(term->backend_state, input_area, input_row, cols);
}

VTerm *terminal_get_vterm(Terminal *term) {
    if (!term || !term->backend || !term->backend->get_vterm)
        return NULL;
    return (VTerm *)term->backend->get_vterm(term->backend_state);
}

VTermScreen *terminal_get_screen(Terminal *term) {
    if (!term || !term->backend || !term->backend->get_vterm_screen)
        return NULL;
    return (VTermScreen *)term->backend->get_vterm_screen(term->backend_state);
}
