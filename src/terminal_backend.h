/* Terminal backend interface - makes terminal emulation backends swappable */

#ifndef TERMINAL_BACKEND_H
#define TERMINAL_BACKEND_H

#include "term_cell.h"
#include <stddef.h>

/* Terminal backend interface using function pointers (vtable pattern)
 * This allows swapping terminal emulation backends (libvterm, simple terminal, etc.)
 * without changing the rest of the codebase.
 *
 * Each backend implements these functions and registers them in the TerminalBackend struct.
 * The Terminal wrapper in terminal.c delegates to the active backend.
 */
typedef struct TerminalBackend {
    /* Lifecycle management */
    void* (*create)(int rows, int cols);
    void (*destroy)(void *state);

    /* Input/Output */
    void (*feed_data)(void *state, const char *data, size_t len);

    /* Cell access - core rendering interface */
    int (*get_cell_at)(void *state, int row, int col, TermCell *cell);
    int (*get_cell_at_scrollback_index)(void *state, int scrollback_index, int col, TermCell *cell);

    /* Size management */
    void (*resize)(void *state, int rows, int cols, int input_visible_rows);
    void (*get_size)(void *state, int *rows, int *cols);

    /* Scrollback/viewport management */
    void (*scroll_up)(void *state, int lines);
    void (*scroll_down)(void *state, int lines);
    void (*scroll_to_bottom)(void *state);
    int (*is_scroll_locked)(void *state);
    int (*get_viewport_offset)(void *state);
    int (*get_scrollback_size)(void *state);
    int (*get_max_scrollback_lines)(void *state);
    void (*set_max_scrollback_lines)(void *state, int max_lines);

    /* Rendering flags */
    int (*needs_redraw)(void *state);
    void (*mark_drawn)(void *state);
    void (*request_redraw)(void *state);

    /* Cursor info */
    void (*get_cursor_info)(void *state, int *row, int *col, int *visible);

    /* Output handling (for telnet/local echo) */
    void (*set_output_callback)(void *state, void (*callback)(const char *s, size_t len, void *user), void *user);
    void (*echo_local)(void *state);
    void (*send_buffer)(void *state, void *telnet);
    void (*clear_output_buffer)(void *state);

    /* Input area rendering (ANSI sequences to vterm) */
    void (*render_dock)(void *state, void *dock, int input_row, int cols, int connected);

    /* Version info */
    const char* (*get_version)(void);

    /* TEMPORARY: VTerm accessors (will be removed when input/renderer are abstracted) */
    void* (*get_vterm)(void *state);  /* Returns VTerm* for vterm backend, NULL for others */
    void* (*get_vterm_screen)(void *state);  /* Returns VTermScreen* for vterm backend, NULL for others */
} TerminalBackend;

/* Backend registry - implemented backends */
extern const TerminalBackend terminal_backend_vterm;
/* Future backends would be declared here */
/* extern const TerminalBackend terminal_backend_simple; */

#endif /* TERMINAL_BACKEND_H */
