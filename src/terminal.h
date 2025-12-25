/* Terminal emulation - backend-independent interface */

#ifndef TERMINAL_H
#define TERMINAL_H

#include "term_cell.h"
#include "dock.h"

/* Opaque Terminal type - actual definition is in terminal.c */
typedef struct Terminal Terminal;

/* Create a new terminal with initial size (uses vterm backend by default) */
Terminal *terminal_create(int rows, int cols);

/* Create a new terminal with specified backend */
Terminal *terminal_create_with_backend(int rows, int cols, const char *backend_name);

/* Destroy terminal */
void terminal_destroy(Terminal *term);

/* Feed data to the terminal */
void terminal_feed_data(Terminal *term, const char *data, size_t len);

/* Resize terminal */
void terminal_resize(Terminal *term, int rows, int cols, int input_visible_rows);

/* Get current terminal size */
void terminal_get_size(Terminal *term, int *rows, int *cols);

/* Check if terminal needs redraw */
int terminal_needs_redraw(Terminal *term);

/* Mark terminal as redrawn */
void terminal_mark_drawn(Terminal *term);

/* Request terminal redraw */
void terminal_request_redraw(Terminal *term);

/* Get cursor information (position and visibility) */
void terminal_get_cursor_info(Terminal *term, int *row, int *col, int *visible);

/* Set telnet connection for sending output */
void terminal_set_telnet(Terminal *term, void *telnet);

/* Clear output buffer (prevents accidentally buffered output from being sent) */
void terminal_clear_output_buffer(Terminal *term);

/* Send buffered output to telnet server */
void terminal_send_buffer(Terminal *term);

/* Echo local input to screen for immediate feedback */
void terminal_echo_local(Terminal *term);

/* Scroll viewport up by N lines */
void terminal_scroll_up(Terminal *term, int lines);

/* Scroll viewport down by N lines */
void terminal_scroll_down(Terminal *term, int lines);

/* Scroll to bottom (show current screen) */
void terminal_scroll_to_bottom(Terminal *term);

/* Check if scroll is locked (user scrolled back from bottom) */
int terminal_is_scroll_locked(Terminal *term);

/* Get viewport offset */
int terminal_get_viewport_offset(Terminal *term);

/* Get scrollback size */
int terminal_get_scrollback_size(Terminal *term);

/* Get max scrollback lines (0 = unbounded) */
int terminal_get_max_scrollback_lines(Terminal *term);

/* Get libvterm version string */
const char *terminal_get_libvterm_version(void);

/* Set max scrollback lines (0 = unbounded, limited by SCROLLBACK_MAX_LINES for safety) */
void terminal_set_max_scrollback_lines(Terminal *term, int max_lines);

/* Get cell at position considering viewport offset */
int terminal_get_cell_at(Terminal *term, int row, int col, TermCell *cell);

/* Get cell at absolute scrollback position (independent of viewport) */
int terminal_get_cell_at_scrollback_index(Terminal *term, int scrollback_index, int col, TermCell *cell);

/* Render input area to vterm using ANSI escape sequences */
void terminal_render_dock(Terminal *term, Dock *dock, int terminal_cols);

/* TEMPORARY: VTerm accessors (will be removed when input/renderer are abstracted) */
#include <vterm.h>
VTerm *terminal_get_vterm(Terminal *term);
VTermScreen *terminal_get_screen(Terminal *term);

#endif /* TERMINAL_H */
