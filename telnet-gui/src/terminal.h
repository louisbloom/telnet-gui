/* Terminal emulation using libvterm */

#ifndef TERMINAL_H
#define TERMINAL_H

#include <vterm.h>

typedef struct Terminal Terminal;

/* Create a new terminal with initial size */
Terminal *terminal_create(int rows, int cols);

/* Destroy terminal */
void terminal_destroy(Terminal *term);

/* Feed data to the terminal */
void terminal_feed_data(Terminal *term, const char *data, size_t len);

/* Get terminal VTerm instance */
VTerm *terminal_get_vterm(Terminal *term);

/* Get terminal screen */
VTermScreen *terminal_get_screen(Terminal *term);

/* Resize terminal */
void terminal_resize(Terminal *term, int rows, int cols);

/* Get current terminal size */
void terminal_get_size(Terminal *term, int *rows, int *cols);

/* Check if terminal needs redraw */
int terminal_needs_redraw(Terminal *term);

/* Mark terminal as redrawn */
void terminal_mark_drawn(Terminal *term);

/* Set telnet connection for sending output */
void terminal_set_telnet(Terminal *term, void *telnet);

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

/* Get viewport offset */
int terminal_get_viewport_offset(Terminal *term);

/* Get scrollback size */
int terminal_get_scrollback_size(Terminal *term);

/* Get max scrollback lines (0 = unbounded) */
int terminal_get_max_scrollback_lines(Terminal *term);

/* Set max scrollback lines (0 = unbounded, limited by SCROLLBACK_MAX_LINES for safety) */
void terminal_set_max_scrollback_lines(Terminal *term, int max_lines);

/* Get cell at position considering viewport offset */
int terminal_get_cell_at(Terminal *term, int row, int col, VTermScreenCell *cell);

#endif /* TERMINAL_H */
