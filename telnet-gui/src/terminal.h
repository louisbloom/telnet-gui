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

#endif /* TERMINAL_H */
