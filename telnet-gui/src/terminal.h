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

#endif /* TERMINAL_H */
