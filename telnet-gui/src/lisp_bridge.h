/* Lisp bridge interface for telnet-gui */

#ifndef LISP_BRIDGE_H
#define LISP_BRIDGE_H

/* Initialize Lisp interpreter, environment, and load bootstrap file */
int lisp_bridge_init(void);

/* Load and evaluate additional Lisp file (after init) */
int lisp_bridge_load_file(const char *filepath);

/* Cleanup Lisp interpreter */
void lisp_bridge_cleanup(void);

/* Handle TAB key completion */
void lisp_bridge_handle_tab(char *buffer, int buffer_size, int *cursor_pos, int *length, int *needs_redraw);

/* Get scroll lines per click from Lisp config (default: 3) */
int lisp_bridge_get_scroll_lines_per_click(void);

/* Get smooth scrolling enabled from Lisp config (default: 1/true) */
int lisp_bridge_get_smooth_scrolling_enabled(void);

/* Future: More functions to expose telnet-gui primitives */
/* - Input/output hooks */
/* - Input area manipulation */
/* - Terminal control */
/* - Window management */
/* - etc. */

#endif /* LISP_BRIDGE_H */
