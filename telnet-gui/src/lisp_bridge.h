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

/* Future: More functions to expose telnet-gui primitives */
/* - Input/output hooks */
/* - Input area manipulation */
/* - Terminal control */
/* - Window management */
/* - etc. */

#endif /* LISP_BRIDGE_H */
