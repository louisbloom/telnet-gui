/* Lisp bridge interface for telnet-gui */

#ifndef LISP_BRIDGE_H
#define LISP_BRIDGE_H

#include <stddef.h>

/* Initialize Lisp interpreter, environment, and load bootstrap file */
int lisp_bridge_init(void);

/* Load and evaluate additional Lisp file (after init) */
int lisp_bridge_load_file(const char *filepath);

/* Cleanup Lisp interpreter */
void lisp_bridge_cleanup(void);

/* Handle TAB key completion (cycles through completions if in tab mode) */
void lisp_bridge_handle_tab(char *buffer, int buffer_size, int *cursor_pos, int *length, int *needs_redraw);

/* Check if tab completion mode is active */
int lisp_bridge_is_tab_mode_active(void);

/* Accept current completion and exit tab mode */
void lisp_bridge_accept_tab_completion(void);

/* Cancel tab mode and revert to original buffer state */
void lisp_bridge_cancel_tab_completion(char *buffer, int buffer_size, int *cursor_pos, int *length, int *needs_redraw);

/* Get scroll lines per click from Lisp config (default: 3) */
int lisp_bridge_get_scroll_lines_per_click(void);

/* Get smooth scrolling enabled from Lisp config (default: 1/true) */
int lisp_bridge_get_smooth_scrolling_enabled(void);

/* Get max scrollback lines from Lisp config (default: 0 = unbounded) */
int lisp_bridge_get_max_scrollback_lines(void);

/* Get scroll-to-bottom-on-user-input from Lisp config (default: 1/true) */
int lisp_bridge_get_scroll_to_bottom_on_user_input(void);

/* Get scroll-to-bottom-on-telnet-input from Lisp config (default: 0/false) */
int lisp_bridge_get_scroll_to_bottom_on_telnet_input(void);

/* Call telnet-input-hook with telnet data (stripped of ANSI codes) */
void lisp_bridge_call_telnet_input_hook(const char *text, size_t len);

/* Call user-input-hook with user input before sending to telnet (returns transformed text or original) */
const char *lisp_bridge_call_user_input_hook(const char *text, int cursor_pos);

/* Color configuration getters - return RGB components (0-255) */
/* Input area colors */
void lisp_bridge_get_input_area_fg_color(int *r, int *g, int *b);
void lisp_bridge_get_input_area_bg_color(int *r, int *g, int *b);
void lisp_bridge_get_selection_fg_color(int *r, int *g, int *b);
void lisp_bridge_get_selection_bg_color(int *r, int *g, int *b);
void lisp_bridge_get_cursor_color(int *r, int *g, int *b);
void lisp_bridge_get_input_separator_color(int *r, int *g, int *b);

/* Mode display area colors */
void lisp_bridge_get_mode_fg_color(int *r, int *g, int *b);
void lisp_bridge_get_mode_bg_color(int *r, int *g, int *b);

/* Terminal default colors */
void lisp_bridge_get_terminal_fg_color(int *r, int *g, int *b);
void lisp_bridge_get_terminal_bg_color(int *r, int *g, int *b);

/* Window chrome colors */
void lisp_bridge_get_resize_bar_color(int *r, int *g, int *b);

/* Mode data structure - set and get as Lisp variables */
void lisp_bridge_set_connection_mode(int connected);
void lisp_bridge_set_input_mode(int input_mode);
const char *lisp_bridge_get_mode_string(void);

/* Future: More functions to expose telnet-gui primitives */
/* - Input/output hooks */
/* - Input area manipulation */
/* - Terminal control */
/* - Window management */
/* - etc. */

#endif /* LISP_BRIDGE_H */
