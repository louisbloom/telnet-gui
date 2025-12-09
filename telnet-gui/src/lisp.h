/* Lisp interface for telnet-gui */

#ifndef TELNET_GUI_LISP_H
#define TELNET_GUI_LISP_H

#include <stddef.h>
#include "terminal.h"

/* Initialize Lisp interpreter, environment, and load bootstrap file */
int lisp_x_init(void);

/* Load and evaluate additional Lisp file (after init) */
int lisp_x_load_file(const char *filepath);

/* Cleanup Lisp interpreter */
void lisp_x_cleanup(void);

/* Handle TAB key completion (cycles through completions if in tab mode) */
void lisp_x_handle_tab(char *buffer, int buffer_size, int *cursor_pos, int *length, int *needs_redraw);

/* Check if tab completion mode is active */
int lisp_x_is_tab_mode_active(void);

/* Accept current completion and exit tab mode */
void lisp_x_accept_tab_completion(void);

/* Cancel tab mode and revert to original buffer state */
void lisp_x_cancel_tab_completion(char *buffer, int buffer_size, int *cursor_pos, int *length, int *needs_redraw);

/* Get scroll lines per click from Lisp config (default: 3) */
int lisp_x_get_scroll_lines_per_click(void);

/* Get smooth scrolling enabled from Lisp config (default: 1/true) */
int lisp_x_get_smooth_scrolling_enabled(void);

/* Get max scrollback lines from Lisp config (default: 0 = unbounded) */
int lisp_x_get_max_scrollback_lines(void);

/* Get scroll-to-bottom-on-user-input from Lisp config (default: 1/true) */
int lisp_x_get_scroll_to_bottom_on_user_input(void);

/* Get scroll-to-bottom-on-telnet-input from Lisp config (default: 0/false) */
int lisp_x_get_scroll_to_bottom_on_telnet_input(void);

/* Get input history size from Lisp config (default: 100) */
int lisp_x_get_input_history_size(void);

/* Call telnet-input-hook with telnet data (stripped of ANSI codes) */
void lisp_x_call_telnet_input_hook(const char *text, size_t len);

/* Call telnet-input-filter-hook with telnet data (with ANSI codes) before displaying in terminal (returns transformed text or original) */
const char *lisp_x_call_telnet_input_filter_hook(const char *text, size_t len, size_t *out_len);

/* Call user-input-hook with user input before sending to telnet (returns transformed text or original) */
const char *lisp_x_call_user_input_hook(const char *text, int cursor_pos);

/* Color configuration getters - return RGB components (0-255) */
/* Input area colors */
void lisp_x_get_input_area_fg_color(int *r, int *g, int *b);
void lisp_x_get_input_area_bg_color(int *r, int *g, int *b);
void lisp_x_get_selection_fg_color(int *r, int *g, int *b);
void lisp_x_get_selection_bg_color(int *r, int *g, int *b);
void lisp_x_get_cursor_color(int *r, int *g, int *b);
void lisp_x_get_input_separator_color(int *r, int *g, int *b);

/* Terminal default colors */
void lisp_x_get_terminal_fg_color(int *r, int *g, int *b);
void lisp_x_get_terminal_bg_color(int *r, int *g, int *b);

/* Forward declaration for Telnet type (defined in telnet.h) */
struct Telnet;

/* Forward declarations for GlyphCache and Window types */
struct GlyphCache;
struct Window;

/* Register terminal pointer for terminal-echo builtin */
void lisp_x_register_terminal(Terminal *term);

/* Register telnet pointer for telnet-send builtin */
void lisp_x_register_telnet(struct Telnet *t);

/* Register glyph cache pointer for terminal-info builtin */
void lisp_x_register_glyph_cache(struct GlyphCache *cache);

/* Register window pointer for terminal-info builtin */
void lisp_x_register_window(struct Window *w);

/* Get lisp environment (for accessing Lisp variables from C) */
void *lisp_x_get_environment(void);

/* Future: More functions to expose telnet-gui primitives */
/* - Input/output hooks */
/* - Input area manipulation */
/* - Terminal control */
/* - Window management */
/* - etc. */

#endif /* TELNET_GUI_LISP_H */
