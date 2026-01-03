/*
 * lineedit.h - Minimal line editor with completion and history
 *
 * A simple, cross-platform line editor for REPL applications.
 * Supports single-line editing, history navigation, and tab completion.
 *
 * Platform support:
 *   - Windows: Uses conio.h (_getch)
 *   - Unix/Linux/macOS: Uses termios for raw input
 */

#ifndef LINEEDIT_H
#define LINEEDIT_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Opaque state structure */
typedef struct LineEditState LineEditState;

/*
 * Completion callback function type.
 *
 * Called when the user presses Tab. Should return a NULL-terminated array
 * of completion strings matching the current input context.
 *
 * Parameters:
 *   buffer     - Current line buffer contents
 *   cursor_pos - Current cursor position (0-indexed)
 *   userdata   - User-provided context pointer
 *
 * Returns:
 *   NULL-terminated array of malloc'd strings, or NULL if no completions.
 *   Caller (lineedit) will free the array and strings via lineedit_free_completions().
 */
typedef char **(*lineedit_completer_fn)(const char *buffer, int cursor_pos,
                                        void *userdata);

/*
 * Create a new line editor state.
 *
 * Returns:
 *   New LineEditState, or NULL on allocation failure.
 *   Must be freed with lineedit_destroy().
 */
LineEditState *lineedit_create(void);

/*
 * Destroy a line editor state and free all resources.
 */
void lineedit_destroy(LineEditState *state);

/*
 * Set the completion callback function.
 *
 * Parameters:
 *   state    - Line editor state
 *   fn       - Completion callback, or NULL to disable completion
 *   userdata - Context pointer passed to callback
 */
void lineedit_set_completer(LineEditState *state, lineedit_completer_fn fn,
                            void *userdata);

/*
 * Set maximum history size.
 *
 * Parameters:
 *   state    - Line editor state
 *   max_size - Maximum number of history entries (default: 100)
 */
void lineedit_set_history_size(LineEditState *state, int max_size);

/*
 * Read a line of input with editing support.
 *
 * Displays prompt, allows editing with arrow keys, supports history
 * navigation with up/down arrows, and tab completion if configured.
 *
 * Parameters:
 *   state  - Line editor state
 *   prompt - Prompt string to display (e.g., ">>> ")
 *
 * Returns:
 *   malloc'd string containing the line (without newline), or
 *   NULL on EOF (Ctrl+D) or error.
 *   Caller must free() the returned string.
 */
char *lineedit_readline(LineEditState *state, const char *prompt);

/*
 * Add a line to history.
 *
 * Empty lines and duplicates of the most recent entry are ignored.
 *
 * Parameters:
 *   state - Line editor state
 *   line  - Line to add (will be copied)
 */
void lineedit_history_add(LineEditState *state, const char *line);

/*
 * Clear all history entries.
 */
void lineedit_history_clear(LineEditState *state);

/*
 * Free a completion array returned by a completer callback.
 *
 * Parameters:
 *   completions - NULL-terminated array of strings, or NULL
 */
void lineedit_free_completions(char **completions);

#ifdef __cplusplus
}
#endif

#endif /* LINEEDIT_H */
