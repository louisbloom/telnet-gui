/* Input area with readline-like functionality */

#ifndef INPUT_AREA_H
#define INPUT_AREA_H

#define INPUT_AREA_MAX_LENGTH 4096
#define INPUT_AREA_HISTORY_SIZE 100 /* Default, can be overridden by Lisp config */

typedef enum { INPUT_AREA_MODE_NORMAL = 0, INPUT_AREA_MODE_EVAL = 1 } InputAreaMode;

typedef struct {
    char buffer[INPUT_AREA_MAX_LENGTH];
    int cursor_pos;   /* Cursor position in buffer */
    int length;       /* Current length of text */
    int needs_redraw; /* Flag to indicate input area needs redraw */

    /* History management */
    char **history;                          /* Dynamic array of history entries */
    int history_max_size;                    /* Maximum history size from Lisp config */
    int history_count;                       /* Number of entries in history */
    int history_index;                       /* -1 = new entry, 0+ = history index */
    char saved_input[INPUT_AREA_MAX_LENGTH]; /* Preserve current input during history navigation */

    /* Prefix search for history (when cursor at EOL during arrow up) */
    char history_search_prefix[INPUT_AREA_MAX_LENGTH]; /* Prefix to search for */
    int history_search_active;                         /* 1 if prefix search is active, 0 otherwise */

    /* Kill ring */
    char kill_ring[INPUT_AREA_MAX_LENGTH];

    /* Text selection */
    int selection_active; /* 1 if selection is active */
    int selection_start;  /* Start position of selection */
    int selection_end;    /* End position of selection */

    /* Mode */
    InputAreaMode mode;
} InputArea;

/* Initialize input area */
void input_area_init(InputArea *area);

/* Insert text at cursor position */
void input_area_insert_text(InputArea *area, const char *text, int text_len);

/* Delete character at cursor */
void input_area_delete_char(InputArea *area);

/* Delete character before cursor (backspace) */
void input_area_backspace(InputArea *area);

/* Move cursor */
void input_area_move_cursor(InputArea *area, int pos);
void input_area_move_cursor_left(InputArea *area);
void input_area_move_cursor_right(InputArea *area);
void input_area_move_cursor_home(InputArea *area);
void input_area_move_cursor_end(InputArea *area);

/* Word navigation */
void input_area_move_cursor_word_left(InputArea *area);
void input_area_move_cursor_word_right(InputArea *area);

/* Line editing shortcuts */
void input_area_move_cursor_beginning(InputArea *area); /* Ctrl+A */
void input_area_move_cursor_end_line(InputArea *area);  /* Ctrl+E */
void input_area_kill_to_end(InputArea *area);           /* Ctrl+K */
void input_area_kill_from_start(InputArea *area);       /* Ctrl+U */
void input_area_kill_word(InputArea *area);             /* Ctrl+W */
void input_area_yank(InputArea *area);                  /* Ctrl+Y */

/* Clipboard operations */
const char *input_area_copy(InputArea *area);             /* Ctrl+C - returns text to copy */
void input_area_paste(InputArea *area, const char *text); /* Ctrl+V - paste text at cursor */
const char *input_area_get_kill_ring(InputArea *area);    /* Get last killed text */

/* Selection management */
void input_area_start_selection(InputArea *area);                           /* Start selection at cursor */
void input_area_clear_selection(InputArea *area);                           /* Clear selection */
int input_area_has_selection(InputArea *area);                              /* Check if selection is active */
void input_area_get_selection_range(InputArea *area, int *start, int *end); /* Get selection range */
int input_area_copy_selection(InputArea *area, char *dest, int dest_size);  /* Copy selection to buffer */

/* History management */
void input_area_history_add(InputArea *area);
void input_area_history_prev(InputArea *area);
void input_area_history_next(InputArea *area);
void input_area_history_save_current(InputArea *area);
void input_area_history_restore_current(InputArea *area);

/* Clear input area */
void input_area_clear(InputArea *area);

/* Get current text */
const char *input_area_get_text(InputArea *area);

/* Get current length */
int input_area_get_length(InputArea *area);

/* Get cursor position */
int input_area_get_cursor_pos(InputArea *area);

/* Check if needs redraw */
int input_area_needs_redraw(InputArea *area);

/* Mark as drawn */
void input_area_mark_drawn(InputArea *area);

/* Get buffer pointer (for compatibility with lisp_bridge) */
char *input_area_get_buffer(InputArea *area);

/* Sync state after external buffer modification (for lisp_bridge compatibility) */
void input_area_sync_state(InputArea *area);

/* Mode management */
void input_area_set_mode(InputArea *area, InputAreaMode mode);
InputAreaMode input_area_get_mode(InputArea *area);

#endif /* INPUT_AREA_H */
