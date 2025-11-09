/* Input area with readline-like functionality */

#ifndef INPUT_AREA_H
#define INPUT_AREA_H

#define INPUT_AREA_MAX_LENGTH 4096
#define INPUT_AREA_HISTORY_SIZE 100

typedef struct {
    char buffer[INPUT_AREA_MAX_LENGTH];
    int cursor_pos;   /* Cursor position in buffer */
    int length;       /* Current length of text */
    int needs_redraw; /* Flag to indicate input area needs redraw */

    /* History management */
    char history[INPUT_AREA_HISTORY_SIZE][INPUT_AREA_MAX_LENGTH];
    int history_count;  /* Number of entries in history */
    int history_index;  /* -1 = new entry, 0+ = history index */
    char saved_input[INPUT_AREA_MAX_LENGTH]; /* Preserve current input during history navigation */

    /* Kill ring */
    char kill_ring[INPUT_AREA_MAX_LENGTH];
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

#endif /* INPUT_AREA_H */
