/* Input area with readline-like functionality */

#ifndef DOCK_H
#define DOCK_H

#include "dynamic_buffer.h"

#define DOCK_MAX_LENGTH 4096
#define DOCK_HISTORY_SIZE 100 /* Default, can be overridden by Lisp config */
#define UNDO_STACK_SIZE 64

/* Fixed dock rows: top divider + bottom divider + notification row */
#define DOCK_FIXED_ROWS 3

/* Calculate total dock height in rows (fixed rows + text input rows) */
static inline int dock_height_rows(int text_rows) {
    return DOCK_FIXED_ROWS + text_rows;
}

typedef enum { DOCK_MODE_NORMAL = 0, DOCK_MODE_EVAL = 1 } DockMode;

/* Undo operation types (for coalescing) */
typedef enum {
    UNDO_OP_NONE = 0,
    UNDO_OP_INSERT,
    UNDO_OP_DELETE,
    UNDO_OP_BACKSPACE,
    UNDO_OP_KILL,
    UNDO_OP_YANK,
    UNDO_OP_CLEAR
} UndoOpType;

/* Undo state snapshot */
typedef struct {
    char buffer[DOCK_MAX_LENGTH];
    int cursor_pos;
    int length;
} UndoEntry;

typedef struct Dock {
    char buffer[DOCK_MAX_LENGTH];
    int cursor_pos;   /* Cursor position in buffer */
    int length;       /* Current length of text */
    int needs_redraw; /* Flag to indicate input area needs redraw */

    /* History management */
    char **history;                          /* Dynamic array of history entries */
    int history_max_size;                    /* Maximum history size from Lisp config */
    int history_count;                       /* Number of entries in history */
    int history_index;                       /* -1 = new entry, 0+ = history index */
    char saved_input[DOCK_MAX_LENGTH]; /* Preserve current input during history navigation */

    /* Prefix search for history (when cursor at EOL during arrow up) */
    char history_search_prefix[DOCK_MAX_LENGTH]; /* Prefix to search for */
    int history_search_active;                         /* 1 if prefix search is active, 0 otherwise */

    /* Kill ring */
    char kill_ring[DOCK_MAX_LENGTH];

    /* Text selection */
    int selection_active; /* 1 if selection is active */
    int selection_start;  /* Start position of selection */
    int selection_end;    /* End position of selection */

    /* Mode */
    DockMode mode;

    /* Multi-line support */
    int line_breaks[64];     /* Byte offsets of '\n' characters (max 64 explicit newlines) */
    int line_break_count;    /* Number of explicit newlines in buffer */
    int visual_rows;         /* Total visual rows (including wrapped lines) */
    int visible_rows;        /* Currently visible rows (for display) */
    int scroll_offset;       /* First visible row (0 = showing top) */
    int max_visible_rows;    /* Config: max rows before scrolling (default: 5) */
    int needs_layout_recalc; /* Flag: recalculate visual rows */

    /* Vterm cursor position (set by terminal_render_dock, used by renderer) */
    int vterm_cursor_row; /* Cursor row within dock (0-indexed from first text row) */
    int vterm_cursor_col; /* Cursor column (0-indexed) */

    /* Echo buffer for user input */
    DynamicBuffer *echo_buf;

    /* Eval buffer for Lisp expression output */
    DynamicBuffer *eval_buf;

    /* Undo/Redo state */
    UndoEntry undo_stack[UNDO_STACK_SIZE];
    int undo_head;            /* Index of most recent entry */
    int undo_count;           /* Number of valid entries */
    UndoEntry redo_stack[UNDO_STACK_SIZE];
    int redo_head;
    int redo_count;
    UndoOpType undo_last_op;  /* For coalescing */
    int undo_last_cursor;     /* Cursor pos after last op */
    int undo_coalesce_active; /* Whether grouping is active */
} Dock;

/* Initialize input area */
void dock_init(Dock *area);

/* Insert text at cursor position */
void dock_insert_text(Dock *area, const char *text, int text_len);

/* Delete character at cursor */
void dock_delete_char(Dock *area);

/* Delete character before cursor (backspace) */
void dock_backspace(Dock *area);

/* Move cursor */
void dock_move_cursor(Dock *area, int pos);
void dock_move_cursor_left(Dock *area);
void dock_move_cursor_right(Dock *area);
void dock_move_cursor_home(Dock *area);
void dock_move_cursor_end(Dock *area);

/* Word navigation */
void dock_move_cursor_word_left(Dock *area);
void dock_move_cursor_word_right(Dock *area);

/* Line editing shortcuts */
void dock_move_cursor_beginning(Dock *area); /* Ctrl+A */
void dock_move_cursor_end_line(Dock *area);  /* Ctrl+E */
void dock_kill_to_end(Dock *area);           /* Ctrl+K */
void dock_kill_from_start(Dock *area);       /* Ctrl+U */
void dock_kill_word(Dock *area);             /* Ctrl+W */
void dock_yank(Dock *area);                  /* Ctrl+Y */

/* Clipboard operations */
const char *dock_copy(Dock *area);             /* Ctrl+C - returns text to copy */
void dock_paste(Dock *area, const char *text); /* Ctrl+V - paste text at cursor */
const char *dock_get_kill_ring(Dock *area);    /* Get last killed text */

/* Selection management */
void dock_start_selection(Dock *area);                           /* Start selection at cursor */
void dock_clear_selection(Dock *area);                           /* Clear selection */
int dock_has_selection(Dock *area);                              /* Check if selection is active */
void dock_get_selection_range(Dock *area, int *start, int *end); /* Get selection range */
int dock_copy_selection(Dock *area, char *dest, int dest_size);  /* Copy selection to buffer */

/* History management */
void dock_history_add(Dock *area);
void dock_history_prev(Dock *area);
void dock_history_next(Dock *area);
void dock_history_save_current(Dock *area);
void dock_history_restore_current(Dock *area);

/* Clear input area */
void dock_clear(Dock *area);

/* Undo/Redo operations */
int dock_undo(Dock *area);       /* Returns 1 if undo performed */
int dock_redo(Dock *area);       /* Returns 1 if redo performed */
int dock_can_undo(Dock *area);
int dock_can_redo(Dock *area);
void dock_undo_clear(Dock *area);

/* Get current text */
const char *dock_get_text(Dock *area);

/* Get current length */
int dock_get_length(Dock *area);

/* Get cursor position */
int dock_get_cursor_pos(Dock *area);

/* Check if needs redraw */
int dock_needs_redraw(Dock *area);

/* Request redraw */
void dock_request_redraw(Dock *area);

/* Mark as drawn */
void dock_mark_drawn(Dock *area);

/* Get buffer pointer (for compatibility with lisp_bridge) */
char *dock_get_buffer(Dock *area);

/* Sync state after external buffer modification (for lisp_bridge compatibility) */
void dock_sync_state(Dock *area);

/* Mode management */
void dock_set_mode(Dock *area, DockMode mode);
DockMode dock_get_mode(Dock *area);

/* Multi-line support */
void dock_recalculate_layout(Dock *area, int terminal_cols);
void dock_get_cursor_visual_position(Dock *area, int cols, int *out_row, int *out_col);
void dock_ensure_cursor_visible(Dock *area, int cols);
int dock_get_text_rows(Dock *area);
int dock_is_at_first_visual_line(Dock *area, int cols);
int dock_is_at_last_visual_line(Dock *area, int cols);
void dock_move_cursor_up_line(Dock *area, int cols);
void dock_move_cursor_down_line(Dock *area, int cols);

#endif /* DOCK_H */
