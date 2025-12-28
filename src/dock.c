/* Input area implementation with readline-like functionality */

#include "dock.h"
#include "lisp.h"
#include "dynamic_buffer.h"
#include "../../telnet-lisp/include/utf8.h"
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>

/* Helper: Reset history prefix search (call when user edits text) */
static void reset_history_search(Dock *area) {
    if (!area)
        return;
    area->history_search_active = 0;
    area->history_search_prefix[0] = '\0';
}

/* Helper: Clear selection (call when text is edited) */
static void clear_selection_if_active(Dock *area) {
    if (!area)
        return;
    if (area->selection_active) {
        area->selection_active = 0;
        area->needs_redraw = 1;
    }
}

/* Helper: Push current state to undo stack */
static void undo_push(Dock *area) {
    if (!area)
        return;

    /* Advance head (circular buffer) */
    area->undo_head = (area->undo_head + 1) % UNDO_STACK_SIZE;
    if (area->undo_count < UNDO_STACK_SIZE) {
        area->undo_count++;
    }

    /* Save current state */
    UndoEntry *entry = &area->undo_stack[area->undo_head];
    memcpy(entry->buffer, area->buffer, area->length + 1);
    entry->cursor_pos = area->cursor_pos;
    entry->length = area->length;
}

/* Helper: Push current state to redo stack */
static void redo_push(Dock *area) {
    if (!area)
        return;

    /* Advance head (circular buffer) */
    area->redo_head = (area->redo_head + 1) % UNDO_STACK_SIZE;
    if (area->redo_count < UNDO_STACK_SIZE) {
        area->redo_count++;
    }

    /* Save current state */
    UndoEntry *entry = &area->redo_stack[area->redo_head];
    memcpy(entry->buffer, area->buffer, area->length + 1);
    entry->cursor_pos = area->cursor_pos;
    entry->length = area->length;
}

/* Helper: Clear redo stack (called on any new modification) */
static void redo_clear(Dock *area) {
    if (!area)
        return;
    area->redo_count = 0;
    area->redo_head = -1;
}

void dock_init(Dock *area) {
    if (!area)
        return;
    memset(area, 0, sizeof(Dock));

    /* Get history size from Lisp config */
    area->history_max_size = lisp_x_get_input_history_size();

    /* Allocate history array */
    area->history = (char **)malloc(area->history_max_size * sizeof(char *));
    if (area->history) {
        for (int i = 0; i < area->history_max_size; i++) {
            area->history[i] = (char *)malloc(DOCK_MAX_LENGTH);
            if (area->history[i]) {
                area->history[i][0] = '\0';
            }
        }
    }

    area->history_index = -1; /* Start with new entry */
    area->mode = DOCK_MODE_NORMAL;

    /* Initialize echo buffer */
    area->echo_buf = dynamic_buffer_create(DOCK_MAX_LENGTH + 10);

    /* Initialize eval buffer */
    area->eval_buf = dynamic_buffer_create(4096);

    /* Initialize multi-line fields */
    area->line_break_count = 0;
    area->visual_rows = 1;         /* Start with one row */
    area->visible_rows = 1;        /* Show one row initially */
    area->scroll_offset = 0;       /* No scrolling initially */
    area->max_visible_rows = 5;    /* Default: up to 5 rows before scrolling */
    area->needs_layout_recalc = 0; /* No recalc needed for empty buffer */

    /* Initialize undo/redo state */
    area->undo_head = -1;
    area->undo_count = 0;
    area->redo_head = -1;
    area->redo_count = 0;
    area->undo_last_op = UNDO_OP_NONE;
    area->undo_last_cursor = 0;
    area->undo_coalesce_active = 0;

    /* Initialize content change flag */
    area->user_input_received = 0;
}

/* Core content modification function - ALL content changes go through this */
void dock_replace_range(Dock *area, int start, int end, const char *text, int len, int save_to_kill_ring) {
    if (!area)
        return;
    if (start < 0)
        start = 0;
    if (end > area->length)
        end = area->length;
    if (start > end)
        return;

    /* Save to kill ring if requested */
    if (save_to_kill_ring && end > start) {
        int kill_len = end - start;
        if (kill_len >= DOCK_MAX_LENGTH)
            kill_len = DOCK_MAX_LENGTH - 1;
        memcpy(area->kill_ring, &area->buffer[start], kill_len);
        area->kill_ring[kill_len] = '\0';
    }

    /* Perform the replacement */
    int delete_len = end - start;
    int delta = len - delete_len;

    /* Check for overflow */
    if (area->length + delta >= DOCK_MAX_LENGTH)
        return;

    /* No-op check: nothing to delete and nothing to insert */
    if (delete_len == 0 && len == 0)
        return;

    /* Save undo state before modification */
    undo_push(area);
    redo_clear(area);

    /* Make room for new text (or shrink) */
    if (delta != 0) {
        memmove(&area->buffer[start + len], &area->buffer[end], area->length - end + 1);
    }

    /* Insert new text */
    if (len > 0 && text) {
        memcpy(&area->buffer[start], text, len);
    }

    area->length += delta;
    area->buffer[area->length] = '\0';
    area->cursor_pos = start + len;

    /* ALL bookkeeping in one place */
    area->needs_redraw = 1;
    area->needs_layout_recalc = 1;
    area->user_input_received = 1;
    area->undo_last_cursor = area->cursor_pos;
    area->undo_coalesce_active = 0; /* Reset coalescing on any modification */
    area->undo_last_op = UNDO_OP_NONE;
    reset_history_search(area);
    clear_selection_if_active(area);
}

void dock_insert_text(Dock *area, const char *text, int text_len) {
    if (!area || !text || text_len <= 0)
        return;
    dock_replace_range(area, area->cursor_pos, area->cursor_pos, text, text_len, 0);
}

void dock_delete_char(Dock *area) {
    if (!area || area->cursor_pos >= area->length)
        return;
    int char_bytes = utf8_char_bytes(&area->buffer[area->cursor_pos]);
    if (char_bytes <= 0)
        char_bytes = 1;
    dock_replace_range(area, area->cursor_pos, area->cursor_pos + char_bytes, "", 0, 0);
}

void dock_backspace(Dock *area) {
    if (!area || area->cursor_pos <= 0)
        return;
    const char *prev = utf8_prev_char(area->buffer, &area->buffer[area->cursor_pos]);
    int prev_pos = prev - area->buffer;
    dock_replace_range(area, prev_pos, area->cursor_pos, "", 0, 0);
}

void dock_move_cursor(Dock *area, int pos) {
    if (!area)
        return;

    if (pos < 0)
        pos = 0;
    if (pos > area->length)
        pos = area->length;

    /* Always mark user input, even if position doesn't change */
    area->user_input_received = 1;

    if (area->cursor_pos != pos) {
        area->cursor_pos = pos;
        /* Update selection end if selection is active */
        if (area->selection_active) {
            area->selection_end = pos;
        }
        area->needs_redraw = 1;
    }
}

void dock_move_cursor_left(Dock *area) {
    if (!area || area->cursor_pos <= 0) {
        if (area)
            area->user_input_received = 1; /* Still mark input even if no movement */
        return;
    }
    const char *prev = utf8_prev_char(area->buffer, &area->buffer[area->cursor_pos]);
    dock_move_cursor(area, prev - area->buffer);
}

void dock_move_cursor_right(Dock *area) {
    if (!area || area->cursor_pos >= area->length) {
        if (area)
            area->user_input_received = 1;
        return;
    }
    int char_bytes = utf8_char_bytes(&area->buffer[area->cursor_pos]);
    dock_move_cursor(area, area->cursor_pos + ((char_bytes > 0) ? char_bytes : 1));
}

void dock_move_cursor_home(Dock *area) {
    if (!area)
        return;
    dock_move_cursor(area, 0);
}

void dock_move_cursor_end(Dock *area) {
    if (!area)
        return;
    dock_move_cursor(area, area->length);
}

/* Find start of word before cursor */
static int find_word_start(const char *buffer, int cursor_pos) {
    if (cursor_pos == 0)
        return 0;

    /* Skip whitespace before cursor */
    int pos = cursor_pos - 1;
    while (pos > 0 && isspace((unsigned char)buffer[pos])) {
        pos--;
    }

    /* Find start of word */
    while (pos > 0 && !isspace((unsigned char)buffer[pos - 1])) {
        pos--;
    }

    return pos;
}

/* Find end of word after cursor */
static int find_word_end(const char *buffer, int length, int cursor_pos) {
    if (cursor_pos >= length)
        return length;

    /* Skip whitespace at cursor */
    int pos = cursor_pos;
    while (pos < length && isspace((unsigned char)buffer[pos])) {
        pos++;
    }

    /* Find end of word */
    while (pos < length && !isspace((unsigned char)buffer[pos])) {
        pos++;
    }

    return pos;
}

void dock_move_cursor_word_left(Dock *area) {
    if (!area)
        return;
    dock_move_cursor(area, find_word_start(area->buffer, area->cursor_pos));
}

void dock_move_cursor_word_right(Dock *area) {
    if (!area)
        return;
    dock_move_cursor(area, find_word_end(area->buffer, area->length, area->cursor_pos));
}

void dock_move_cursor_beginning(Dock *area) {
    dock_move_cursor_home(area);
}

void dock_move_cursor_end_line(Dock *area) {
    dock_move_cursor_end(area);
}

void dock_kill_to_end(Dock *area) {
    if (!area || area->cursor_pos >= area->length)
        return;
    dock_replace_range(area, area->cursor_pos, area->length, "", 0, 1);
}

void dock_kill_from_start(Dock *area) {
    if (!area || area->cursor_pos <= 0)
        return;
    dock_replace_range(area, 0, area->cursor_pos, "", 0, 1);
}

void dock_kill_word(Dock *area) {
    if (!area || area->cursor_pos <= 0)
        return;
    int word_start = find_word_start(area->buffer, area->cursor_pos);
    if (word_start < area->cursor_pos)
        dock_replace_range(area, word_start, area->cursor_pos, "", 0, 1);
}

void dock_yank(Dock *area) {
    if (!area)
        return;
    int kill_len = strlen(area->kill_ring);
    if (kill_len > 0)
        dock_replace_range(area, area->cursor_pos, area->cursor_pos, area->kill_ring, kill_len, 0);
}

const char *dock_copy(Dock *area) {
    if (!area)
        return NULL;

    /* Return pointer to current buffer text (caller will copy to clipboard) */
    return area->buffer;
}

void dock_paste(Dock *area, const char *text) {
    if (!area || !text)
        return;
    int text_len = strlen(text);
    if (text_len > 0)
        dock_replace_range(area, area->cursor_pos, area->cursor_pos, text, text_len, 0);
}

const char *dock_get_kill_ring(Dock *area) {
    if (!area)
        return NULL;
    return area->kill_ring;
}

void dock_start_selection(Dock *area) {
    if (!area)
        return;
    area->selection_active = 1;
    area->selection_start = area->cursor_pos;
    area->selection_end = area->cursor_pos;
    area->needs_redraw = 1;
}

void dock_clear_selection(Dock *area) {
    if (!area)
        return;
    if (area->selection_active) {
        area->selection_active = 0;
        area->needs_redraw = 1;
    }
}

int dock_has_selection(Dock *area) {
    if (!area)
        return 0;
    return area->selection_active && (area->selection_start != area->selection_end);
}

void dock_get_selection_range(Dock *area, int *start, int *end) {
    if (!area || !start || !end)
        return;

    if (area->selection_active) {
        /* Return selection in ascending order */
        if (area->selection_start < area->selection_end) {
            *start = area->selection_start;
            *end = area->selection_end;
        } else {
            *start = area->selection_end;
            *end = area->selection_start;
        }
    } else {
        *start = 0;
        *end = 0;
    }
}

int dock_copy_selection(Dock *area, char *dest, int dest_size) {
    if (!area || !dest || dest_size <= 0)
        return 0;

    if (!dock_has_selection(area)) {
        dest[0] = '\0';
        return 0;
    }

    int start, end;
    dock_get_selection_range(area, &start, &end);

    int copy_len = end - start;
    if (copy_len > dest_size - 1)
        copy_len = dest_size - 1;

    memcpy(dest, &area->buffer[start], copy_len);
    dest[copy_len] = '\0';

    return copy_len;
}

void dock_history_save_current(Dock *area) {
    if (!area)
        return;

    /* Save current input if we're on a new entry */
    if (area->history_index == -1) {
        memcpy(area->saved_input, area->buffer, area->length + 1);
    }
}

void dock_history_restore_current(Dock *area) {
    if (!area)
        return;

    /* Restore saved input */
    strcpy(area->buffer, area->saved_input);
    area->length = strlen(area->buffer);
    area->cursor_pos = area->length;
    area->history_index = -1;
    area->needs_redraw = 1;
    area->needs_layout_recalc = 1;
    area->user_input_received = 1;

    /* Reset prefix search */
    area->history_search_active = 0;
    area->history_search_prefix[0] = '\0';
}

void dock_history_add(Dock *area) {
    if (!area)
        return;

    /* Don't add empty entries */
    if (area->length == 0)
        return;

    /* Search for duplicate anywhere in history and remove it */
    for (int i = 0; i < area->history_count; i++) {
        if (strcmp(area->buffer, area->history[i]) == 0) {
            /* Found duplicate - remove it by shifting entries after it */
            char *duplicate = area->history[i];
            memmove(&area->history[i], &area->history[i + 1], (area->history_count - i - 1) * sizeof(char *));
            /* Put the removed entry at the end for reuse */
            area->history[area->history_count - 1] = duplicate;
            area->history_count--;
            break;
        }
    }

    /* Shift history if full */
    if (area->history_count >= area->history_max_size) {
        /* Remove oldest entry by shifting pointers */
        char *oldest = area->history[0];
        memmove(&area->history[0], &area->history[1], (area->history_max_size - 1) * sizeof(char *));
        area->history[area->history_max_size - 1] = oldest;
        area->history_count--;
    }

    /* Add new entry */
    memcpy(area->history[area->history_count], area->buffer, area->length + 1);
    area->history_count++;
    area->history_index = -1;
}

void dock_history_prev(Dock *area) {
    if (!area)
        return;

    if (area->history_count == 0)
        return;

    /* Starting history navigation */
    if (area->history_index == -1) {
        dock_history_save_current(area);

        /* If cursor is at end of line, enable prefix search */
        if (area->cursor_pos == area->length) {
            area->history_search_active = 1;
            memcpy(area->history_search_prefix, area->buffer, area->length + 1);
        } else {
            area->history_search_active = 0;
            area->history_search_prefix[0] = '\0';
        }
    }

    /* Determine search start position */
    int start_idx = (area->history_index < 0) ? area->history_count - 1 : area->history_index - 1;

    /* Search backwards through history */
    for (int i = start_idx; i >= 0; i--) {
        /* Check if entry matches (prefix search or no filter) */
        int matches = 0;
        if (area->history_search_active) {
            /* Prefix search: check if history entry starts with prefix */
            int prefix_len = strlen(area->history_search_prefix);
            if (prefix_len == 0 || strncmp(area->history[i], area->history_search_prefix, prefix_len) == 0) {
                matches = 1;
            }
        } else {
            /* No filter: all entries match */
            matches = 1;
        }

        if (matches) {
            /* Load this history entry */
            area->history_index = i;
            strcpy(area->buffer, area->history[area->history_index]);
            area->length = strlen(area->buffer);
            area->cursor_pos = area->length;
            area->needs_redraw = 1;
            area->needs_layout_recalc = 1;
            area->user_input_received = 1;
            return;
        }
    }

    /* No match found - stay at current position */
}

void dock_history_next(Dock *area) {
    if (!area)
        return;

    if (area->history_index < 0)
        return; /* Not in history navigation */

    /* Search forward through history */
    for (int i = area->history_index + 1; i < area->history_count; i++) {
        /* Check if entry matches (prefix search or no filter) */
        int matches = 0;
        if (area->history_search_active) {
            /* Prefix search: check if history entry starts with prefix */
            int prefix_len = strlen(area->history_search_prefix);
            if (prefix_len == 0 || strncmp(area->history[i], area->history_search_prefix, prefix_len) == 0) {
                matches = 1;
            }
        } else {
            /* No filter: all entries match */
            matches = 1;
        }

        if (matches) {
            /* Load this history entry */
            area->history_index = i;
            strcpy(area->buffer, area->history[area->history_index]);
            area->length = strlen(area->buffer);
            area->cursor_pos = area->length;
            area->needs_redraw = 1;
            area->needs_layout_recalc = 1;
            area->user_input_received = 1;
            return;
        }
    }

    /* No more matches - restore saved input */
    dock_history_restore_current(area);
    area->history_search_active = 0;
    area->history_search_prefix[0] = '\0';
}

void dock_clear(Dock *area) {
    if (!area || area->length <= 0)
        return;
    dock_replace_range(area, 0, area->length, "", 0, 0);
}

/* Undo/Redo operations */

int dock_undo(Dock *area) {
    if (!area || area->undo_count == 0)
        return 0;

    /* Push current state to redo stack */
    redo_push(area);

    /* Pop from undo stack and restore */
    UndoEntry *entry = &area->undo_stack[area->undo_head];
    memcpy(area->buffer, entry->buffer, entry->length + 1);
    area->cursor_pos = entry->cursor_pos;
    area->length = entry->length;

    /* Update undo stack pointers */
    area->undo_head = (area->undo_head - 1 + UNDO_STACK_SIZE) % UNDO_STACK_SIZE;
    area->undo_count--;

    /* Break coalescing */
    area->undo_coalesce_active = 0;
    area->undo_last_op = UNDO_OP_NONE;

    /* Request redraw */
    area->needs_redraw = 1;
    area->needs_layout_recalc = 1;
    area->user_input_received = 1;

    /* Reset history search */
    reset_history_search(area);

    return 1;
}

int dock_redo(Dock *area) {
    if (!area || area->redo_count == 0)
        return 0;

    /* Push current state to undo stack */
    undo_push(area);

    /* Pop from redo stack and restore */
    UndoEntry *entry = &area->redo_stack[area->redo_head];
    memcpy(area->buffer, entry->buffer, entry->length + 1);
    area->cursor_pos = entry->cursor_pos;
    area->length = entry->length;

    /* Update redo stack pointers */
    area->redo_head = (area->redo_head - 1 + UNDO_STACK_SIZE) % UNDO_STACK_SIZE;
    area->redo_count--;

    /* Break coalescing */
    area->undo_coalesce_active = 0;
    area->undo_last_op = UNDO_OP_NONE;

    /* Request redraw */
    area->needs_redraw = 1;
    area->needs_layout_recalc = 1;
    area->user_input_received = 1;

    /* Reset history search */
    reset_history_search(area);

    return 1;
}

int dock_can_undo(Dock *area) {
    if (!area)
        return 0;
    return area->undo_count > 0;
}

int dock_can_redo(Dock *area) {
    if (!area)
        return 0;
    return area->redo_count > 0;
}

void dock_undo_clear(Dock *area) {
    if (!area)
        return;
    area->undo_count = 0;
    area->undo_head = -1;
    area->redo_count = 0;
    area->redo_head = -1;
    area->undo_coalesce_active = 0;
    area->undo_last_op = UNDO_OP_NONE;
}

const char *dock_get_text(Dock *area) {
    if (!area)
        return NULL;
    return area->buffer;
}

int dock_get_length(Dock *area) {
    if (!area)
        return 0;
    return area->length;
}

int dock_get_cursor_pos(Dock *area) {
    if (!area)
        return 0;
    return area->cursor_pos;
}

int dock_needs_redraw(Dock *area) {
    if (!area)
        return 0;
    return area->needs_redraw;
}

void dock_request_redraw(Dock *area) {
    if (!area)
        return;
    area->needs_redraw = 1;
}

void dock_mark_drawn(Dock *area) {
    if (!area)
        return;
    area->needs_redraw = 0;
}

char *dock_get_buffer(Dock *area) {
    if (!area)
        return NULL;
    return area->buffer;
}

void dock_sync_state(Dock *area) {
    if (!area)
        return;
    /* Recalculate length from buffer */
    area->length = strlen(area->buffer);
    /* Ensure cursor is within bounds */
    if (area->cursor_pos > area->length) {
        area->cursor_pos = area->length;
    }
    area->needs_redraw = 1;
    /* Clear undo history - external modifications invalidate it */
    dock_undo_clear(area);
}

/* Mode management */
void dock_set_mode(Dock *area, DockMode mode) {
    if (!area)
        return;

    if (area->mode != mode) {
        DockMode old_mode = area->mode;
        area->mode = mode;
        area->needs_redraw = 1;

        /* Auto-manage eval divider mode indicator */
        if (mode == DOCK_MODE_EVAL && old_mode != DOCK_MODE_EVAL) {
            /* Entering eval mode - show lightning bolt */
            lisp_x_set_divider_mode("eval", "\xE2\x9A\xA1", 10); /* ⚡ U+26A1 */
        } else if (mode != DOCK_MODE_EVAL && old_mode == DOCK_MODE_EVAL) {
            /* Leaving eval mode - remove indicator */
            lisp_x_remove_divider_mode("eval");
        }
    }
}

DockMode dock_get_mode(Dock *area) {
    if (!area)
        return DOCK_MODE_NORMAL;
    return area->mode;
}

/* Multi-line support functions */

/* Calculate visual rows considering wrapping and explicit newlines */
static int calculate_visual_rows(const char *buffer, int length, int cols) {
    if (cols <= 0)
        cols = 80; /* Default to avoid division by zero */
    if (length == 0)
        return 1; /* Empty buffer still takes one row */

    int visual_rows = 0;
    int current_col = 0;

    for (int i = 0; i < length; i++) {
        if (buffer[i] == '\n') {
            visual_rows++;
            current_col = 0;
        } else {
            current_col++;
            if (current_col >= cols) {
                visual_rows++;
                current_col = 0;
            }
        }
    }

    /* Always count the final row where the cursor is (even if empty after newline) */
    visual_rows++;

    return visual_rows;
}

void dock_recalculate_layout(Dock *area, int terminal_cols) {
    if (!area)
        return;

    if (!area->needs_layout_recalc)
        return;

    /* Find all explicit newlines */
    area->line_break_count = 0;
    for (int i = 0; i < area->length && area->line_break_count < 64; i++) {
        if (area->buffer[i] == '\n') {
            area->line_breaks[area->line_break_count++] = i;
        }
    }

    /* Calculate visual rows considering wrapping */
    area->visual_rows = calculate_visual_rows(area->buffer, area->length, terminal_cols);

    /* Update visible rows (capped at max_visible_rows) */
    area->visible_rows = (area->visual_rows < area->max_visible_rows) ? area->visual_rows : area->max_visible_rows;

    /* Ensure cursor is visible */
    dock_ensure_cursor_visible(area, terminal_cols);

    area->needs_layout_recalc = 0;
}

void dock_get_cursor_visual_position(Dock *area, int cols, int *out_row, int *out_col) {
    if (!area || !out_row || !out_col) {
        if (out_row)
            *out_row = 0;
        if (out_col)
            *out_col = 0;
        return;
    }

    if (cols <= 0)
        cols = 80; /* Default */

    int visual_row = 0;
    int visual_col = 0;

    for (int i = 0; i < area->cursor_pos;) {
        if (area->buffer[i] == '\n') {
            visual_row++;
            visual_col = 0;
            i++;
        } else {
            /* Get UTF-8 character info */
            int cp = utf8_get_codepoint(&area->buffer[i]);
            int char_bytes = utf8_char_bytes(&area->buffer[i]);
            if (char_bytes <= 0)
                char_bytes = 1; /* Safety: skip at least 1 byte */
            int char_width = utf8_codepoint_width(cp);

            visual_col += char_width;
            if (visual_col >= cols) {
                visual_row++;
                visual_col = 0;
            }
            i += char_bytes;
        }
    }

    *out_row = visual_row;
    *out_col = visual_col;
}

void dock_ensure_cursor_visible(Dock *area, int cols) {
    if (!area)
        return;

    int cursor_row, cursor_col;
    dock_get_cursor_visual_position(area, cols, &cursor_row, &cursor_col);

    /* Scroll up if cursor is above viewport */
    if (cursor_row < area->scroll_offset) {
        area->scroll_offset = cursor_row;
    }

    /* Scroll down if cursor is below viewport */
    int viewport_bottom = area->scroll_offset + area->visible_rows - 1;
    if (cursor_row > viewport_bottom) {
        area->scroll_offset = cursor_row - area->visible_rows + 1;
    }

    /* Clamp scroll_offset to valid range */
    int max_scroll = area->visual_rows - area->visible_rows;
    if (max_scroll < 0)
        max_scroll = 0;
    if (area->scroll_offset > max_scroll)
        area->scroll_offset = max_scroll;
    if (area->scroll_offset < 0)
        area->scroll_offset = 0;
}

int dock_get_text_rows(Dock *area) {
    if (!area)
        return 1;
    return area->visible_rows;
}

int dock_is_at_first_visual_line(Dock *area, int cols) {
    if (!area)
        return 1;

    int cursor_row, cursor_col;
    dock_get_cursor_visual_position(area, cols, &cursor_row, &cursor_col);
    return (cursor_row == 0);
}

int dock_is_at_last_visual_line(Dock *area, int cols) {
    if (!area)
        return 1;

    int cursor_row, cursor_col;
    dock_get_cursor_visual_position(area, cols, &cursor_row, &cursor_col);
    return (cursor_row >= area->visual_rows - 1);
}

void dock_move_cursor_up_line(Dock *area, int cols) {
    if (!area || cols <= 0)
        return;

    int cursor_row, cursor_col;
    dock_get_cursor_visual_position(area, cols, &cursor_row, &cursor_col);

    int target_pos = area->cursor_pos; /* Default: stay at current position */

    if (cursor_row > 0) {
        /* Find position in previous visual line at same column */
        int target_row = cursor_row - 1;
        int current_row = 0;
        int current_col = 0;
        target_pos = 0;

        for (int i = 0; i <= area->length; i++) {
            if (current_row == target_row && current_col == cursor_col) {
                target_pos = i;
                break;
            }
            if (current_row == target_row && current_col > cursor_col) {
                target_pos = i;
                break;
            }
            if (i == area->length) {
                target_pos = i;
                break;
            }

            if (area->buffer[i] == '\n') {
                current_row++;
                current_col = 0;
                if (current_row == target_row + 1 && current_col == 0) {
                    target_pos = i;
                    break;
                }
            } else {
                current_col++;
                if (current_col >= cols) {
                    current_row++;
                    current_col = 0;
                }
            }
        }
    }

    dock_move_cursor(area, target_pos);
}

void dock_move_cursor_down_line(Dock *area, int cols) {
    if (!area || cols <= 0)
        return;

    int cursor_row, cursor_col;
    dock_get_cursor_visual_position(area, cols, &cursor_row, &cursor_col);

    int target_pos = area->cursor_pos; /* Default: stay at current position */

    if (cursor_row < area->visual_rows - 1) {
        /* Find position in next visual line at same column */
        int target_row = cursor_row + 1;
        int current_row = 0;
        int current_col = 0;
        target_pos = area->length;

        for (int i = 0; i <= area->length; i++) {
            if (current_row == target_row && current_col == cursor_col) {
                target_pos = i;
                break;
            }
            if (current_row == target_row && current_col > cursor_col) {
                target_pos = i;
                break;
            }
            if (i == area->length) {
                target_pos = i;
                break;
            }

            if (area->buffer[i] == '\n') {
                current_row++;
                current_col = 0;
                if (current_row == target_row + 1) {
                    target_pos = i;
                    break;
                }
            } else {
                current_col++;
                if (current_col >= cols) {
                    current_row++;
                    current_col = 0;
                }
            }
        }
    }

    dock_move_cursor(area, target_pos);
}
