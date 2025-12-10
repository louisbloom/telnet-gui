/* Input area implementation with readline-like functionality */

#include "input_area.h"
#include "lisp.h"
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

/* Helper: Reset history prefix search (call when user edits text) */
static void reset_history_search(InputArea *area) {
    if (!area)
        return;
    area->history_search_active = 0;
    area->history_search_prefix[0] = '\0';
}

/* Helper: Clear selection (call when text is edited) */
static void clear_selection_if_active(InputArea *area) {
    if (!area)
        return;
    if (area->selection_active) {
        area->selection_active = 0;
        area->needs_redraw = 1;
    }
}

void input_area_init(InputArea *area) {
    if (!area)
        return;
    memset(area, 0, sizeof(InputArea));

    /* Get history size from Lisp config */
    area->history_max_size = lisp_x_get_input_history_size();

    /* Allocate history array */
    area->history = (char **)malloc(area->history_max_size * sizeof(char *));
    if (area->history) {
        for (int i = 0; i < area->history_max_size; i++) {
            area->history[i] = (char *)malloc(INPUT_AREA_MAX_LENGTH);
            if (area->history[i]) {
                area->history[i][0] = '\0';
            }
        }
    }

    area->history_index = -1; /* Start with new entry */
    area->mode = INPUT_AREA_MODE_NORMAL;

    /* Initialize multi-line fields */
    area->line_break_count = 0;
    area->visual_rows = 1;         /* Start with one row */
    area->visible_rows = 1;        /* Show one row initially */
    area->scroll_offset = 0;       /* No scrolling initially */
    area->max_visible_rows = 5;    /* Default: up to 5 rows before scrolling */
    area->needs_layout_recalc = 0; /* No recalc needed for empty buffer */
}

void input_area_insert_text(InputArea *area, const char *text, int text_len) {
    if (!area || !text || text_len <= 0)
        return;

    if (area->length + text_len < INPUT_AREA_MAX_LENGTH) {
        /* Make room for new text */
        memmove(&area->buffer[area->cursor_pos + text_len], &area->buffer[area->cursor_pos],
                area->length - area->cursor_pos);
        /* Insert new text */
        memcpy(&area->buffer[area->cursor_pos], text, text_len);
        area->cursor_pos += text_len;
        area->length += text_len;
        /* Null-terminate to ensure no stale data is read */
        if (area->length < INPUT_AREA_MAX_LENGTH) {
            area->buffer[area->length] = '\0';
        }
        area->needs_redraw = 1;
        area->needs_layout_recalc = 1; /* Recalculate visual rows for multi-line */

        /* Reset history prefix search when text is edited */
        reset_history_search(area);
        /* Clear selection when text is edited */
        clear_selection_if_active(area);
    }
}

void input_area_delete_char(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos < area->length) {
        memmove(&area->buffer[area->cursor_pos], &area->buffer[area->cursor_pos + 1],
                area->length - area->cursor_pos - 1);
        area->length--;
        /* Clear the last byte to ensure no stale data */
        if (area->length < INPUT_AREA_MAX_LENGTH) {
            area->buffer[area->length] = '\0';
        }
        area->needs_redraw = 1;
        area->needs_layout_recalc = 1; /* Recalculate visual rows for multi-line */

        /* Reset history prefix search when text is edited */
        reset_history_search(area);
        /* Clear selection when text is edited */
        clear_selection_if_active(area);
    }
}

void input_area_backspace(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos > 0) {
        memmove(&area->buffer[area->cursor_pos - 1], &area->buffer[area->cursor_pos], area->length - area->cursor_pos);
        area->cursor_pos--;
        area->length--;
        /* Clear the last byte to ensure no stale data */
        if (area->length < INPUT_AREA_MAX_LENGTH) {
            area->buffer[area->length] = '\0';
        }
        area->needs_redraw = 1;
        area->needs_layout_recalc = 1; /* Recalculate visual rows for multi-line */

        /* Reset history prefix search when text is edited */
        reset_history_search(area);
        /* Clear selection when text is edited */
        clear_selection_if_active(area);
    }
}

void input_area_move_cursor(InputArea *area, int pos) {
    if (!area)
        return;

    if (pos < 0)
        pos = 0;
    if (pos > area->length)
        pos = area->length;

    if (area->cursor_pos != pos) {
        area->cursor_pos = pos;
        /* Update selection end if selection is active */
        if (area->selection_active) {
            area->selection_end = pos;
        }
        area->needs_redraw = 1;
    }
}

void input_area_move_cursor_left(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos > 0) {
        area->cursor_pos--;
        /* Update selection end if selection is active */
        if (area->selection_active) {
            area->selection_end = area->cursor_pos;
        }
        area->needs_redraw = 1;
    }
}

void input_area_move_cursor_right(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos < area->length) {
        area->cursor_pos++;
        /* Update selection end if selection is active */
        if (area->selection_active) {
            area->selection_end = area->cursor_pos;
        }
        area->needs_redraw = 1;
    }
}

void input_area_move_cursor_home(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos != 0) {
        area->cursor_pos = 0;
        /* Update selection end if selection is active */
        if (area->selection_active) {
            area->selection_end = area->cursor_pos;
        }
        area->needs_redraw = 1;
    }
}

void input_area_move_cursor_end(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos != area->length) {
        area->cursor_pos = area->length;
        /* Update selection end if selection is active */
        if (area->selection_active) {
            area->selection_end = area->cursor_pos;
        }
        area->needs_redraw = 1;
    }
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

void input_area_move_cursor_word_left(InputArea *area) {
    if (!area)
        return;

    int new_pos = find_word_start(area->buffer, area->cursor_pos);
    if (new_pos != area->cursor_pos) {
        area->cursor_pos = new_pos;
        /* Update selection end if selection is active */
        if (area->selection_active) {
            area->selection_end = area->cursor_pos;
        }
        area->needs_redraw = 1;
    }
}

void input_area_move_cursor_word_right(InputArea *area) {
    if (!area)
        return;

    int new_pos = find_word_end(area->buffer, area->length, area->cursor_pos);
    if (new_pos != area->cursor_pos) {
        area->cursor_pos = new_pos;
        /* Update selection end if selection is active */
        if (area->selection_active) {
            area->selection_end = area->cursor_pos;
        }
        area->needs_redraw = 1;
    }
}

void input_area_move_cursor_beginning(InputArea *area) {
    input_area_move_cursor_home(area);
}

void input_area_move_cursor_end_line(InputArea *area) {
    input_area_move_cursor_end(area);
}

void input_area_kill_to_end(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos < area->length) {
        /* Copy killed text to kill ring */
        int kill_len = area->length - area->cursor_pos;
        memcpy(area->kill_ring, &area->buffer[area->cursor_pos], kill_len);
        area->kill_ring[kill_len] = '\0';

        /* Remove killed text */
        area->length = area->cursor_pos;
        area->buffer[area->length] = '\0';
        area->needs_redraw = 1;

        /* Reset history prefix search when text is edited */
        reset_history_search(area);
    }
}

void input_area_kill_from_start(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos > 0) {
        /* Copy killed text to kill ring */
        memcpy(area->kill_ring, area->buffer, area->cursor_pos);
        area->kill_ring[area->cursor_pos] = '\0';

        /* Remove killed text */
        memmove(area->buffer, &area->buffer[area->cursor_pos], area->length - area->cursor_pos);
        area->length -= area->cursor_pos;
        area->cursor_pos = 0;
        area->buffer[area->length] = '\0';
        area->needs_redraw = 1;

        /* Reset history prefix search when text is edited */
        reset_history_search(area);
    }
}

void input_area_kill_word(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos > 0) {
        int word_start = find_word_start(area->buffer, area->cursor_pos);
        int kill_len = area->cursor_pos - word_start;

        /* Copy killed text to kill ring */
        memcpy(area->kill_ring, &area->buffer[word_start], kill_len);
        area->kill_ring[kill_len] = '\0';

        /* Remove killed text */
        memmove(&area->buffer[word_start], &area->buffer[area->cursor_pos], area->length - area->cursor_pos);
        area->length -= kill_len;
        area->cursor_pos = word_start;
        area->buffer[area->length] = '\0';
        area->needs_redraw = 1;

        /* Reset history prefix search when text is edited */
        reset_history_search(area);
    }
}

void input_area_yank(InputArea *area) {
    if (!area)
        return;

    int kill_len = strlen(area->kill_ring);
    if (kill_len > 0 && area->length + kill_len < INPUT_AREA_MAX_LENGTH) {
        /* Make room for yanked text */
        memmove(&area->buffer[area->cursor_pos + kill_len], &area->buffer[area->cursor_pos],
                area->length - area->cursor_pos);
        /* Insert yanked text */
        memcpy(&area->buffer[area->cursor_pos], area->kill_ring, kill_len);
        area->cursor_pos += kill_len;
        area->length += kill_len;
        area->buffer[area->length] = '\0';
        area->needs_redraw = 1;

        /* Reset history prefix search when text is edited */
        reset_history_search(area);
    }
}

const char *input_area_copy(InputArea *area) {
    if (!area)
        return NULL;

    /* Return pointer to current buffer text (caller will copy to clipboard) */
    return area->buffer;
}

void input_area_paste(InputArea *area, const char *text) {
    if (!area || !text)
        return;

    int text_len = strlen(text);
    if (text_len > 0 && area->length + text_len < INPUT_AREA_MAX_LENGTH) {
        /* Make room for pasted text */
        memmove(&area->buffer[area->cursor_pos + text_len], &area->buffer[area->cursor_pos],
                area->length - area->cursor_pos);
        /* Insert pasted text */
        memcpy(&area->buffer[area->cursor_pos], text, text_len);
        area->cursor_pos += text_len;
        area->length += text_len;
        area->buffer[area->length] = '\0';
        area->needs_redraw = 1;
        area->needs_layout_recalc = 1; /* Recalculate visual rows for multi-line */

        /* Reset history prefix search when text is edited */
        reset_history_search(area);
    }
}

const char *input_area_get_kill_ring(InputArea *area) {
    if (!area)
        return NULL;
    return area->kill_ring;
}

void input_area_start_selection(InputArea *area) {
    if (!area)
        return;
    area->selection_active = 1;
    area->selection_start = area->cursor_pos;
    area->selection_end = area->cursor_pos;
    area->needs_redraw = 1;
}

void input_area_clear_selection(InputArea *area) {
    if (!area)
        return;
    if (area->selection_active) {
        area->selection_active = 0;
        area->needs_redraw = 1;
    }
}

int input_area_has_selection(InputArea *area) {
    if (!area)
        return 0;
    return area->selection_active && (area->selection_start != area->selection_end);
}

void input_area_get_selection_range(InputArea *area, int *start, int *end) {
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

int input_area_copy_selection(InputArea *area, char *dest, int dest_size) {
    if (!area || !dest || dest_size <= 0)
        return 0;

    if (!input_area_has_selection(area)) {
        dest[0] = '\0';
        return 0;
    }

    int start, end;
    input_area_get_selection_range(area, &start, &end);

    int copy_len = end - start;
    if (copy_len > dest_size - 1)
        copy_len = dest_size - 1;

    memcpy(dest, &area->buffer[start], copy_len);
    dest[copy_len] = '\0';

    return copy_len;
}

void input_area_history_save_current(InputArea *area) {
    if (!area)
        return;

    /* Save current input if we're on a new entry */
    if (area->history_index == -1) {
        memcpy(area->saved_input, area->buffer, area->length + 1);
    }
}

void input_area_history_restore_current(InputArea *area) {
    if (!area)
        return;

    /* Restore saved input */
    strcpy(area->buffer, area->saved_input);
    area->length = strlen(area->buffer);
    area->cursor_pos = area->length;
    area->history_index = -1;
    area->needs_redraw = 1;
    area->needs_layout_recalc = 1; /* Recalculate visual rows for multi-line */

    /* Reset prefix search */
    area->history_search_active = 0;
    area->history_search_prefix[0] = '\0';
}

void input_area_history_add(InputArea *area) {
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

void input_area_history_prev(InputArea *area) {
    if (!area)
        return;

    if (area->history_count == 0)
        return;

    /* Starting history navigation */
    if (area->history_index == -1) {
        input_area_history_save_current(area);

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
            area->needs_layout_recalc = 1; /* Recalculate visual rows for multi-line */
            return;
        }
    }

    /* No match found - stay at current position */
}

void input_area_history_next(InputArea *area) {
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
            area->needs_layout_recalc = 1; /* Recalculate visual rows for multi-line */
            return;
        }
    }

    /* No more matches - restore saved input */
    input_area_history_restore_current(area);
    area->history_search_active = 0;
    area->history_search_prefix[0] = '\0';
}

void input_area_clear(InputArea *area) {
    if (!area)
        return;

    area->buffer[0] = '\0';
    area->length = 0;
    area->cursor_pos = 0;
    area->needs_redraw = 1;
    area->needs_layout_recalc = 1; /* Recalculate visual rows for multi-line */
}

const char *input_area_get_text(InputArea *area) {
    if (!area)
        return NULL;
    return area->buffer;
}

int input_area_get_length(InputArea *area) {
    if (!area)
        return 0;
    return area->length;
}

int input_area_get_cursor_pos(InputArea *area) {
    if (!area)
        return 0;
    return area->cursor_pos;
}

int input_area_needs_redraw(InputArea *area) {
    if (!area)
        return 0;
    return area->needs_redraw;
}

void input_area_request_redraw(InputArea *area) {
    if (!area)
        return;
    area->needs_redraw = 1;
}

void input_area_mark_drawn(InputArea *area) {
    if (!area)
        return;
    area->needs_redraw = 0;
}

char *input_area_get_buffer(InputArea *area) {
    if (!area)
        return NULL;
    return area->buffer;
}

void input_area_sync_state(InputArea *area) {
    if (!area)
        return;
    /* Recalculate length from buffer */
    area->length = strlen(area->buffer);
    /* Ensure cursor is within bounds */
    if (area->cursor_pos > area->length) {
        area->cursor_pos = area->length;
    }
    area->needs_redraw = 1;
}

/* Mode management */
void input_area_set_mode(InputArea *area, InputAreaMode mode) {
    if (!area)
        return;

    area->mode = mode;
}

InputAreaMode input_area_get_mode(InputArea *area) {
    if (!area)
        return INPUT_AREA_MODE_NORMAL;
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

void input_area_recalculate_layout(InputArea *area, int terminal_cols) {
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
    input_area_ensure_cursor_visible(area, terminal_cols);

    area->needs_layout_recalc = 0;
}

void input_area_get_cursor_visual_position(InputArea *area, int cols, int *out_row, int *out_col) {
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

    for (int i = 0; i < area->cursor_pos; i++) {
        if (area->buffer[i] == '\n') {
            visual_row++;
            visual_col = 0;
        } else {
            visual_col++;
            if (visual_col >= cols) {
                visual_row++;
                visual_col = 0;
            }
        }
    }

    *out_row = visual_row;
    *out_col = visual_col;
}

void input_area_ensure_cursor_visible(InputArea *area, int cols) {
    if (!area)
        return;

    int cursor_row, cursor_col;
    input_area_get_cursor_visual_position(area, cols, &cursor_row, &cursor_col);

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

int input_area_get_visible_rows(InputArea *area) {
    if (!area)
        return 1;
    return area->visible_rows;
}

int input_area_is_at_first_visual_line(InputArea *area, int cols) {
    if (!area)
        return 1;

    int cursor_row, cursor_col;
    input_area_get_cursor_visual_position(area, cols, &cursor_row, &cursor_col);
    return (cursor_row == 0);
}

int input_area_is_at_last_visual_line(InputArea *area, int cols) {
    if (!area)
        return 1;

    int cursor_row, cursor_col;
    input_area_get_cursor_visual_position(area, cols, &cursor_row, &cursor_col);
    return (cursor_row >= area->visual_rows - 1);
}

void input_area_move_cursor_up_line(InputArea *area, int cols) {
    if (!area || cols <= 0)
        return;

    int cursor_row, cursor_col;
    input_area_get_cursor_visual_position(area, cols, &cursor_row, &cursor_col);

    if (cursor_row == 0)
        return; /* Already at first line */

    /* Find position in previous visual line at same column */
    int target_row = cursor_row - 1;
    int current_row = 0;
    int current_col = 0;
    int target_pos = 0;

    for (int i = 0; i <= area->length; i++) {
        if (current_row == target_row && current_col == cursor_col) {
            target_pos = i;
            break;
        }
        if (current_row == target_row && current_col > cursor_col) {
            /* Target column doesn't exist on previous line, use end of line */
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
                /* Previous line ends with newline */
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

    input_area_move_cursor(area, target_pos);
}

void input_area_move_cursor_down_line(InputArea *area, int cols) {
    if (!area || cols <= 0)
        return;

    int cursor_row, cursor_col;
    input_area_get_cursor_visual_position(area, cols, &cursor_row, &cursor_col);

    if (cursor_row >= area->visual_rows - 1)
        return; /* Already at last line */

    /* Find position in next visual line at same column */
    int target_row = cursor_row + 1;
    int current_row = 0;
    int current_col = 0;
    int target_pos = area->length;

    for (int i = 0; i <= area->length; i++) {
        if (current_row == target_row && current_col == cursor_col) {
            target_pos = i;
            break;
        }
        if (current_row == target_row && current_col > cursor_col) {
            /* Target column doesn't exist on next line, use end of line */
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
                /* Next line ends with newline */
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

    input_area_move_cursor(area, target_pos);
}
