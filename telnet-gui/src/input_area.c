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
    /* Initialize status area with mode indicator */
    strcpy(area->mode_text, "N");
    area->mode_length = 1;
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

    if (area->mode != mode) {
        area->mode = mode;
        /* Update status area text based on mode */
        if (mode == INPUT_AREA_MODE_NORMAL) {
            strcpy(area->mode_text, "N");
            area->mode_length = 1;
        } else if (mode == INPUT_AREA_MODE_EVAL) {
            strcpy(area->mode_text, "E");
            area->mode_length = 1;
        }
        area->mode_needs_redraw = 1;
        area->needs_redraw = 1;
    }
}

InputAreaMode input_area_get_mode(InputArea *area) {
    if (!area)
        return INPUT_AREA_MODE_NORMAL;
    return area->mode;
}

/* Mode display area management */
void input_area_mode_set_text(InputArea *area, const char *text) {
    if (!area || !text)
        return;

    int len = strlen(text);
    if (len >= 256)
        len = 255;

    strncpy(area->mode_text, text, len);
    area->mode_text[len] = '\0';
    area->mode_length = len;
    area->mode_needs_redraw = 1;
    area->needs_redraw = 1;
}

void input_area_mode_clear(InputArea *area) {
    if (!area)
        return;

    area->mode_text[0] = '\0';
    area->mode_length = 0;
    area->mode_needs_redraw = 1;
    area->needs_redraw = 1;
}

const char *input_area_mode_get_text(InputArea *area) {
    if (!area)
        return NULL;
    return area->mode_text;
}

int input_area_mode_get_length(InputArea *area) {
    if (!area)
        return 0;
    return area->mode_length;
}

int input_area_mode_needs_redraw(InputArea *area) {
    if (!area)
        return 0;
    return area->mode_needs_redraw;
}

void input_area_mode_mark_drawn(InputArea *area) {
    if (!area)
        return;
    area->mode_needs_redraw = 0;
}

/* Update mode display to show Lisp mode data structure */
void input_area_update_mode(InputArea *area, int connected) {
    if (!area)
        return;

    /* Update connection mode in Lisp environment */
    lisp_x_set_connection_mode(connected);

    /* Update input mode in Lisp environment */
    InputAreaMode input_mode = input_area_get_mode(area);
    lisp_x_set_input_mode(input_mode);

    /* Get mode string from Lisp environment and update display */
    const char *mode_text = lisp_x_get_mode_string();
    input_area_mode_set_text(area, mode_text);
}
