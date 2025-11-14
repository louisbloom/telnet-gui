/* Input area implementation with readline-like functionality */

#include "input_area.h"
#include <string.h>
#include <ctype.h>

void input_area_init(InputArea *area) {
    if (!area)
        return;
    memset(area, 0, sizeof(InputArea));
    area->history_index = -1; /* Start with new entry */
    area->mode = INPUT_AREA_MODE_NORMAL;
    /* Initialize visual area with mode indicator */
    strcpy(area->visual_text, "N");
    area->visual_length = 1;
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
        area->needs_redraw = 1;
    }
}

void input_area_move_cursor_left(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos > 0) {
        area->cursor_pos--;
        area->needs_redraw = 1;
    }
}

void input_area_move_cursor_right(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos < area->length) {
        area->cursor_pos++;
        area->needs_redraw = 1;
    }
}

void input_area_move_cursor_home(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos != 0) {
        area->cursor_pos = 0;
        area->needs_redraw = 1;
    }
}

void input_area_move_cursor_end(InputArea *area) {
    if (!area)
        return;

    if (area->cursor_pos != area->length) {
        area->cursor_pos = area->length;
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
        area->needs_redraw = 1;
    }
}

void input_area_move_cursor_word_right(InputArea *area) {
    if (!area)
        return;

    int new_pos = find_word_end(area->buffer, area->length, area->cursor_pos);
    if (new_pos != area->cursor_pos) {
        area->cursor_pos = new_pos;
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
    }
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
}

void input_area_history_add(InputArea *area) {
    if (!area)
        return;

    /* Don't add empty entries */
    if (area->length == 0)
        return;

    /* Don't add duplicate of last entry */
    if (area->history_count > 0) {
        if (strcmp(area->buffer, area->history[area->history_count - 1]) == 0) {
            area->history_index = -1;
            return;
        }
    }

    /* Shift history if full */
    if (area->history_count >= INPUT_AREA_HISTORY_SIZE) {
        /* Remove oldest entry */
        memmove(&area->history[0], &area->history[1], (INPUT_AREA_HISTORY_SIZE - 1) * INPUT_AREA_MAX_LENGTH);
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

    /* Save current input if we're starting history navigation */
    if (area->history_index == -1) {
        input_area_history_save_current(area);
    }

    /* Navigate to previous entry */
    if (area->history_count > 0) {
        if (area->history_index < 0) {
            area->history_index = area->history_count - 1;
        } else if (area->history_index > 0) {
            area->history_index--;
        }

        /* Load history entry */
        strcpy(area->buffer, area->history[area->history_index]);
        area->length = strlen(area->buffer);
        area->cursor_pos = area->length;
        area->needs_redraw = 1;
    }
}

void input_area_history_next(InputArea *area) {
    if (!area)
        return;

    if (area->history_index >= 0) {
        area->history_index++;
        if (area->history_index >= area->history_count) {
            /* Restore saved input */
            input_area_history_restore_current(area);
        } else {
            /* Load next history entry */
            strcpy(area->buffer, area->history[area->history_index]);
            area->length = strlen(area->buffer);
            area->cursor_pos = area->length;
            area->needs_redraw = 1;
        }
    }
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
        /* Update visual area text based on mode */
        if (mode == INPUT_AREA_MODE_NORMAL) {
            strcpy(area->visual_text, "N");
            area->visual_length = 1;
        } else if (mode == INPUT_AREA_MODE_EVAL) {
            strcpy(area->visual_text, "E");
            area->visual_length = 1;
        }
        area->visual_needs_redraw = 1;
        area->needs_redraw = 1;
    }
}

InputAreaMode input_area_get_mode(InputArea *area) {
    if (!area)
        return INPUT_AREA_MODE_NORMAL;
    return area->mode;
}

/* Visual area management */
void input_area_visual_set_text(InputArea *area, const char *text) {
    if (!area || !text)
        return;

    int len = strlen(text);
    if (len >= 256)
        len = 255;

    strncpy(area->visual_text, text, len);
    area->visual_text[len] = '\0';
    area->visual_length = len;
    area->visual_needs_redraw = 1;
    area->needs_redraw = 1;
}

void input_area_visual_clear(InputArea *area) {
    if (!area)
        return;

    area->visual_text[0] = '\0';
    area->visual_length = 0;
    area->visual_needs_redraw = 1;
    area->needs_redraw = 1;
}

const char *input_area_visual_get_text(InputArea *area) {
    if (!area)
        return NULL;
    return area->visual_text;
}

int input_area_visual_get_length(InputArea *area) {
    if (!area)
        return 0;
    return area->visual_length;
}

int input_area_visual_needs_redraw(InputArea *area) {
    if (!area)
        return 0;
    return area->visual_needs_redraw;
}

void input_area_visual_mark_drawn(InputArea *area) {
    if (!area)
        return;
    area->visual_needs_redraw = 0;
}
