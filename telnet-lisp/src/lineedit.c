/*
 * lineedit.c - Minimal line editor with completion and history
 *
 * Cross-platform implementation using raw terminal input.
 */

#include "lineedit.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <conio.h>
#include <io.h>
#define isatty _isatty
#define fileno _fileno
#else
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#endif

/* Configuration */
#define DEFAULT_HISTORY_SIZE 100
#define DEFAULT_LINE_SIZE 4096
#define MAX_COMPLETIONS_DISPLAY 20

/* Key codes */
#define KEY_CTRL_A 1
#define KEY_CTRL_B 2
#define KEY_CTRL_C 3
#define KEY_CTRL_D 4
#define KEY_CTRL_E 5
#define KEY_CTRL_F 6
#define KEY_CTRL_K 11
#define KEY_CTRL_L 12
#define KEY_CTRL_N 14
#define KEY_CTRL_P 16
#define KEY_CTRL_U 21
#define KEY_CTRL_W 23
#define KEY_TAB 9
#define KEY_ENTER 13
#define KEY_ESC 27
#define KEY_BACKSPACE 127

/* ANSI escape sequences */
#define ANSI_CLEAR_RIGHT "\033[K"
#define ANSI_MOVE_LEFT "\033[D"
#define ANSI_MOVE_RIGHT "\033[C"
#define ANSI_CURSOR_HIDE "\033[?25l"
#define ANSI_CURSOR_SHOW "\033[?25h"

/* Line editor state */
struct LineEditState {
    /* Line buffer */
    char *buf;
    int len;
    int pos;
    int capacity;

    /* History */
    char **history;
    int history_size;
    int history_capacity;
    int history_index;   /* -1 when not navigating history */
    char *history_saved; /* Saved current line when navigating */

    /* Completion */
    lineedit_completer_fn completer;
    void *completer_userdata;
    char **completions;
    int completion_count;
    int completion_index;

#ifndef _WIN32
    /* Terminal state (Unix) */
    struct termios orig_termios;
    int raw_mode;
#endif
};

/* Forward declarations */
static int lineedit_getchar(LineEditState *state);
static void lineedit_refresh(LineEditState *state, const char *prompt);
static void enable_raw_mode(LineEditState *state);
static void disable_raw_mode(LineEditState *state);
static int get_terminal_width(void);
static void lineedit_show_completions(LineEditState *state, const char *prompt);

/*
 * Create a new line editor state.
 */
LineEditState *lineedit_create(void) {
    LineEditState *state = calloc(1, sizeof(LineEditState));
    if (!state)
        return NULL;

    state->capacity = DEFAULT_LINE_SIZE;
    state->buf = malloc(state->capacity);
    if (!state->buf) {
        free(state);
        return NULL;
    }
    state->buf[0] = '\0';
    state->len = 0;
    state->pos = 0;

    state->history_capacity = DEFAULT_HISTORY_SIZE;
    state->history = calloc(state->history_capacity, sizeof(char *));
    if (!state->history) {
        free(state->buf);
        free(state);
        return NULL;
    }
    state->history_size = 0;
    state->history_index = -1;
    state->history_saved = NULL;

    state->completer = NULL;
    state->completer_userdata = NULL;
    state->completions = NULL;
    state->completion_count = 0;
    state->completion_index = -1;

#ifndef _WIN32
    state->raw_mode = 0;
#endif

    return state;
}

/*
 * Destroy a line editor state.
 */
void lineedit_destroy(LineEditState *state) {
    if (!state)
        return;

    free(state->buf);
    free(state->history_saved);

    for (int i = 0; i < state->history_size; i++) {
        free(state->history[i]);
    }
    free(state->history);

    lineedit_free_completions(state->completions);

    free(state);
}

/*
 * Set completion callback.
 */
void lineedit_set_completer(LineEditState *state, lineedit_completer_fn fn, void *userdata) {
    if (!state)
        return;
    state->completer = fn;
    state->completer_userdata = userdata;
}

/*
 * Set history size.
 */
void lineedit_set_history_size(LineEditState *state, int max_size) {
    if (!state || max_size < 1)
        return;

    /* Shrink if necessary */
    while (state->history_size > max_size) {
        free(state->history[0]);
        memmove(state->history, state->history + 1, (state->history_size - 1) * sizeof(char *));
        state->history_size--;
    }

    /* Resize array */
    char **new_history = realloc(state->history, max_size * sizeof(char *));
    if (new_history) {
        state->history = new_history;
        state->history_capacity = max_size;
    }
}

/*
 * Free completions array.
 */
void lineedit_free_completions(char **completions) {
    if (!completions)
        return;

    for (int i = 0; completions[i]; i++) {
        free(completions[i]);
    }
    free(completions);
}

/*
 * Add line to history.
 */
void lineedit_history_add(LineEditState *state, const char *line) {
    if (!state || !line || !*line)
        return;

    /* Skip duplicates of most recent */
    if (state->history_size > 0 && strcmp(state->history[state->history_size - 1], line) == 0) {
        return;
    }

    /* Make room if full */
    if (state->history_size >= state->history_capacity) {
        free(state->history[0]);
        memmove(state->history, state->history + 1, (state->history_size - 1) * sizeof(char *));
        state->history_size--;
    }

    /* Add new entry */
    state->history[state->history_size] = strdup(line);
    if (state->history[state->history_size]) {
        state->history_size++;
    }
}

/*
 * Clear history.
 */
void lineedit_history_clear(LineEditState *state) {
    if (!state)
        return;

    for (int i = 0; i < state->history_size; i++) {
        free(state->history[i]);
        state->history[i] = NULL;
    }
    state->history_size = 0;
}

/*
 * Enable raw terminal mode (Unix).
 */
static void enable_raw_mode(LineEditState *state) {
#ifndef _WIN32
    if (state->raw_mode || !isatty(fileno(stdin)))
        return;

    if (tcgetattr(fileno(stdin), &state->orig_termios) == -1)
        return;

    struct termios raw = state->orig_termios;
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 1;
    raw.c_cc[VTIME] = 0;

    if (tcsetattr(fileno(stdin), TCSAFLUSH, &raw) == 0) {
        state->raw_mode = 1;
    }
#else
    (void)state;
#endif
}

/*
 * Disable raw terminal mode (Unix).
 */
static void disable_raw_mode(LineEditState *state) {
#ifndef _WIN32
    if (state->raw_mode) {
        tcsetattr(fileno(stdin), TCSAFLUSH, &state->orig_termios);
        state->raw_mode = 0;
    }
#else
    (void)state;
#endif
}

/*
 * Get terminal width.
 */
static int get_terminal_width(void) {
#ifdef _WIN32
    /* Windows: use console info */
    return 80; /* Default fallback */
#else
    struct winsize ws;
    if (ioctl(fileno(stdout), TIOCGWINSZ, &ws) == 0 && ws.ws_col > 0) {
        return ws.ws_col;
    }
    return 80; /* Default fallback */
#endif
}

/*
 * Read a single character (handles escape sequences).
 */
static int lineedit_getchar(LineEditState *state) {
    (void)state;

#ifdef _WIN32
    int c = _getch();
    if (c == 0 || c == 224) {
        /* Extended key - read second byte */
        int c2 = _getch();
        switch (c2) {
        case 72:
            return 1000; /* Up */
        case 80:
            return 1001; /* Down */
        case 75:
            return 1002; /* Left */
        case 77:
            return 1003; /* Right */
        case 71:
            return 1004; /* Home */
        case 79:
            return 1005; /* End */
        case 83:
            return 1006; /* Delete */
        default:
            return -1;
        }
    }
    if (c == KEY_BACKSPACE || c == 8) {
        return KEY_BACKSPACE;
    }
    return c;
#else
    unsigned char c;
    if (read(fileno(stdin), &c, 1) != 1)
        return -1;

    if (c == KEY_ESC) {
        /* Escape sequence */
        unsigned char seq[3];
        if (read(fileno(stdin), &seq[0], 1) != 1)
            return KEY_ESC;
        if (read(fileno(stdin), &seq[1], 1) != 1)
            return KEY_ESC;

        if (seq[0] == '[') {
            switch (seq[1]) {
            case 'A':
                return 1000; /* Up */
            case 'B':
                return 1001; /* Down */
            case 'C':
                return 1003; /* Right */
            case 'D':
                return 1002; /* Left */
            case 'H':
                return 1004; /* Home */
            case 'F':
                return 1005; /* End */
            case '3':
                /* Delete key: ESC [ 3 ~ */
                read(fileno(stdin), &seq[2], 1);
                return 1006; /* Delete */
            }
        } else if (seq[0] == 'O') {
            switch (seq[1]) {
            case 'H':
                return 1004; /* Home */
            case 'F':
                return 1005; /* End */
            }
        }
        return KEY_ESC;
    }
    return c;
#endif
}

/*
 * Refresh the line display.
 */
static void lineedit_refresh(LineEditState *state, const char *prompt) {
    /* Move cursor to start of line and clear */
    printf("\r" ANSI_CLEAR_RIGHT);

    /* Print prompt and buffer */
    printf("%s%s", prompt, state->buf);

    /* Clear to end of line */
    printf(ANSI_CLEAR_RIGHT);

    /* Move cursor to correct position */
    int cursor_offset = state->len - state->pos;
    for (int i = 0; i < cursor_offset; i++) {
        printf(ANSI_MOVE_LEFT);
    }

    fflush(stdout);
}

/*
 * Insert character at cursor position.
 */
static void lineedit_insert(LineEditState *state, char c) {
    /* Ensure capacity */
    if (state->len + 1 >= state->capacity) {
        int new_cap = state->capacity * 2;
        char *new_buf = realloc(state->buf, new_cap);
        if (!new_buf)
            return;
        state->buf = new_buf;
        state->capacity = new_cap;
    }

    /* Shift characters right */
    memmove(state->buf + state->pos + 1, state->buf + state->pos, state->len - state->pos + 1);

    /* Insert character */
    state->buf[state->pos] = c;
    state->pos++;
    state->len++;
}

/*
 * Delete character at cursor position.
 */
static void lineedit_delete(LineEditState *state) {
    if (state->pos < state->len) {
        memmove(state->buf + state->pos, state->buf + state->pos + 1, state->len - state->pos);
        state->len--;
    }
}

/*
 * Delete character before cursor (backspace).
 */
static void lineedit_backspace(LineEditState *state) {
    if (state->pos > 0) {
        state->pos--;
        lineedit_delete(state);
    }
}

/*
 * Clear completions.
 */
static void lineedit_clear_completions(LineEditState *state) {
    lineedit_free_completions(state->completions);
    state->completions = NULL;
    state->completion_count = 0;
    state->completion_index = -1;
}

/*
 * Find the start of the word being completed.
 */
static int find_word_start(const char *buf, int pos) {
    int start = pos;
    while (start > 0 && buf[start - 1] != ' ' && buf[start - 1] != '(' && buf[start - 1] != ')' &&
           buf[start - 1] != '\t' && buf[start - 1] != '\'' && buf[start - 1] != '`') {
        start--;
    }
    return start;
}

/*
 * Find the longest common prefix among all completions.
 * Returns malloc'd string (caller must free).
 */
static char *find_common_prefix(char **completions, int count) {
    if (!completions || count == 0 || !completions[0])
        return strdup("");

    /* Start with the first completion as the prefix */
    int prefix_len = strlen(completions[0]);

    /* Compare with each subsequent completion */
    for (int i = 1; i < count; i++) {
        int j = 0;
        while (j < prefix_len && completions[i][j] && completions[0][j] == completions[i][j]) {
            j++;
        }
        prefix_len = j;
        if (prefix_len == 0)
            break;
    }

    char *prefix = malloc(prefix_len + 1);
    if (!prefix)
        return strdup("");
    strncpy(prefix, completions[0], prefix_len);
    prefix[prefix_len] = '\0';
    return prefix;
}

/*
 * Apply a completion string at current position.
 */
static void apply_completion(LineEditState *state, const char *completion) {
    int word_start = find_word_start(state->buf, state->pos);

    /* Remove current word */
    int word_len = state->pos - word_start;
    memmove(state->buf + word_start, state->buf + state->pos, state->len - state->pos + 1);
    state->len -= word_len;
    state->pos = word_start;

    /* Insert completion */
    int comp_len = strlen(completion);
    if (state->len + comp_len >= state->capacity) {
        int new_cap = state->capacity * 2;
        while (new_cap < state->len + comp_len + 1)
            new_cap *= 2;
        char *new_buf = realloc(state->buf, new_cap);
        if (!new_buf)
            return;
        state->buf = new_buf;
        state->capacity = new_cap;
    }

    memmove(state->buf + word_start + comp_len, state->buf + word_start, state->len - word_start + 1);
    memcpy(state->buf + word_start, completion, comp_len);
    state->len += comp_len;
    state->pos = word_start + comp_len;
}

/*
 * Handle tab completion.
 * First Tab: complete to longest common prefix
 * Second Tab: show all completions
 */
static void lineedit_complete(LineEditState *state, const char *prompt) {
    if (!state->completer)
        return;

    /* If completions already exist from first Tab, show them all */
    if (state->completions) {
        lineedit_show_completions(state, prompt);
        lineedit_clear_completions(state);
        return;
    }

    /* First Tab: get completions */
    state->completions = state->completer(state->buf, state->pos, state->completer_userdata);
    if (!state->completions || !state->completions[0]) {
        lineedit_clear_completions(state);
        return;
    }

    /* Count completions */
    state->completion_count = 0;
    while (state->completions[state->completion_count])
        state->completion_count++;

    /* Single match: complete it fully and done */
    if (state->completion_count == 1) {
        apply_completion(state, state->completions[0]);
        lineedit_clear_completions(state);
        return;
    }

    /* Multiple matches: find common prefix */
    char *common = find_common_prefix(state->completions, state->completion_count);
    int word_start = find_word_start(state->buf, state->pos);
    int current_word_len = state->pos - word_start;
    int common_len = strlen(common);

    if (common_len > current_word_len) {
        /* Common prefix extends beyond typed word - apply it */
        apply_completion(state, common);
        /* Keep completions for second Tab */
    } else {
        /* Already at common prefix - show all completions */
        lineedit_show_completions(state, prompt);
        lineedit_clear_completions(state);
    }

    free(common);
}

/*
 * Show all completions.
 */
static void lineedit_show_completions(LineEditState *state, const char *prompt) {
    if (!state->completions || state->completion_count == 0)
        return;

    int term_width = get_terminal_width();
    int max_len = 0;

    /* Find max completion length */
    for (int i = 0; i < state->completion_count; i++) {
        int len = strlen(state->completions[i]);
        if (len > max_len)
            max_len = len;
    }

    /* Calculate columns */
    int col_width = max_len + 2;
    int cols = term_width / col_width;
    if (cols < 1)
        cols = 1;

    /* Print completions */
    printf("\n");
    for (int i = 0; i < state->completion_count; i++) {
        printf("%-*s", col_width, state->completions[i]);
        if ((i + 1) % cols == 0 || i == state->completion_count - 1) {
            printf("\n");
        }
    }

    /* Redraw prompt and line */
    lineedit_refresh(state, prompt);
}

/*
 * Navigate history.
 */
static void lineedit_history_navigate(LineEditState *state, int direction) {
    if (state->history_size == 0)
        return;

    /* Save current line if just starting navigation */
    if (state->history_index == -1) {
        free(state->history_saved);
        state->history_saved = strdup(state->buf);
        state->history_index = state->history_size;
    }

    /* Navigate */
    int new_index = state->history_index + direction;
    if (new_index < 0)
        new_index = 0;
    if (new_index > state->history_size)
        new_index = state->history_size;

    if (new_index == state->history_index)
        return;

    state->history_index = new_index;

    /* Replace line with history entry or saved line */
    const char *new_line;
    if (state->history_index == state->history_size) {
        new_line = state->history_saved ? state->history_saved : "";
    } else {
        new_line = state->history[state->history_index];
    }

    strncpy(state->buf, new_line, state->capacity - 1);
    state->buf[state->capacity - 1] = '\0';
    state->len = strlen(state->buf);
    state->pos = state->len;
}

/*
 * Read a line of input.
 */
char *lineedit_readline(LineEditState *state, const char *prompt) {
    if (!state)
        return NULL;

    /* Check if input is a terminal */
    if (!isatty(fileno(stdin))) {
        /* Non-interactive: use fgets */
        static char line[DEFAULT_LINE_SIZE];
        printf("%s", prompt);
        fflush(stdout);
        if (!fgets(line, sizeof(line), stdin))
            return NULL;
        /* Remove newline */
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n')
            line[len - 1] = '\0';
        return strdup(line);
    }

    /* Reset state */
    state->buf[0] = '\0';
    state->len = 0;
    state->pos = 0;
    state->history_index = -1;
    free(state->history_saved);
    state->history_saved = NULL;
    lineedit_clear_completions(state);

    /* Enable raw mode */
    enable_raw_mode(state);

    /* Print prompt */
    printf("%s", prompt);
    fflush(stdout);

    int done = 0;
    int last_key = 0;
    char *result = NULL;

    while (!done) {
        int c = lineedit_getchar(state);
        if (c == -1) {
            done = 1;
            continue;
        }

        /* Clear completions on non-tab key */
        if (c != KEY_TAB && state->completions) {
            lineedit_clear_completions(state);
        }

        switch (c) {
        case KEY_ENTER:
        case '\n':
            /* Submit line */
            printf("\n");
            result = strdup(state->buf);
            done = 1;
            break;

        case KEY_CTRL_C:
            /* Cancel line */
            printf("^C\n");
            state->buf[0] = '\0';
            state->len = 0;
            state->pos = 0;
            lineedit_refresh(state, prompt);
            break;

        case KEY_CTRL_D:
            /* EOF if line is empty */
            if (state->len == 0) {
                printf("\n");
                done = 1;
            } else {
                lineedit_delete(state);
                lineedit_refresh(state, prompt);
            }
            break;

        case KEY_TAB:
            if (last_key == KEY_TAB && state->completions) {
                /* Double tab: show all completions */
                lineedit_show_completions(state, prompt);
            } else {
                lineedit_complete(state, prompt);
            }
            lineedit_refresh(state, prompt);
            break;

        case KEY_BACKSPACE:
        case 8: /* Ctrl+H */
            lineedit_backspace(state);
            lineedit_refresh(state, prompt);
            break;

        case 1000: /* Up */
        case KEY_CTRL_P:
            lineedit_history_navigate(state, -1);
            lineedit_refresh(state, prompt);
            break;

        case 1001: /* Down */
        case KEY_CTRL_N:
            lineedit_history_navigate(state, 1);
            lineedit_refresh(state, prompt);
            break;

        case 1002: /* Left */
        case KEY_CTRL_B:
            if (state->pos > 0)
                state->pos--;
            lineedit_refresh(state, prompt);
            break;

        case 1003: /* Right */
        case KEY_CTRL_F:
            if (state->pos < state->len)
                state->pos++;
            lineedit_refresh(state, prompt);
            break;

        case 1004: /* Home */
        case KEY_CTRL_A:
            state->pos = 0;
            lineedit_refresh(state, prompt);
            break;

        case 1005: /* End */
        case KEY_CTRL_E:
            state->pos = state->len;
            lineedit_refresh(state, prompt);
            break;

        case 1006: /* Delete */
            lineedit_delete(state);
            lineedit_refresh(state, prompt);
            break;

        case KEY_CTRL_K:
            /* Kill to end of line */
            state->buf[state->pos] = '\0';
            state->len = state->pos;
            lineedit_refresh(state, prompt);
            break;

        case KEY_CTRL_U:
            /* Kill entire line */
            state->buf[0] = '\0';
            state->len = 0;
            state->pos = 0;
            lineedit_refresh(state, prompt);
            break;

        case KEY_CTRL_L:
            /* Clear screen */
            printf("\033[H\033[2J");
            lineedit_refresh(state, prompt);
            break;

        case KEY_ESC:
            /* Cancel completion */
            lineedit_clear_completions(state);
            lineedit_refresh(state, prompt);
            break;

        default:
            /* Insert printable character */
            if (c >= 32 && c < 127) {
                lineedit_insert(state, (char)c);
                lineedit_refresh(state, prompt);
            }
            break;
        }

        last_key = c;
    }

    /* Disable raw mode */
    disable_raw_mode(state);

    return result;
}
