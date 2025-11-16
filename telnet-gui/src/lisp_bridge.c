/* Lisp bridge implementation for telnet-gui */

#include "lisp_bridge.h"
#include "input_area.h"
#include "lisp.h"
#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Lisp environment for completion hooks and future primitives */
static Environment *lisp_env = NULL;

/* Static buffer for ANSI code stripping (pre-allocated at startup, freed on exit) */
static char *ansi_strip_buffer = NULL;
static size_t ansi_strip_buffer_size = 0;

/* Tab completion cycling state */
#define MAX_COMPLETIONS 256
#define TAB_BUFFER_SIZE 4096
static int tab_mode_active = 0;
static char *tab_completions[MAX_COMPLETIONS]; /* Array of completion strings */
static int tab_completion_count = 0;
static int tab_completion_index = 0;
static char tab_original_buffer[TAB_BUFFER_SIZE]; /* Original buffer before tab mode */
static int tab_original_cursor_pos = 0;
static int tab_original_length = 0;
static int tab_start_pos = 0;   /* Where the partial text starts */
static int tab_partial_len = 0; /* Length of the partial text */

/* Extract partial text to complete using regex pattern */
static int extract_partial_text(const char *buffer, int cursor_pos, int length, int *start_pos, int *partial_len) {
    if (!buffer || cursor_pos < 0 || cursor_pos > length || !start_pos || !partial_len) {
        return 0;
    }

    /* For now, simple implementation: look back to whitespace or beginning */
    /* TODO: Use regex pattern from *completion-pattern* for more sophisticated matching */
    int start = cursor_pos;
    while (start > 0 && buffer[start - 1] != ' ' && buffer[start - 1] != '\t' && buffer[start - 1] != '\n') {
        start--;
    }

    *start_pos = start;
    *partial_len = cursor_pos - start;
    return 1;
}

/* Replace partial text in buffer with completion */
static void replace_partial_text(char *buffer, int buffer_size, int *cursor_pos, int *length, int start_pos,
                                 int partial_len, const char *completion) {
    if (!buffer || !completion || start_pos < 0 || partial_len < 0 || !cursor_pos || !length) {
        return;
    }

    int completion_len = strlen(completion);
    int new_length = *length - partial_len + completion_len;

    if (new_length >= buffer_size) {
        return; /* Too long */
    }

    /* Make room for new text */
    if (completion_len != partial_len) {
        memmove(&buffer[start_pos + completion_len], &buffer[start_pos + partial_len],
                *length - start_pos - partial_len);
    }

    /* Insert completion */
    memcpy(&buffer[start_pos], completion, completion_len);
    *length = new_length;
    *cursor_pos = start_pos + completion_len;
    buffer[*length] = '\0';
}

/* Strip ANSI escape sequences and control codes from input */
static char *strip_ansi_codes(const char *input, size_t len, size_t *out_len) {
    if (!input || len == 0 || !out_len) {
        if (out_len)
            *out_len = 0;
        return NULL;
    }

    /* Ensure buffer is initialized and large enough */
    if (!ansi_strip_buffer || ansi_strip_buffer_size < len + 1) {
        size_t new_size = len + 1;
        if (new_size < 4096) {
            new_size = 4096; /* Minimum size */
        }
        char *new_buffer = realloc(ansi_strip_buffer, new_size);
        if (!new_buffer) {
            if (out_len)
                *out_len = 0;
            return NULL;
        }
        ansi_strip_buffer = new_buffer;
        ansi_strip_buffer_size = new_size;
    }

    size_t out_pos = 0;
    int in_escape = 0;
    int in_csi = 0; /* Control Sequence Introducer (ESC[) */

    for (size_t i = 0; i < len; i++) {
        unsigned char c = (unsigned char)input[i];

        if (in_escape) {
            if (c == '[') {
                /* Start of CSI sequence */
                in_csi = 1;
                in_escape = 0;
            } else if (c >= 0x40 && c <= 0x5F) {
                /* Single character escape sequence (e.g., ESC m) */
                in_escape = 0;
            } else if (c == 0x1B) {
                /* Another ESC - keep in escape state */
            } else {
                /* End of escape sequence */
                in_escape = 0;
            }
            continue;
        }

        if (in_csi) {
            /* Inside CSI sequence - skip until terminator */
            if ((c >= 0x40 && c <= 0x7E) || c == 0x1B) {
                /* Terminator found or new ESC */
                in_csi = 0;
                if (c == 0x1B) {
                    in_escape = 1;
                }
            }
            continue;
        }

        if (c == 0x1B) {
            /* Start of escape sequence */
            in_escape = 1;
            continue;
        }

        /* Keep printable characters and whitespace */
        if (c >= 0x20 || c == '\n' || c == '\r' || c == '\t') {
            if (out_pos < ansi_strip_buffer_size - 1) {
                ansi_strip_buffer[out_pos++] = c;
            }
        }
        /* Skip other control characters */
    }

    /* Null terminate */
    ansi_strip_buffer[out_pos] = '\0';
    *out_len = out_pos;
    return ansi_strip_buffer;
}

/* Call Lisp completion hook */
static LispObject *call_completion_hook(const char *partial_text) {
    if (!lisp_env || !partial_text) {
        return NIL;
    }

    /* Look up completion-hook */
    LispObject *hook = env_lookup(lisp_env, "completion-hook");
    if (!hook || (hook->type != LISP_LAMBDA && hook->type != LISP_BUILTIN)) {
        return NIL;
    }

    /* Create argument list with partial text */
    LispObject *arg = lisp_make_string(partial_text);
    LispObject *args = lisp_make_cons(arg, NIL);

    /* Create function call: (completion-hook "partial-text") */
    LispObject *call_expr = lisp_make_cons(hook, args);

    /* Evaluate the function call */
    LispObject *result = lisp_eval(call_expr, lisp_env);

    /* Check for errors */
    if (result && result->type == LISP_ERROR) {
        return NIL;
    }

    return result ? result : NIL;
}

/* Load bootstrap Lisp file */
static int load_bootstrap_file(void) {
    if (!lisp_env) {
        return 0;
    }

    /* Get executable base path using SDL */
    char *base_path = SDL_GetBasePath();
    char bootstrap_path[1024] = {0};
    const char *bootstrap_paths[10];
    const char *bootstrap_path_labels[10];
    int bootstrap_path_count = 0;

    fprintf(stderr, "Bootstrap file resolution: Starting search...\n");

    /* Try bootstrap.lisp relative to executable first (installation path) */
    if (base_path) {
        size_t base_len = strlen(base_path);
        const char *sep =
            (base_len > 0 && (base_path[base_len - 1] == '/' || base_path[base_len - 1] == '\\')) ? "" : "/";

        /* Normalize path separators - use backslashes on Windows for consistency */
        char normalized_path[1024];
        snprintf(normalized_path, sizeof(normalized_path), "%s%sbootstrap.lisp", base_path, sep);

/* Convert forward slashes to backslashes on Windows for file access compatibility */
#ifdef _WIN32
        for (char *p = normalized_path; *p; p++) {
            if (*p == '/')
                *p = '\\';
        }
#endif

        strncpy(bootstrap_path, normalized_path, sizeof(bootstrap_path) - 1);
        bootstrap_path[sizeof(bootstrap_path) - 1] = '\0';

        bootstrap_paths[bootstrap_path_count] = bootstrap_path;
        bootstrap_path_labels[bootstrap_path_count] = "executable-relative (installation path)";
        bootstrap_path_count++;
        fprintf(stderr, "Bootstrap file resolution: Executable base path: %s\n", base_path);
        fprintf(stderr, "Bootstrap file resolution: Constructed bootstrap path: %s\n", bootstrap_path);
        SDL_free(base_path);
    } else {
        fprintf(stderr, "Bootstrap file resolution: Warning - SDL_GetBasePath() returned NULL\n");
    }

    /* Add fallback paths - try source tree paths */
    bootstrap_paths[bootstrap_path_count] = "telnet-gui/bootstrap.lisp";
    bootstrap_path_labels[bootstrap_path_count++] = "source tree path (from build root)";
    bootstrap_paths[bootstrap_path_count] = "../../telnet-gui/bootstrap.lisp";
    bootstrap_path_labels[bootstrap_path_count++] = "source tree path (nested build dir)";
    bootstrap_paths[bootstrap_path_count] = "../telnet-gui/bootstrap.lisp";
    bootstrap_path_labels[bootstrap_path_count++] = "source tree path (parent dir)";
    bootstrap_paths[bootstrap_path_count] = "bootstrap.lisp";
    bootstrap_path_labels[bootstrap_path_count++] = "current directory relative (build/development)";
    bootstrap_paths[bootstrap_path_count] = "../bootstrap.lisp";
    bootstrap_path_labels[bootstrap_path_count++] = "parent directory relative";
    bootstrap_paths[bootstrap_path_count] = NULL;

    const char *loaded_bootstrap_path = NULL;
    const char *loaded_bootstrap_label = NULL;

    for (int i = 0; bootstrap_paths[i] != NULL; i++) {
        fprintf(stderr, "Bootstrap file resolution: Trying [%d] %s: %s\n", i + 1, bootstrap_path_labels[i],
                bootstrap_paths[i]);

        /* Check if file exists before trying to load */
        FILE *test = fopen(bootstrap_paths[i], "rb");
        if (test) {
            fclose(test);
            fprintf(stderr, "Bootstrap file resolution: File exists, attempting to load...\n");
        } else {
            fprintf(stderr, "Bootstrap file resolution: File does not exist, skipping...\n");
            continue;
        }

        LispObject *result = lisp_load_file(bootstrap_paths[i], lisp_env);
        if (result && result->type == LISP_ERROR) {
            char *err_str = lisp_print(result);
            fprintf(stderr, "Bootstrap file evaluation ERROR in %s:\n%s\n", bootstrap_paths[i], err_str);
            /* Continue trying other paths */
        } else {
            loaded_bootstrap_path = bootstrap_paths[i];
            loaded_bootstrap_label = bootstrap_path_labels[i];
            fprintf(stderr, "Bootstrap file resolution: SUCCESS! Loaded bootstrap file from [%d] %s\n", i + 1,
                    loaded_bootstrap_label);
            fprintf(stderr, "Bootstrap file resolution: Bootstrap file path: %s\n", loaded_bootstrap_path);
            return 1;
        }
    }

    fprintf(stderr, "Bootstrap file resolution: ERROR - Failed to load bootstrap file from all attempted paths\n");
    return 0;
}

int lisp_bridge_init(void) {
    /* Initialize Lisp interpreter */
    if (lisp_init() < 0) {
        fprintf(stderr, "Failed to initialize Lisp interpreter\n");
        return -1;
    }
    lisp_env = env_create_global();
    if (!lisp_env) {
        fprintf(stderr, "Failed to create Lisp environment\n");
        lisp_cleanup();
        return -1;
    }

    /* Allocate static buffer for ANSI stripping */
    ansi_strip_buffer_size = 4096; /* Initial size */
    ansi_strip_buffer = malloc(ansi_strip_buffer_size);
    if (!ansi_strip_buffer) {
        fprintf(stderr, "Failed to allocate ANSI strip buffer\n");
        env_free(lisp_env);
        lisp_env = NULL;
        lisp_cleanup();
        return -1;
    }

    /* Load bootstrap file */
    if (!load_bootstrap_file()) {
        fprintf(stderr, "Warning: Failed to load bootstrap file, continuing with defaults\n");
        /* Initialize default completion variables if bootstrap failed */
        LispObject *pattern = lisp_make_string("\\S+$");
        env_define(lisp_env, "*completion-pattern*", pattern);

        /* Default completion hook returns empty list */
        char default_hook_code[] = "(lambda (text) ())";
        LispObject *hook_expr = lisp_eval_string(default_hook_code, lisp_env);
        if (hook_expr && hook_expr->type != LISP_ERROR) {
            env_define(lisp_env, "completion-hook", hook_expr);
        } else {
            fprintf(stderr, "Warning: Failed to initialize default completion hook\n");
        }

        /* Initialize default telnet-input-hook */
        char default_telnet_hook_code[] = "(lambda (text) ())";
        LispObject *telnet_hook_expr = lisp_eval_string(default_telnet_hook_code, lisp_env);
        if (telnet_hook_expr && telnet_hook_expr->type != LISP_ERROR) {
            env_define(lisp_env, "telnet-input-hook", telnet_hook_expr);
        } else {
            fprintf(stderr, "Warning: Failed to initialize default telnet-input-hook\n");
        }

        /* Initialize default user-input-hook */
        char default_user_input_hook_code[] = "(lambda (text cursor-pos) text)";
        LispObject *user_input_hook_expr = lisp_eval_string(default_user_input_hook_code, lisp_env);
        if (user_input_hook_expr && user_input_hook_expr->type != LISP_ERROR) {
            env_define(lisp_env, "user-input-hook", user_input_hook_expr);
        } else {
            fprintf(stderr, "Warning: Failed to initialize default user-input-hook\n");
        }

        /* Initialize default scroll config variables if bootstrap failed */
        LispObject *scroll_lines = lisp_make_integer(3);
        env_define(lisp_env, "*scroll-lines-per-click*", scroll_lines);

        LispObject *smooth_scrolling = lisp_make_boolean(1); /* true */
        env_define(lisp_env, "*smooth-scrolling-enabled*", smooth_scrolling);

        LispObject *max_scrollback = lisp_make_integer(0); /* 0 = unbounded */
        env_define(lisp_env, "*max-scrollback-lines*", max_scrollback);

        LispObject *scroll_on_user_input = lisp_make_boolean(1); /* true */
        env_define(lisp_env, "*scroll-to-bottom-on-user-input*", scroll_on_user_input);

        LispObject *scroll_on_telnet_input = lisp_make_boolean(0); /* false */
        env_define(lisp_env, "*scroll-to-bottom-on-telnet-input*", scroll_on_telnet_input);
    }

    return 0;
}

int lisp_bridge_load_file(const char *filepath) {
    if (!lisp_env || !filepath) {
        return -1;
    }

    LispObject *result = lisp_load_file(filepath, lisp_env);
    if (result && result->type == LISP_ERROR) {
        char *err_str = lisp_print(result);
        fprintf(stderr, "Error loading Lisp file '%s': %s\n", filepath, err_str);
        return -1;
    }

    fprintf(stderr, "Loaded Lisp file: %s\n", filepath);
    return 0;
}

void lisp_bridge_cleanup(void) {
    if (lisp_env) {
        env_free(lisp_env);
        lisp_env = NULL;
    }
    if (ansi_strip_buffer) {
        free(ansi_strip_buffer);
        ansi_strip_buffer = NULL;
        ansi_strip_buffer_size = 0;
    }
    lisp_cleanup();
}

/* Free all tab completion strings */
static void free_tab_completions(void) {
    for (int i = 0; i < tab_completion_count; i++) {
        if (tab_completions[i]) {
            free(tab_completions[i]);
            tab_completions[i] = NULL;
        }
    }
    tab_completion_count = 0;
}

/* Exit tab mode and cleanup */
static void exit_tab_mode(void) {
    free_tab_completions();
    tab_mode_active = 0;
    tab_completion_index = 0;
    tab_start_pos = 0;
    tab_partial_len = 0;
}

/* Enter tab mode with completions from Lisp list */
static int enter_tab_mode(LispObject *completions, const char *buffer, int cursor_pos, int length, int start_pos,
                          int partial_len) {
    if (!completions || completions == NIL || completions->type != LISP_CONS || !buffer) {
        return 0;
    }

    /* Free any existing completions */
    free_tab_completions();

    /* Save original buffer state */
    int save_len = length < TAB_BUFFER_SIZE ? length : TAB_BUFFER_SIZE - 1;
    memcpy(tab_original_buffer, buffer, save_len);
    tab_original_buffer[save_len] = '\0';
    tab_original_cursor_pos = cursor_pos;
    tab_original_length = length;
    tab_start_pos = start_pos;
    tab_partial_len = partial_len;

    /* Convert Lisp list to array of strings */
    LispObject *current = completions;
    while (current != NIL && current->type == LISP_CONS && tab_completion_count < MAX_COMPLETIONS) {
        LispObject *car = current->value.cons.car;
        if (car && car->type == LISP_STRING) {
            tab_completions[tab_completion_count] = strdup(car->value.string);
            if (tab_completions[tab_completion_count]) {
                tab_completion_count++;
            }
        }
        current = current->value.cons.cdr;
    }

    if (tab_completion_count == 0) {
        return 0; /* No valid completions */
    }

    tab_mode_active = 1;
    tab_completion_index = 0;
    return 1;
}

/* Apply current completion to buffer */
static void apply_current_completion(char *buffer, int buffer_size, int *cursor_pos, int *length) {
    if (!tab_mode_active || tab_completion_index >= tab_completion_count || !buffer || !cursor_pos || !length) {
        return;
    }

    const char *completion = tab_completions[tab_completion_index];
    if (!completion) {
        return;
    }

    /* Replace partial text with current completion */
    replace_partial_text(buffer, buffer_size, cursor_pos, length, tab_start_pos, tab_partial_len, completion);

    /* Update partial_len to match the completion we just inserted, so next cycle replaces the whole completion */
    tab_partial_len = strlen(completion);
}

/* Cycle to next completion */
static void cycle_next_completion(char *buffer, int buffer_size, int *cursor_pos, int *length, int *needs_redraw) {
    if (!tab_mode_active || tab_completion_count == 0) {
        return;
    }

    tab_completion_index = (tab_completion_index + 1) % tab_completion_count;
    apply_current_completion(buffer, buffer_size, cursor_pos, length);
    if (needs_redraw) {
        *needs_redraw = 1;
    }
}

void lisp_bridge_handle_tab(char *buffer, int buffer_size, int *cursor_pos, int *length, int *needs_redraw) {
    if (!buffer || !cursor_pos || !length || *cursor_pos < 0 || *cursor_pos > *length) {
        return;
    }

    /* If already in tab mode, cycle to next completion */
    if (tab_mode_active) {
        cycle_next_completion(buffer, buffer_size, cursor_pos, length, needs_redraw);
        return;
    }

    /* Not in tab mode - extract partial text and get completions */
    int start_pos, partial_len;
    if (!extract_partial_text(buffer, *cursor_pos, *length, &start_pos, &partial_len)) {
        return;
    }

    /* Extract partial text using stack buffer */
    char partial_buf[1024];
    int copy_len = partial_len;
    if ((size_t)copy_len >= sizeof(partial_buf)) {
        copy_len = (int)(sizeof(partial_buf) - 1);
    }
    memcpy(partial_buf, &buffer[start_pos], copy_len);
    partial_buf[copy_len] = '\0';

    /* Call Lisp completion hook */
    LispObject *completions = call_completion_hook(partial_buf);
    if (!completions || completions == NIL || completions->type != LISP_CONS) {
        return; /* No completions */
    }

    /* Count completions */
    int count = 0;
    LispObject *current = completions;
    while (current != NIL && current->type == LISP_CONS && count < MAX_COMPLETIONS) {
        LispObject *car = current->value.cons.car;
        if (car && car->type == LISP_STRING) {
            count++;
        }
        current = current->value.cons.cdr;
    }

    if (count == 0) {
        return; /* No valid completions */
    } else if (count == 1) {
        /* Single completion - apply it directly without entering tab mode */
        LispObject *first = completions->value.cons.car;
        if (first && first->type == LISP_STRING) {
            replace_partial_text(buffer, buffer_size, cursor_pos, length, start_pos, partial_len, first->value.string);
            if (needs_redraw) {
                *needs_redraw = 1;
            }
        }
    } else {
        /* Multiple completions - enter tab mode and apply first */
        if (enter_tab_mode(completions, buffer, *cursor_pos, *length, start_pos, partial_len)) {
            apply_current_completion(buffer, buffer_size, cursor_pos, length);
            if (needs_redraw) {
                *needs_redraw = 1;
            }
        }
    }
}

/* Check if tab mode is active */
int lisp_bridge_is_tab_mode_active(void) {
    return tab_mode_active;
}

/* Accept current completion and exit tab mode */
void lisp_bridge_accept_tab_completion(void) {
    if (tab_mode_active) {
        exit_tab_mode();
    }
}

/* Cancel tab mode and revert to original buffer */
void lisp_bridge_cancel_tab_completion(char *buffer, int buffer_size, int *cursor_pos, int *length, int *needs_redraw) {
    if (!tab_mode_active || !buffer || !cursor_pos || !length) {
        return;
    }

    /* Restore original buffer state */
    int restore_len = tab_original_length < buffer_size ? tab_original_length : buffer_size - 1;
    memcpy(buffer, tab_original_buffer, restore_len);
    buffer[restore_len] = '\0';
    *length = tab_original_length;
    *cursor_pos = tab_original_cursor_pos;
    if (needs_redraw) {
        *needs_redraw = 1;
    }

    /* Exit tab mode */
    exit_tab_mode();
}

int lisp_bridge_get_scroll_lines_per_click(void) {
    if (!lisp_env) {
        return 3; /* Default */
    }

    LispObject *value = env_lookup(lisp_env, "*scroll-lines-per-click*");
    if (!value || value == NIL) {
        return 3; /* Default */
    }

    /* Check if it's an integer */
    if (value->type == LISP_INTEGER) {
        int lines = (int)value->value.integer;
        /* Clamp to reasonable range (1-100) */
        if (lines < 1)
            lines = 1;
        if (lines > 100)
            lines = 100;
        return lines;
    }

    /* If it's a number, convert to integer */
    if (value->type == LISP_NUMBER) {
        int lines = (int)value->value.number;
        if (lines < 1)
            lines = 1;
        if (lines > 100)
            lines = 100;
        return lines;
    }

    /* Invalid type, return default */
    return 3;
}

int lisp_bridge_get_smooth_scrolling_enabled(void) {
    if (!lisp_env) {
        return 1; /* Default: enabled */
    }

    LispObject *value = env_lookup(lisp_env, "*smooth-scrolling-enabled*");
    if (!value || value == NIL) {
        return 1; /* Default: enabled */
    }

    /* Check if it's a boolean */
    if (value->type == LISP_BOOLEAN) {
        return value->value.boolean ? 1 : 0;
    }

    /* Use truthy check for other types */
    return lisp_is_truthy(value) ? 1 : 0;
}

int lisp_bridge_get_max_scrollback_lines(void) {
    if (!lisp_env) {
        return 0; /* Default: unbounded */
    }

    LispObject *value = env_lookup(lisp_env, "*max-scrollback-lines*");
    if (!value || value == NIL) {
        return 0; /* Default: unbounded */
    }

    /* Check if it's an integer */
    if (value->type == LISP_INTEGER) {
        int lines = (int)value->value.integer;
        /* Ensure non-negative */
        if (lines < 0)
            lines = 0;
        return lines;
    }

    /* Check if it's a number (convert to integer) */
    if (value->type == LISP_NUMBER) {
        int lines = (int)value->value.number;
        /* Ensure non-negative */
        if (lines < 0)
            lines = 0;
        return lines;
    }

    /* Invalid type, return default */
    return 0;
}

int lisp_bridge_get_scroll_to_bottom_on_user_input(void) {
    if (!lisp_env) {
        return 1; /* Default: enabled */
    }

    LispObject *value = env_lookup(lisp_env, "*scroll-to-bottom-on-user-input*");
    if (!value || value == NIL) {
        return 1; /* Default: enabled */
    }

    /* Check if it's a boolean */
    if (value->type == LISP_BOOLEAN) {
        return value->value.boolean ? 1 : 0;
    }

    /* Use truthy check for other types */
    return lisp_is_truthy(value) ? 1 : 0;
}

int lisp_bridge_get_scroll_to_bottom_on_telnet_input(void) {
    if (!lisp_env) {
        return 0; /* Default: disabled */
    }

    LispObject *value = env_lookup(lisp_env, "*scroll-to-bottom-on-telnet-input*");
    if (!value || value == NIL) {
        return 0; /* Default: disabled */
    }

    /* Check if it's a boolean */
    if (value->type == LISP_BOOLEAN) {
        return value->value.boolean ? 1 : 0;
    }

    /* Use truthy check for other types */
    return lisp_is_truthy(value) ? 1 : 0;
}

void lisp_bridge_call_telnet_input_hook(const char *text, size_t len) {
    if (!lisp_env || !text || len == 0) {
        return;
    }

    /* Look up telnet-input-hook */
    LispObject *hook = env_lookup(lisp_env, "telnet-input-hook");
    if (!hook || (hook->type != LISP_LAMBDA && hook->type != LISP_BUILTIN)) {
        return; /* Hook not found or wrong type - silently do nothing */
    }

    /* Strip ANSI codes from input text */
    size_t stripped_len = 0;
    char *stripped_text = strip_ansi_codes(text, len, &stripped_len);
    if (!stripped_text || stripped_len == 0) {
        return; /* Nothing to process after stripping */
    }

    /* Create Lisp string from stripped text */
    LispObject *arg = lisp_make_string(stripped_text);
    if (!arg || arg->type == LISP_ERROR) {
        return; /* Failed to create string */
    }

    /* Create argument list */
    LispObject *args = lisp_make_cons(arg, NIL);

    /* Create function call: (telnet-input-hook "stripped-text") */
    LispObject *call_expr = lisp_make_cons(hook, args);

    /* Evaluate the function call */
    LispObject *result = lisp_eval(call_expr, lisp_env);

    /* Check for errors - log but continue */
    if (result && result->type == LISP_ERROR) {
        char *err_str = lisp_print(result);
        if (err_str) {
            fprintf(stderr, "Error in telnet-input-hook: %s\n", err_str);
        }
    }
    /* Ignore return value - hook is side-effect only */
}

const char *lisp_bridge_call_user_input_hook(const char *text, int cursor_pos) {
    if (!lisp_env || !text) {
        return text; /* Return original if no environment or text */
    }

    /* Look up user-input-hook */
    LispObject *hook = env_lookup(lisp_env, "user-input-hook");
    if (!hook || (hook->type != LISP_LAMBDA && hook->type != LISP_BUILTIN)) {
        return text; /* Hook not found - return original */
    }

    /* Create Lisp string from input text */
    LispObject *text_arg = lisp_make_string(text);
    if (!text_arg || text_arg->type == LISP_ERROR) {
        return text; /* Failed to create string - return original */
    }

    /* Create Lisp integer for cursor position */
    LispObject *cursor_arg = lisp_make_integer(cursor_pos);
    if (!cursor_arg || cursor_arg->type == LISP_ERROR) {
        return text; /* Failed to create integer - return original */
    }

    /* Create argument list: (text cursor-pos) */
    LispObject *args = lisp_make_cons(text_arg, lisp_make_cons(cursor_arg, NIL));

    /* Create function call: (user-input-hook "text" cursor-pos) */
    LispObject *call_expr = lisp_make_cons(hook, args);

    /* Evaluate the function call */
    LispObject *result = lisp_eval(call_expr, lisp_env);

    /* Check for errors */
    if (!result || result->type == LISP_ERROR) {
        char *err_str = lisp_print(result);
        if (err_str) {
            fprintf(stderr, "Error in user-input-hook: %s\n", err_str);
        }
        return text; /* Error - return original */
    }

    /* Check if result is a string */
    if (result->type != LISP_STRING) {
        fprintf(stderr, "Warning: user-input-hook returned non-string, using original text\n");
        return text; /* Not a string - return original */
    }

    /* Return the transformed string (Lisp string memory is managed by GC) */
    return result->value.string;
}

/* Helper: Extract RGB from Lisp list (r g b) or return default */
static void get_color_from_lisp(const char *var_name, int *r, int *g, int *b, int default_r, int default_g,
                                int default_b) {
    /* Set defaults */
    *r = default_r;
    *g = default_g;
    *b = default_b;

    if (!lisp_env) {
        return;
    }

    LispObject *value = env_lookup(lisp_env, var_name);
    if (!value || value == NIL || value->type != LISP_CONS) {
        return; /* Not found or not a list */
    }

    /* Extract (r g b) from list */
    LispObject *r_obj = lisp_car(value);
    LispObject *rest = lisp_cdr(value);
    if (!rest || rest->type != LISP_CONS)
        return;

    LispObject *g_obj = lisp_car(rest);
    rest = lisp_cdr(rest);
    if (!rest || rest->type != LISP_CONS)
        return;

    LispObject *b_obj = lisp_car(rest);

    /* Convert to integers */
    if (r_obj && (r_obj->type == LISP_INTEGER || r_obj->type == LISP_NUMBER)) {
        int val = (r_obj->type == LISP_INTEGER) ? (int)r_obj->value.integer : (int)r_obj->value.number;
        if (val >= 0 && val <= 255)
            *r = val;
    }

    if (g_obj && (g_obj->type == LISP_INTEGER || g_obj->type == LISP_NUMBER)) {
        int val = (g_obj->type == LISP_INTEGER) ? (int)g_obj->value.integer : (int)g_obj->value.number;
        if (val >= 0 && val <= 255)
            *g = val;
    }

    if (b_obj && (b_obj->type == LISP_INTEGER || b_obj->type == LISP_NUMBER)) {
        int val = (b_obj->type == LISP_INTEGER) ? (int)b_obj->value.integer : (int)b_obj->value.number;
        if (val >= 0 && val <= 255)
            *b = val;
    }
}

/* Input area colors */
void lisp_bridge_get_input_area_fg_color(int *r, int *g, int *b) {
    get_color_from_lisp("*input-area-fg-color*", r, g, b, 255, 255, 0); /* Yellow */
}

void lisp_bridge_get_input_area_bg_color(int *r, int *g, int *b) {
    get_color_from_lisp("*input-area-bg-color*", r, g, b, 25, 40, 60); /* Dark blue */
}

void lisp_bridge_get_selection_fg_color(int *r, int *g, int *b) {
    get_color_from_lisp("*selection-fg-color*", r, g, b, 0, 0, 0); /* Black */
}

void lisp_bridge_get_selection_bg_color(int *r, int *g, int *b) {
    get_color_from_lisp("*selection-bg-color*", r, g, b, 255, 140, 0); /* Orange */
}

void lisp_bridge_get_cursor_color(int *r, int *g, int *b) {
    get_color_from_lisp("*cursor-color*", r, g, b, 200, 200, 200); /* Light gray */
}

void lisp_bridge_get_input_separator_color(int *r, int *g, int *b) {
    get_color_from_lisp("*input-separator-color*", r, g, b, 100, 100, 100); /* Gray */
}

/* Mode display area colors */
void lisp_bridge_get_mode_fg_color(int *r, int *g, int *b) {
    get_color_from_lisp("*mode-fg-color*", r, g, b, 150, 255, 150); /* Greenish */
}

void lisp_bridge_get_mode_bg_color(int *r, int *g, int *b) {
    get_color_from_lisp("*mode-bg-color*", r, g, b, 45, 65, 85); /* Lighter blue */
}

/* Terminal default colors */
void lisp_bridge_get_terminal_fg_color(int *r, int *g, int *b) {
    get_color_from_lisp("*terminal-fg-color*", r, g, b, 255, 255, 255); /* White */
}

void lisp_bridge_get_terminal_bg_color(int *r, int *g, int *b) {
    get_color_from_lisp("*terminal-bg-color*", r, g, b, 0, 0, 0); /* Black */
}

/* Set connection mode in Lisp environment */
void lisp_bridge_set_connection_mode(int connected) {
    if (!lisp_env) {
        return;
    }

    /* Create connection mode symbol: conn or disc */
    const char *conn_str = connected ? "conn" : "disc";
    LispObject *conn_symbol = lisp_make_symbol(conn_str);

    /* Define *connection-mode* variable */
    env_define(lisp_env, "*connection-mode*", conn_symbol);

    /* Update *mode* alist */
    LispObject *connection_key = lisp_make_string("connection");
    LispObject *input_key = lisp_make_string("input");

    /* Get current input mode */
    LispObject *input_mode_obj = env_lookup(lisp_env, "*input-mode*");
    if (!input_mode_obj || input_mode_obj == NIL) {
        input_mode_obj = lisp_make_symbol("normal"); /* Default */
    }

    /* Create alist: (("connection" . conn/disc) ("input" . normal/eval)) */
    LispObject *connection_pair = lisp_make_cons(connection_key, conn_symbol);
    LispObject *input_pair = lisp_make_cons(input_key, input_mode_obj);
    LispObject *mode_list = lisp_make_cons(connection_pair, lisp_make_cons(input_pair, NIL));

    /* Define *mode* variable */
    env_define(lisp_env, "*mode*", mode_list);
}

/* Set input mode in Lisp environment */
void lisp_bridge_set_input_mode(int input_mode) {
    if (!lisp_env) {
        return;
    }

    /* Create input mode symbol: normal or eval */
    const char *input_str = (input_mode == INPUT_AREA_MODE_EVAL) ? "eval" : "normal";
    LispObject *input_symbol = lisp_make_symbol(input_str);

    /* Define *input-mode* variable */
    env_define(lisp_env, "*input-mode*", input_symbol);

    /* Update *mode* alist */
    LispObject *connection_key = lisp_make_string("connection");
    LispObject *input_key = lisp_make_string("input");

    /* Get current connection mode */
    LispObject *connection_mode_obj = env_lookup(lisp_env, "*connection-mode*");
    if (!connection_mode_obj || connection_mode_obj == NIL) {
        connection_mode_obj = lisp_make_symbol("disc"); /* Default */
    }

    /* Create alist: (("connection" . conn/disc) ("input" . normal/eval)) */
    LispObject *connection_pair = lisp_make_cons(connection_key, connection_mode_obj);
    LispObject *input_pair = lisp_make_cons(input_key, input_symbol);
    LispObject *mode_list = lisp_make_cons(connection_pair, lisp_make_cons(input_pair, NIL));

    /* Define *mode* variable */
    env_define(lisp_env, "*mode*", mode_list);
}

/* Get mode string from Lisp environment */
const char *lisp_bridge_get_mode_string(void) {
    if (!lisp_env) {
        return "((connection . disc) (input . normal))"; /* Default fallback */
    }

    /* Lookup *mode* variable */
    LispObject *mode = env_lookup(lisp_env, "*mode*");
    if (!mode || mode == NIL) {
        return "((connection . disc) (input . normal))"; /* Default fallback */
    }

    /* Try to call mode-render-hook if defined */
    LispObject *hook = env_lookup(lisp_env, "mode-render-hook");
    if (hook && (hook->type == LISP_LAMBDA || hook->type == LISP_BUILTIN)) {
        /* Call hook using eval_string to properly handle lambda closures */
        LispObject *result = lisp_eval_string("(mode-render-hook *mode*)", lisp_env);

        /* Check if result is a string */
        if (result && result->type == LISP_STRING) {
            return result->value.string;
        }

        /* Check for errors - log but continue to fallback */
        if (result && result->type == LISP_ERROR) {
            char *err_str = lisp_print(result);
            if (err_str) {
                fprintf(stderr, "Error in mode-render-hook: %s\n", err_str);
            }
        }
    }

    /* Fallback: Convert to string using lisp_print */
    char *mode_str = lisp_print(mode);
    return mode_str ? mode_str : "((connection . disc) (input . normal))";
}
