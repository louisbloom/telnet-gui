/* Lisp bridge implementation for telnet-gui */

#include "lisp_bridge.h"
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

/* Find common prefix from Lisp list of strings */
static LispObject *find_common_prefix(LispObject *list) {
    if (!list || list == NIL || list->type != LISP_CONS) {
        return NIL;
    }

    /* Get first string */
    LispObject *first = list->value.cons.car;
    if (!first || first->type != LISP_STRING) {
        return NIL;
    }

    const char *first_str = first->value.string;
    int first_len = strlen(first_str);

    /* Check if there's a second element */
    LispObject *rest = list->value.cons.cdr;
    if (rest == NIL || rest->type != LISP_CONS) {
        /* Single item: return first string */
        return first;
    }

    /* Find minimum length across all strings */
    int min_len = first_len;
    LispObject *current = rest;
    while (current != NIL && current->type == LISP_CONS) {
        LispObject *car = current->value.cons.car;
        if (car && car->type == LISP_STRING) {
            int len = strlen(car->value.string);
            if (len < min_len) {
                min_len = len;
            }
        } else {
            return NIL; /* Non-string in list */
        }
        current = current->value.cons.cdr;
    }

    /* Find common prefix */
    int prefix_len = 0;
    for (int i = 0; i < min_len; i++) {
        char c = first_str[i];
        int match = 1;

        /* Check if all strings have this character */
        current = rest;
        while (current != NIL && current->type == LISP_CONS) {
            LispObject *car = current->value.cons.car;
            if (car && car->type == LISP_STRING) {
                if (car->value.string[i] != c) {
                    match = 0;
                    break;
                }
            } else {
                return NIL; /* Non-string in list */
            }
            current = current->value.cons.cdr;
        }

        if (!match) {
            break;
        }
        prefix_len++;
    }

    if (prefix_len == 0) {
        return NIL;
    }

    /* Create Lisp string with common prefix using stack buffer */
    char prefix_buf[1024]; /* Reasonable limit for prefix */
    if ((size_t)prefix_len >= sizeof(prefix_buf)) {
        prefix_len = (int)(sizeof(prefix_buf) - 1);
    }
    memcpy(prefix_buf, first_str, prefix_len);
    prefix_buf[prefix_len] = '\0';
    LispObject *prefix = lisp_make_string(prefix_buf);
    return prefix;
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

/* Handle completion results */
static void handle_completion(LispObject *completions, char *buffer, int buffer_size, int *cursor_pos, int *length,
                              int *needs_redraw, int start_pos, int partial_len) {
    if (!completions || completions == NIL || completions->type != LISP_CONS || !buffer || !cursor_pos || !length) {
        return;
    }

    /* Get first element */
    LispObject *first = completions->value.cons.car;
    if (!first || first->type != LISP_STRING) {
        return; /* Invalid completion */
    }

    /* Check if there's a second element */
    LispObject *rest = completions->value.cons.cdr;
    if (rest == NIL || rest->type != LISP_CONS) {
        /* Single item: replace partial text with completion */
        replace_partial_text(buffer, buffer_size, cursor_pos, length, start_pos, partial_len, first->value.string);
        if (needs_redraw) {
            *needs_redraw = 1;
        }
    } else {
        /* Multiple items: find common prefix */
        LispObject *prefix = find_common_prefix(completions);
        if (prefix && prefix != NIL && prefix->type == LISP_STRING && strlen(prefix->value.string) > 0) {
            replace_partial_text(buffer, buffer_size, cursor_pos, length, start_pos, partial_len, prefix->value.string);
            if (needs_redraw) {
                *needs_redraw = 1;
            }
        }
        /* Otherwise do nothing */
    }
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

        /* Initialize default scroll config variables if bootstrap failed */
        LispObject *scroll_lines = lisp_make_integer(3);
        env_define(lisp_env, "*scroll-lines-per-click*", scroll_lines);

        LispObject *smooth_scrolling = lisp_make_boolean(1); /* true */
        env_define(lisp_env, "*smooth-scrolling-enabled*", smooth_scrolling);
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

void lisp_bridge_handle_tab(char *buffer, int buffer_size, int *cursor_pos, int *length, int *needs_redraw) {
    if (!buffer || !cursor_pos || !length || *cursor_pos < 0 || *cursor_pos > *length) {
        return;
    }

    /* Extract partial text */
    int start_pos, partial_len;
    if (extract_partial_text(buffer, *cursor_pos, *length, &start_pos, &partial_len)) {
        /* Extract partial text using stack buffer */
        char partial_buf[1024]; /* Reasonable limit for partial text */
        int copy_len = partial_len;
        if ((size_t)copy_len >= sizeof(partial_buf)) {
            copy_len = (int)(sizeof(partial_buf) - 1);
        }
        memcpy(partial_buf, &buffer[start_pos], copy_len);
        partial_buf[copy_len] = '\0';

        /* Call Lisp completion hook */
        LispObject *completions = call_completion_hook(partial_buf);
        if (completions) {
            handle_completion(completions, buffer, buffer_size, cursor_pos, length, needs_redraw, start_pos,
                              partial_len);
        }
    }
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
