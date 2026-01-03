#include "lisp.h"
#include "file_utils.h"
#include "lineedit.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>

/* Version fallback if not defined by CMake */
#ifndef TELNET_LISP_VERSION
#define TELNET_LISP_VERSION "unknown"
#endif

/* Global environment for completion callback */
static Environment *g_env = NULL;

/*
 * Detect completion context by scanning backwards from cursor position.
 * Returns LISP_COMPLETE_CALLABLE if we're completing the first element after '('
 * (i.e., function/macro position), LISP_COMPLETE_VARIABLE if we're in the first
 * argument of set!, otherwise LISP_COMPLETE_ALL.
 */
static LispCompleteContext detect_context(const char *buffer, int cursor_pos) {
    /* Scan backwards from cursor, skipping the word being typed */
    int i = cursor_pos - 1;

    /* Skip current word (non-separator characters) */
    while (i >= 0 && buffer[i] != ' ' && buffer[i] != '\t' && buffer[i] != '(' && buffer[i] != ')' &&
           buffer[i] != '\'' && buffer[i] != '`') {
        i--;
    }

    /* Skip whitespace */
    while (i >= 0 && (buffer[i] == ' ' || buffer[i] == '\t')) {
        i--;
    }

    /* Check what's before the whitespace */
    if (i >= 0 && buffer[i] == '(') {
        /* We're right after an open paren - function position */
        return LISP_COMPLETE_CALLABLE;
    }

    /* Check if we're in the first argument of a special form like set! */
    /* Look for pattern: "(set! " before our position */
    int j = i;
    /* We already skipped whitespace, now check for function name */
    if (j >= 0) {
        /* Find start of the function name */
        int func_end = j + 1;
        while (j >= 0 && buffer[j] != ' ' && buffer[j] != '\t' && buffer[j] != '(') {
            j--;
        }
        int func_start = j + 1;
        int func_len = func_end - func_start;

        /* Check if it's set! */
        if (func_len == 4 && strncmp(buffer + func_start, "set!", 4) == 0) {
            /* Skip whitespace before function name */
            while (j >= 0 && (buffer[j] == ' ' || buffer[j] == '\t')) {
                j--;
            }
            /* Check if there's an open paren */
            if (j >= 0 && buffer[j] == '(') {
                return LISP_COMPLETE_VARIABLE;
            }
        }
    }

    /* Otherwise, we're in argument position or top-level */
    return LISP_COMPLETE_ALL;
}

/*
 * Find the start of the word being typed (for extracting prefix).
 */
static int find_word_start(const char *buffer, int cursor_pos) {
    int i = cursor_pos;
    while (i > 0 && buffer[i - 1] != ' ' && buffer[i - 1] != '\t' && buffer[i - 1] != '(' && buffer[i - 1] != ')' &&
           buffer[i - 1] != '\'' && buffer[i - 1] != '`') {
        i--;
    }
    return i;
}

/*
 * Completion callback for lineedit.
 * Returns NULL-terminated array of completion strings.
 */
static char **repl_completer(const char *buffer, int cursor_pos, void *userdata) {
    Environment *env = (Environment *)userdata;
    if (!env || !buffer)
        return NULL;

    /* Find the prefix being typed */
    int word_start = find_word_start(buffer, cursor_pos);
    int prefix_len = cursor_pos - word_start;

    /* Extract prefix */
    char *prefix = malloc(prefix_len + 1);
    if (!prefix)
        return NULL;
    strncpy(prefix, buffer + word_start, prefix_len);
    prefix[prefix_len] = '\0';

    /* Detect context */
    LispCompleteContext ctx = detect_context(buffer, cursor_pos);

    /* Get completions */
    char **completions = lisp_get_completions(env, prefix, ctx);

    free(prefix);
    return completions;
}

static void print_welcome(void) {
    printf("Telnet Lisp Interpreter v%s\n", TELNET_LISP_VERSION);
    printf("Type expressions to evaluate, :quit to exit, :load <file> to load a file\n");
    printf("Tab for completion, Up/Down for history\n\n");
}

static void print_help(void) {
    printf("Telnet Lisp Interpreter v%s\n", TELNET_LISP_VERSION);
    printf("\n");
    printf("Usage:\n");
    printf("  lisp-repl                 Start interactive REPL\n");
    printf("  lisp-repl -e \"CODE\"       Execute CODE and exit\n");
    printf("  lisp-repl FILE [FILE...]  Execute FILE(s) and exit\n");
    printf("  lisp-repl -h, --help      Show this help message\n");
    printf("\n");
    printf("Examples:\n");
    printf("  lisp-repl                           # Start REPL\n");
    printf("  lisp-repl -e \"(+ 1 2 3)\"            # Execute code\n");
    printf("  lisp-repl script.lisp               # Run file\n");
    printf("  lisp-repl -e \"(define x 10) (* x 5)\" # Multiple expressions\n");
    printf("\n");
    printf("REPL Commands:\n");
    printf("  :quit             Exit the REPL\n");
    printf("  :load <filename>  Load and execute a Lisp file\n");
    printf("\n");
    printf("Editing Keys:\n");
    printf("  Tab               Complete symbol\n");
    printf("  Tab Tab           Show all completions\n");
    printf("  Up/Down           Navigate history\n");
    printf("  Left/Right        Move cursor\n");
    printf("  Ctrl+A/Ctrl+E     Beginning/End of line\n");
    printf("  Ctrl+U            Clear line\n");
    printf("  Ctrl+L            Clear screen\n");
    printf("  Ctrl+D            Exit (on empty line)\n");
    printf("\n");
    printf("See LANGUAGE_REFERENCE.md for complete language documentation.\n");
}

static int handle_command(const char *input, Environment *env) {
    /* Skip leading whitespace */
    while (*input == ' ' || *input == '\t')
        input++;

    if (strncmp(input, ":quit", 5) == 0) {
        return 1; /* Exit */
    }

    if (strncmp(input, ":load", 5) == 0) {
        const char *filename = input + 5;
        while (*filename == ' ' || *filename == '\t')
            filename++;

        if (*filename == '\0') {
            printf("ERROR: :load requires a filename\n");
            return 0;
        }

        /* Copy filename (already trimmed, no newline from lineedit) */
        char *fname = GC_strdup(filename);

        LispObject *result = lisp_load_file(fname, env);

        if (result->type == LISP_ERROR) {
            char *err_str = lisp_print(result);
            printf("ERROR: %s\n", err_str);
        } else {
            char *output = lisp_print(result);
            printf("%s\n", output);
        }

        return 0;
    }

    return -1; /* Not a command */
}

int main(int argc, char **argv) {
    /* Set locale for UTF-8 support */
    setlocale(LC_ALL, "");

    /* Handle help flag */
    if (argc > 1 && (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-?") == 0)) {
        print_help();
        return 0;
    }

    /* Initialize interpreter */
    lisp_init();
    Environment *env = env_create_global();
    g_env = env; /* Store globally for completion callback */

    /* Handle -e/--eval/-c flag for executing code from command line */
    if (argc > 2 && (strcmp(argv[1], "-e") == 0 || strcmp(argv[1], "--eval") == 0 || strcmp(argv[1], "-c") == 0)) {
        const char *code = argv[2];
        const char *input = code;

        while (*input) {
            /* Parse expression - let lisp_read handle whitespace and comments */
            const char *parse_start = input;
            LispObject *expr = lisp_read(&input);

            /* If input didn't advance or NULL, we're done */
            if (expr == NULL || input == parse_start) {
                break;
            }

            if (expr->type == LISP_ERROR) {
                char *err_str = lisp_print(expr);
                fprintf(stderr, "ERROR: %s\n", err_str);
                lisp_cleanup();
                return 1;
            }

            LispObject *result = lisp_eval(expr, env);

            /* Only report uncaught errors (caught errors are just values) */
            if (result->type == LISP_ERROR && !result->value.error_with_stack.caught) {
                char *err_str = lisp_print(result);
                fprintf(stderr, "ERROR: %s\n", err_str);
                lisp_cleanup();
                return 1;
            }

            char *output = lisp_print(result);
            printf("%s\n", output);
        }

        /* Exit after running code */
        lisp_cleanup();
        return 0;
    }

    /* If file argument provided, load and execute it */
    if (argc > 1) {
        for (int i = 1; i < argc; i++) {
            FILE *file = file_open(argv[i], "r");
            if (file == NULL) {
                fprintf(stderr, "ERROR: Cannot open file: %s\n", argv[i]);
                return 1;
            }

            /* Read entire file */
            fseek(file, 0, SEEK_END);
            long size = ftell(file);
            fseek(file, 0, SEEK_SET);

            char *buffer = GC_malloc(size + 1);
            fread(buffer, 1, size, file);
            buffer[size] = '\0';
            fclose(file);

            /* Evaluate and print each expression */
            const char *input = buffer;

            while (*input) {
                /* Parse expression - let lisp_read handle whitespace and comments
                 */
                const char *parse_start = input;
                LispObject *expr = lisp_read(&input);

                /* If input didn't advance or NULL, we're done */
                if (expr == NULL || input == parse_start) {
                    break;
                }

                if (expr->type == LISP_ERROR) {
                    char *err_str = lisp_print(expr);
                    fprintf(stderr, "ERROR in %s: %s\n", argv[i], err_str);
                    return 1;
                }

                LispObject *result = lisp_eval(expr, env);

                /* Only report uncaught errors (caught errors are just values) */
                if (result->type == LISP_ERROR && !result->value.error_with_stack.caught) {
                    char *err_str = lisp_print(result);
                    fprintf(stderr, "ERROR in %s: %s\n", argv[i], err_str);
                    return 1;
                }

                char *output = lisp_print(result);
                printf("%s\n", output);
                /* output is GC-allocated, don't free */
            }
        }

        /* Exit after running files */
        lisp_cleanup();
        return 0;
    }

    /* Interactive REPL with line editing */
    print_welcome();

    /* Create line editor */
    LineEditState *le = lineedit_create();
    if (!le) {
        fprintf(stderr, "ERROR: Failed to create line editor\n");
        env_free(env);
        lisp_cleanup();
        return 1;
    }

    /* Set up completion */
    lineedit_set_completer(le, repl_completer, env);

    /* Buffer for multi-line expressions */
    static char expr_buffer[8192] = {0};
    static int expr_pos = 0;

    const char *prompt = ">>> ";
    const char *cont_prompt = "... ";

    char *line;
    while ((line = lineedit_readline(le, expr_pos > 0 ? cont_prompt : prompt)) != NULL) {

        /* Skip empty lines only if not accumulating */
        if (line[0] == '\0' && expr_pos == 0) {
            free(line);
            continue;
        }

        /* Handle commands only on first line of input */
        if (expr_pos == 0) {
            int cmd_result = handle_command(line, env);
            if (cmd_result == 1) {
                free(line);
                break; /* Exit */
            } else if (cmd_result == 0) {
                free(line);
                continue; /* Command handled */
            }
        }

        /* Add non-empty lines to history */
        if (line[0] != '\0' && expr_pos == 0) {
            lineedit_history_add(le, line);
        }

        /* Read and accumulate input until expression is complete */
        size_t line_len = strlen(line);
        if (line_len > 0 && expr_pos + line_len < sizeof(expr_buffer) - 2) {
            if (expr_pos > 0) {
                expr_buffer[expr_pos++] = ' ';
            }
            strcpy(expr_buffer + expr_pos, line);
            expr_pos += line_len;
        }

        free(line);

        /* Try to parse the buffer */
        const char *input_ptr = expr_buffer;
        LispObject *expr = lisp_read(&input_ptr);

        /* If we got an error and it's an unclosed list, continue reading */
        if (expr != NULL && expr->type == LISP_ERROR && strstr(expr->value.error, "Unclosed") != NULL) {
            /* Need more input, continue reading */
            continue;
        }

        if (expr == NULL) {
            /* Empty input or incomplete, continue reading if buffer has content
             */
            if (expr_pos == 0) {
                continue;
            }
            /* We're accumulating, so continue reading */
            continue;
        }

        if (expr->type == LISP_ERROR) {
            printf("ERROR: %s\n", expr->value.error);
            expr_pos = 0;
            expr_buffer[0] = '\0';
            continue;
        }

        LispObject *result = lisp_eval(expr, env);

        /* Only report uncaught errors (caught errors are just values) */
        if (result->type == LISP_ERROR && !result->value.error_with_stack.caught) {
            char *err_str = lisp_print(result);
            printf("ERROR: %s\n", err_str);
            /* Reset buffer on error too */
            expr_pos = 0;
            expr_buffer[0] = '\0';
        } else {
            char *output = lisp_print(result);
            printf("%s\n", output);
            /* Reset buffer after successful evaluation */
            expr_pos = 0;
            expr_buffer[0] = '\0';
        }
    }

    printf("\nGoodbye!\n");

    /* Cleanup */
    lineedit_destroy(le);
    env_free(env);
    lisp_cleanup();

    return 0;
}
