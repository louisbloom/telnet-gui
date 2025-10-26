#include "lisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>

#define MAX_INPUT 4096

static void print_welcome(void) {
    printf("Telnet Lisp Interpreter v1.0\n");
    printf("Type expressions to evaluate, :quit to exit, :load <file> to load a file\n");
    printf(">>> ");
    fflush(stdout);
}

static void print_prompt(void) {
    printf(">>> ");
    fflush(stdout);
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

        /* Remove trailing newline */
        char *fname = GC_strdup(filename);
        char *newline = strchr(fname, '\n');
        if (newline)
            *newline = '\0';

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

    /* Initialize interpreter */
    lisp_init();
    Environment *env = env_create_global();

    /* If file argument provided, load and execute it */
    if (argc > 1) {
        for (int i = 1; i < argc; i++) {
            FILE *file = fopen(argv[i], "r");
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
                /* Skip whitespace and comments */
                while (*input == ' ' || *input == '\t' || *input == '\n' || *input == '\r' || *input == ';') {
                    if (*input == ';') {
                        while (*input && *input != '\n')
                            input++;
                    } else {
                        input++;
                    }
                }

                /* End of input */
                if (*input == '\0')
                    break;

                /* Parse expression */
                const char *parse_start = input;
                LispObject *expr = lisp_read(&input);

                /* If input didn't advance, we're done */
                if (expr == NULL || input == parse_start) {
                    break;
                }

                if (expr->type == LISP_ERROR) {
                    char *err_str = lisp_print(expr);
                    fprintf(stderr, "ERROR in %s: %s\n", argv[i], err_str);
                    return 1;
                }

                LispObject *result = lisp_eval(expr, env);

                if (result->type == LISP_ERROR) {
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

    /* REPL */
    print_welcome();

    char input[MAX_INPUT];

    /* Buffer for multi-line expressions */
    static char expr_buffer[8192] = {0};
    static int expr_pos = 0;

    while (fgets(input, sizeof(input), stdin) != NULL) {
        /* Remove trailing newline */
        size_t len = strlen(input);
        if (len > 0 && input[len - 1] == '\n') {
            input[len - 1] = '\0';
        }

        /* Skip empty lines only if not accumulating */
        if (input[0] == '\0' && expr_pos == 0) {
            print_prompt();
            continue;
        }

        /* Handle commands only on first line of input */
        if (expr_pos == 0) {
            int cmd_result = handle_command(input, env);
            if (cmd_result == 1) {
                break; /* Exit */
            } else if (cmd_result == 0) {
                print_prompt();
                continue; /* Command handled */
            }
        }

        /* Read and accumulate input until expression is complete */
        /* Append this line to buffer */
        size_t input_len = strlen(input);
        if (input_len > 0 && expr_pos + input_len < sizeof(expr_buffer) - 1) {
            if (expr_pos > 0) {
                expr_buffer[expr_pos++] = ' ';
            }
            strcpy(expr_buffer + expr_pos, input);
            expr_pos += input_len;
        }

        /* Try to parse the buffer */
        const char *input_ptr = expr_buffer;
        LispObject *expr = lisp_read(&input_ptr);

        /* If we got an error and it's an unclosed list, continue reading */
        if (expr != NULL && expr->type == LISP_ERROR && strstr(expr->value.error, "Unclosed") != NULL) {
            /* Need more input, continue reading */
            print_prompt();
            continue;
        }

        if (expr == NULL) {
            /* Empty input or incomplete, continue reading if buffer has content */
            if (expr_pos == 0) {
                print_prompt();
                continue;
            }
            /* We're accumulating, so continue reading */
            print_prompt();
            continue;
        }

        if (expr->type == LISP_ERROR) {
            printf("ERROR: %s\n", expr->value.error);
            expr_pos = 0;
            expr_buffer[0] = '\0';
            print_prompt();
            continue;
        }

        LispObject *result = lisp_eval(expr, env);

        if (result->type == LISP_ERROR) {
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

        print_prompt();
    }

    printf("\nGoodbye!\n");

    /* Cleanup */
    env_free(env);
    lisp_cleanup();

    return 0;
}
