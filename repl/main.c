#include "../include/lisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INPUT 4096

static void print_welcome(void) {
    printf("Telnet LISP Interpreter v1.0\n");
    printf("Type expressions to evaluate, :quit to exit, :load <file> to load a file\n");
    printf(">>> ");
    fflush(stdout);
}

static void print_prompt(void) {
    printf(">>> ");
    fflush(stdout);
}

static int handle_command(const char* input, Environment* env) {
    /* Skip leading whitespace */
    while (*input == ' ' || *input == '\t') input++;

    if (strncmp(input, ":quit", 5) == 0) {
        return 1; /* Exit */
    }

    if (strncmp(input, ":load", 5) == 0) {
        const char* filename = input + 5;
        while (*filename == ' ' || *filename == '\t') filename++;

        if (*filename == '\0') {
            printf("ERROR: :load requires a filename\n");
            return 0;
        }

        /* Remove trailing newline */
        char* fname = GC_strdup(filename);
        char* newline = strchr(fname, '\n');
        if (newline) *newline = '\0';

        LispObject* result = lisp_load_file(fname, env);

        if (result->type == LISP_ERROR) {
            printf("ERROR: %s\n", result->value.error);
        } else {
            char* output = lisp_print(result);
            printf("%s\n", output);
        }

        return 0;
    }

    return -1; /* Not a command */
}

int main(int argc, char** argv) {
    /* Initialize interpreter */
    lisp_init();
    Environment* env = env_create_global();

    /* If file argument provided, load and execute it */
    if (argc > 1) {
        for (int i = 1; i < argc; i++) {
            LispObject* result = lisp_load_file(argv[i], env);

            if (result->type == LISP_ERROR) {
                fprintf(stderr, "ERROR in %s: %s\n", argv[i], result->value.error);
                return 1;
            }

            char* output = lisp_print(result);
            printf("%s\n", output);
            free(output);
        }

        /* Exit after running files if not in interactive mode */
        if (argc > 1) {
            lisp_cleanup();
            return 0;
        }
    }

    /* REPL */
    print_welcome();

    char input[MAX_INPUT];

    while (fgets(input, sizeof(input), stdin) != NULL) {
        /* Remove trailing newline */
        size_t len = strlen(input);
        if (len > 0 && input[len - 1] == '\n') {
            input[len - 1] = '\0';
        }

        /* Skip empty lines */
        if (input[0] == '\0') {
            print_prompt();
            continue;
        }

        /* Handle commands */
        int cmd_result = handle_command(input, env);
        if (cmd_result == 1) {
            break; /* Exit */
        } else if (cmd_result == 0) {
            print_prompt();
            continue; /* Command handled */
        }

        /* Evaluate expression */
        const char* input_ptr = input;
        LispObject* expr = lisp_read(&input_ptr);

        if (expr == NULL) {
            print_prompt();
            continue;
        }

        if (expr->type == LISP_ERROR) {
            printf("ERROR: %s\n", expr->value.error);
            print_prompt();
            continue;
        }

        LispObject* result = lisp_eval(expr, env);

        if (result->type == LISP_ERROR) {
            printf("ERROR: %s\n", result->value.error);
        } else {
            char* output = lisp_print(result);
            printf("%s\n", output);
        }

        print_prompt();
    }

    printf("\nGoodbye!\n");

    /* Cleanup */
    env_free(env);
    lisp_cleanup();

    return 0;
}
