#include "../include/lisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void print_list(LispObject *obj, char **buffer, size_t *size, size_t *pos);
static void append_str(char **buffer, size_t *size, size_t *pos, const char *str);

static void append_str(char **buffer, size_t *size, size_t *pos, const char *str) {
    size_t len = strlen(str);
    while (*pos + len + 1 > *size) {
        *size *= 2;
        char *new_buffer = GC_malloc(*size);
        memcpy(new_buffer, *buffer, *pos);
        *buffer = new_buffer;
    }
    strcpy(*buffer + *pos, str);
    *pos += len;
}

static void print_object(LispObject *obj, char **buffer, size_t *size, size_t *pos) {
    char temp[256];

    if (obj == NULL || obj == NIL) {
        append_str(buffer, size, pos, "nil");
        return;
    }

    switch (obj->type) {
    case LISP_NIL:
        append_str(buffer, size, pos, "nil");
        break;

    case LISP_NUMBER:
        snprintf(temp, sizeof(temp), "%.15g", obj->value.number);
        append_str(buffer, size, pos, temp);
        break;

    case LISP_STRING:
        append_str(buffer, size, pos, "\"");
        append_str(buffer, size, pos, obj->value.string);
        append_str(buffer, size, pos, "\"");
        break;

    case LISP_SYMBOL:
        append_str(buffer, size, pos, obj->value.symbol);
        break;

    case LISP_CONS:
        print_list(obj, buffer, size, pos);
        break;

    case LISP_BUILTIN:
        snprintf(temp, sizeof(temp), "#<builtin:%s>", obj->value.builtin.name);
        append_str(buffer, size, pos, temp);
        break;

    case LISP_LAMBDA:
        append_str(buffer, size, pos, "#<lambda ");
        if (obj->value.lambda.name != NULL) {
            append_str(buffer, size, pos, obj->value.lambda.name);
            append_str(buffer, size, pos, " ");
        }
        /* Print full parameter list (shows &optional and &rest markers) */
        print_object(obj->value.lambda.params, buffer, size, pos);
        append_str(buffer, size, pos, ">");
        break;

    case LISP_MACRO:
        if (obj->value.macro.name != NULL) {
            append_str(buffer, size, pos, "#<macro:");
            append_str(buffer, size, pos, obj->value.macro.name);
            append_str(buffer, size, pos, ">");
        } else {
            append_str(buffer, size, pos, "#<macro>");
        }
        break;

    case LISP_ERROR: {
        append_str(buffer, size, pos, "ERROR: [");

        /* Print error type symbol */
        if (obj->value.error_with_stack.error_type != NULL &&
            obj->value.error_with_stack.error_type->type == LISP_SYMBOL) {
            append_str(buffer, size, pos, obj->value.error_with_stack.error_type->value.symbol);
        } else {
            append_str(buffer, size, pos, "error");
        }

        append_str(buffer, size, pos, "] ");
        append_str(buffer, size, pos, obj->value.error_with_stack.message);

        /* Print data if present */
        if (obj->value.error_with_stack.data != NIL && obj->value.error_with_stack.data != NULL) {
            append_str(buffer, size, pos, " (data: ");
            print_object(obj->value.error_with_stack.data, buffer, size, pos);
            append_str(buffer, size, pos, ")");
        }

        /* Print stack trace if available */
        if (obj->value.error_with_stack.stack_trace != NIL && obj->value.error_with_stack.stack_trace != NULL) {
            LispObject *stack = obj->value.error_with_stack.stack_trace;
            append_str(buffer, size, pos, "\nCall stack:");
            int frame_num = 0;
            while (stack != NIL && stack->type == LISP_CONS && frame_num < 20) {
                append_str(buffer, size, pos, "\n  at ");
                print_object(lisp_car(stack), buffer, size, pos);
                stack = lisp_cdr(stack);
                frame_num++;
            }
        }
        break;
    }

    case LISP_INTEGER:
        snprintf(temp, sizeof(temp), "%lld", obj->value.integer);
        append_str(buffer, size, pos, temp);
        break;

    case LISP_BOOLEAN:
        append_str(buffer, size, pos, obj->value.boolean ? "#t" : "#f");
        break;

    case LISP_VECTOR:
        append_str(buffer, size, pos, "#(");
        for (size_t i = 0; i < obj->value.vector.size; i++) {
            if (i > 0) {
                append_str(buffer, size, pos, " ");
            }
            print_object(obj->value.vector.items[i], buffer, size, pos);
        }
        append_str(buffer, size, pos, ")");
        break;

    case LISP_HASH_TABLE:
        append_str(buffer, size, pos, "#<hash-table>");
        break;

    case LISP_FILE_STREAM:
        append_str(buffer, size, pos, "#<file-stream>");
        break;

    case LISP_TAIL_CALL:
        append_str(buffer, size, pos, "#<tail-call>");
        break;
    }
}

static void print_list(LispObject *obj, char **buffer, size_t *size, size_t *pos) {
    append_str(buffer, size, pos, "(");

    while (obj != NULL && obj != NIL && obj->type == LISP_CONS) {
        print_object(obj->value.cons.car, buffer, size, pos);
        obj = obj->value.cons.cdr;

        if (obj != NULL && obj != NIL) {
            if (obj->type == LISP_CONS) {
                append_str(buffer, size, pos, " ");
            } else {
                append_str(buffer, size, pos, " . ");
                print_object(obj, buffer, size, pos);
                break;
            }
        }
    }

    append_str(buffer, size, pos, ")");
}

char *lisp_print(LispObject *obj) {
    size_t size = 256;
    size_t pos = 0;
    char *buffer = GC_malloc(size);
    buffer[0] = '\0';

    print_object(obj, &buffer, &size, &pos);

    return buffer;
}

/* Helper functions for princ (prints directly to stdout, strings without quotes) */
static void princ_list(LispObject *obj);
static void princ_object(LispObject *obj);

static void princ_object(LispObject *obj) {
    if (obj == NULL || obj == NIL) {
        printf("nil");
        return;
    }

    switch (obj->type) {
    case LISP_NIL:
        printf("nil");
        break;

    case LISP_NUMBER:
        printf("%.15g", obj->value.number);
        break;

    case LISP_STRING:
        /* Display strings without quotes (unlike lisp_print) */
        printf("%s", obj->value.string);
        break;

    case LISP_SYMBOL:
        printf("%s", obj->value.symbol);
        break;

    case LISP_CONS:
        princ_list(obj);
        break;

    case LISP_BUILTIN:
        printf("#<builtin:%s>", obj->value.builtin.name);
        break;

    case LISP_LAMBDA:
        printf("#<lambda ");
        if (obj->value.lambda.name != NULL) {
            printf("%s ", obj->value.lambda.name);
        }
        /* Print full parameter list (shows &optional and &rest markers) */
        princ_object(obj->value.lambda.params);
        printf(">");
        break;

    case LISP_MACRO:
        if (obj->value.macro.name != NULL) {
            printf("#<macro:%s>", obj->value.macro.name);
        } else {
            printf("#<macro>");
        }
        break;

    case LISP_ERROR: {
        printf("ERROR: [");

        /* Print error type symbol */
        if (obj->value.error_with_stack.error_type != NULL &&
            obj->value.error_with_stack.error_type->type == LISP_SYMBOL) {
            printf("%s", obj->value.error_with_stack.error_type->value.symbol);
        } else {
            printf("error");
        }

        printf("] %s", obj->value.error_with_stack.message);

        /* Print data if present */
        if (obj->value.error_with_stack.data != NIL && obj->value.error_with_stack.data != NULL) {
            printf(" (data: ");
            princ_object(obj->value.error_with_stack.data);
            printf(")");
        }

        /* Print stack trace if available */
        if (obj->value.error_with_stack.stack_trace != NIL && obj->value.error_with_stack.stack_trace != NULL) {
            LispObject *stack = obj->value.error_with_stack.stack_trace;
            printf("\nCall stack:");
            int frame_num = 0;
            while (stack != NIL && stack->type == LISP_CONS && frame_num < 20) {
                printf("\n  at ");
                princ_object(lisp_car(stack));
                stack = lisp_cdr(stack);
                frame_num++;
            }
        }
        break;
    }

    case LISP_INTEGER:
        printf("%lld", obj->value.integer);
        break;

    case LISP_BOOLEAN:
        printf("%s", obj->value.boolean ? "#t" : "#f");
        break;

    case LISP_VECTOR:
        printf("#(");
        for (size_t i = 0; i < obj->value.vector.size; i++) {
            if (i > 0) {
                printf(" ");
            }
            princ_object(obj->value.vector.items[i]);
        }
        printf(")");
        break;

    case LISP_HASH_TABLE:
        printf("#<hash-table>");
        break;

    case LISP_FILE_STREAM:
        printf("#<file-stream>");
        break;

    case LISP_TAIL_CALL:
        printf("#<tail-call>");
        break;
    }
}

static void princ_list(LispObject *obj) {
    printf("(");

    while (obj != NULL && obj != NIL && obj->type == LISP_CONS) {
        princ_object(obj->value.cons.car);
        obj = obj->value.cons.cdr;

        if (obj != NULL && obj != NIL) {
            if (obj->type == LISP_CONS) {
                printf(" ");
            } else {
                printf(" . ");
                princ_object(obj);
                break;
            }
        }
    }

    printf(")");
}

/* Common Lisp style printing functions */
void lisp_princ(LispObject *obj) {
    princ_object(obj);
    fflush(stdout);
}

void lisp_prin1(LispObject *obj) {
    char *str = lisp_print(obj);
    printf("%s", str);
    fflush(stdout);
}

void lisp_print_cl(LispObject *obj) {
    printf("\n"); /* Newline BEFORE */
    lisp_prin1(obj);
    printf("\n"); /* Newline AFTER */
    fflush(stdout);
}
