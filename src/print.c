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
        append_str(buffer, size, pos, "#<lambda>");
        break;

    case LISP_ERROR:
        append_str(buffer, size, pos, "ERROR: ");
        append_str(buffer, size, pos, obj->value.error);
        break;

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
