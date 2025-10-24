#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static void skip_whitespace(const char** input);
static LispObject* read_atom(const char** input);
static LispObject* read_string(const char** input);
static LispObject* read_list(const char** input);
static LispObject* read_quote(const char** input);

static void skip_whitespace(const char** input) {
    while (**input && (isspace(**input) || **input == ';')) {
        if (**input == ';') {
            /* Skip comment until end of line */
            while (**input && **input != '\n') {
                (*input)++;
            }
        }
        if (**input) (*input)++;
    }
}

static LispObject* read_string(const char** input) {
    (*input)++; /* Skip opening quote */

    size_t capacity = 64;
    size_t length = 0;
    char* buffer = GC_malloc(capacity);

    while (**input && **input != '"') {
        if (length + 2 >= capacity) {
            capacity *= 2;
            char* new_buffer = GC_malloc(capacity);
            memcpy(new_buffer, buffer, length);
            buffer = new_buffer;
        }

        if (**input == '\\') {
            (*input)++;
            switch (**input) {
                case 'n': buffer[length++] = '\n'; break;
                case 't': buffer[length++] = '\t'; break;
                case 'r': buffer[length++] = '\r'; break;
                case '\\': buffer[length++] = '\\'; break;
                case '"': buffer[length++] = '"'; break;
                default: buffer[length++] = **input; break;
            }
            (*input)++;
        } else {
            buffer[length++] = **input;
            (*input)++;
        }
    }

    if (**input == '"') {
        (*input)++;
    }

    buffer[length] = '\0';
    LispObject* obj = lisp_make_string(buffer);
    return obj;
}

static LispObject* read_atom(const char** input) {
    const char* start = *input;

    /* Check for number */
    int is_number = 1;
    int has_dot = 0;
    const char* p = start;

    if (*p == '-' || *p == '+') p++;

    if (!isdigit(*p)) {
        is_number = 0;
    }

    while (*p && !isspace(*p) && *p != ')' && *p != '(' && *p != ';') {
        if (*p == '.') {
            if (has_dot) {
                is_number = 0;
                break;
            }
            has_dot = 1;
        } else if (!isdigit(*p)) {
            is_number = 0;
            break;
        }
        p++;
    }

    /* Read token */
    size_t length = 0;
    while (**input && !isspace(**input) && **input != ')' && **input != '(' && **input != ';') {
        length++;
        (*input)++;
    }

    char* token = GC_malloc(length + 1);
    strncpy(token, start, length);
    token[length] = '\0';

    LispObject* obj;
    if (is_number && length > 0) {
        obj = lisp_make_number(atof(token));
    } else if (strcmp(token, "nil") == 0) {
        obj = NIL;
    } else {
        obj = lisp_make_symbol(token);
    }

    return obj;
}

static LispObject* read_list(const char** input) {
    (*input)++; /* Skip opening paren */
    skip_whitespace(input);

    if (**input == ')') {
        (*input)++;
        return NIL;
    }

    LispObject* head = NULL;
    LispObject* tail = NULL;

    while (**input && **input != ')') {
        LispObject* elem = lisp_read(input);
        if (elem == NULL) {
            return lisp_make_error("Syntax error in list");
        }

        LispObject* new_cons = lisp_make_cons(elem, NIL);

        if (head == NULL) {
            head = new_cons;
            tail = new_cons;
        } else {
            tail->value.cons.cdr = new_cons;
            tail = new_cons;
        }

        skip_whitespace(input);
    }

    if (**input == ')') {
        (*input)++;
    } else {
        return lisp_make_error("Unclosed list");
    }

    return head;
}

static LispObject* read_quote(const char** input) {
    (*input)++; /* Skip quote character */
    LispObject* quoted = lisp_read(input);
    if (quoted == NULL) {
        return lisp_make_error("Nothing to quote");
    }

    /* Build (quote <quoted>) */
    LispObject* quote_sym = lisp_make_symbol("quote");
    LispObject* quoted_list = lisp_make_cons(quoted, NIL);
    return lisp_make_cons(quote_sym, quoted_list);
}

LispObject* lisp_read(const char** input) {
    skip_whitespace(input);

    if (**input == '\0') {
        return NULL;
    }

    if (**input == '(') {
        return read_list(input);
    }

    if (**input == ')') {
        return lisp_make_error("Unexpected ')'");
    }

    if (**input == '\'') {
        return read_quote(input);
    }

    if (**input == '"') {
        return read_string(input);
    }

    return read_atom(input);
}
