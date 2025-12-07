#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

static void skip_whitespace(const char **input);
static LispObject *read_atom(const char **input);
static LispObject *read_string(const char **input);
static LispObject *read_list(const char **input);
static LispObject *read_quote(const char **input);
static LispObject *read_backquote(const char **input);
static LispObject *read_unquote(const char **input);
static LispObject *read_unquote_splicing(const char **input);
static LispObject *read_vector(const char **input);

static void skip_whitespace(const char **input) {
    while (**input && (isspace(**input) || **input == ';')) {
        if (**input == ';') {
            /* Skip comment until end of line */
            while (**input && **input != '\n') {
                (*input)++;
            }
        }
        if (**input)
            (*input)++;
    }
}

static LispObject *read_string(const char **input) {
    (*input)++; /* Skip opening quote */

    size_t capacity = 64;
    size_t length = 0;
    char *buffer = GC_malloc(capacity);

    while (**input && **input != '"') {
        if (length + 2 >= capacity) {
            capacity *= 2;
            char *new_buffer = GC_malloc(capacity);
            memcpy(new_buffer, buffer, length);
            buffer = new_buffer;
        }

        if (**input == '\\') {
            (*input)++;
            switch (**input) {
            case 'n':
                buffer[length++] = '\n';
                (*input)++;
                break;
            case 't':
                buffer[length++] = '\t';
                (*input)++;
                break;
            case 'r':
                buffer[length++] = '\r';
                (*input)++;
                break;
            case '\\':
                buffer[length++] = '\\';
                (*input)++;
                break;
            case '"':
                buffer[length++] = '"';
                (*input)++;
                break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7': {
                /* Octal escape sequence: \0, \00, \000 (1-3 digits) */
                int octal_value = 0;
                int digit_count = 0;
                while (**input >= '0' && **input <= '7' && digit_count < 3) {
                    octal_value = octal_value * 8 + (**input - '0');
                    (*input)++;
                    digit_count++;
                }
                buffer[length++] = (char)octal_value;
                break;
            }
            default:
                buffer[length++] = **input;
                (*input)++;
                break;
            }
        } else {
            buffer[length++] = **input;
            (*input)++;
        }
    }

    if (**input == '"') {
        (*input)++;
    }

    buffer[length] = '\0';
    LispObject *obj = lisp_make_string(buffer);
    return obj;
}

static LispObject *read_atom(const char **input) {
    const char *start = *input;

    /* Check for number */
    int is_number = 1;
    int has_dot = 0;
    const char *p = start;

    if (*p == '-' || *p == '+')
        p++;

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

    char *token = GC_malloc(length + 1);
    strncpy(token, start, length);
    token[length] = '\0';

    LispObject *obj;
    if (is_number && length > 0) {
        /* Try to parse as integer first */
        if (!has_dot && strchr(token, '.') == NULL) {
            long long val = atoll(token);
            obj = lisp_make_integer(val);
        } else {
            obj = lisp_make_number(atof(token));
        }
    } else if (strcmp(token, "nil") == 0 || strcmp(token, "#f") == 0) {
        obj = NIL;
    } else if (strcmp(token, "#t") == 0) {
        obj = lisp_make_boolean(1);
    } else {
        obj = lisp_make_symbol(token);
    }

    return obj;
}

static LispObject *read_list(const char **input) {
    (*input)++; /* Skip opening paren */
    skip_whitespace(input);

    if (**input == ')') {
        (*input)++;
        return NIL;
    }

    LispObject *head = NULL;
    LispObject *tail = NULL;

    while (**input && **input != ')') {
        /* Check for dot notation */
        if (**input == '.' && isspace(*(*input + 1))) {
            (*input)++; /* Skip dot */
            skip_whitespace(input);

            /* Read the cdr */
            LispObject *cdr_elem = lisp_read(input);
            if (cdr_elem == NULL) {
                return lisp_make_error("Syntax error after dot in list");
            }

            /* Set the cdr of the last cons cell */
            if (tail != NULL) {
                tail->value.cons.cdr = cdr_elem;
            } else {
                return lisp_make_error("Dot cannot appear at start of list");
            }

            skip_whitespace(input);

            /* Should be closing paren now */
            if (**input != ')') {
                return lisp_make_error("Expected ) after dotted pair");
            }
            break;
        }

        LispObject *elem = lisp_read(input);
        if (elem == NULL) {
            return lisp_make_error("Syntax error in list");
        }

        LispObject *new_cons = lisp_make_cons(elem, NIL);

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

static LispObject *read_vector(const char **input) {
    (*input)++; /* Skip # */
    (*input)++; /* Skip ( */
    skip_whitespace(input);

    LispObject *vec = lisp_make_vector(8); /* Start with capacity 8 */

    if (**input == ')') {
        (*input)++;
        return vec;
    }

    while (**input && **input != ')') {
        LispObject *elem = lisp_read(input);
        if (elem == NULL) {
            return lisp_make_error("Syntax error in vector");
        }
        if (elem->type == LISP_ERROR) {
            return elem;
        }

        /* Add element to vector */
        if (vec->value.vector.size >= vec->value.vector.capacity) {
            /* Expand capacity */
            size_t new_capacity = vec->value.vector.capacity * 2;
            LispObject **new_items = GC_malloc(sizeof(LispObject *) * new_capacity);
            for (size_t i = 0; i < vec->value.vector.size; i++) {
                new_items[i] = vec->value.vector.items[i];
            }
            for (size_t i = vec->value.vector.size; i < new_capacity; i++) {
                new_items[i] = NIL;
            }
            vec->value.vector.items = new_items;
            vec->value.vector.capacity = new_capacity;
        }

        vec->value.vector.items[vec->value.vector.size++] = elem;

        skip_whitespace(input);
    }

    if (**input == ')') {
        (*input)++;
    } else {
        return lisp_make_error("Unclosed vector");
    }

    return vec;
}

static LispObject *read_quote(const char **input) {
    (*input)++; /* Skip quote character */
    LispObject *quoted = lisp_read(input);
    if (quoted == NULL) {
        return lisp_make_error("Nothing to quote");
    }

    /* Build (quote <quoted>) */
    LispObject *quoted_list = lisp_make_cons(quoted, NIL);
    return lisp_make_cons(sym_quote, quoted_list);
}

static LispObject *read_backquote(const char **input) {
    (*input)++; /* Skip backquote character */
    LispObject *expr = lisp_read(input);
    if (expr == NULL) {
        return lisp_make_error("Nothing to backquote");
    }

    /* Build (quasiquote <expr>) */
    LispObject *expr_list = lisp_make_cons(expr, NIL);
    return lisp_make_cons(sym_quasiquote, expr_list);
}

static LispObject *read_unquote(const char **input) {
    (*input)++; /* Skip comma character */
    LispObject *expr = lisp_read(input);
    if (expr == NULL) {
        return lisp_make_error("Nothing to unquote");
    }

    /* Build (unquote <expr>) */
    LispObject *expr_list = lisp_make_cons(expr, NIL);
    return lisp_make_cons(sym_unquote, expr_list);
}

static LispObject *read_unquote_splicing(const char **input) {
    (*input) += 2; /* Skip ,@ characters */
    LispObject *expr = lisp_read(input);
    if (expr == NULL) {
        return lisp_make_error("Nothing to unquote-splice");
    }

    /* Build (unquote-splicing <expr>) */
    LispObject *expr_list = lisp_make_cons(expr, NIL);
    return lisp_make_cons(sym_unquote_splicing, expr_list);
}

LispObject *lisp_read(const char **input) {
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

    if (**input == '`') {
        return read_backquote(input);
    }

    if (**input == ',') {
        /* Check for ,@ (unquote-splicing) */
        if (*(*input + 1) == '@') {
            return read_unquote_splicing(input);
        }
        return read_unquote(input);
    }

    if (**input == '"') {
        return read_string(input);
    }

    /* Handle #t, #f, and #(...) */
    if (**input == '#') {
        const char *p = *input + 1;
        if (*p == 't') {
            (*input) += 2;
            return lisp_make_boolean(1);
        } else if (*p == 'f') {
            (*input) += 2;
            return NIL;
        } else if (*p == '(') {
            return read_vector(input);
        }
        return lisp_make_error("Unknown # syntax");
    }

    return read_atom(input);
}
