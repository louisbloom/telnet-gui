#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Arithmetic operations */
static LispObject* builtin_add(LispObject* args, Environment* env);
static LispObject* builtin_subtract(LispObject* args, Environment* env);
static LispObject* builtin_multiply(LispObject* args, Environment* env);
static LispObject* builtin_divide(LispObject* args, Environment* env);

/* Number comparisons */
static LispObject* builtin_gt(LispObject* args, Environment* env);
static LispObject* builtin_lt(LispObject* args, Environment* env);
static LispObject* builtin_eq(LispObject* args, Environment* env);
static LispObject* builtin_gte(LispObject* args, Environment* env);
static LispObject* builtin_lte(LispObject* args, Environment* env);

/* String operations */
static LispObject* builtin_concat(LispObject* args, Environment* env);
static LispObject* builtin_split(LispObject* args, Environment* env);
static LispObject* builtin_string_eq(LispObject* args, Environment* env);
static LispObject* builtin_string_lt(LispObject* args, Environment* env);
static LispObject* builtin_string_gt(LispObject* args, Environment* env);
static LispObject* builtin_string_lte(LispObject* args, Environment* env);
static LispObject* builtin_string_gte(LispObject* args, Environment* env);
static LispObject* builtin_string_contains(LispObject* args, Environment* env);
static LispObject* builtin_string_match(LispObject* args, Environment* env);

/* Boolean operations */
static LispObject* builtin_and(LispObject* args, Environment* env);
static LispObject* builtin_or(LispObject* args, Environment* env);
static LispObject* builtin_not(LispObject* args, Environment* env);

/* List operations */
static LispObject* builtin_car(LispObject* args, Environment* env);
static LispObject* builtin_cdr(LispObject* args, Environment* env);
static LispObject* builtin_cons(LispObject* args, Environment* env);
static LispObject* builtin_list(LispObject* args, Environment* env);

/* Predicates */
static LispObject* builtin_null(LispObject* args, Environment* env);
static LispObject* builtin_atom(LispObject* args, Environment* env);

/* Helper for wildcard matching */
static int wildcard_match(const char* pattern, const char* str);

void register_builtins(Environment* env) {
    env_define(env, "+", lisp_make_builtin(builtin_add, "+"));
    env_define(env, "-", lisp_make_builtin(builtin_subtract, "-"));
    env_define(env, "*", lisp_make_builtin(builtin_multiply, "*"));
    env_define(env, "/", lisp_make_builtin(builtin_divide, "/"));

    env_define(env, ">", lisp_make_builtin(builtin_gt, ">"));
    env_define(env, "<", lisp_make_builtin(builtin_lt, "<"));
    env_define(env, "=", lisp_make_builtin(builtin_eq, "="));
    env_define(env, ">=", lisp_make_builtin(builtin_gte, ">="));
    env_define(env, "<=", lisp_make_builtin(builtin_lte, "<="));

    env_define(env, "concat", lisp_make_builtin(builtin_concat, "concat"));
    env_define(env, "split", lisp_make_builtin(builtin_split, "split"));
    env_define(env, "string=", lisp_make_builtin(builtin_string_eq, "string="));
    env_define(env, "string<", lisp_make_builtin(builtin_string_lt, "string<"));
    env_define(env, "string>", lisp_make_builtin(builtin_string_gt, "string>"));
    env_define(env, "string<=", lisp_make_builtin(builtin_string_lte, "string<="));
    env_define(env, "string>=", lisp_make_builtin(builtin_string_gte, "string>="));
    env_define(env, "string-contains", lisp_make_builtin(builtin_string_contains, "string-contains"));
    env_define(env, "string-match", lisp_make_builtin(builtin_string_match, "string-match"));

    env_define(env, "and", lisp_make_builtin(builtin_and, "and"));
    env_define(env, "or", lisp_make_builtin(builtin_or, "or"));
    env_define(env, "not", lisp_make_builtin(builtin_not, "not"));

    env_define(env, "car", lisp_make_builtin(builtin_car, "car"));
    env_define(env, "cdr", lisp_make_builtin(builtin_cdr, "cdr"));
    env_define(env, "cons", lisp_make_builtin(builtin_cons, "cons"));
    env_define(env, "list", lisp_make_builtin(builtin_list, "list"));

    env_define(env, "null", lisp_make_builtin(builtin_null, "null"));
    env_define(env, "atom", lisp_make_builtin(builtin_atom, "atom"));
}

/* Arithmetic operations */
static LispObject* builtin_add(LispObject* args, Environment* env) {
    (void)env;
    double sum = 0;

    while (args != NIL && args != NULL) {
        LispObject* arg = lisp_car(args);
        if (arg->type != LISP_NUMBER) {
            return lisp_make_error("+ requires numbers");
        }
        sum += arg->value.number;
        args = lisp_cdr(args);
    }

    return lisp_make_number(sum);
}

static LispObject* builtin_subtract(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("- requires at least one argument");
    }

    LispObject* first = lisp_car(args);
    if (first->type != LISP_NUMBER) {
        return lisp_make_error("- requires numbers");
    }

    double result = first->value.number;
    args = lisp_cdr(args);

    if (args == NIL) {
        return lisp_make_number(-result);
    }

    while (args != NIL && args != NULL) {
        LispObject* arg = lisp_car(args);
        if (arg->type != LISP_NUMBER) {
            return lisp_make_error("- requires numbers");
        }
        result -= arg->value.number;
        args = lisp_cdr(args);
    }

    return lisp_make_number(result);
}

static LispObject* builtin_multiply(LispObject* args, Environment* env) {
    (void)env;
    double product = 1;

    while (args != NIL && args != NULL) {
        LispObject* arg = lisp_car(args);
        if (arg->type != LISP_NUMBER) {
            return lisp_make_error("* requires numbers");
        }
        product *= arg->value.number;
        args = lisp_cdr(args);
    }

    return lisp_make_number(product);
}

static LispObject* builtin_divide(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("/ requires at least one argument");
    }

    LispObject* first = lisp_car(args);
    if (first->type != LISP_NUMBER) {
        return lisp_make_error("/ requires numbers");
    }

    double result = first->value.number;
    args = lisp_cdr(args);

    if (args == NIL) {
        if (result == 0) {
            return lisp_make_error("Division by zero");
        }
        return lisp_make_number(1.0 / result);
    }

    while (args != NIL && args != NULL) {
        LispObject* arg = lisp_car(args);
        if (arg->type != LISP_NUMBER) {
            return lisp_make_error("/ requires numbers");
        }
        if (arg->value.number == 0) {
            return lisp_make_error("Division by zero");
        }
        result /= arg->value.number;
        args = lisp_cdr(args);
    }

    return lisp_make_number(result);
}

/* Number comparisons */
static LispObject* builtin_gt(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("> requires at least 2 arguments");
    }

    LispObject* a = lisp_car(args);
    LispObject* b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_NUMBER || b->type != LISP_NUMBER) {
        return lisp_make_error("> requires numbers");
    }

    return (a->value.number > b->value.number) ? lisp_make_number(1) : NIL;
}

static LispObject* builtin_lt(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("< requires at least 2 arguments");
    }

    LispObject* a = lisp_car(args);
    LispObject* b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_NUMBER || b->type != LISP_NUMBER) {
        return lisp_make_error("< requires numbers");
    }

    return (a->value.number < b->value.number) ? lisp_make_number(1) : NIL;
}

static LispObject* builtin_eq(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("= requires at least 2 arguments");
    }

    LispObject* a = lisp_car(args);
    LispObject* b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_NUMBER || b->type != LISP_NUMBER) {
        return lisp_make_error("= requires numbers");
    }

    return (a->value.number == b->value.number) ? lisp_make_number(1) : NIL;
}

static LispObject* builtin_gte(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error(">= requires at least 2 arguments");
    }

    LispObject* a = lisp_car(args);
    LispObject* b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_NUMBER || b->type != LISP_NUMBER) {
        return lisp_make_error(">= requires numbers");
    }

    return (a->value.number >= b->value.number) ? lisp_make_number(1) : NIL;
}

static LispObject* builtin_lte(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("<= requires at least 2 arguments");
    }

    LispObject* a = lisp_car(args);
    LispObject* b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_NUMBER || b->type != LISP_NUMBER) {
        return lisp_make_error("<= requires numbers");
    }

    return (a->value.number <= b->value.number) ? lisp_make_number(1) : NIL;
}

/* String operations */
static LispObject* builtin_concat(LispObject* args, Environment* env) {
    (void)env;
    size_t total_len = 0;

    /* Calculate total length */
    LispObject* curr = args;
    while (curr != NIL && curr != NULL) {
        LispObject* arg = lisp_car(curr);
        if (arg->type != LISP_STRING) {
            return lisp_make_error("concat requires strings");
        }
        total_len += strlen(arg->value.string);
        curr = lisp_cdr(curr);
    }

    /* Concatenate */
    char* result = GC_malloc(total_len + 1);
    result[0] = '\0';

    curr = args;
    while (curr != NIL && curr != NULL) {
        LispObject* arg = lisp_car(curr);
        strcat(result, arg->value.string);
        curr = lisp_cdr(curr);
    }

    LispObject* obj = lisp_make_string(result);
    return obj;
}

static int wildcard_match(const char* pattern, const char* str) {
    while (*pattern && *str) {
        if (*pattern == '*') {
            pattern++;
            if (*pattern == '\0') return 1;
            while (*str) {
                if (wildcard_match(pattern, str)) return 1;
                str++;
            }
            return 0;
        } else if (*pattern == '?' || *pattern == *str) {
            pattern++;
            str++;
        } else {
            return 0;
        }
    }

    while (*pattern == '*') pattern++;
    return (*pattern == '\0' && *str == '\0');
}

static LispObject* builtin_split(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("split requires 2 arguments");
    }

    LispObject* str_obj = lisp_car(args);
    LispObject* pattern_obj = lisp_car(lisp_cdr(args));

    if (str_obj->type != LISP_STRING || pattern_obj->type != LISP_STRING) {
        return lisp_make_error("split requires strings");
    }

    const char* str = str_obj->value.string;
    const char* pattern = pattern_obj->value.string;
    size_t pattern_len = strlen(pattern);

    /* Handle empty pattern */
    if (pattern_len == 0) {
        return lisp_make_cons(lisp_make_string(str), NIL);
    }

    LispObject* result = NIL;
    LispObject* tail = NULL;

    const char* start = str;
    const char* p = str;

    /* Check if pattern contains wildcards */
    int has_wildcards = (strchr(pattern, '*') != NULL || strchr(pattern, '?') != NULL);

    while (*p) {
        int match = 0;

        if (has_wildcards) {
            match = wildcard_match(pattern, p);
        } else {
            /* Literal string match */
            match = (strncmp(p, pattern, pattern_len) == 0);
        }

        if (match) {
            /* Found match */
            size_t len = p - start;
            char* token = GC_malloc(len + 1);
            strncpy(token, start, len);
            token[len] = '\0';

            LispObject* token_obj = lisp_make_string(token);

            LispObject* new_cons = lisp_make_cons(token_obj, NIL);
            if (result == NIL) {
                result = new_cons;
                tail = new_cons;
            } else {
                tail->value.cons.cdr = new_cons;
                tail = new_cons;
            }

            /* Skip pattern */
            p += pattern_len;
            start = p;
        } else {
            p++;
        }
    }

    /* Add remaining */
    if (*start || result != NIL) {
        LispObject* token_obj = lisp_make_string(start);
        LispObject* new_cons = lisp_make_cons(token_obj, NIL);
        if (result == NIL) {
            result = new_cons;
        } else {
            tail->value.cons.cdr = new_cons;
        }
    }

    return result;
}

static LispObject* builtin_string_eq(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string= requires 2 arguments");
    }

    LispObject* a = lisp_car(args);
    LispObject* b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string= requires strings");
    }

    return (strcmp(a->value.string, b->value.string) == 0) ? lisp_make_number(1) : NIL;
}

static LispObject* builtin_string_lt(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string< requires 2 arguments");
    }

    LispObject* a = lisp_car(args);
    LispObject* b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string< requires strings");
    }

    return (strcmp(a->value.string, b->value.string) < 0) ? lisp_make_number(1) : NIL;
}

static LispObject* builtin_string_gt(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string> requires 2 arguments");
    }

    LispObject* a = lisp_car(args);
    LispObject* b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string> requires strings");
    }

    return (strcmp(a->value.string, b->value.string) > 0) ? lisp_make_number(1) : NIL;
}

static LispObject* builtin_string_lte(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string<= requires 2 arguments");
    }

    LispObject* a = lisp_car(args);
    LispObject* b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string<= requires strings");
    }

    return (strcmp(a->value.string, b->value.string) <= 0) ? lisp_make_number(1) : NIL;
}

static LispObject* builtin_string_gte(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string>= requires 2 arguments");
    }

    LispObject* a = lisp_car(args);
    LispObject* b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string>= requires strings");
    }

    return (strcmp(a->value.string, b->value.string) >= 0) ? lisp_make_number(1) : NIL;
}

static LispObject* builtin_string_contains(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-contains requires 2 arguments");
    }

    LispObject* haystack = lisp_car(args);
    LispObject* needle = lisp_car(lisp_cdr(args));

    if (haystack->type != LISP_STRING || needle->type != LISP_STRING) {
        return lisp_make_error("string-contains requires strings");
    }

    return (strstr(haystack->value.string, needle->value.string) != NULL) ? lisp_make_number(1) : NIL;
}

static LispObject* builtin_string_match(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-match requires 2 arguments");
    }

    LispObject* str = lisp_car(args);
    LispObject* pattern = lisp_car(lisp_cdr(args));

    if (str->type != LISP_STRING || pattern->type != LISP_STRING) {
        return lisp_make_error("string-match requires strings");
    }

    return wildcard_match(pattern->value.string, str->value.string) ? lisp_make_number(1) : NIL;
}

/* Boolean operations */
static LispObject* builtin_and(LispObject* args, Environment* env) {
    (void)env;
    LispObject* last = lisp_make_number(1);

    while (args != NIL && args != NULL) {
        LispObject* arg = lisp_car(args);
        if (!lisp_is_truthy(arg)) {
            return NIL;
        }
        last = arg;
        args = lisp_cdr(args);
    }

    return last;
}

static LispObject* builtin_or(LispObject* args, Environment* env) {
    (void)env;
    while (args != NIL && args != NULL) {
        LispObject* arg = lisp_car(args);
        if (lisp_is_truthy(arg)) {
            return arg;
        }
        args = lisp_cdr(args);
    }

    return NIL;
}

static LispObject* builtin_not(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("not requires 1 argument");
    }

    LispObject* arg = lisp_car(args);
    return lisp_is_truthy(arg) ? NIL : lisp_make_number(1);
}

/* List operations */
static LispObject* builtin_car(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("car requires 1 argument");
    }

    LispObject* arg = lisp_car(args);
    if (arg == NIL) {
        return NIL;
    }

    if (arg->type != LISP_CONS) {
        return lisp_make_error("car requires a list");
    }

    return arg->value.cons.car;
}

static LispObject* builtin_cdr(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("cdr requires 1 argument");
    }

    LispObject* arg = lisp_car(args);
    if (arg == NIL) {
        return NIL;
    }

    if (arg->type != LISP_CONS) {
        return lisp_make_error("cdr requires a list");
    }

    return arg->value.cons.cdr;
}

static LispObject* builtin_cons(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("cons requires 2 arguments");
    }

    LispObject* car = lisp_car(args);
    LispObject* cdr = lisp_car(lisp_cdr(args));

    return lisp_make_cons(car, cdr);
}

static LispObject* builtin_list(LispObject* args, Environment* env) {
    (void)env;
    return args;
}

/* Predicates */
static LispObject* builtin_null(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("null requires 1 argument");
    }

    LispObject* arg = lisp_car(args);
    return (arg == NIL || arg == NULL) ? lisp_make_number(1) : NIL;
}

static LispObject* builtin_atom(LispObject* args, Environment* env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("atom requires 1 argument");
    }

    LispObject* arg = lisp_car(args);
    return (arg->type != LISP_CONS) ? lisp_make_number(1) : NIL;
}
