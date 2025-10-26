#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Arithmetic operations */
static LispObject *builtin_add(LispObject *args, Environment *env);
static LispObject *builtin_subtract(LispObject *args, Environment *env);
static LispObject *builtin_multiply(LispObject *args, Environment *env);
static LispObject *builtin_divide(LispObject *args, Environment *env);

/* Number comparisons */
static LispObject *builtin_gt(LispObject *args, Environment *env);
static LispObject *builtin_lt(LispObject *args, Environment *env);
static LispObject *builtin_eq(LispObject *args, Environment *env);
static LispObject *builtin_gte(LispObject *args, Environment *env);
static LispObject *builtin_lte(LispObject *args, Environment *env);

/* String operations */
static LispObject *builtin_concat(LispObject *args, Environment *env);
static LispObject *builtin_split(LispObject *args, Environment *env);
static LispObject *builtin_string_eq(LispObject *args, Environment *env);
static LispObject *builtin_string_lt(LispObject *args, Environment *env);
static LispObject *builtin_string_gt(LispObject *args, Environment *env);
static LispObject *builtin_string_lte(LispObject *args, Environment *env);
static LispObject *builtin_string_gte(LispObject *args, Environment *env);
static LispObject *builtin_string_contains(LispObject *args, Environment *env);
static LispObject *builtin_string_match(LispObject *args, Environment *env);
static LispObject *builtin_string_length(LispObject *args, Environment *env);
static LispObject *builtin_substring(LispObject *args, Environment *env);
static LispObject *builtin_string_ref(LispObject *args, Environment *env);

/* Boolean operations */
static LispObject *builtin_and(LispObject *args, Environment *env);
static LispObject *builtin_or(LispObject *args, Environment *env);
static LispObject *builtin_not(LispObject *args, Environment *env);

/* List operations */
static LispObject *builtin_car(LispObject *args, Environment *env);
static LispObject *builtin_cdr(LispObject *args, Environment *env);
static LispObject *builtin_cons(LispObject *args, Environment *env);
static LispObject *builtin_list(LispObject *args, Environment *env);
static LispObject *builtin_list_length(LispObject *args, Environment *env);
static LispObject *builtin_list_ref(LispObject *args, Environment *env);

/* Predicates */
static LispObject *builtin_null_question(LispObject *args, Environment *env);
static LispObject *builtin_atom_question(LispObject *args, Environment *env);

/* Regex operations */
static LispObject *builtin_regex_match(LispObject *args, Environment *env);
static LispObject *builtin_regex_find(LispObject *args, Environment *env);
static LispObject *builtin_regex_find_all(LispObject *args, Environment *env);
static LispObject *builtin_regex_extract(LispObject *args, Environment *env);
static LispObject *builtin_regex_replace(LispObject *args, Environment *env);
static LispObject *builtin_regex_replace_all(LispObject *args, Environment *env);
static LispObject *builtin_regex_split(LispObject *args, Environment *env);
static LispObject *builtin_regex_escape(LispObject *args, Environment *env);
static LispObject *builtin_regex_valid(LispObject *args, Environment *env);

/* Integer operations */
static LispObject *builtin_quotient(LispObject *args, Environment *env);
static LispObject *builtin_remainder(LispObject *args, Environment *env);
static LispObject *builtin_even_question(LispObject *args, Environment *env);
static LispObject *builtin_odd_question(LispObject *args, Environment *env);

/* Hash table operations */
static LispObject *builtin_make_hash_table(LispObject *args, Environment *env);
static LispObject *builtin_hash_ref(LispObject *args, Environment *env);
static LispObject *builtin_hash_set_bang(LispObject *args, Environment *env);
static LispObject *builtin_hash_remove_bang(LispObject *args, Environment *env);
static LispObject *builtin_hash_clear_bang(LispObject *args, Environment *env);
static LispObject *builtin_hash_count(LispObject *args, Environment *env);
static LispObject *builtin_hash_keys(LispObject *args, Environment *env);
static LispObject *builtin_hash_values(LispObject *args, Environment *env);
static LispObject *builtin_hash_entries(LispObject *args, Environment *env);

/* File I/O operations */
static LispObject *builtin_open(LispObject *args, Environment *env);
static LispObject *builtin_close(LispObject *args, Environment *env);
static LispObject *builtin_read_line(LispObject *args, Environment *env);
static LispObject *builtin_write_line(LispObject *args, Environment *env);

/* Type predicates */
static LispObject *builtin_integer_question(LispObject *args, Environment *env);
static LispObject *builtin_boolean_question(LispObject *args, Environment *env);
static LispObject *builtin_number_question(LispObject *args, Environment *env);
static LispObject *builtin_vector_question(LispObject *args, Environment *env);
static LispObject *builtin_hash_table_question(LispObject *args, Environment *env);

/* Vector operations */
static LispObject *builtin_make_vector(LispObject *args, Environment *env);
static LispObject *builtin_vector_ref(LispObject *args, Environment *env);
static LispObject *builtin_vector_set_bang(LispObject *args, Environment *env);
static LispObject *builtin_vector_length(LispObject *args, Environment *env);
static LispObject *builtin_vector_push_bang(LispObject *args, Environment *env);
static LispObject *builtin_vector_pop_bang(LispObject *args, Environment *env);

/* Helper for wildcard matching */
static int match_char_class(const char **pattern, char c);
static int wildcard_match(const char *pattern, const char *str);

void register_builtins(Environment *env) {
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
    env_define(env, "string-length", lisp_make_builtin(builtin_string_length, "string-length"));
    env_define(env, "substring", lisp_make_builtin(builtin_substring, "substring"));
    env_define(env, "string-ref", lisp_make_builtin(builtin_string_ref, "string-ref"));

    env_define(env, "and", lisp_make_builtin(builtin_and, "and"));
    env_define(env, "or", lisp_make_builtin(builtin_or, "or"));
    env_define(env, "not", lisp_make_builtin(builtin_not, "not"));

    env_define(env, "car", lisp_make_builtin(builtin_car, "car"));
    env_define(env, "cdr", lisp_make_builtin(builtin_cdr, "cdr"));
    env_define(env, "cons", lisp_make_builtin(builtin_cons, "cons"));
    env_define(env, "list", lisp_make_builtin(builtin_list, "list"));
    env_define(env, "list-length", lisp_make_builtin(builtin_list_length, "list-length"));
    env_define(env, "list-ref", lisp_make_builtin(builtin_list_ref, "list-ref"));

    env_define(env, "null?", lisp_make_builtin(builtin_null_question, "null?"));
    env_define(env, "atom?", lisp_make_builtin(builtin_atom_question, "atom?"));

    env_define(env, "regex-match", lisp_make_builtin(builtin_regex_match, "regex-match"));
    env_define(env, "regex-find", lisp_make_builtin(builtin_regex_find, "regex-find"));
    env_define(env, "regex-find-all", lisp_make_builtin(builtin_regex_find_all, "regex-find-all"));
    env_define(env, "regex-extract", lisp_make_builtin(builtin_regex_extract, "regex-extract"));
    env_define(env, "regex-replace", lisp_make_builtin(builtin_regex_replace, "regex-replace"));
    env_define(env, "regex-replace-all", lisp_make_builtin(builtin_regex_replace_all, "regex-replace-all"));
    env_define(env, "regex-split", lisp_make_builtin(builtin_regex_split, "regex-split"));
    env_define(env, "regex-escape", lisp_make_builtin(builtin_regex_escape, "regex-escape"));
    env_define(env, "regex-valid?", lisp_make_builtin(builtin_regex_valid, "regex-valid?"));

    /* File I/O functions */
    env_define(env, "open", lisp_make_builtin(builtin_open, "open"));
    env_define(env, "close", lisp_make_builtin(builtin_close, "close"));
    env_define(env, "read-line", lisp_make_builtin(builtin_read_line, "read-line"));
    env_define(env, "write-line", lisp_make_builtin(builtin_write_line, "write-line"));

    /* Type predicates */
    env_define(env, "integer?", lisp_make_builtin(builtin_integer_question, "integer?"));
    env_define(env, "boolean?", lisp_make_builtin(builtin_boolean_question, "boolean?"));
    env_define(env, "number?", lisp_make_builtin(builtin_number_question, "number?"));
    env_define(env, "vector?", lisp_make_builtin(builtin_vector_question, "vector?"));
    env_define(env, "hash-table?", lisp_make_builtin(builtin_hash_table_question, "hash-table?"));

    /* Vector operations */
    env_define(env, "make-vector", lisp_make_builtin(builtin_make_vector, "make-vector"));
    env_define(env, "vector-ref", lisp_make_builtin(builtin_vector_ref, "vector-ref"));
    env_define(env, "vector-set!", lisp_make_builtin(builtin_vector_set_bang, "vector-set!"));
    env_define(env, "vector-length", lisp_make_builtin(builtin_vector_length, "vector-length"));
    env_define(env, "vector-push!", lisp_make_builtin(builtin_vector_push_bang, "vector-push!"));
    env_define(env, "vector-pop!", lisp_make_builtin(builtin_vector_pop_bang, "vector-pop!"));

    /* Integer operations */
    env_define(env, "quotient", lisp_make_builtin(builtin_quotient, "quotient"));
    env_define(env, "remainder", lisp_make_builtin(builtin_remainder, "remainder"));
    env_define(env, "even?", lisp_make_builtin(builtin_even_question, "even?"));
    env_define(env, "odd?", lisp_make_builtin(builtin_odd_question, "odd?"));

    /* Hash table operations */
    env_define(env, "make-hash-table", lisp_make_builtin(builtin_make_hash_table, "make-hash-table"));
    env_define(env, "hash-ref", lisp_make_builtin(builtin_hash_ref, "hash-ref"));
    env_define(env, "hash-set!", lisp_make_builtin(builtin_hash_set_bang, "hash-set!"));
    env_define(env, "hash-remove!", lisp_make_builtin(builtin_hash_remove_bang, "hash-remove!"));
    env_define(env, "hash-clear!", lisp_make_builtin(builtin_hash_clear_bang, "hash-clear!"));
    env_define(env, "hash-count", lisp_make_builtin(builtin_hash_count, "hash-count"));
    env_define(env, "hash-keys", lisp_make_builtin(builtin_hash_keys, "hash-keys"));
    env_define(env, "hash-values", lisp_make_builtin(builtin_hash_values, "hash-values"));
    env_define(env, "hash-entries", lisp_make_builtin(builtin_hash_entries, "hash-entries"));
}

/* Helper function to get numeric value */
static double get_numeric_value(LispObject *obj, int *is_integer) {
    if (obj->type == LISP_INTEGER) {
        *is_integer = 1;
        return (double)obj->value.integer;
    } else if (obj->type == LISP_NUMBER) {
        *is_integer = 0;
        return obj->value.number;
    }
    return 0.0;
}

/* Arithmetic operations */
static LispObject *builtin_add(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_integer(0); /* Return 0 for (+), not 0.0 */
    }

    /* Check if all integers */
    int all_integers = 1;
    double sum = 0;
    int first_is_integer = 0;
    double first_val = 0;

    LispObject *first = lisp_car(args);
    first_val = get_numeric_value(first, &first_is_integer);
    all_integers = first_is_integer;
    sum = first_val;
    args = lisp_cdr(args);

    while (args != NIL && args != NULL) {
        LispObject *arg = lisp_car(args);
        int arg_is_integer = 0;
        double val = get_numeric_value(arg, &arg_is_integer);
        if (arg_is_integer == 0 && arg->type != LISP_NUMBER && arg->type != LISP_INTEGER) {
            return lisp_make_error("+ requires numbers");
        }
        if (!arg_is_integer) {
            all_integers = 0;
        }
        sum += val;
        args = lisp_cdr(args);
    }

    if (all_integers) {
        return lisp_make_integer((long long)sum);
    } else {
        return lisp_make_number(sum);
    }
}

static LispObject *builtin_subtract(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("- requires at least one argument");
    }

    LispObject *first = lisp_car(args);
    int first_is_integer = 0;
    double result = get_numeric_value(first, &first_is_integer);
    int all_integers = first_is_integer;

    args = lisp_cdr(args);

    if (args == NIL) {
        /* Unary negation */
        if (first_is_integer) {
            return lisp_make_integer((long long)-result);
        } else {
            return lisp_make_number(-result);
        }
    }

    while (args != NIL && args != NULL) {
        LispObject *arg = lisp_car(args);
        int arg_is_integer = 0;
        double val = get_numeric_value(arg, &arg_is_integer);
        if (arg_is_integer == 0 && arg->type != LISP_NUMBER && arg->type != LISP_INTEGER) {
            return lisp_make_error("- requires numbers");
        }
        if (!arg_is_integer) {
            all_integers = 0;
        }
        result -= val;
        args = lisp_cdr(args);
    }

    if (all_integers) {
        return lisp_make_integer((long long)result);
    } else {
        return lisp_make_number(result);
    }
}

static LispObject *builtin_multiply(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_integer(1); /* Return 1 for (*), not 1.0 */
    }

    int all_integers = 1;
    double product = 1;

    while (args != NIL && args != NULL) {
        LispObject *arg = lisp_car(args);
        int arg_is_integer = 0;
        double val = get_numeric_value(arg, &arg_is_integer);
        if (arg_is_integer == 0 && arg->type != LISP_NUMBER && arg->type != LISP_INTEGER) {
            return lisp_make_error("* requires numbers");
        }
        if (!arg_is_integer) {
            all_integers = 0;
        }
        product *= val;
        args = lisp_cdr(args);
    }

    if (all_integers) {
        return lisp_make_integer((long long)product);
    } else {
        return lisp_make_number(product);
    }
}

static LispObject *builtin_divide(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("/ requires at least one argument");
    }

    LispObject *first = lisp_car(args);
    int first_is_integer;
    double result = get_numeric_value(first, &first_is_integer);
    args = lisp_cdr(args);

    if (args == NIL) {
        /* Unary reciprocal */
        if (result == 0) {
            return lisp_make_error("Division by zero");
        }
        return lisp_make_number(1.0 / result); /* Always return float */
    }

    /* Division always returns float */
    while (args != NIL && args != NULL) {
        LispObject *arg = lisp_car(args);
        int arg_is_integer = 0;
        double val = get_numeric_value(arg, &arg_is_integer);
        if (arg_is_integer == 0 && arg->type != LISP_NUMBER && arg->type != LISP_INTEGER) {
            return lisp_make_error("/ requires numbers");
        }
        if (val == 0) {
            return lisp_make_error("Division by zero");
        }
        result /= val;
        args = lisp_cdr(args);
    }

    return lisp_make_number(result); /* Always return float */
}

static LispObject *builtin_quotient(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("quotient requires 2 arguments");
    }

    LispObject *first = lisp_car(args);
    LispObject *second = lisp_car(lisp_cdr(args));

    int first_is_integer;
    int second_is_integer;
    double first_val = get_numeric_value(first, &first_is_integer);
    double second_val = get_numeric_value(second, &second_is_integer);

    if (first_val == 0 || (first_is_integer == 0 && first->type != LISP_INTEGER && first->type != LISP_NUMBER)) {
        return lisp_make_error("quotient requires numbers");
    }
    if (second_val == 0) {
        return lisp_make_error("Division by zero");
    }

    /* Truncate to integer */
    long long result = (long long)(first_val / second_val);
    return lisp_make_integer(result);
}

static LispObject *builtin_remainder(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("remainder requires 2 arguments");
    }

    LispObject *first = lisp_car(args);
    LispObject *second = lisp_car(lisp_cdr(args));

    int first_is_integer;
    int second_is_integer;
    double first_val = get_numeric_value(first, &first_is_integer);
    double second_val = get_numeric_value(second, &second_is_integer);

    if (first_val == 0 || (first_is_integer == 0 && first->type != LISP_INTEGER && first->type != LISP_NUMBER)) {
        return lisp_make_error("remainder requires numbers");
    }
    if (second_val == 0) {
        return lisp_make_error("Division by zero");
    }

    long long result = (long long)first_val % (long long)second_val;
    return lisp_make_integer(result);
}

static LispObject *builtin_even_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("even? requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    int arg_is_integer;
    double arg_val = get_numeric_value(arg, &arg_is_integer);

    if (arg->type != LISP_INTEGER && arg->type != LISP_NUMBER) {
        return lisp_make_error("even? requires a number");
    }

    long long val = (long long)arg_val;
    if ((val & 1) == 0) {
        return lisp_make_boolean(1);
    }
    return lisp_make_boolean(0);
}

static LispObject *builtin_odd_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("odd? requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    int arg_is_integer;
    double arg_val = get_numeric_value(arg, &arg_is_integer);

    if (arg->type != LISP_INTEGER && arg->type != LISP_NUMBER) {
        return lisp_make_error("odd? requires a number");
    }

    long long val = (long long)arg_val;
    if ((val & 1) == 1) {
        return lisp_make_boolean(1);
    }
    return lisp_make_boolean(0);
}

/* Number comparisons */
static LispObject *builtin_gt(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("> requires at least 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    int a_is_integer = 0;
    int b_is_integer = 0;
    double a_val = get_numeric_value(a, &a_is_integer);
    double b_val = get_numeric_value(b, &b_is_integer);

    if ((!a_is_integer && a->type != LISP_NUMBER && a->type != LISP_INTEGER) ||
        (!b_is_integer && b->type != LISP_NUMBER && b->type != LISP_INTEGER)) {
        return lisp_make_error("> requires numbers");
    }

    return (a_val > b_val) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_lt(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("< requires at least 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    int a_is_integer = 0;
    int b_is_integer = 0;
    double a_val = get_numeric_value(a, &a_is_integer);
    double b_val = get_numeric_value(b, &b_is_integer);

    if ((!a_is_integer && a->type != LISP_NUMBER && a->type != LISP_INTEGER) ||
        (!b_is_integer && b->type != LISP_NUMBER && b->type != LISP_INTEGER)) {
        return lisp_make_error("< requires numbers");
    }

    return (a_val < b_val) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_eq(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("= requires at least 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    int a_is_integer = 0;
    int b_is_integer = 0;
    double a_val = get_numeric_value(a, &a_is_integer);
    double b_val = get_numeric_value(b, &b_is_integer);

    if ((!a_is_integer && a->type != LISP_NUMBER && a->type != LISP_INTEGER) ||
        (!b_is_integer && b->type != LISP_NUMBER && b->type != LISP_INTEGER)) {
        return lisp_make_error("= requires numbers");
    }

    return (a_val == b_val) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_gte(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error(">= requires at least 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    int a_is_integer = 0;
    int b_is_integer = 0;
    double a_val = get_numeric_value(a, &a_is_integer);
    double b_val = get_numeric_value(b, &b_is_integer);

    if ((!a_is_integer && a->type != LISP_NUMBER && a->type != LISP_INTEGER) ||
        (!b_is_integer && b->type != LISP_NUMBER && b->type != LISP_INTEGER)) {
        return lisp_make_error(">= requires numbers");
    }

    return (a_val >= b_val) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_lte(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("<= requires at least 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    int a_is_integer = 0;
    int b_is_integer = 0;
    double a_val = get_numeric_value(a, &a_is_integer);
    double b_val = get_numeric_value(b, &b_is_integer);

    if ((!a_is_integer && a->type != LISP_NUMBER && a->type != LISP_INTEGER) ||
        (!b_is_integer && b->type != LISP_NUMBER && b->type != LISP_INTEGER)) {
        return lisp_make_error("<= requires numbers");
    }

    return (a_val <= b_val) ? lisp_make_number(1) : NIL;
}

/* String operations */
static LispObject *builtin_concat(LispObject *args, Environment *env) {
    (void)env;
    size_t total_len = 0;

    /* Calculate total length */
    LispObject *curr = args;
    while (curr != NIL && curr != NULL) {
        LispObject *arg = lisp_car(curr);
        if (arg->type != LISP_STRING) {
            return lisp_make_error("concat requires strings");
        }
        total_len += strlen(arg->value.string);
        curr = lisp_cdr(curr);
    }

    /* Concatenate */
    char *result = GC_malloc(total_len + 1);
    result[0] = '\0';

    curr = args;
    while (curr != NIL && curr != NULL) {
        LispObject *arg = lisp_car(curr);
        strcat(result, arg->value.string);
        curr = lisp_cdr(curr);
    }

    LispObject *obj = lisp_make_string(result);
    return obj;
}

static int match_char_class(const char **pattern, char c) {
    const char *p = *pattern + 1; /* Skip '[' */
    int negate = 0;
    int match = 0;

    if (*p == '!') {
        negate = 1;
        p++;
    }

    while (*p && *p != ']') {
        if (*(p + 1) == '-' && *(p + 2) != ']' && *(p + 2) != '\0') {
            /* Range */
            if (c >= *p && c <= *(p + 2)) {
                match = 1;
            }
            p += 3;
        } else {
            /* Single character */
            if (c == *p) {
                match = 1;
            }
            p++;
        }
    }

    if (*p == ']') {
        *pattern = p + 1;
    }

    return negate ? !match : match;
}

static int wildcard_match(const char *pattern, const char *str) {
    while (*pattern && *str) {
        if (*pattern == '*') {
            pattern++;
            if (*pattern == '\0')
                return 1;
            while (*str) {
                if (wildcard_match(pattern, str))
                    return 1;
                str++;
            }
            return 0;
        } else if (*pattern == '?') {
            pattern++;
            str++;
        } else if (*pattern == '[') {
            if (match_char_class(&pattern, *str)) {
                str++;
            } else {
                return 0;
            }
        } else if (*pattern == *str) {
            pattern++;
            str++;
        } else {
            return 0;
        }
    }

    while (*pattern == '*')
        pattern++;
    return (*pattern == '\0' && *str == '\0');
}

static LispObject *builtin_split(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("split requires 2 arguments");
    }

    LispObject *str_obj = lisp_car(args);
    LispObject *pattern_obj = lisp_car(lisp_cdr(args));

    if (str_obj->type != LISP_STRING || pattern_obj->type != LISP_STRING) {
        return lisp_make_error("split requires strings");
    }

    const char *str = str_obj->value.string;
    const char *pattern = pattern_obj->value.string;
    size_t pattern_len = strlen(pattern);

    /* Handle empty pattern */
    if (pattern_len == 0) {
        return lisp_make_cons(lisp_make_string(str), NIL);
    }

    LispObject *result = NIL;
    LispObject *tail = NULL;

    const char *start = str;
    const char *p = str;

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
            char *token = GC_malloc(len + 1);
            strncpy(token, start, len);
            token[len] = '\0';

            LispObject *token_obj = lisp_make_string(token);

            LispObject *new_cons = lisp_make_cons(token_obj, NIL);
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
        LispObject *token_obj = lisp_make_string(start);
        LispObject *new_cons = lisp_make_cons(token_obj, NIL);
        if (result == NIL) {
            result = new_cons;
        } else {
            tail->value.cons.cdr = new_cons;
        }
    }

    return result;
}

static LispObject *builtin_string_eq(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string= requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string= requires strings");
    }

    return (strcmp(a->value.string, b->value.string) == 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_lt(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string< requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string< requires strings");
    }

    return (strcmp(a->value.string, b->value.string) < 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_gt(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string> requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string> requires strings");
    }

    return (strcmp(a->value.string, b->value.string) > 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_lte(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string<= requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string<= requires strings");
    }

    return (strcmp(a->value.string, b->value.string) <= 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_gte(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string>= requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string>= requires strings");
    }

    return (strcmp(a->value.string, b->value.string) >= 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_contains(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-contains requires 2 arguments");
    }

    LispObject *haystack = lisp_car(args);
    LispObject *needle = lisp_car(lisp_cdr(args));

    if (haystack->type != LISP_STRING || needle->type != LISP_STRING) {
        return lisp_make_error("string-contains requires strings");
    }

    return (strstr(haystack->value.string, needle->value.string) != NULL) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_match(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-match requires 2 arguments");
    }

    LispObject *str = lisp_car(args);
    LispObject *pattern = lisp_car(lisp_cdr(args));

    if (str->type != LISP_STRING || pattern->type != LISP_STRING) {
        return lisp_make_error("string-match requires strings");
    }

    return wildcard_match(pattern->value.string, str->value.string) ? lisp_make_number(1) : NIL;
}

/* UTF-8 String operations */
static LispObject *builtin_string_length(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("string-length requires 1 argument");
    }

    LispObject *str_obj = lisp_car(args);
    if (str_obj->type != LISP_STRING) {
        return lisp_make_error("string-length requires a string");
    }

    size_t char_count = utf8_strlen(str_obj->value.string);
    return lisp_make_integer((long long)char_count);
}

static LispObject *builtin_substring(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("substring requires at least 2 arguments");
    }

    LispObject *str_obj = lisp_car(args);
    LispObject *start_obj = lisp_car(lisp_cdr(args));
    LispObject *end_obj = lisp_cdr(lisp_cdr(args)) != NIL ? lisp_car(lisp_cdr(lisp_cdr(args))) : NIL;

    if (str_obj->type != LISP_STRING) {
        return lisp_make_error("substring requires a string");
    }
    if (start_obj->type != LISP_INTEGER) {
        return lisp_make_error("substring requires integer start index");
    }

    long long start = start_obj->value.integer;
    long long end;

    if (end_obj == NIL || end_obj == NULL) {
        /* No end specified, use string length */
        end = utf8_strlen(str_obj->value.string);
    } else {
        if (end_obj->type != LISP_INTEGER) {
            return lisp_make_error("substring requires integer end index");
        }
        end = end_obj->value.integer;
    }

    if (start < 0 || end < 0 || start > end) {
        return lisp_make_error("substring: invalid start/end indices");
    }

    size_t start_offset = utf8_byte_offset(str_obj->value.string, start);
    size_t end_offset = utf8_byte_offset(str_obj->value.string, end);

    size_t result_len = end_offset - start_offset;
    char *result = GC_malloc(result_len + 1);
    memcpy(result, str_obj->value.string + start_offset, result_len);
    result[result_len] = '\0';

    return lisp_make_string(result);
}

static LispObject *builtin_string_ref(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-ref requires 2 arguments");
    }

    LispObject *str_obj = lisp_car(args);
    LispObject *index_obj = lisp_car(lisp_cdr(args));

    if (str_obj->type != LISP_STRING) {
        return lisp_make_error("string-ref requires a string");
    }
    if (index_obj->type != LISP_INTEGER) {
        return lisp_make_error("string-ref requires an integer index");
    }

    long long index = index_obj->value.integer;
    if (index < 0) {
        return lisp_make_error("string-ref: negative index");
    }

    size_t char_count = utf8_strlen(str_obj->value.string);
    if (index >= (long long)char_count) {
        return lisp_make_error("string-ref: index out of bounds");
    }

    const char *char_ptr = utf8_char_at(str_obj->value.string, index);
    if (char_ptr == NULL) {
        return lisp_make_error("string-ref: invalid character at index");
    }

    int bytes = utf8_char_bytes(char_ptr);
    char *result = GC_malloc(bytes + 1);
    memcpy(result, char_ptr, bytes);
    result[bytes] = '\0';

    return lisp_make_string(result);
}

/* Boolean operations */
static LispObject *builtin_and(LispObject *args, Environment *env) {
    (void)env;
    LispObject *last = lisp_make_number(1);

    while (args != NIL && args != NULL) {
        LispObject *arg = lisp_car(args);
        if (!lisp_is_truthy(arg)) {
            return NIL;
        }
        last = arg;
        args = lisp_cdr(args);
    }

    return last;
}

static LispObject *builtin_or(LispObject *args, Environment *env) {
    (void)env;
    while (args != NIL && args != NULL) {
        LispObject *arg = lisp_car(args);
        if (lisp_is_truthy(arg)) {
            return arg;
        }
        args = lisp_cdr(args);
    }

    return NIL;
}

static LispObject *builtin_not(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("not requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    return lisp_is_truthy(arg) ? NIL : lisp_make_number(1);
}

/* List operations */
static LispObject *builtin_car(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("car requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    if (arg == NIL) {
        return NIL;
    }

    if (arg->type != LISP_CONS) {
        return lisp_make_error("car requires a list");
    }

    return arg->value.cons.car;
}

static LispObject *builtin_cdr(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("cdr requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    if (arg == NIL) {
        return NIL;
    }

    if (arg->type != LISP_CONS) {
        return lisp_make_error("cdr requires a list");
    }

    return arg->value.cons.cdr;
}

static LispObject *builtin_cons(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("cons requires 2 arguments");
    }

    LispObject *car = lisp_car(args);
    LispObject *cdr = lisp_car(lisp_cdr(args));

    return lisp_make_cons(car, cdr);
}

static LispObject *builtin_list(LispObject *args, Environment *env) {
    (void)env;
    return args;
}

static LispObject *builtin_list_length(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("list-length requires 1 argument");
    }

    LispObject *lst = lisp_car(args);
    long long count = 0;

    while (lst != NIL && lst != NULL) {
        count++;
        lst = lst->value.cons.cdr;
    }

    return lisp_make_integer(count);
}

static LispObject *builtin_list_ref(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("list-ref requires 2 arguments");
    }

    LispObject *lst = lisp_car(args);
    LispObject *index_obj = lisp_car(lisp_cdr(args));

    int index_is_integer;
    double index_val = get_numeric_value(index_obj, &index_is_integer);

    if (index_obj->type != LISP_INTEGER && index_obj->type != LISP_NUMBER) {
        return lisp_make_error("list-ref index must be a number");
    }

    long long index = (long long)index_val;
    if (index < 0) {
        return lisp_make_error("list-ref index must be non-negative");
    }

    for (long long i = 0; i < index && lst != NIL && lst != NULL; i++) {
        lst = lst->value.cons.cdr;
    }

    if (lst == NIL || lst == NULL) {
        return lisp_make_error("list-ref index out of bounds");
    }

    return lst->value.cons.car;
}

/* Predicates */
static LispObject *builtin_null_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("null? requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    return (arg == NIL || arg == NULL) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_atom_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("atom? requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    return (arg->type != LISP_CONS) ? lisp_make_number(1) : NIL;
}

/* Regex functions */
static LispObject *builtin_regex_match(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("regex-match requires 2 arguments");
    }

    LispObject *pattern_obj = lisp_car(args);
    LispObject *string_obj = lisp_car(lisp_cdr(args));

    if (pattern_obj->type != LISP_STRING || string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-match requires strings");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "regex-match: %s", error_msg);
        return lisp_make_error(error);
    }

    pcre2_match_data *match_data = execute_regex(re, string_obj->value.string);
    int result = (match_data != NULL);

    free_regex_resources(re, match_data);

    return result ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_regex_find(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("regex-find requires 2 arguments");
    }

    LispObject *pattern_obj = lisp_car(args);
    LispObject *string_obj = lisp_car(lisp_cdr(args));

    if (pattern_obj->type != LISP_STRING || string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-find requires strings");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "regex-find: %s", error_msg);
        return lisp_make_error(error);
    }

    pcre2_match_data *match_data = execute_regex(re, string_obj->value.string);

    if (match_data == NULL) {
        free_regex_resources(re, NULL);
        return NIL;
    }

    char *matched = extract_capture(match_data, string_obj->value.string, 0);
    LispObject *result = matched ? lisp_make_string(matched) : NIL;

    free_regex_resources(re, match_data);

    return result;
}

static LispObject *builtin_regex_find_all(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("regex-find-all requires 2 arguments");
    }

    LispObject *pattern_obj = lisp_car(args);
    LispObject *string_obj = lisp_car(lisp_cdr(args));

    if (pattern_obj->type != LISP_STRING || string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-find-all requires strings");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "regex-find-all: %s", error_msg);
        return lisp_make_error(error);
    }

    LispObject *result = NIL;
    LispObject *tail = NULL;

    const char *subject = string_obj->value.string;
    size_t offset = 0;
    size_t subject_len = strlen(subject);

    while (offset < subject_len) {
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        int rc = pcre2_match(re, (PCRE2_SPTR)subject, subject_len, offset, 0, match_data, NULL);

        if (rc < 0) {
            pcre2_match_data_free(match_data);
            break;
        }

        char *matched = extract_capture(match_data, subject, 0);
        if (matched) {
            LispObject *match_obj = lisp_make_string(matched);
            LispObject *new_cons = lisp_make_cons(match_obj, NIL);

            if (result == NIL) {
                result = new_cons;
                tail = new_cons;
            } else {
                tail->value.cons.cdr = new_cons;
                tail = new_cons;
            }
        }

        PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
        offset = ovector[1];

        pcre2_match_data_free(match_data);

        if (offset == ovector[0]) {
            offset++; /* Avoid infinite loop on zero-length match */
        }
    }

    pcre2_code_free(re);

    return result;
}

static LispObject *builtin_regex_extract(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("regex-extract requires 2 arguments");
    }

    LispObject *pattern_obj = lisp_car(args);
    LispObject *string_obj = lisp_car(lisp_cdr(args));

    if (pattern_obj->type != LISP_STRING || string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-extract requires strings");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "regex-extract: %s", error_msg);
        return lisp_make_error(error);
    }

    pcre2_match_data *match_data = execute_regex(re, string_obj->value.string);

    if (match_data == NULL) {
        free_regex_resources(re, NULL);
        return NIL;
    }

    int capture_count = get_capture_count(re);
    LispObject *result = NIL;
    LispObject *tail = NULL;

    /* Extract capture groups (skip group 0 which is the whole match) */
    for (int i = 1; i <= capture_count; i++) {
        char *captured = extract_capture(match_data, string_obj->value.string, i);
        if (captured) {
            LispObject *cap_obj = lisp_make_string(captured);
            LispObject *new_cons = lisp_make_cons(cap_obj, NIL);

            if (result == NIL) {
                result = new_cons;
                tail = new_cons;
            } else {
                tail->value.cons.cdr = new_cons;
                tail = new_cons;
            }
        }
    }

    free_regex_resources(re, match_data);

    return result;
}

static LispObject *builtin_regex_replace(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL || lisp_cdr(lisp_cdr(args)) == NIL) {
        return lisp_make_error("regex-replace requires 3 arguments");
    }

    LispObject *pattern_obj = lisp_car(args);
    LispObject *replacement_obj = lisp_car(lisp_cdr(args));
    LispObject *string_obj = lisp_car(lisp_cdr(lisp_cdr(args)));

    if (pattern_obj->type != LISP_STRING || replacement_obj->type != LISP_STRING || string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-replace requires strings");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "regex-replace: %s", error_msg);
        return lisp_make_error(error);
    }

    size_t output_len = strlen(string_obj->value.string) * 2 + 256;
    PCRE2_UCHAR *output = GC_malloc(output_len);

    int rc = pcre2_substitute(re, (PCRE2_SPTR)string_obj->value.string, PCRE2_ZERO_TERMINATED, 0, /* start offset */
                              PCRE2_SUBSTITUTE_GLOBAL, /* options - replace all */
                              NULL,                    /* match data */
                              NULL,                    /* match context */
                              (PCRE2_SPTR)replacement_obj->value.string, PCRE2_ZERO_TERMINATED, output, &output_len);

    pcre2_code_free(re);

    if (rc < 0) {
        return lisp_make_error("regex-replace: substitution failed");
    }

    return lisp_make_string((char *)output);
}

static LispObject *builtin_regex_replace_all(LispObject *args, Environment *env) {
    /* Same as regex-replace since we use PCRE2_SUBSTITUTE_GLOBAL */
    return builtin_regex_replace(args, env);
}

static LispObject *builtin_regex_split(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("regex-split requires 2 arguments");
    }

    LispObject *pattern_obj = lisp_car(args);
    LispObject *string_obj = lisp_car(lisp_cdr(args));

    if (pattern_obj->type != LISP_STRING || string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-split requires strings");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "regex-split: %s", error_msg);
        return lisp_make_error(error);
    }

    LispObject *result = NIL;
    LispObject *tail = NULL;

    const char *subject = string_obj->value.string;
    size_t offset = 0;
    size_t last_end = 0;
    size_t subject_len = strlen(subject);

    while (offset <= subject_len) {
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        int rc = pcre2_match(re, (PCRE2_SPTR)subject, subject_len, offset, 0, match_data, NULL);

        if (rc < 0) {
            pcre2_match_data_free(match_data);
            break;
        }

        PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
        size_t match_start = ovector[0];
        size_t match_end = ovector[1];

        /* Add substring before match */
        size_t part_len = match_start - last_end;
        char *part = GC_malloc(part_len + 1);
        strncpy(part, subject + last_end, part_len);
        part[part_len] = '\0';

        LispObject *part_obj = lisp_make_string(part);
        LispObject *new_cons = lisp_make_cons(part_obj, NIL);

        if (result == NIL) {
            result = new_cons;
            tail = new_cons;
        } else {
            tail->value.cons.cdr = new_cons;
            tail = new_cons;
        }

        last_end = match_end;
        offset = match_end;

        pcre2_match_data_free(match_data);

        if (offset == match_start) {
            offset++; /* Avoid infinite loop */
        }
    }

    /* Add remaining substring */
    if (last_end <= subject_len) {
        char *part = GC_malloc(subject_len - last_end + 1);
        strcpy(part, subject + last_end);

        LispObject *part_obj = lisp_make_string(part);
        LispObject *new_cons = lisp_make_cons(part_obj, NIL);

        if (result == NIL) {
            result = new_cons;
        } else {
            tail->value.cons.cdr = new_cons;
        }
    }

    pcre2_code_free(re);

    return result;
}

static LispObject *builtin_regex_escape(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("regex-escape requires 1 argument");
    }

    LispObject *string_obj = lisp_car(args);

    if (string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-escape requires a string");
    }

    const char *str = string_obj->value.string;
    size_t len = strlen(str);
    char *escaped = GC_malloc(len * 2 + 1);
    size_t j = 0;

    const char *special = ".^$*+?()[]{}|\\";

    for (size_t i = 0; i < len; i++) {
        if (strchr(special, str[i])) {
            escaped[j++] = '\\';
        }
        escaped[j++] = str[i];
    }
    escaped[j] = '\0';

    return lisp_make_string(escaped);
}

static LispObject *builtin_regex_valid(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("regex-valid? requires 1 argument");
    }

    LispObject *pattern_obj = lisp_car(args);

    if (pattern_obj->type != LISP_STRING) {
        return lisp_make_error("regex-valid? requires a string");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        return NIL;
    }

    pcre2_code_free(re);
    return lisp_make_number(1);
}

/* File I/O operations */

/* Helper function to create a file stream object */
LispObject *lisp_make_file_stream(FILE *file) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_FILE_STREAM;
    obj->value.file = file;
    return obj;
}

static LispObject *builtin_open(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("open requires at least 1 argument");
    }

    LispObject *filename_obj = lisp_car(args);
    if (filename_obj->type != LISP_STRING) {
        return lisp_make_error("open requires a string filename");
    }

    /* Default mode is "r" (read) */
    const char *mode = "r";
    if (lisp_cdr(args) != NIL && lisp_cdr(args) != NULL) {
        LispObject *mode_obj = lisp_car(lisp_cdr(args));
        if (mode_obj->type != LISP_STRING) {
            return lisp_make_error("open mode must be a string");
        }
        mode = mode_obj->value.string;
    }

    FILE *file = fopen(filename_obj->value.string, mode);
    if (file == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "open: cannot open file '%s'", filename_obj->value.string);
        return lisp_make_error(error);
    }

    return lisp_make_file_stream(file);
}

static LispObject *builtin_close(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("close requires 1 argument");
    }

    LispObject *stream_obj = lisp_car(args);
    if (stream_obj->type != LISP_FILE_STREAM) {
        return lisp_make_error("close requires a file stream");
    }

    if (stream_obj->value.file != NULL) {
        fclose(stream_obj->value.file);
        stream_obj->value.file = NULL;
    }

    return NIL;
}

static LispObject *builtin_read_line(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("read-line requires 1 argument");
    }

    LispObject *stream_obj = lisp_car(args);
    if (stream_obj->type != LISP_FILE_STREAM) {
        return lisp_make_error("read-line requires a file stream");
    }

    FILE *file = stream_obj->value.file;
    if (file == NULL) {
        return lisp_make_error("read-line: file is closed");
    }

    /* Read line with dynamic buffer */
    size_t size = 256;
    char *buffer = GC_malloc(size);
    size_t pos = 0;

    int c;
    while ((c = fgetc(file)) != EOF) {
        if (c == '\n') {
            /* Unix line ending */
            break;
        } else if (c == '\r') {
            /* Check for Windows (\r\n) or old Mac (\r) line ending */
            int next_c = fgetc(file);
            if (next_c == '\n') {
                /* Windows line ending \r\n */
                break;
            } else if (next_c != EOF) {
                /* Old Mac line ending \r, put back the character */
                ungetc(next_c, file);
            }
            /* We got \r, now break */
            break;
        }

        if (pos >= size - 1) {
            size *= 2;
            char *new_buffer = GC_malloc(size);
            strncpy(new_buffer, buffer, pos);
            buffer = new_buffer;
        }

        buffer[pos++] = c;
    }

    /* Check for EOF without newline */
    if (pos == 0 && c == EOF) {
        return NIL; /* End of file */
    }

    buffer[pos] = '\0';
    return lisp_make_string(buffer);
}

static LispObject *builtin_write_line(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("write-line requires at least 1 argument");
    }

    LispObject *stream_obj = lisp_car(args);
    if (stream_obj->type != LISP_FILE_STREAM) {
        return lisp_make_error("write-line requires a file stream");
    }

    FILE *file = stream_obj->value.file;
    if (file == NULL) {
        return lisp_make_error("write-line: file is closed");
    }

    LispObject *rest = lisp_cdr(args);
    if (rest == NIL) {
        return lisp_make_error("write-line requires a string to write");
    }

    LispObject *text_obj = lisp_car(rest);
    if (text_obj->type != LISP_STRING) {
        return lisp_make_error("write-line requires a string");
    }

    fprintf(file, "%s\n", text_obj->value.string);
    fflush(file);

    return text_obj;
}

/* Type predicates */
static LispObject *builtin_integer_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("integer? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_INTEGER) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_boolean_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("boolean? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_BOOLEAN) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_number_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("number? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_NUMBER || arg->type == LISP_INTEGER) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_vector_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("vector? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_VECTOR) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_hash_table_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("hash-table? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_HASH_TABLE) ? lisp_make_number(1) : NIL;
}

/* Vector operations */
static LispObject *builtin_make_vector(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("make-vector requires at least 1 argument");
    }
    LispObject *size_obj = lisp_car(args);
    if (size_obj->type != LISP_NUMBER && size_obj->type != LISP_INTEGER) {
        return lisp_make_error("make-vector size must be a number");
    }
    size_t size = (size_t)(size_obj->type == LISP_INTEGER ? size_obj->value.integer : size_obj->value.number);
    return lisp_make_vector(size);
}

static LispObject *builtin_vector_ref(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("vector-ref requires 2 arguments");
    }
    LispObject *vec_obj = lisp_car(args);
    if (vec_obj->type != LISP_VECTOR) {
        return lisp_make_error("vector-ref requires a vector");
    }
    LispObject *idx_obj = lisp_car(lisp_cdr(args));
    if (idx_obj->type != LISP_NUMBER && idx_obj->type != LISP_INTEGER) {
        return lisp_make_error("vector-ref index must be a number");
    }
    size_t idx = (size_t)(idx_obj->type == LISP_INTEGER ? idx_obj->value.integer : idx_obj->value.number);
    if (idx >= vec_obj->value.vector.size) {
        return lisp_make_error("vector-ref: index out of bounds");
    }
    return vec_obj->value.vector.items[idx];
}

static LispObject *builtin_vector_set_bang(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL || lisp_cdr(lisp_cdr(args)) == NIL) {
        return lisp_make_error("vector-set! requires 3 arguments");
    }
    LispObject *vec_obj = lisp_car(args);
    if (vec_obj->type != LISP_VECTOR) {
        return lisp_make_error("vector-set! requires a vector");
    }
    LispObject *idx_obj = lisp_car(lisp_cdr(args));
    if (idx_obj->type != LISP_NUMBER && idx_obj->type != LISP_INTEGER) {
        return lisp_make_error("vector-set! index must be a number");
    }
    size_t idx = (size_t)(idx_obj->type == LISP_INTEGER ? idx_obj->value.integer : idx_obj->value.number);
    if (idx >= vec_obj->value.vector.size) {
        vec_obj->value.vector.size = idx + 1;
        /* Expand capacity if needed */
        if (vec_obj->value.vector.size > vec_obj->value.vector.capacity) {
            size_t new_capacity = vec_obj->value.vector.capacity;
            while (new_capacity < vec_obj->value.vector.size) {
                new_capacity *= 2;
            }
            LispObject **new_items = GC_malloc(sizeof(LispObject *) * new_capacity);
            for (size_t i = 0; i < vec_obj->value.vector.size - 1; i++) {
                new_items[i] = vec_obj->value.vector.items[i];
            }
            for (size_t i = vec_obj->value.vector.size - 1; i < new_capacity; i++) {
                new_items[i] = NIL;
            }
            vec_obj->value.vector.items = new_items;
            vec_obj->value.vector.capacity = new_capacity;
        }
    }
    LispObject *value = lisp_car(lisp_cdr(lisp_cdr(args)));
    vec_obj->value.vector.items[idx] = value;
    return value;
}

static LispObject *builtin_vector_length(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("vector-length requires 1 argument");
    }
    LispObject *vec_obj = lisp_car(args);
    if (vec_obj->type != LISP_VECTOR) {
        return lisp_make_error("vector-length requires a vector");
    }
    return lisp_make_number((double)vec_obj->value.vector.size);
}

static LispObject *builtin_vector_push_bang(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("vector-push! requires 2 arguments");
    }
    LispObject *vec_obj = lisp_car(args);
    if (vec_obj->type != LISP_VECTOR) {
        return lisp_make_error("vector-push! requires a vector");
    }
    LispObject *value = lisp_car(lisp_cdr(args));
    /* Check if we need to expand */
    if (vec_obj->value.vector.size >= vec_obj->value.vector.capacity) {
        size_t new_capacity = vec_obj->value.vector.capacity * 2;
        LispObject **new_items = GC_malloc(sizeof(LispObject *) * new_capacity);
        for (size_t i = 0; i < vec_obj->value.vector.size; i++) {
            new_items[i] = vec_obj->value.vector.items[i];
        }
        for (size_t i = vec_obj->value.vector.size; i < new_capacity; i++) {
            new_items[i] = NIL;
        }
        vec_obj->value.vector.items = new_items;
        vec_obj->value.vector.capacity = new_capacity;
    }
    vec_obj->value.vector.items[vec_obj->value.vector.size] = value;
    vec_obj->value.vector.size++;
    return value;
}

static LispObject *builtin_vector_pop_bang(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("vector-pop! requires 1 argument");
    }
    LispObject *vec_obj = lisp_car(args);
    if (vec_obj->type != LISP_VECTOR) {
        return lisp_make_error("vector-pop! requires a vector");
    }
    if (vec_obj->value.vector.size == 0) {
        return lisp_make_error("vector-pop!: cannot pop from empty vector");
    }
    vec_obj->value.vector.size--;
    return vec_obj->value.vector.items[vec_obj->value.vector.size];
}

/* Hash table operations */
static LispObject *builtin_make_hash_table(LispObject *args, Environment *env) {
    (void)env;
    (void)args;
    return lisp_make_hash_table();
}

static LispObject *builtin_hash_ref(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("hash-ref requires 2 arguments");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-ref requires a hash table");
    }

    LispObject *key_obj = lisp_car(lisp_cdr(args));
    if (key_obj->type != LISP_STRING && key_obj->type != LISP_SYMBOL) {
        return lisp_make_error("hash-ref key must be a string or symbol");
    }

    const char *key = (key_obj->type == LISP_STRING) ? key_obj->value.string : key_obj->value.symbol;
    struct HashEntry *entry = hash_table_get_entry(table, key);

    if (entry) {
        return entry->value;
    }

    return NIL; /* Key not found */
}

static LispObject *builtin_hash_set_bang(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL || lisp_cdr(lisp_cdr(args)) == NIL) {
        return lisp_make_error("hash-set! requires 3 arguments");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-set! requires a hash table");
    }

    LispObject *key_obj = lisp_car(lisp_cdr(args));
    if (key_obj->type != LISP_STRING && key_obj->type != LISP_SYMBOL) {
        return lisp_make_error("hash-set! key must be a string or symbol");
    }

    const char *key = (key_obj->type == LISP_STRING) ? key_obj->value.string : key_obj->value.symbol;
    LispObject *value = lisp_car(lisp_cdr(lisp_cdr(args)));

    hash_table_set_entry(table, key, value);
    return value;
}

static LispObject *builtin_hash_remove_bang(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("hash-remove! requires 2 arguments");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-remove! requires a hash table");
    }

    LispObject *key_obj = lisp_car(lisp_cdr(args));
    if (key_obj->type != LISP_STRING && key_obj->type != LISP_SYMBOL) {
        return lisp_make_error("hash-remove! key must be a string or symbol");
    }

    const char *key = (key_obj->type == LISP_STRING) ? key_obj->value.string : key_obj->value.symbol;
    int removed = hash_table_remove_entry(table, key);

    return removed ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_hash_clear_bang(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("hash-clear! requires 1 argument");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-clear! requires a hash table");
    }

    hash_table_clear(table);
    return NIL;
}

static LispObject *builtin_hash_count(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("hash-count requires 1 argument");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-count requires a hash table");
    }

    return lisp_make_number((double)table->value.hash_table.entry_count);
}

static LispObject *builtin_hash_keys(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("hash-keys requires 1 argument");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-keys requires a hash table");
    }

    struct HashEntry **buckets = (struct HashEntry **)table->value.hash_table.buckets;
    size_t bucket_count = table->value.hash_table.bucket_count;

    LispObject *result = NIL;
    LispObject *tail = NULL;

    for (size_t i = 0; i < bucket_count; i++) {
        struct HashEntry *entry = buckets[i];
        while (entry != NULL) {
            LispObject *key_obj = lisp_make_string(entry->key);
            LispObject *new_cons = lisp_make_cons(key_obj, NIL);

            if (result == NIL) {
                result = new_cons;
                tail = new_cons;
            } else {
                tail->value.cons.cdr = new_cons;
                tail = new_cons;
            }

            entry = entry->next;
        }
    }

    return result;
}

static LispObject *builtin_hash_values(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("hash-values requires 1 argument");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-values requires a hash table");
    }

    struct HashEntry **buckets = (struct HashEntry **)table->value.hash_table.buckets;
    size_t bucket_count = table->value.hash_table.bucket_count;

    LispObject *result = NIL;
    LispObject *tail = NULL;

    for (size_t i = 0; i < bucket_count; i++) {
        struct HashEntry *entry = buckets[i];
        while (entry != NULL) {
            LispObject *value_obj = entry->value;
            LispObject *new_cons = lisp_make_cons(value_obj, NIL);

            if (result == NIL) {
                result = new_cons;
                tail = new_cons;
            } else {
                tail->value.cons.cdr = new_cons;
                tail = new_cons;
            }

            entry = entry->next;
        }
    }

    return result;
}

static LispObject *builtin_hash_entries(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("hash-entries requires 1 argument");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-entries requires a hash table");
    }

    struct HashEntry **buckets = (struct HashEntry **)table->value.hash_table.buckets;
    size_t bucket_count = table->value.hash_table.bucket_count;

    LispObject *result = NIL;
    LispObject *tail = NULL;

    for (size_t i = 0; i < bucket_count; i++) {
        struct HashEntry *entry = buckets[i];
        while (entry != NULL) {
            LispObject *key_obj = lisp_make_string(entry->key);
            LispObject *value_obj = entry->value;

            /* Create (key . value) pair */
            LispObject *pair = lisp_make_cons(key_obj, value_obj);
            LispObject *new_cons = lisp_make_cons(pair, NIL);

            if (result == NIL) {
                result = new_cons;
                tail = new_cons;
            } else {
                tail->value.cons.cdr = new_cons;
                tail = new_cons;
            }

            entry = entry->next;
        }
    }

    return result;
}
