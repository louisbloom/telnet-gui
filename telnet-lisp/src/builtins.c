#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>

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
static LispObject *builtin_number_to_string(LispObject *args, Environment *env);
static LispObject *builtin_string_to_number(LispObject *args, Environment *env);
static LispObject *builtin_split(LispObject *args, Environment *env);
static LispObject *builtin_string_lt(LispObject *args, Environment *env);
static LispObject *builtin_string_gt(LispObject *args, Environment *env);
static LispObject *builtin_string_lte(LispObject *args, Environment *env);
static LispObject *builtin_string_gte(LispObject *args, Environment *env);
static LispObject *builtin_string_contains(LispObject *args, Environment *env);
static LispObject *builtin_string_match(LispObject *args, Environment *env);
static LispObject *builtin_string_length(LispObject *args, Environment *env);
static LispObject *builtin_substring(LispObject *args, Environment *env);
static LispObject *builtin_string_ref(LispObject *args, Environment *env);
static LispObject *builtin_string_prefix_question(LispObject *args, Environment *env);
static LispObject *builtin_string_replace(LispObject *args, Environment *env);
static LispObject *builtin_string_upcase(LispObject *args, Environment *env);
static LispObject *builtin_string_downcase(LispObject *args, Environment *env);

/* Boolean operations */
static LispObject *builtin_not(LispObject *args, Environment *env);

/* List operations */
static LispObject *builtin_car(LispObject *args, Environment *env);
static LispObject *builtin_cdr(LispObject *args, Environment *env);
static LispObject *builtin_cons(LispObject *args, Environment *env);
static LispObject *builtin_list(LispObject *args, Environment *env);
static LispObject *builtin_list_length(LispObject *args, Environment *env);
static LispObject *builtin_list_ref(LispObject *args, Environment *env);
static LispObject *builtin_reverse(LispObject *args, Environment *env);

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
static LispObject *builtin_read_sexp(LispObject *args, Environment *env);
static LispObject *builtin_read_json(LispObject *args, Environment *env);
static LispObject *builtin_load(LispObject *args, Environment *env);

/* Common Lisp printing operations */
static LispObject *builtin_princ(LispObject *args, Environment *env);
static LispObject *builtin_prin1(LispObject *args, Environment *env);
static LispObject *builtin_print_cl(LispObject *args, Environment *env);
static LispObject *builtin_format(LispObject *args, Environment *env);
static LispObject *builtin_terpri(LispObject *args, Environment *env);

/* Type predicates */
static LispObject *builtin_integer_question(LispObject *args, Environment *env);
static LispObject *builtin_boolean_question(LispObject *args, Environment *env);
static LispObject *builtin_number_question(LispObject *args, Environment *env);
static LispObject *builtin_vector_question(LispObject *args, Environment *env);
static LispObject *builtin_hash_table_question(LispObject *args, Environment *env);
static LispObject *builtin_string_question(LispObject *args, Environment *env);
static LispObject *builtin_symbol_question(LispObject *args, Environment *env);
static LispObject *builtin_list_question(LispObject *args, Environment *env);

/* Symbol operations */
static LispObject *builtin_symbol_to_string(LispObject *args, Environment *env);

/* Vector operations */
static LispObject *builtin_make_vector(LispObject *args, Environment *env);
static LispObject *builtin_vector_ref(LispObject *args, Environment *env);
static LispObject *builtin_vector_set_bang(LispObject *args, Environment *env);
static LispObject *builtin_vector_length(LispObject *args, Environment *env);
static LispObject *builtin_vector_push_bang(LispObject *args, Environment *env);
static LispObject *builtin_vector_pop_bang(LispObject *args, Environment *env);

/* Alist and mapping operations */
static LispObject *builtin_assoc(LispObject *args, Environment *env);
static LispObject *builtin_assq(LispObject *args, Environment *env);
static LispObject *builtin_assv(LispObject *args, Environment *env);
static LispObject *builtin_alist_get(LispObject *args, Environment *env);
static LispObject *builtin_map(LispObject *args, Environment *env);
static LispObject *builtin_mapcar(LispObject *args, Environment *env);

/* Error introspection and handling */
static LispObject *builtin_error_question(LispObject *args, Environment *env);
static LispObject *builtin_error_type(LispObject *args, Environment *env);
static LispObject *builtin_error_message(LispObject *args, Environment *env);
static LispObject *builtin_error_stack(LispObject *args, Environment *env);
static LispObject *builtin_error_data(LispObject *args, Environment *env);
static LispObject *builtin_signal(LispObject *args, Environment *env);
static LispObject *builtin_error(LispObject *args, Environment *env);

/* Equality predicates */
static LispObject *builtin_eq_predicate(LispObject *args, Environment *env);
static LispObject *builtin_equal_predicate(LispObject *args, Environment *env);
static LispObject *builtin_string_eq_predicate(LispObject *args, Environment *env);

/* Path expansion functions */
static LispObject *builtin_home_directory(LispObject *args, Environment *env);
static LispObject *builtin_expand_path(LispObject *args, Environment *env);

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
    env_define(env, "number->string", lisp_make_builtin(builtin_number_to_string, "number->string"));
    env_define(env, "string->number", lisp_make_builtin(builtin_string_to_number, "string->number"));
    env_define(env, "split", lisp_make_builtin(builtin_split, "split"));
    env_define(env, "string<?", lisp_make_builtin(builtin_string_lt, "string<?"));
    env_define(env, "string>?", lisp_make_builtin(builtin_string_gt, "string>?"));
    env_define(env, "string<=?", lisp_make_builtin(builtin_string_lte, "string<=?"));
    env_define(env, "string>=?", lisp_make_builtin(builtin_string_gte, "string>=?"));
    env_define(env, "string-contains?", lisp_make_builtin(builtin_string_contains, "string-contains?"));
    env_define(env, "string-match?", lisp_make_builtin(builtin_string_match, "string-match?"));
    env_define(env, "string-length", lisp_make_builtin(builtin_string_length, "string-length"));
    env_define(env, "substring", lisp_make_builtin(builtin_substring, "substring"));
    env_define(env, "string-ref", lisp_make_builtin(builtin_string_ref, "string-ref"));
    env_define(env, "string-prefix?", lisp_make_builtin(builtin_string_prefix_question, "string-prefix?"));
    env_define(env, "string-replace", lisp_make_builtin(builtin_string_replace, "string-replace"));
    env_define(env, "string-upcase", lisp_make_builtin(builtin_string_upcase, "string-upcase"));
    env_define(env, "string-downcase", lisp_make_builtin(builtin_string_downcase, "string-downcase"));

    env_define(env, "not", lisp_make_builtin(builtin_not, "not"));

    env_define(env, "car", lisp_make_builtin(builtin_car, "car"));
    env_define(env, "cdr", lisp_make_builtin(builtin_cdr, "cdr"));
    env_define(env, "cons", lisp_make_builtin(builtin_cons, "cons"));
    env_define(env, "list", lisp_make_builtin(builtin_list, "list"));
    env_define(env, "list-length", lisp_make_builtin(builtin_list_length, "list-length"));
    env_define(env, "list-ref", lisp_make_builtin(builtin_list_ref, "list-ref"));
    env_define(env, "reverse", lisp_make_builtin(builtin_reverse, "reverse"));

    /* Alist operations */
    env_define(env, "assoc", lisp_make_builtin(builtin_assoc, "assoc"));
    env_define(env, "assq", lisp_make_builtin(builtin_assq, "assq"));
    env_define(env, "assv", lisp_make_builtin(builtin_assv, "assv"));
    env_define(env, "alist-get", lisp_make_builtin(builtin_alist_get, "alist-get"));

    /* Equality predicates */
    env_define(env, "eq?", lisp_make_builtin(builtin_eq_predicate, "eq?"));
    env_define(env, "equal?", lisp_make_builtin(builtin_equal_predicate, "equal?"));
    env_define(env, "string=?", lisp_make_builtin(builtin_string_eq_predicate, "string=?"));

    /* Mapping operations */
    env_define(env, "map", lisp_make_builtin(builtin_map, "map"));
    env_define(env, "mapcar", lisp_make_builtin(builtin_mapcar, "mapcar"));

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
    env_define(env, "read-sexp", lisp_make_builtin(builtin_read_sexp, "read-sexp"));
    env_define(env, "read-json", lisp_make_builtin(builtin_read_json, "read-json"));
    env_define(env, "load", lisp_make_builtin(builtin_load, "load"));

    /* Path expansion functions */
    env_define(env, "home-directory", lisp_make_builtin(builtin_home_directory, "home-directory"));
    env_define(env, "expand-path", lisp_make_builtin(builtin_expand_path, "expand-path"));

    /* Common Lisp printing functions */
    env_define(env, "princ", lisp_make_builtin(builtin_princ, "princ"));
    env_define(env, "prin1", lisp_make_builtin(builtin_prin1, "prin1"));
    env_define(env, "print", lisp_make_builtin(builtin_print_cl, "print"));
    env_define(env, "format", lisp_make_builtin(builtin_format, "format"));
    env_define(env, "terpri", lisp_make_builtin(builtin_terpri, "terpri"));

    /* Type predicates */
    env_define(env, "integer?", lisp_make_builtin(builtin_integer_question, "integer?"));
    env_define(env, "boolean?", lisp_make_builtin(builtin_boolean_question, "boolean?"));
    env_define(env, "number?", lisp_make_builtin(builtin_number_question, "number?"));
    env_define(env, "vector?", lisp_make_builtin(builtin_vector_question, "vector?"));
    env_define(env, "hash-table?", lisp_make_builtin(builtin_hash_table_question, "hash-table?"));
    env_define(env, "string?", lisp_make_builtin(builtin_string_question, "string?"));
    env_define(env, "symbol?", lisp_make_builtin(builtin_symbol_question, "symbol?"));
    env_define(env, "list?", lisp_make_builtin(builtin_list_question, "list?"));

    /* Symbol operations */
    env_define(env, "symbol->string", lisp_make_builtin(builtin_symbol_to_string, "symbol->string"));

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

    /* Error introspection and handling */
    env_define(env, "error?", lisp_make_builtin(builtin_error_question, "error?"));
    env_define(env, "error-type", lisp_make_builtin(builtin_error_type, "error-type"));
    env_define(env, "error-message", lisp_make_builtin(builtin_error_message, "error-message"));
    env_define(env, "error-stack", lisp_make_builtin(builtin_error_stack, "error-stack"));
    env_define(env, "error-data", lisp_make_builtin(builtin_error_data, "error-data"));
    env_define(env, "signal", lisp_make_builtin(builtin_signal, "signal"));
    env_define(env, "error", lisp_make_builtin(builtin_error, "error"));
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

    if (first->type != LISP_INTEGER && first->type != LISP_NUMBER) {
        return lisp_make_error("quotient requires numbers");
    }
    if (second->type != LISP_INTEGER && second->type != LISP_NUMBER) {
        return lisp_make_error("quotient requires numbers");
    }

    int first_is_integer;
    int second_is_integer;
    double first_val = get_numeric_value(first, &first_is_integer);
    double second_val = get_numeric_value(second, &second_is_integer);

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

    if (first->type != LISP_INTEGER && first->type != LISP_NUMBER) {
        return lisp_make_error("remainder requires numbers");
    }
    if (second->type != LISP_INTEGER && second->type != LISP_NUMBER) {
        return lisp_make_error("remainder requires numbers");
    }

    int first_is_integer;
    int second_is_integer;
    double first_val = get_numeric_value(first, &first_is_integer);
    double second_val = get_numeric_value(second, &second_is_integer);

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

static LispObject *builtin_number_to_string(LispObject *args, Environment *env) {
    (void)env;
    if (args == NULL || args == NIL) {
        return lisp_make_error("number->string: expected at least 1 argument");
    }

    /* Parse arguments: number (required), radix (optional) */
    LispObject *num = lisp_car(args);
    LispObject *radix_obj = (lisp_cdr(args) != NIL) ? lisp_car(lisp_cdr(args)) : NIL;

    if (num == NULL || num == NIL) {
        return lisp_make_error("number->string: first argument cannot be nil");
    }

    /* Validate number argument */
    if (num->type != LISP_INTEGER && num->type != LISP_NUMBER) {
        return lisp_make_error("number->string: first argument must be a number");
    }

    int radix = 10; /* default base */

    /* Parse optional radix */
    if (radix_obj != NIL) {
        if (radix_obj->type != LISP_INTEGER) {
            return lisp_make_error("number->string: radix must be an integer");
        }
        radix = (int)radix_obj->value.integer;
        if (radix < 2 || radix > 36) {
            return lisp_make_error("number->string: radix must be between 2 and 36");
        }
    }

    /* Float formatting (only base 10) */
    if (num->type == LISP_NUMBER) {
        if (radix != 10) {
            return lisp_make_error("number->string: floats only supported in base 10");
        }
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "%.15g", num->value.number);
        return lisp_make_string(buffer);
    }

    /* Integer formatting with arbitrary radix */
    long long value = num->value.integer;

    /* Special case: zero */
    if (value == 0) {
        return lisp_make_string("0");
    }

    char buffer[128];
    int pos = 0;
    int negative = (value < 0);

    /* Handle negative numbers */
    if (negative) {
        value = -value;
    }

    /* Convert to given radix */
    const char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
    char temp[128];
    int temp_pos = 0;

    while (value > 0) {
        temp[temp_pos++] = digits[value % radix];
        value /= radix;
    }

    /* Add sign */
    if (negative) {
        buffer[pos++] = '-';
    }

    /* Reverse digits */
    while (temp_pos > 0) {
        buffer[pos++] = temp[--temp_pos];
    }

    buffer[pos] = '\0';
    return lisp_make_string(buffer);
}

static LispObject *builtin_string_to_number(LispObject *args, Environment *env) {
    (void)env;

    if (args == NULL || args == NIL) {
        return lisp_make_error("string->number: expected at least 1 argument");
    }

    /* Parse arguments: string (required), radix (optional) */
    LispObject *str_obj = lisp_car(args);
    LispObject *radix_obj = (lisp_cdr(args) != NIL) ? lisp_car(lisp_cdr(args)) : NIL;

    /* Validate string argument */
    if (str_obj == NULL || str_obj == NIL) {
        return lisp_make_error("string->number: first argument cannot be nil");
    }
    if (str_obj->type != LISP_STRING) {
        return lisp_make_error("string->number: first argument must be a string");
    }

    const char *str = str_obj->value.string;
    int radix = 10; /* default base */

    /* Parse optional radix */
    if (radix_obj != NIL) {
        if (radix_obj->type != LISP_INTEGER) {
            return lisp_make_error("string->number: radix must be an integer");
        }
        radix = (int)radix_obj->value.integer;
        if (radix < 2 || radix > 36) {
            return lisp_make_error("string->number: radix must be between 2 and 36");
        }
    }

    /* Skip leading whitespace */
    while (*str && isspace((unsigned char)*str)) {
        str++;
    }

    /* Empty string returns #f */
    if (*str == '\0') {
        return NIL;
    }

    /* Handle radix prefix (only if radix not explicitly specified) */
    if (radix == 10 && str[0] == '#' && str[1]) {
        char prefix = str[1];
        if (prefix == 'b' || prefix == 'B') {
            radix = 2;
            str += 2;
        } else if (prefix == 'o' || prefix == 'O') {
            radix = 8;
            str += 2;
        } else if (prefix == 'd' || prefix == 'D') {
            radix = 10;
            str += 2;
        } else if (prefix == 'x' || prefix == 'X') {
            radix = 16;
            str += 2;
        }
    }

    /* Try parsing as float (only for base 10) */
    if (radix == 10 && (strchr(str, '.') != NULL || strchr(str, 'e') != NULL || strchr(str, 'E') != NULL)) {
        char *endptr;
        errno = 0;
        double value = strtod(str, &endptr);

        /* Skip trailing whitespace */
        while (*endptr && isspace((unsigned char)*endptr)) {
            endptr++;
        }

        /* Return float if parse succeeded, else #f */
        if (*endptr == '\0' && errno != ERANGE) {
            return lisp_make_number(value);
        }
        return NIL;
    }

    /* Try parsing as integer */
    char *endptr;
    errno = 0;
    long long value = strtoll(str, &endptr, radix);

    /* Skip trailing whitespace */
    while (*endptr && isspace((unsigned char)*endptr)) {
        endptr++;
    }

    /* Return integer if parse succeeded, else #f */
    if (*endptr != '\0' || errno == ERANGE) {
        return NIL;
    }

    return lisp_make_integer(value);
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

static LispObject *builtin_string_lt(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string<? requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string<? requires strings");
    }

    return (strcmp(a->value.string, b->value.string) < 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_gt(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string>? requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string>? requires strings");
    }

    return (strcmp(a->value.string, b->value.string) > 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_lte(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string<=? requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string<=? requires strings");
    }

    return (strcmp(a->value.string, b->value.string) <= 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_gte(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string>=? requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string>=? requires strings");
    }

    return (strcmp(a->value.string, b->value.string) >= 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_contains(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-contains? requires 2 arguments");
    }

    LispObject *haystack = lisp_car(args);
    LispObject *needle = lisp_car(lisp_cdr(args));

    if (haystack->type != LISP_STRING || needle->type != LISP_STRING) {
        return lisp_make_error("string-contains? requires strings");
    }

    return (strstr(haystack->value.string, needle->value.string) != NULL) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_match(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-match? requires 2 arguments");
    }

    LispObject *str = lisp_car(args);
    LispObject *pattern = lisp_car(lisp_cdr(args));

    if (str->type != LISP_STRING || pattern->type != LISP_STRING) {
        return lisp_make_error("string-match? requires strings");
    }

    return wildcard_match(pattern->value.string, str->value.string) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_prefix_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-prefix? requires 2 arguments");
    }

    LispObject *prefix = lisp_car(args);
    LispObject *str = lisp_car(lisp_cdr(args));

    if (prefix->type != LISP_STRING || str->type != LISP_STRING) {
        return lisp_make_error("string-prefix? requires strings");
    }

    size_t prefix_len = strlen(prefix->value.string);
    size_t str_len = strlen(str->value.string);

    /* If prefix is longer than string, it can't be a prefix */
    if (prefix_len > str_len) {
        return NIL;
    }

    /* Use strncmp to check if prefix matches the beginning of str */
    return (strncmp(prefix->value.string, str->value.string, prefix_len) == 0) ? lisp_make_number(1) : NIL;
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

/* String replace */
static LispObject *builtin_string_replace(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL || lisp_cdr(lisp_cdr(args)) == NIL) {
        return lisp_make_error("string-replace requires 3 arguments");
    }

    LispObject *old_obj = lisp_car(args);
    LispObject *new_obj = lisp_car(lisp_cdr(args));
    LispObject *str_obj = lisp_car(lisp_cdr(lisp_cdr(args)));

    if (old_obj->type != LISP_STRING || new_obj->type != LISP_STRING || str_obj->type != LISP_STRING) {
        return lisp_make_error("string-replace requires strings");
    }

    const char *old_str = old_obj->value.string;
    const char *new_str = new_obj->value.string;
    const char *str = str_obj->value.string;

    /* If old string is empty, return original string */
    if (old_str[0] == '\0') {
        return lisp_make_string(GC_strdup(str));
    }

    /* Count occurrences to estimate result size */
    size_t old_len = strlen(old_str);
    size_t new_len = strlen(new_str);
    size_t str_len = strlen(str);
    size_t count = 0;
    const char *pos = str;
    while ((pos = strstr(pos, old_str)) != NULL) {
        count++;
        pos += old_len;
    }

    /* Calculate result size */
    size_t result_len = str_len + (new_len - old_len) * count;
    char *result = GC_malloc(result_len + 1);
    char *result_ptr = result;
    const char *str_ptr = str;

    /* Replace all occurrences */
    while ((pos = strstr(str_ptr, old_str)) != NULL) {
        /* Copy part before match */
        size_t before_len = pos - str_ptr;
        memcpy(result_ptr, str_ptr, before_len);
        result_ptr += before_len;

        /* Copy new string */
        memcpy(result_ptr, new_str, new_len);
        result_ptr += new_len;

        /* Advance past old string */
        str_ptr = pos + old_len;
    }

    /* Copy remaining part */
    size_t remaining = strlen(str_ptr);
    memcpy(result_ptr, str_ptr, remaining);
    result_ptr += remaining;
    *result_ptr = '\0';

    return lisp_make_string(result);
}

/* String upcase */
static LispObject *builtin_string_upcase(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("string-upcase requires 1 argument");
    }

    LispObject *str_obj = lisp_car(args);
    if (str_obj->type != LISP_STRING) {
        return lisp_make_error("string-upcase requires a string");
    }

    const char *str = str_obj->value.string;
    size_t len = strlen(str);
    char *result = GC_malloc(len + 1);
    const char *src = str;
    char *dst = result;

    /* Convert each character */
    while (*src) {
        int codepoint = utf8_get_codepoint(src);
        if (codepoint >= 0 && codepoint < 128) {
            /* ASCII - use toupper */
            *dst = toupper((unsigned char)*src);
            src++;
            dst++;
        } else {
            /* Unicode - preserve as-is (full Unicode case conversion requires tables) */
            int bytes = utf8_char_bytes(src);
            memcpy(dst, src, bytes);
            src += bytes;
            dst += bytes;
        }
    }
    *dst = '\0';

    return lisp_make_string(result);
}

/* String downcase */
static LispObject *builtin_string_downcase(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("string-downcase requires 1 argument");
    }

    LispObject *str_obj = lisp_car(args);
    if (str_obj->type != LISP_STRING) {
        return lisp_make_error("string-downcase requires a string");
    }

    const char *str = str_obj->value.string;
    size_t len = strlen(str);
    char *result = GC_malloc(len + 1);
    const char *src = str;
    char *dst = result;

    /* Convert each character */
    while (*src) {
        int codepoint = utf8_get_codepoint(src);
        if (codepoint >= 0 && codepoint < 128) {
            /* ASCII - use tolower */
            *dst = tolower((unsigned char)*src);
            src++;
            dst++;
        } else {
            /* Unicode - preserve as-is (full Unicode case conversion requires tables) */
            int bytes = utf8_char_bytes(src);
            memcpy(dst, src, bytes);
            src += bytes;
            dst += bytes;
        }
    }
    *dst = '\0';

    return lisp_make_string(result);
}

/* Boolean operations */
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

static LispObject *builtin_reverse(LispObject *args, Environment *env) {
    (void)env;

    if (args == NULL || args == NIL) {
        return lisp_make_error("reverse: expected 1 argument");
    }

    LispObject *lst = lisp_car(args);

    /* Handle empty list */
    if (lst == NIL || lst == NULL) {
        return NIL;
    }

    /* Validate it's a list */
    if (lst->type != LISP_CONS) {
        return lisp_make_error("reverse: argument must be a list");
    }

    /* Build reversed list iteratively */
    LispObject *result = NIL;
    while (lst != NIL && lst != NULL && lst->type == LISP_CONS) {
        result = lisp_make_cons(lisp_car(lst), result);
        lst = lisp_cdr(lst);
    }

    return result;
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

/* Read S-expressions from file */
static LispObject *builtin_read_sexp(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("read-sexp requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    FILE *file = NULL;
    int should_close = 0;

    /* Check if argument is file stream or filename */
    if (arg->type == LISP_FILE_STREAM) {
        file = arg->value.file;
        if (file == NULL) {
            return lisp_make_error("read-sexp: file is closed");
        }
    } else if (arg->type == LISP_STRING) {
        /* Open file */
        file = fopen(arg->value.string, "r");
        if (file == NULL) {
            char error[512];
            snprintf(error, sizeof(error), "read-sexp: cannot open file '%s'", arg->value.string);
            return lisp_make_error(error);
        }
        should_close = 1;
    } else {
        return lisp_make_error("read-sexp requires a filename (string) or file stream");
    }

    /* Read entire file */
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (size < 0) {
        if (should_close) {
            fclose(file);
        }
        return lisp_make_error("read-sexp: cannot determine file size");
    }

    char *buffer = GC_malloc(size + 1);
    size_t read_size = fread(buffer, 1, size, file);
    buffer[read_size] = '\0';

    if (should_close) {
        fclose(file);
    }

    /* Parse all expressions */
    const char *input = buffer;
    LispObject *result = NIL;
    LispObject *last = NIL;

    while (*input) {
        /* Skip whitespace and comments */
        while (*input == ' ' || *input == '\t' || *input == '\n' || *input == '\r' || *input == ';') {
            if (*input == ';') {
                while (*input && *input != '\n') {
                    input++;
                }
            } else {
                input++;
            }
        }

        if (*input == '\0') {
            break;
        }

        /* Parse expression */
        LispObject *expr = lisp_read(&input);
        if (expr == NULL) {
            break;
        }

        if (expr->type == LISP_ERROR) {
            return expr;
        }

        /* Add to result list */
        LispObject *cell = lisp_make_cons(expr, NIL);
        if (result == NIL) {
            result = cell;
            last = cell;
        } else {
            last->value.cons.cdr = cell;
            last = cell;
        }
    }

    /* Return single expression if only one, otherwise return list */
    if (result != NIL && lisp_cdr(result) == NIL) {
        return lisp_car(result);
    }

    return result;
}

/* Read JSON from file - basic JSON parser */
static LispObject *builtin_read_json(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("read-json requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    FILE *file = NULL;
    int should_close = 0;

    /* Check if argument is file stream or filename */
    if (arg->type == LISP_FILE_STREAM) {
        file = arg->value.file;
        if (file == NULL) {
            return lisp_make_error("read-json: file is closed");
        }
    } else if (arg->type == LISP_STRING) {
        /* Open file */
        file = fopen(arg->value.string, "r");
        if (file == NULL) {
            char error[512];
            snprintf(error, sizeof(error), "read-json: cannot open file '%s'", arg->value.string);
            return lisp_make_error(error);
        }
        should_close = 1;
    } else {
        return lisp_make_error("read-json requires a filename (string) or file stream");
    }

    /* Read entire file */
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (size < 0) {
        if (should_close) {
            fclose(file);
        }
        return lisp_make_error("read-json: cannot determine file size");
    }

    char *buffer = GC_malloc(size + 1);
    size_t read_size = fread(buffer, 1, size, file);
    buffer[read_size] = '\0';

    if (should_close) {
        fclose(file);
    }

    /* Simple JSON parser - this is a basic implementation */
    /* Skip whitespace */
    const char *p = buffer;
    while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) {
        p++;
    }

    if (*p == '\0') {
        return NIL;
    }

    /* Parse JSON value */
    LispObject *result = NULL;
    if (*p == '{') {
        /* JSON object -> hash table */
        result = lisp_make_hash_table();
        p++; /* Skip '{' */
        while (*p && *p != '}') {
            /* Skip whitespace */
            while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) {
                p++;
            }
            if (*p == '}') {
                break;
            }

            /* Parse key (must be string) */
            if (*p != '"') {
                return lisp_make_error("read-json: object key must be a string");
            }
            p++; /* Skip '"' */
            const char *key_start = p;
            while (*p && *p != '"') {
                if (*p == '\\' && *(p + 1)) {
                    p += 2; /* Skip escape sequence */
                } else {
                    p++;
                }
            }
            if (*p != '"') {
                return lisp_make_error("read-json: unterminated string");
            }
            size_t key_len = p - key_start;
            char *key = GC_malloc(key_len + 1);
            memcpy(key, key_start, key_len);
            key[key_len] = '\0';
            p++; /* Skip '"' */

            /* Skip whitespace */
            while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) {
                p++;
            }
            if (*p != ':') {
                return lisp_make_error("read-json: expected ':' after key");
            }
            p++; /* Skip ':' */

            /* Skip whitespace */
            while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) {
                p++;
            }

            /* Parse value (recursive) */
            LispObject *json_value = NULL;
            if (*p == '"') {
                /* String */
                p++;
                const char *val_start = p;
                while (*p && *p != '"') {
                    if (*p == '\\' && *(p + 1)) {
                        p += 2;
                    } else {
                        p++;
                    }
                }
                if (*p != '"') {
                    return lisp_make_error("read-json: unterminated string");
                }
                size_t val_len = p - val_start;
                char *val_str = GC_malloc(val_len + 1);
                memcpy(val_str, val_start, val_len);
                val_str[val_len] = '\0';
                json_value = lisp_make_string(val_str);
                p++; /* Skip '"' */
            } else if (*p == '{') {
                /* Nested object - simplified: return error for now */
                return lisp_make_error("read-json: nested objects not yet supported");
            } else if (*p == '[') {
                /* Array - simplified: return error for now */
                return lisp_make_error("read-json: arrays not yet supported");
            } else if (strncmp(p, "true", 4) == 0) {
                json_value = lisp_make_boolean(1);
                p += 4;
            } else if (strncmp(p, "false", 5) == 0) {
                json_value = NIL;
                p += 5;
            } else if (strncmp(p, "null", 4) == 0) {
                json_value = NIL;
                p += 4;
            } else if (isdigit(*p) || *p == '-') {
                /* Number - simplified parsing */
                const char *num_start = p;
                while (*p && (isdigit(*p) || *p == '.' || *p == '-' || *p == '+' || *p == 'e' || *p == 'E')) {
                    p++;
                }
                size_t num_len = p - num_start;
                char *num_str = GC_malloc(num_len + 1);
                memcpy(num_str, num_start, num_len);
                num_str[num_len] = '\0';
                if (strchr(num_str, '.') != NULL) {
                    json_value = lisp_make_number(atof(num_str));
                } else {
                    json_value = lisp_make_integer(atoll(num_str));
                }
            } else {
                return lisp_make_error("read-json: unexpected character");
            }

            /* Store in hash table */
            hash_table_set_entry(result, key, json_value);

            /* Skip whitespace */
            while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) {
                p++;
            }
            if (*p == ',') {
                p++; /* Skip ',' */
            } else if (*p != '}') {
                return lisp_make_error("read-json: expected ',' or '}'");
            }
        }
        if (*p == '}') {
            p++;
        }
    } else if (*p == '"') {
        /* JSON string */
        p++;
        const char *str_start = p;
        while (*p && *p != '"') {
            if (*p == '\\' && *(p + 1)) {
                p += 2;
            } else {
                p++;
            }
        }
        if (*p != '"') {
            return lisp_make_error("read-json: unterminated string");
        }
        size_t str_len = p - str_start;
        char *str = GC_malloc(str_len + 1);
        memcpy(str, str_start, str_len);
        str[str_len] = '\0';
        result = lisp_make_string(str);
        p++;
    } else if (strncmp(p, "true", 4) == 0) {
        result = lisp_make_boolean(1);
    } else if (strncmp(p, "false", 5) == 0) {
        result = NIL;
    } else if (strncmp(p, "null", 4) == 0) {
        result = NIL;
    } else if (isdigit(*p) || *p == '-') {
        /* Number */
        const char *num_start = p;
        while (*p && (isdigit(*p) || *p == '.' || *p == '-' || *p == '+' || *p == 'e' || *p == 'E')) {
            p++;
        }
        size_t num_len = p - num_start;
        char *num_str = GC_malloc(num_len + 1);
        memcpy(num_str, num_start, num_len);
        num_str[num_len] = '\0';
        if (strchr(num_str, '.') != NULL) {
            result = lisp_make_number(atof(num_str));
        } else {
            result = lisp_make_integer(atoll(num_str));
        }
    } else {
        return lisp_make_error("read-json: unsupported JSON value type");
    }

    return result;
}

/* Load and evaluate a Lisp file */
static LispObject *builtin_load(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("load requires 1 argument");
    }

    LispObject *filename_obj = lisp_car(args);
    if (filename_obj->type != LISP_STRING) {
        return lisp_make_error("load requires a string filename");
    }

    LispObject *result = lisp_load_file(filename_obj->value.string, env);

    /* Return the result of the last expression evaluated, or nil if error */
    if (result && result->type == LISP_ERROR) {
        return result;
    }

    /* Return the last evaluated expression, or nil if file was empty */
    return result ? result : NIL;
}

static LispObject *builtin_princ(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("princ requires 1 argument");
    }

    LispObject *obj = lisp_car(args);
    lisp_princ(obj);

    /* Return the object (Common Lisp convention) */
    return obj;
}

static LispObject *builtin_prin1(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("prin1 requires 1 argument");
    }

    LispObject *obj = lisp_car(args);
    lisp_prin1(obj);

    /* Return the object (Common Lisp convention) */
    return obj;
}

static LispObject *builtin_print_cl(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("print requires 1 argument");
    }

    LispObject *obj = lisp_car(args);
    lisp_print_cl(obj);

    /* Return the object (Common Lisp convention) */
    return obj;
}

static LispObject *builtin_format(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("format requires at least 2 arguments");
    }

    LispObject *dest = lisp_car(args);
    LispObject *format_str_obj = lisp_car(lisp_cdr(args));

    if (format_str_obj->type != LISP_STRING) {
        return lisp_make_error("format requires a string as format argument");
    }

    const char *format_str = format_str_obj->value.string;
    LispObject *format_args = lisp_cdr(lisp_cdr(args));

    /* Build output string */
    char *output = GC_malloc(4096);
    size_t output_len = 0;
    size_t output_capacity = 4096;

    LispObject *current_arg = format_args;

    for (const char *p = format_str; *p; p++) {
        if (*p == '~' && *(p + 1)) {
            p++;
            char directive = *p;

            if (directive == '%') {
                /* Newline */
                if (output_len + 1 >= output_capacity) {
                    output_capacity *= 2;
                    char *new_output = GC_malloc(output_capacity);
                    memcpy(new_output, output, output_len);
                    output = new_output;
                }
                output[output_len++] = '\n';
            } else if (directive == '~') {
                /* Literal tilde */
                if (output_len + 1 >= output_capacity) {
                    output_capacity *= 2;
                    char *new_output = GC_malloc(output_capacity);
                    memcpy(new_output, output, output_len);
                    output = new_output;
                }
                output[output_len++] = '~';
            } else if (directive == 'A' || directive == 'a') {
                /* Aesthetic - princ style (no quotes) */
                if (current_arg == NIL) {
                    return lisp_make_error("format: not enough arguments for format directives");
                }
                LispObject *arg = lisp_car(current_arg);
                char *arg_str = lisp_print(arg);
                /* Remove quotes from strings for aesthetic output */
                if (arg->type == LISP_STRING) {
                    arg_str = arg->value.string;
                }
                size_t arg_len = strlen(arg_str);
                if (output_len + arg_len >= output_capacity) {
                    while (output_len + arg_len >= output_capacity) {
                        output_capacity *= 2;
                    }
                    char *new_output = GC_malloc(output_capacity);
                    memcpy(new_output, output, output_len);
                    output = new_output;
                }
                memcpy(output + output_len, arg_str, arg_len);
                output_len += arg_len;
                current_arg = lisp_cdr(current_arg);
            } else if (directive == 'S' || directive == 's') {
                /* S-expression - prin1 style (with quotes) */
                if (current_arg == NIL) {
                    return lisp_make_error("format: not enough arguments for format directives");
                }
                LispObject *arg = lisp_car(current_arg);
                char *arg_str = lisp_print(arg);
                size_t arg_len = strlen(arg_str);
                if (output_len + arg_len >= output_capacity) {
                    while (output_len + arg_len >= output_capacity) {
                        output_capacity *= 2;
                    }
                    char *new_output = GC_malloc(output_capacity);
                    memcpy(new_output, output, output_len);
                    output = new_output;
                }
                memcpy(output + output_len, arg_str, arg_len);
                output_len += arg_len;
                current_arg = lisp_cdr(current_arg);
            } else {
                /* Unknown directive - just output it */
                if (output_len + 2 >= output_capacity) {
                    output_capacity *= 2;
                    char *new_output = GC_malloc(output_capacity);
                    memcpy(new_output, output, output_len);
                    output = new_output;
                }
                output[output_len++] = '~';
                output[output_len++] = directive;
            }
        } else {
            /* Regular character */
            if (output_len + 1 >= output_capacity) {
                output_capacity *= 2;
                char *new_output = GC_malloc(output_capacity);
                memcpy(new_output, output, output_len);
                output = new_output;
            }
            output[output_len++] = *p;
        }
    }

    output[output_len] = '\0';

    /* Handle destination */
    if (dest == NIL) {
        /* Return as string */
        return lisp_make_string(output);
    } else if (dest->type == LISP_BOOLEAN && dest->value.boolean) {
        /* Output to stdout */
        printf("%s", output);
        fflush(stdout);
        return NIL;
    } else {
        return lisp_make_error("format: invalid destination (use nil for string or #t for stdout)");
    }
}

static LispObject *builtin_terpri(LispObject *args, Environment *env) {
    (void)env;
    (void)args;
    printf("\n");
    fflush(stdout);
    return NIL;
}

/* Path expansion functions */

/* Get user's home directory path (cross-platform)
 * Unix/Linux/macOS: $HOME
 * Windows: %USERPROFILE% or %HOMEDRIVE%%HOMEPATH%
 * Returns: String with home directory or NIL if not found
 */
static LispObject *builtin_home_directory(LispObject *args, Environment *env) {
    (void)args; /* Takes no arguments */
    (void)env;

    const char *home = NULL;

#if defined(_WIN32) || defined(_WIN64)
    /* Windows: Try USERPROFILE first */
    home = getenv("USERPROFILE");

    /* Fallback: HOMEDRIVE + HOMEPATH */
    if (home == NULL) {
        const char *homedrive = getenv("HOMEDRIVE");
        const char *homepath = getenv("HOMEPATH");

        if (homedrive != NULL && homepath != NULL) {
            size_t len = strlen(homedrive) + strlen(homepath) + 1;
            char *combined = GC_malloc(len);
            snprintf(combined, len, "%s%s", homedrive, homepath);
            home = combined;
        }
    }
#else
    /* Unix/Linux/macOS: Use HOME */
    home = getenv("HOME");
#endif

    if (home == NULL) {
        return NIL; /* No home directory found */
    }

    return lisp_make_string(home);
}

/* Expand ~/ in file paths to home directory (cross-platform)
 * Takes: String (file path)
 * Returns: String (expanded path) or original if no ~/ prefix
 * Example: (expand-path "~/config.lisp") => "/home/user/config.lisp"
 */
static LispObject *builtin_expand_path(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("expand-path requires 1 argument");
    }

    LispObject *path_obj = lisp_car(args);
    if (path_obj->type != LISP_STRING) {
        return lisp_make_error("expand-path requires a string argument");
    }

    const char *path = path_obj->value.string;

    /* Check if path starts with ~/ */
    if (path[0] != '~' || (path[1] != '/' && path[1] != '\\' && path[1] != '\0')) {
        /* Not a ~/ path - return original */
        return path_obj;
    }

    /* Get home directory */
    LispObject *home_obj = builtin_home_directory(NIL, env);
    if (home_obj == NIL || home_obj->type != LISP_STRING) {
        /* No home directory - return error */
        return lisp_make_error("expand-path: cannot determine home directory");
    }

    const char *home = home_obj->value.string;

    /* Calculate expanded path length */
    /* If path is just "~", use home directory directly */
    if (path[1] == '\0') {
        return home_obj;
    }

    /* Skip ~/ or ~\ */
    const char *rest = path + 2;

    /* Build expanded path: home + / + rest */
    size_t home_len = strlen(home);
    size_t rest_len = strlen(rest);
    size_t total_len = home_len + 1 + rest_len + 1;

    char *expanded = GC_malloc(total_len);

#if defined(_WIN32) || defined(_WIN64)
    /* Windows: Use backslash separator */
    snprintf(expanded, total_len, "%s\\%s", home, rest);
#else
    /* Unix: Use forward slash separator */
    snprintf(expanded, total_len, "%s/%s", home, rest);
#endif

    return lisp_make_string(expanded);
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

static LispObject *builtin_string_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("string? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_STRING) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_symbol_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("symbol? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_SYMBOL) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_list_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("list? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    /* A list is either NIL or a cons cell */
    return (arg == NIL || arg->type == LISP_CONS) ? lisp_make_number(1) : NIL;
}

/* Symbol operations */

static LispObject *builtin_symbol_to_string(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("symbol->string requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    if (arg->type != LISP_SYMBOL) {
        return lisp_make_error("symbol->string requires a symbol argument");
    }
    return lisp_make_string(arg->value.symbol);
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

    LispObject *vec = lisp_make_vector(size);

    /* If initial value is provided, set all elements to it */
    if (lisp_cdr(args) != NIL) {
        LispObject *init_val = lisp_car(lisp_cdr(args));
        for (size_t i = 0; i < size; i++) {
            vec->value.vector.items[i] = init_val;
        }
        /* Set the size to match the capacity when initializing */
        vec->value.vector.size = size;
    }

    return vec;
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

/* Alist operations */

/* Helper function to check deep structural equality (used by assoc, equal?, etc.) */
static int objects_equal_recursive(LispObject *a, LispObject *b) {
    /* Fast path: pointer equality */
    if (a == b)
        return 1;

    /* NIL checks */
    if (a == NIL || b == NIL)
        return 0;

    /* Type mismatch */
    if (a->type != b->type)
        return 0;

    switch (a->type) {
    case LISP_NUMBER:
        return a->value.number == b->value.number;

    case LISP_INTEGER:
        return a->value.integer == b->value.integer;

    case LISP_STRING:
        return strcmp(a->value.string, b->value.string) == 0;

    case LISP_SYMBOL:
        return strcmp(a->value.symbol, b->value.symbol) == 0;

    case LISP_BOOLEAN:
        return a->value.boolean == b->value.boolean;

    case LISP_CONS:
        /* Recursive list comparison */
        return objects_equal_recursive(a->value.cons.car, b->value.cons.car) &&
               objects_equal_recursive(a->value.cons.cdr, b->value.cons.cdr);

    case LISP_VECTOR:
        /* Compare vector lengths */
        if (a->value.vector.size != b->value.vector.size) {
            return 0;
        }
        /* Compare each element */
        for (size_t i = 0; i < a->value.vector.size; i++) {
            if (!objects_equal_recursive(a->value.vector.items[i], b->value.vector.items[i])) {
                return 0;
            }
        }
        return 1;

    case LISP_HASH_TABLE:
        /* Compare hash table sizes */
        if (a->value.hash_table.entry_count != b->value.hash_table.entry_count) {
            return 0;
        }
        /* Compare each key-value pair */
        /* Iterate through all buckets in hash table a */
        struct HashEntry **a_buckets = (struct HashEntry **)a->value.hash_table.buckets;
        for (size_t i = 0; i < a->value.hash_table.bucket_count; i++) {
            struct HashEntry *entry = a_buckets[i];
            while (entry != NULL) {
                /* Look up the key in hash table b */
                struct HashEntry *b_entry = hash_table_get_entry(b, entry->key);
                if (b_entry == NULL) {
                    return 0; /* Key doesn't exist in b */
                }
                /* Compare values */
                if (!objects_equal_recursive(entry->value, b_entry->value)) {
                    return 0;
                }
                entry = entry->next;
            }
        }
        return 1;

    default:
        /* For other types (lambdas, builtins, etc.), use pointer equality */
        return 0;
    }
}

static LispObject *builtin_assoc(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("assoc requires 2 arguments");
    }

    LispObject *key = lisp_car(args);
    LispObject *alist = lisp_car(lisp_cdr(args));

    /* Iterate through association list */
    while (alist != NIL && alist != NULL) {
        if (alist->type != LISP_CONS) {
            return lisp_make_error("assoc requires an association list");
        }

        LispObject *pair = lisp_car(alist);
        if (pair != NIL && pair->type == LISP_CONS) {
            LispObject *pair_key = lisp_car(pair);
            if (objects_equal_recursive(key, pair_key)) {
                return pair;
            }
        }

        alist = lisp_cdr(alist);
    }

    return NIL;
}

static LispObject *builtin_assq(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("assq requires 2 arguments");
    }

    LispObject *key = lisp_car(args);
    LispObject *alist = lisp_car(lisp_cdr(args));

    /* Iterate through association list using pointer equality (eq) */
    while (alist != NIL && alist != NULL) {
        if (alist->type != LISP_CONS) {
            return lisp_make_error("assq requires an association list");
        }

        LispObject *pair = lisp_car(alist);
        if (pair != NIL && pair->type == LISP_CONS) {
            LispObject *pair_key = lisp_car(pair);
            if (key == pair_key) {
                return pair;
            }
        }

        alist = lisp_cdr(alist);
    }

    return NIL;
}

static LispObject *builtin_assv(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("assv requires 2 arguments");
    }

    LispObject *key = lisp_car(args);
    LispObject *alist = lisp_car(lisp_cdr(args));

    /* Iterate through association list using eqv equality (same as assoc for our purposes) */
    while (alist != NIL && alist != NULL) {
        if (alist->type != LISP_CONS) {
            return lisp_make_error("assv requires an association list");
        }

        LispObject *pair = lisp_car(alist);
        if (pair != NIL && pair->type == LISP_CONS) {
            LispObject *pair_key = lisp_car(pair);
            if (objects_equal_recursive(key, pair_key)) {
                return pair;
            }
        }

        alist = lisp_cdr(alist);
    }

    return NIL;
}

static LispObject *builtin_alist_get(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("alist-get requires at least 2 arguments");
    }

    LispObject *key = lisp_car(args);
    LispObject *alist = lisp_car(lisp_cdr(args));
    LispObject *default_val = NIL;

    /* Optional third argument is default value */
    if (lisp_cdr(lisp_cdr(args)) != NIL) {
        default_val = lisp_car(lisp_cdr(lisp_cdr(args)));
    }

    /* Iterate through association list */
    while (alist != NIL && alist != NULL) {
        if (alist->type != LISP_CONS) {
            return lisp_make_error("alist-get requires an association list");
        }

        LispObject *pair = lisp_car(alist);
        if (pair != NIL && pair->type == LISP_CONS) {
            LispObject *pair_key = lisp_car(pair);
            if (objects_equal_recursive(key, pair_key)) {
                return lisp_cdr(pair);
            }
        }

        alist = lisp_cdr(alist);
    }

    return default_val;
}

/* Equality predicates */

static LispObject *builtin_eq_predicate(LispObject *args, Environment *env) {
    (void)env;
    /* Validate exactly 2 arguments */
    if (args == NIL || args->value.cons.cdr == NIL || args->value.cons.cdr->value.cons.cdr != NIL) {
        return lisp_make_error("eq? expects exactly 2 arguments");
    }

    LispObject *a = args->value.cons.car;
    LispObject *b = args->value.cons.cdr->value.cons.car;

    /* Pointer equality - same object in memory */
    return (a == b) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_equal_predicate(LispObject *args, Environment *env) {
    (void)env;
    /* Validate exactly 2 arguments */
    if (args == NIL || args->value.cons.cdr == NIL || args->value.cons.cdr->value.cons.cdr != NIL) {
        return lisp_make_error("equal? expects exactly 2 arguments");
    }

    LispObject *a = args->value.cons.car;
    LispObject *b = args->value.cons.cdr->value.cons.car;

    /* Use recursive structural equality */
    return objects_equal_recursive(a, b) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_eq_predicate(LispObject *args, Environment *env) {
    (void)env;
    /* Validate exactly 2 arguments */
    if (args == NIL || args->value.cons.cdr == NIL || args->value.cons.cdr->value.cons.cdr != NIL) {
        return lisp_make_error("string=? expects exactly 2 arguments");
    }

    LispObject *a = args->value.cons.car;
    LispObject *b = args->value.cons.cdr->value.cons.car;

    /* Type validation */
    if (a->type != LISP_STRING) {
        return lisp_make_error("string=?: first argument must be a string");
    }
    if (b->type != LISP_STRING) {
        return lisp_make_error("string=?: second argument must be a string");
    }

    /* String comparison */
    return (strcmp(a->value.string, b->value.string) == 0) ? lisp_make_number(1) : NIL;
}

/* Mapping operations */

static LispObject *builtin_map(LispObject *args, Environment *env) {
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("map requires at least 2 arguments");
    }

    LispObject *func = lisp_car(args);
    LispObject *list = lisp_car(lisp_cdr(args));

    if (func->type != LISP_BUILTIN && func->type != LISP_LAMBDA) {
        return lisp_make_error("map requires a function as first argument");
    }

    LispObject *result = NIL;
    LispObject *tail = NULL;

    while (list != NIL && list != NULL) {
        if (list->type != LISP_CONS) {
            return lisp_make_error("map requires a list");
        }

        LispObject *item = lisp_car(list);
        LispObject *func_args = lisp_make_cons(item, NIL);

        LispObject *mapped;
        if (func->type == LISP_BUILTIN) {
            mapped = func->value.builtin.func(func_args, env);
        } else {
            /* Lambda function - manually apply it */
            Environment *lambda_env = env_create(func->value.lambda.closure);

            /* Bind parameter to argument */
            LispObject *params = func->value.lambda.params;
            if (params == NIL || params->type != LISP_CONS) {
                return lisp_make_error("map: lambda must have at least one parameter");
            }

            LispObject *param = lisp_car(params);
            if (param->type != LISP_SYMBOL) {
                return lisp_make_error("map: lambda parameter must be a symbol");
            }

            /* Bind the parameter to the item */
            env_define(lambda_env, param->value.symbol, item);

            /* Check for extra parameters (should only have one for map) */
            if (lisp_cdr(params) != NIL) {
                return lisp_make_error("map: lambda should take exactly one argument");
            }

            /* Evaluate lambda body */
            LispObject *body = func->value.lambda.body;
            mapped = NIL;
            while (body != NIL && body != NULL) {
                mapped = lisp_eval(lisp_car(body), lambda_env);
                if (mapped->type == LISP_ERROR) {
                    env_free(lambda_env);
                    return mapped;
                }
                body = lisp_cdr(body);
            }

            env_free(lambda_env);
        }

        if (mapped->type == LISP_ERROR) {
            return mapped;
        }

        LispObject *new_cons = lisp_make_cons(mapped, NIL);
        if (result == NIL) {
            result = new_cons;
            tail = new_cons;
        } else {
            tail->value.cons.cdr = new_cons;
            tail = new_cons;
        }

        list = lisp_cdr(list);
    }

    return result;
}

static LispObject *builtin_mapcar(LispObject *args, Environment *env) {
    /* mapcar is the same as map in this implementation */
    return builtin_map(args, env);
}

/* Error introspection and handling functions */

/* error? - Test if object is an error */
static LispObject *builtin_error_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_boolean(0);
    }
    LispObject *obj = lisp_car(args);
    return lisp_make_boolean(obj->type == LISP_ERROR);
}

/* error-type - Get error type symbol */
static LispObject *builtin_error_type(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_typed_error_simple("wrong-number-of-arguments", "error-type requires 1 argument", env);
    }
    LispObject *obj = lisp_car(args);
    if (obj->type != LISP_ERROR) {
        return lisp_make_typed_error_simple("wrong-type-argument", "error-type: argument must be an error", env);
    }
    /* Return error type (guaranteed to be a symbol) */
    if (obj->value.error_with_stack.error_type != NULL) {
        return obj->value.error_with_stack.error_type;
    }
    /* Fallback to 'error symbol if somehow NULL */
    return sym_error;
}

/* error-message - Get error message string */
static LispObject *builtin_error_message(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_typed_error_simple("wrong-number-of-arguments", "error-message requires 1 argument", env);
    }
    LispObject *obj = lisp_car(args);
    if (obj->type != LISP_ERROR) {
        return lisp_make_typed_error_simple("wrong-type-argument", "error-message: argument must be an error", env);
    }
    return lisp_make_string(obj->value.error_with_stack.message);
}

/* error-stack - Get error stack trace */
static LispObject *builtin_error_stack(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_typed_error_simple("wrong-number-of-arguments", "error-stack requires 1 argument", env);
    }
    LispObject *obj = lisp_car(args);
    if (obj->type != LISP_ERROR) {
        return lisp_make_typed_error_simple("wrong-type-argument", "error-stack: argument must be an error", env);
    }
    LispObject *stack = obj->value.error_with_stack.stack_trace;
    return (stack != NULL) ? stack : NIL;
}

/* error-data - Get error data payload */
static LispObject *builtin_error_data(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_typed_error_simple("wrong-number-of-arguments", "error-data requires 1 argument", env);
    }
    LispObject *obj = lisp_car(args);
    if (obj->type != LISP_ERROR) {
        return lisp_make_typed_error_simple("wrong-type-argument", "error-data: argument must be an error", env);
    }
    LispObject *data = obj->value.error_with_stack.data;
    return (data != NULL) ? data : NIL;
}

/* signal - Raise a typed error
 * (signal ERROR-SYMBOL DATA)
 */
static LispObject *builtin_signal(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_typed_error_simple("wrong-number-of-arguments", "signal requires at least 1 argument", env);
    }

    LispObject *error_type = lisp_car(args);
    if (error_type->type != LISP_SYMBOL) {
        return lisp_make_typed_error_simple("wrong-type-argument", "signal: first argument must be a symbol", env);
    }

    /* Get optional data argument */
    LispObject *rest = lisp_cdr(args);
    LispObject *data = (rest != NIL && rest != NULL) ? lisp_car(rest) : NIL;

    /* Build error message from data */
    char message[512];
    if (data != NIL && data->type == LISP_STRING) {
        snprintf(message, sizeof(message), "%s", data->value.string);
    } else if (data != NIL) {
        char *data_str = lisp_print(data);
        snprintf(message, sizeof(message), "%s: %s", error_type->value.symbol, data_str);
    } else {
        snprintf(message, sizeof(message), "%s", error_type->value.symbol);
    }

    return lisp_make_typed_error(error_type, message, data, env);
}

/* error - Convenience function to signal generic error
 * (error MESSAGE)
 */
static LispObject *builtin_error(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_typed_error_simple("wrong-number-of-arguments", "error requires 1 argument", env);
    }

    LispObject *message_obj = lisp_car(args);
    const char *message;

    if (message_obj->type == LISP_STRING) {
        message = message_obj->value.string;
    } else {
        message = lisp_print(message_obj);
    }

    return lisp_make_typed_error_simple("error", message, env);
}
