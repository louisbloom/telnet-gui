#include "../include/lisp.h"
#include "../include/file_utils.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Global NIL object */
LispObject nil_obj = {.type = LISP_NIL};
LispObject *NIL = &nil_obj;

/* Global boolean objects */
LispObject true_obj = {.type = LISP_BOOLEAN, .value = {.boolean = 1}};
LispObject *LISP_TRUE = &true_obj;

/* Global symbol intern table */
LispObject *symbol_table = NULL;

/* Pre-interned special form symbols for fast comparison */
LispObject *sym_quote = NULL;
LispObject *sym_quasiquote = NULL;
LispObject *sym_unquote = NULL;
LispObject *sym_unquote_splicing = NULL;
LispObject *sym_if = NULL;
LispObject *sym_define = NULL;
LispObject *sym_set = NULL;
LispObject *sym_lambda = NULL;
LispObject *sym_defmacro = NULL;
LispObject *sym_let = NULL;
LispObject *sym_let_star = NULL;
LispObject *sym_progn = NULL;
LispObject *sym_do = NULL;
LispObject *sym_cond = NULL;
LispObject *sym_case = NULL;
LispObject *sym_and = NULL;
LispObject *sym_or = NULL;
LispObject *sym_condition_case = NULL;
LispObject *sym_unwind_protect = NULL;
LispObject *sym_else = NULL;
LispObject *sym_optional = NULL;
LispObject *sym_rest = NULL;
LispObject *sym_error = NULL;

/* Object creation functions */
LispObject *lisp_make_number(double value) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_NUMBER;
    obj->value.number = value;
    return obj;
}

LispObject *lisp_make_integer(long long value) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_INTEGER;
    obj->value.integer = value;
    return obj;
}

LispObject *lisp_make_char(unsigned int codepoint) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_CHAR;
    obj->value.character = codepoint;
    return obj;
}

LispObject *lisp_make_boolean(int value) {
    /* Return interned boolean objects */
    if (value) {
        return LISP_TRUE;
    } else {
        return NIL; /* #f is NIL */
    }
}

LispObject *lisp_make_string(const char *value) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_STRING;
    obj->value.string = GC_strdup(value);
    return obj;
}

LispObject *lisp_intern(const char *name) {
    /* Initialize symbol table on first use */
    if (symbol_table == NULL) {
        symbol_table = lisp_make_hash_table();
    }

    /* Look up symbol in intern table */
    struct HashEntry *entry = hash_table_get_entry(symbol_table, name);
    if (entry != NULL) {
        return entry->value; /* Return existing symbol */
    }

    /* Create new symbol struct */
    Symbol *sym = GC_malloc(sizeof(Symbol));
    sym->name = GC_strdup(name);
    sym->docstring = NULL;

    /* Create LispObject wrapper */
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_SYMBOL;
    obj->value.symbol = sym;

    /* Add to intern table */
    hash_table_set_entry(symbol_table, name, obj);

    return obj;
}

/* Keep old name for backward compatibility, but redirect to intern */
LispObject *lisp_make_symbol(const char *name) {
    return lisp_intern(name);
}

LispObject *lisp_make_cons(LispObject *car, LispObject *cdr) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_CONS;
    obj->value.cons.car = car;
    obj->value.cons.cdr = cdr;
    return obj;
}

/* New typed error creation function */
LispObject *lisp_make_typed_error(LispObject *error_type, const char *message, LispObject *data, Environment *env) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_ERROR;
    obj->value.error_with_stack.error_type = error_type;
    obj->value.error_with_stack.message = GC_strdup(message);
    obj->value.error_with_stack.data = data;
    obj->value.error_with_stack.stack_trace = (env != NULL) ? capture_call_stack(env) : NIL;
    obj->value.error_with_stack.caught = 0; /* Not caught initially - will propagate */
    return obj;
}

/* Convenience function for creating typed errors from strings */
LispObject *lisp_make_typed_error_simple(const char *error_type_name, const char *message, Environment *env) {
    return lisp_make_typed_error(lisp_intern(error_type_name), message, NIL, env);
}

/* Backward-compatible error creation (uses 'error type) */
LispObject *lisp_make_error(const char *message) {
    return lisp_make_typed_error(sym_error, message, NIL, NULL);
}

/* Backward-compatible error creation with stack trace */
LispObject *lisp_make_error_with_stack(const char *message, Environment *env) {
    return lisp_make_typed_error(sym_error, message, NIL, env);
}

LispObject *lisp_attach_stack_trace(LispObject *error, Environment *env) {
    if (error->type == LISP_ERROR) {
        if (error->value.error_with_stack.stack_trace == NIL || error->value.error_with_stack.stack_trace == NULL) {
            error->value.error_with_stack.stack_trace = capture_call_stack(env);
        }
    }
    return error;
}

LispObject *lisp_make_builtin(BuiltinFunc func, const char *name, const char *docstring) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_BUILTIN;
    obj->value.builtin.func = func;
    obj->value.builtin.name = name;
    obj->value.builtin.docstring = docstring;
    return obj;
}

LispObject *lisp_make_lambda(LispObject *params, LispObject *body, Environment *closure, const char *name) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_LAMBDA;
    obj->value.lambda.params = params;
    obj->value.lambda.required_params = params; /* For backward compat: all params are required */
    obj->value.lambda.optional_params = NIL;
    obj->value.lambda.rest_param = NULL;
    obj->value.lambda.required_count = 0; /* Will be computed during apply */
    obj->value.lambda.optional_count = 0;
    obj->value.lambda.body = body;
    obj->value.lambda.closure = closure;
    obj->value.lambda.name = name ? GC_strdup(name) : NULL;
    obj->value.lambda.docstring = NULL;
    return obj;
}

LispObject *lisp_make_lambda_ext(LispObject *params, LispObject *required_params, LispObject *optional_params,
                                 LispObject *rest_param, int required_count, int optional_count, LispObject *body,
                                 Environment *closure, const char *name) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_LAMBDA;
    obj->value.lambda.params = params;
    obj->value.lambda.required_params = required_params;
    obj->value.lambda.optional_params = optional_params;
    obj->value.lambda.rest_param = rest_param;
    obj->value.lambda.required_count = required_count;
    obj->value.lambda.optional_count = optional_count;
    obj->value.lambda.body = body;
    obj->value.lambda.closure = closure;
    obj->value.lambda.name = name ? GC_strdup(name) : NULL;
    obj->value.lambda.docstring = NULL;
    return obj;
}

LispObject *lisp_make_macro(LispObject *params, LispObject *body, Environment *closure, const char *name) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_MACRO;
    obj->value.macro.params = params;
    obj->value.macro.body = body;
    obj->value.macro.closure = closure;
    obj->value.macro.name = name ? GC_strdup(name) : NULL;
    obj->value.macro.docstring = NULL;
    return obj;
}

LispObject *lisp_make_tail_call(LispObject *func, LispObject *args) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_TAIL_CALL;
    obj->value.tail_call.func = func;
    obj->value.tail_call.args = args;
    return obj;
}

/* Object utilities */
int lisp_is_truthy(LispObject *obj) {
    /* Traditional Lisp semantics: only nil is false, everything else is true */
    if (obj == NULL || obj == NIL) {
        return 0;
    }

    if (obj->type == LISP_NIL) {
        return 0;
    }

    /* Boolean false (#f) is falsy (since #f == nil) */
    if (obj->type == LISP_BOOLEAN && obj->value.boolean == 0) {
        return 0;
    }

    /* Everything else is truthy, including:
     * - Integer 0
     * - Float 0.0
     * - Empty strings ""
     * - Empty vectors
     * - Empty hash tables
     */
    return 1;
}

int lisp_is_list(LispObject *obj) {
    if (obj == NIL)
        return 1;
    if (obj->type != LISP_CONS)
        return 0;

    while (obj != NIL && obj->type == LISP_CONS) {
        obj = obj->value.cons.cdr;
    }

    return (obj == NIL);
}

size_t lisp_list_length(LispObject *list) {
    size_t len = 0;
    while (list != NIL && list->type == LISP_CONS) {
        len++;
        list = list->value.cons.cdr;
    }
    return len;
}

LispObject *lisp_make_vector(size_t capacity) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_VECTOR;
    obj->value.vector.capacity = capacity > 0 ? capacity : 8;
    obj->value.vector.size = 0;
    obj->value.vector.items = GC_malloc(sizeof(LispObject *) * obj->value.vector.capacity);
    /* Initialize to NULL */
    for (size_t i = 0; i < obj->value.vector.capacity; i++) {
        obj->value.vector.items[i] = NIL;
    }
    return obj;
}

LispObject *lisp_make_hash_table(void) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_HASH_TABLE;
    obj->value.hash_table.capacity = 16;
    obj->value.hash_table.bucket_count = 16;
    obj->value.hash_table.entry_count = 0;
    obj->value.hash_table.buckets = GC_malloc(sizeof(void *) * 16);
    /* Initialize buckets to NULL */
    for (size_t i = 0; i < 16; i++) {
        ((void **)obj->value.hash_table.buckets)[i] = NULL;
    }
    return obj;
}

/* List utilities */
LispObject *lisp_car(LispObject *obj) {
    if (obj == NIL || obj == NULL)
        return NIL;
    if (obj->type != LISP_CONS)
        return NIL;
    return obj->value.cons.car;
}

LispObject *lisp_cdr(LispObject *obj) {
    if (obj == NIL || obj == NULL)
        return NIL;
    if (obj->type != LISP_CONS)
        return NIL;
    return obj->value.cons.cdr;
}

/* Simple API */
static Environment *global_env = NULL;

int lisp_init(void) {
    /* Initialize Boehm GC */
    GC_INIT();

    /* Initialize symbol intern table */
    symbol_table = lisp_make_hash_table();

    /* Pre-intern special form symbols */
    sym_quote = lisp_intern("quote");
    sym_quasiquote = lisp_intern("quasiquote");
    sym_unquote = lisp_intern("unquote");
    sym_unquote_splicing = lisp_intern("unquote-splicing");
    sym_if = lisp_intern("if");
    sym_define = lisp_intern("define");
    sym_set = lisp_intern("set!");
    sym_lambda = lisp_intern("lambda");
    sym_defmacro = lisp_intern("defmacro");
    sym_let = lisp_intern("let");
    sym_let_star = lisp_intern("let*");
    sym_progn = lisp_intern("progn");
    sym_do = lisp_intern("do");
    sym_cond = lisp_intern("cond");
    sym_case = lisp_intern("case");
    sym_and = lisp_intern("and");
    sym_or = lisp_intern("or");
    sym_condition_case = lisp_intern("condition-case");
    sym_unwind_protect = lisp_intern("unwind-protect");
    sym_else = lisp_intern("else");
    sym_optional = lisp_intern("&optional");
    sym_rest = lisp_intern("&rest");
    sym_error = lisp_intern("error");

    if (global_env == NULL) {
        global_env = env_create_global();
    }
    return 0;
}

LispObject *lisp_eval_string(const char *code, Environment *env) {
    if (env == NULL) {
        env = global_env;
    }

    const char *input = code;
    LispObject *expr = lisp_read(&input);

    if (expr == NULL) {
        return NIL;
    }

    if (expr->type == LISP_ERROR) {
        return expr;
    }

    return lisp_eval(expr, env);
}

void lisp_cleanup(void) {
    /* GC handles cleanup automatically */
    if (global_env != NULL) {
        env_free(global_env);
        global_env = NULL;
    }
}

/* Load file */
LispObject *lisp_load_file(const char *filename, Environment *env) {
    FILE *file = file_open(filename, "r");
    if (file == NULL) {
        return lisp_make_error("Cannot open file");
    }

    /* Read entire file */
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *buffer = GC_malloc(size + 1);
    fread(buffer, 1, size, file);
    buffer[size] = '\0';
    fclose(file);

    /* Evaluate all expressions */
    const char *input = buffer;
    LispObject *result = NIL;

    while (*input) {
        LispObject *expr = lisp_read(&input);
        if (expr == NULL)
            break;

        if (expr->type == LISP_ERROR) {
            return expr;
        }

        result = lisp_eval(expr, env);

        if (result->type == LISP_ERROR) {
            return result;
        }
    }

    return result;
}
