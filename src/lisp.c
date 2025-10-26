#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Global NIL object */
LispObject nil_obj = {.type = LISP_NIL};
LispObject *NIL = &nil_obj;

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

LispObject *lisp_make_boolean(int value) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_BOOLEAN;
    obj->value.boolean = value;
    return obj;
}

LispObject *lisp_make_string(const char *value) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_STRING;
    obj->value.string = GC_strdup(value);
    return obj;
}

LispObject *lisp_make_symbol(const char *name) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_SYMBOL;
    obj->value.symbol = GC_strdup(name);
    return obj;
}

LispObject *lisp_make_cons(LispObject *car, LispObject *cdr) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_CONS;
    obj->value.cons.car = car;
    obj->value.cons.cdr = cdr;
    return obj;
}

LispObject *lisp_make_error(const char *message) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_ERROR;
    obj->value.error_with_stack.error = GC_strdup(message);
    obj->value.error_with_stack.stack_trace = NIL;
    return obj;
}

LispObject *lisp_make_error_with_stack(const char *message, Environment *env) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_ERROR;
    obj->value.error_with_stack.error = GC_strdup(message);
    obj->value.error_with_stack.stack_trace = capture_call_stack(env);
    return obj;
}

LispObject *lisp_attach_stack_trace(LispObject *error, Environment *env) {
    if (error->type == LISP_ERROR) {
        if (error->value.error_with_stack.stack_trace == NIL || error->value.error_with_stack.stack_trace == NULL) {
            error->value.error_with_stack.stack_trace = capture_call_stack(env);
        }
    }
    return error;
}

LispObject *lisp_make_builtin(BuiltinFunc func, const char *name) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_BUILTIN;
    obj->value.builtin.func = func;
    obj->value.builtin.name = name;
    return obj;
}

LispObject *lisp_make_lambda(LispObject *params, LispObject *body, Environment *closure) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_LAMBDA;
    obj->value.lambda.params = params;
    obj->value.lambda.body = body;
    obj->value.lambda.closure = closure;
    return obj;
}

/* Object utilities */
int lisp_is_truthy(LispObject *obj) {
    if (obj == NULL || obj == NIL) {
        return 0;
    }

    if (obj->type == LISP_NIL) {
        return 0;
    }

    if (obj->type == LISP_STRING && strlen(obj->value.string) == 0) {
        return 0;
    }

    /* Integer 0 is falsy */
    if (obj->type == LISP_INTEGER && obj->value.integer == 0) {
        return 0;
    }

    /* Boolean false (#f) is falsy */
    if (obj->type == LISP_BOOLEAN && obj->value.boolean == 0) {
        return 0;
    }

    /* Number 0.0 is falsy (keep existing behavior) */
    if (obj->type == LISP_NUMBER && obj->value.number == 0.0) {
        return 0;
    }

    /* Empty vectors are falsy */
    if (obj->type == LISP_VECTOR && obj->value.vector.size == 0) {
        return 0;
    }

    /* Empty hash tables are falsy */
    if (obj->type == LISP_HASH_TABLE && obj->value.hash_table.entry_count == 0) {
        return 0;
    }

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
    FILE *file = fopen(filename, "r");
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
