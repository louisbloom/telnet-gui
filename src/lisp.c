#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Global NIL object */
LispObject nil_obj = { .type = LISP_NIL };
LispObject* NIL = &nil_obj;

/* Object creation functions */
LispObject* lisp_make_number(double value) {
    LispObject* obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_NUMBER;
    obj->value.number = value;
    return obj;
}

LispObject* lisp_make_string(const char* value) {
    LispObject* obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_STRING;
    obj->value.string = GC_strdup(value);
    return obj;
}

LispObject* lisp_make_symbol(const char* name) {
    LispObject* obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_SYMBOL;
    obj->value.symbol = GC_strdup(name);
    return obj;
}

LispObject* lisp_make_cons(LispObject* car, LispObject* cdr) {
    LispObject* obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_CONS;
    obj->value.cons.car = car;
    obj->value.cons.cdr = cdr;
    return obj;
}

LispObject* lisp_make_error(const char* message) {
    LispObject* obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_ERROR;
    obj->value.error = GC_strdup(message);
    return obj;
}

LispObject* lisp_make_builtin(BuiltinFunc func, const char* name) {
    LispObject* obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_BUILTIN;
    obj->value.builtin.func = func;
    obj->value.builtin.name = name;
    return obj;
}

LispObject* lisp_make_lambda(LispObject* params, LispObject* body, Environment* closure) {
    LispObject* obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_LAMBDA;
    obj->value.lambda.params = params;
    obj->value.lambda.body = body;
    obj->value.lambda.closure = closure;
    return obj;
}

/* Object utilities */
int lisp_is_truthy(LispObject* obj) {
    if (obj == NULL || obj == NIL) {
        return 0;
    }

    if (obj->type == LISP_NIL) {
        return 0;
    }

    if (obj->type == LISP_STRING && strlen(obj->value.string) == 0) {
        return 0;
    }

    return 1;
}

int lisp_is_list(LispObject* obj) {
    if (obj == NIL) return 1;
    if (obj->type != LISP_CONS) return 0;

    while (obj != NIL && obj->type == LISP_CONS) {
        obj = obj->value.cons.cdr;
    }

    return (obj == NIL);
}

size_t lisp_list_length(LispObject* list) {
    size_t len = 0;
    while (list != NIL && list->type == LISP_CONS) {
        len++;
        list = list->value.cons.cdr;
    }
    return len;
}

/* List utilities */
LispObject* lisp_car(LispObject* obj) {
    if (obj == NIL || obj == NULL) return NIL;
    if (obj->type != LISP_CONS) return NIL;
    return obj->value.cons.car;
}

LispObject* lisp_cdr(LispObject* obj) {
    if (obj == NIL || obj == NULL) return NIL;
    if (obj->type != LISP_CONS) return NIL;
    return obj->value.cons.cdr;
}

/* Simple API */
static Environment* global_env = NULL;

int lisp_init(void) {
    /* Initialize Boehm GC */
    GC_INIT();

    if (global_env == NULL) {
        global_env = env_create_global();
    }
    return 0;
}

LispObject* lisp_eval_string(const char* code, Environment* env) {
    if (env == NULL) {
        env = global_env;
    }

    const char* input = code;
    LispObject* expr = lisp_read(&input);

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
LispObject* lisp_load_file(const char* filename, Environment* env) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        return lisp_make_error("Cannot open file");
    }

    /* Read entire file */
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* buffer = GC_malloc(size + 1);
    fread(buffer, 1, size, file);
    buffer[size] = '\0';
    fclose(file);

    /* Evaluate all expressions */
    const char* input = buffer;
    LispObject* result = NIL;

    while (*input) {
        LispObject* expr = lisp_read(&input);
        if (expr == NULL) break;

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
