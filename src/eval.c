#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>

/* Forward declarations */
static LispObject *eval_list(LispObject *list, Environment *env);
static LispObject *eval_if(LispObject *args, Environment *env);
static LispObject *eval_define(LispObject *args, Environment *env);
static LispObject *eval_set_bang(LispObject *args, Environment *env);
static LispObject *eval_lambda(LispObject *args, Environment *env);
static LispObject *eval_let(LispObject *args, Environment *env);
static LispObject *eval_let_star(LispObject *args, Environment *env);
static LispObject *eval_progn(LispObject *args, Environment *env);
static LispObject *apply(LispObject *func, LispObject *args, Environment *env);

LispObject *lisp_eval(LispObject *expr, Environment *env) {
    if (expr == NULL || expr == NIL) {
        return NIL;
    }

    switch (expr->type) {
    case LISP_NIL:
        return NIL;

    case LISP_NUMBER:
    case LISP_INTEGER:
    case LISP_STRING:
    case LISP_BOOLEAN:
    case LISP_BUILTIN:
    case LISP_LAMBDA:
    case LISP_FILE_STREAM:
    case LISP_VECTOR:
    case LISP_HASH_TABLE:
        return expr;

    case LISP_ERROR:
        return expr;

    case LISP_SYMBOL: {
        LispObject *value = env_lookup(env, expr->value.symbol);
        if (value == NULL) {
            char error[256];
            snprintf(error, sizeof(error), "Undefined symbol: %s", expr->value.symbol);
            return lisp_make_error(error);
        }
        return value;
    }

    case LISP_CONS:
        return eval_list(expr, env);
    }

    return lisp_make_error("Unknown expression type");
}

static LispObject *eval_list(LispObject *list, Environment *env) {
    if (list == NIL) {
        return NIL;
    }

    LispObject *first = lisp_car(list);
    if (first == NULL) {
        return lisp_make_error("Empty list in function position");
    }

    /* Check for special forms */
    if (first->type == LISP_SYMBOL) {
        if (strcmp(first->value.symbol, "quote") == 0) {
            LispObject *rest = lisp_cdr(list);
            if (rest == NIL) {
                return lisp_make_error("quote requires an argument");
            }
            return lisp_car(rest);
        }

        if (strcmp(first->value.symbol, "if") == 0) {
            return eval_if(lisp_cdr(list), env);
        }

        if (strcmp(first->value.symbol, "define") == 0) {
            return eval_define(lisp_cdr(list), env);
        }

        if (strcmp(first->value.symbol, "set!") == 0) {
            return eval_set_bang(lisp_cdr(list), env);
        }

        if (strcmp(first->value.symbol, "lambda") == 0) {
            return eval_lambda(lisp_cdr(list), env);
        }

        if (strcmp(first->value.symbol, "let") == 0) {
            return eval_let(lisp_cdr(list), env);
        }

        if (strcmp(first->value.symbol, "let*") == 0) {
            return eval_let_star(lisp_cdr(list), env);
        }

        if (strcmp(first->value.symbol, "progn") == 0) {
            return eval_progn(lisp_cdr(list), env);
        }
    }

    /* Function application */
    LispObject *func = lisp_eval(first, env);
    if (func->type == LISP_ERROR) {
        return func;
    }

    /* Evaluate arguments */
    LispObject *evaled_args = NIL;
    LispObject *tail = NULL;
    LispObject *args = lisp_cdr(list);

    while (args != NIL && args != NULL) {
        LispObject *arg = lisp_car(args);
        LispObject *evaled = lisp_eval(arg, env);

        if (evaled->type == LISP_ERROR) {
            return evaled;
        }

        LispObject *new_cons = lisp_make_cons(evaled, NIL);
        if (evaled_args == NIL) {
            evaled_args = new_cons;
            tail = new_cons;
        } else {
            tail->value.cons.cdr = new_cons;
            tail = new_cons;
        }

        args = lisp_cdr(args);
    }

    return apply(func, evaled_args, env);
}

static LispObject *eval_if(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("if requires at least 2 arguments");
    }

    LispObject *cond = lisp_car(args);
    LispObject *rest = lisp_cdr(args);

    if (rest == NIL) {
        return lisp_make_error("if requires at least 2 arguments");
    }

    LispObject *then_expr = lisp_car(rest);
    LispObject *else_rest = lisp_cdr(rest);
    LispObject *else_expr = (else_rest != NIL) ? lisp_car(else_rest) : NIL;

    LispObject *cond_val = lisp_eval(cond, env);
    if (cond_val->type == LISP_ERROR) {
        return cond_val;
    }

    if (lisp_is_truthy(cond_val)) {
        return lisp_eval(then_expr, env);
    } else {
        return lisp_eval(else_expr, env);
    }
}

static LispObject *eval_define(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("define requires 2 arguments");
    }

    LispObject *name = lisp_car(args);
    if (name->type != LISP_SYMBOL) {
        return lisp_make_error("define requires a symbol as first argument");
    }

    LispObject *rest = lisp_cdr(args);
    if (rest == NIL) {
        return lisp_make_error("define requires 2 arguments");
    }

    LispObject *value_expr = lisp_car(rest);
    LispObject *value = lisp_eval(value_expr, env);

    if (value->type == LISP_ERROR) {
        return value;
    }

    env_define(env, name->value.symbol, value);
    return value;
}

static LispObject *eval_set_bang(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("set! requires 2 arguments");
    }

    LispObject *name = lisp_car(args);
    if (name->type != LISP_SYMBOL) {
        return lisp_make_error("set! requires a symbol as first argument");
    }

    LispObject *rest = lisp_cdr(args);
    if (rest == NIL) {
        return lisp_make_error("set! requires 2 arguments");
    }

    LispObject *value_expr = lisp_car(rest);
    LispObject *value = lisp_eval(value_expr, env);

    if (value->type == LISP_ERROR) {
        return value;
    }

    /* Try to update existing binding */
    if (!env_set(env, name->value.symbol, value)) {
        /* If not found, error */
        char error[256];
        snprintf(error, sizeof(error), "set!: cannot set undefined variable: %s", name->value.symbol);
        return lisp_make_error(error);
    }

    return value;
}

static LispObject *eval_lambda(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("lambda requires at least 2 arguments");
    }

    LispObject *params = lisp_car(args);
    LispObject *rest = lisp_cdr(args);

    if (rest == NIL) {
        return lisp_make_error("lambda requires a body");
    }

    LispObject *body = lisp_car(rest);

    return lisp_make_lambda(params, body, env);
}

static LispObject *eval_let(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("let requires at least 2 arguments");
    }

    LispObject *bindings = lisp_car(args);
    LispObject *rest = lisp_cdr(args);

    if (rest == NIL) {
        return lisp_make_error("let requires a body");
    }

    LispObject *body = lisp_car(rest);

    /* Create new environment */
    Environment *new_env = env_create(env);

    /* Process bindings */
    while (bindings != NIL && bindings != NULL) {
        LispObject *binding = lisp_car(bindings);

        if (binding->type != LISP_CONS) {
            return lisp_make_error("let binding must be a list");
        }

        LispObject *name = lisp_car(binding);
        if (name->type != LISP_SYMBOL) {
            return lisp_make_error("let binding name must be a symbol");
        }

        LispObject *value_list = lisp_cdr(binding);
        if (value_list == NIL) {
            return lisp_make_error("let binding requires a value");
        }

        LispObject *value_expr = lisp_car(value_list);
        LispObject *value = lisp_eval(value_expr, env);

        if (value->type == LISP_ERROR) {
            return value;
        }

        env_define(new_env, name->value.symbol, value);
        bindings = lisp_cdr(bindings);
    }

    return lisp_eval(body, new_env);
}

static LispObject *eval_let_star(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("let* requires at least 2 arguments");
    }

    LispObject *bindings = lisp_car(args);
    LispObject *rest = lisp_cdr(args);

    if (rest == NIL) {
        return lisp_make_error("let* requires a body");
    }

    LispObject *body = lisp_car(rest);

    /* Create new environment - will be extended as we process bindings */
    Environment *new_env = env_create(env);

    /* Process bindings sequentially */
    while (bindings != NIL && bindings != NULL) {
        LispObject *binding = lisp_car(bindings);

        if (binding->type != LISP_CONS) {
            return lisp_make_error("let* binding must be a list");
        }

        LispObject *name = lisp_car(binding);
        if (name->type != LISP_SYMBOL) {
            return lisp_make_error("let* binding name must be a symbol");
        }

        LispObject *value_list = lisp_cdr(binding);
        if (value_list == NIL) {
            return lisp_make_error("let* binding requires a value");
        }

        LispObject *value_expr = lisp_car(value_list);

        /* Evaluate in the new_env (which includes previous bindings) */
        LispObject *value = lisp_eval(value_expr, new_env);

        if (value->type == LISP_ERROR) {
            return value;
        }

        env_define(new_env, name->value.symbol, value);
        bindings = lisp_cdr(bindings);
    }

    return lisp_eval(body, new_env);
}

static LispObject *eval_progn(LispObject *args, Environment *env) {
    if (args == NIL) {
        return NIL;
    }

    LispObject *result = NIL;

    while (args != NIL && args != NULL) {
        LispObject *expr = lisp_car(args);
        result = lisp_eval(expr, env);

        if (result->type == LISP_ERROR) {
            return result;
        }

        args = lisp_cdr(args);
    }

    return result;
}

static LispObject *apply(LispObject *func, LispObject *args, Environment *env) {
    if (func->type == LISP_BUILTIN) {
        return func->value.builtin.func(args, env);
    }

    if (func->type == LISP_LAMBDA) {
        /* Create new environment with closure as parent */
        Environment *new_env = env_create(func->value.lambda.closure);

        /* Bind parameters */
        LispObject *params = func->value.lambda.params;
        LispObject *arg_list = args;

        while (params != NIL && params != NULL) {
            if (params->type != LISP_CONS) {
                return lisp_make_error("Invalid lambda parameter list");
            }

            LispObject *param = lisp_car(params);
            if (param->type != LISP_SYMBOL) {
                return lisp_make_error("Lambda parameter must be a symbol");
            }

            if (arg_list == NIL) {
                return lisp_make_error("Too few arguments to lambda");
            }

            LispObject *arg = lisp_car(arg_list);
            env_define(new_env, param->value.symbol, arg);

            params = lisp_cdr(params);
            arg_list = lisp_cdr(arg_list);
        }

        if (arg_list != NIL) {
            return lisp_make_error("Too many arguments to lambda");
        }

        return lisp_eval(func->value.lambda.body, new_env);
    }

    return lisp_make_error("Cannot apply non-function");
}
