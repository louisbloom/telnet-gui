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
static LispObject *eval_do(LispObject *args, Environment *env);
static LispObject *eval_cond(LispObject *args, Environment *env);
static LispObject *eval_case(LispObject *args, Environment *env);
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
            return lisp_make_error_with_stack(error, env);
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

        if (strcmp(first->value.symbol, "do") == 0) {
            return eval_do(lisp_cdr(list), env);
        }

        if (strcmp(first->value.symbol, "cond") == 0) {
            return eval_cond(lisp_cdr(list), env);
        }

        if (strcmp(first->value.symbol, "case") == 0) {
            return eval_case(lisp_cdr(list), env);
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

    /* If defining a lambda, attach the name for better debugging */
    if (value->type == LISP_LAMBDA && value->value.lambda.name == NULL) {
        value->value.lambda.name = GC_strdup(name->value.symbol);
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

    return lisp_make_lambda(params, body, env, NULL);
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

static LispObject *eval_do(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("do requires at least 2 arguments");
    }

    /* Parse bindings and test clause */
    LispObject *bindings = lisp_car(args);
    LispObject *rest = lisp_cdr(args);

    if (rest == NIL) {
        return lisp_make_error("do requires a test clause");
    }

    LispObject *test_clause = lisp_car(rest);
    LispObject *body_exprs = lisp_cdr(rest);

    /* Validate test clause is a list */
    if (test_clause->type != LISP_CONS) {
        return lisp_make_error("do test clause must be a list");
    }

    /* Extract test condition and result expression */
    LispObject *test_expr = lisp_car(test_clause);
    LispObject *result_rest = lisp_cdr(test_clause);
    LispObject *result_expr = (result_rest != NIL) ? lisp_car(result_rest) : NIL;

    /* Create new environment for loop variables */
    Environment *loop_env = env_create(env);

    /* Initialize loop variables from bindings */
    LispObject *binding_list = bindings;
    while (binding_list != NIL && binding_list != NULL) {
        LispObject *binding = lisp_car(binding_list);

        if (binding->type != LISP_CONS) {
            return lisp_make_error("do binding must be a list");
        }

        LispObject *name = lisp_car(binding);
        if (name->type != LISP_SYMBOL) {
            return lisp_make_error("do binding name must be a symbol");
        }

        LispObject *init_list = lisp_cdr(binding);
        if (init_list == NIL) {
            return lisp_make_error("do binding requires an initial value");
        }

        LispObject *init_val = lisp_car(init_list);
        LispObject *init_result = lisp_eval(init_val, env);

        if (init_result->type == LISP_ERROR) {
            return init_result;
        }

        env_define(loop_env, name->value.symbol, init_result);
        binding_list = lisp_cdr(binding_list);
    }

    /* Main loop */
    while (1) {
        /* Evaluate test condition */
        LispObject *test_result = lisp_eval(test_expr, loop_env);
        if (test_result->type == LISP_ERROR) {
            return test_result;
        }

        /* If test is true, evaluate and return result expression */
        if (lisp_is_truthy(test_result)) {
            if (result_expr != NIL) {
                return lisp_eval(result_expr, loop_env);
            }
            return NIL;
        }

        /* Evaluate body expressions (for side effects) */
        LispObject *body = body_exprs;
        while (body != NIL && body != NULL) {
            LispObject *body_expr = lisp_car(body);
            LispObject *body_result = lisp_eval(body_expr, loop_env);

            if (body_result->type == LISP_ERROR) {
                return body_result;
            }

            body = lisp_cdr(body);
        }

        /* Update loop variables with step expressions (evaluate in loop_env) */
        binding_list = bindings;
        while (binding_list != NIL && binding_list != NULL) {
            LispObject *binding = lisp_car(binding_list);
            LispObject *name = lisp_car(binding);
            LispObject *init_list = lisp_cdr(binding);

            /* Check if there's a step expression */
            LispObject *step_list = lisp_cdr(init_list);
            if (step_list != NIL) {
                LispObject *step_val = lisp_car(step_list);
                LispObject *step_result = lisp_eval(step_val, loop_env);

                if (step_result->type == LISP_ERROR) {
                    return step_result;
                }

                /* Update the variable */
                env_set(loop_env, name->value.symbol, step_result);
            }

            binding_list = lisp_cdr(binding_list);
        }
    }
}

static LispObject *eval_cond(LispObject *args, Environment *env) {
    if (args == NIL) {
        return NIL;
    }

    while (args != NIL && args != NULL) {
        LispObject *clause = lisp_car(args);

        if (clause->type != LISP_CONS) {
            return lisp_make_error("cond clause must be a list");
        }

        LispObject *test = lisp_car(clause);
        LispObject *result = lisp_cdr(clause);

        /* Check for 'else' */
        if (test->type == LISP_SYMBOL && strcmp(test->value.symbol, "else") == 0) {
            if (result != NIL && result != NULL) {
                return lisp_eval(lisp_car(result), env);
            }
            return NIL;
        }

        /* Evaluate test */
        LispObject *test_result = lisp_eval(test, env);
        if (test_result->type == LISP_ERROR) {
            return test_result;
        }

        /* If true, evaluate and return result */
        if (lisp_is_truthy(test_result)) {
            if (result != NIL && result != NULL) {
                return lisp_eval(lisp_car(result), env);
            }
            return test_result;
        }

        args = lisp_cdr(args);
    }

    return NIL;
}

static int objects_equal(LispObject *a, LispObject *b) {
    if (a->type != b->type) {
        return 0;
    }

    switch (a->type) {
    case LISP_INTEGER:
        return a->value.integer == b->value.integer;
    case LISP_NUMBER:
        return a->value.number == b->value.number;
    case LISP_STRING:
        return strcmp(a->value.string, b->value.string) == 0;
    case LISP_BOOLEAN:
        return a->value.boolean == b->value.boolean;
    case LISP_NIL:
        return 1;
    default:
        return 0;
    }
}

static LispObject *eval_case(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("case requires at least 1 argument");
    }

    /* Evaluate key expression once */
    LispObject *key = lisp_eval(lisp_car(args), env);
    if (key->type == LISP_ERROR) {
        return key;
    }

    LispObject *clauses = lisp_cdr(args);

    while (clauses != NIL && clauses != NULL) {
        LispObject *clause = lisp_car(clauses);

        if (clause->type != LISP_CONS) {
            return lisp_make_error("case clause must be a list");
        }

        LispObject *values = lisp_car(clause);
        LispObject *result = lisp_cdr(clause);

        /* Check for 'else' */
        if (values->type == LISP_SYMBOL && strcmp(values->value.symbol, "else") == 0) {
            if (result != NIL && result != NULL) {
                return lisp_eval(lisp_car(result), env);
            }
            return NIL;
        }

        /* Compare key with each value in the list */
        if (values->type == LISP_CONS) {
            LispObject *val_list = values;
            while (val_list != NIL && val_list != NULL) {
                LispObject *val = lisp_car(val_list);
                LispObject *val_result = lisp_eval(val, env);

                if (val_result->type == LISP_ERROR) {
                    return val_result;
                }

                if (objects_equal(key, val_result)) {
                    if (result != NIL && result != NULL) {
                        return lisp_eval(lisp_car(result), env);
                    }
                    return NIL;
                }

                val_list = lisp_cdr(val_list);
            }
        }

        clauses = lisp_cdr(clauses);
    }

    return NIL;
}

static LispObject *apply(LispObject *func, LispObject *args, Environment *env) {
    if (func->type == LISP_BUILTIN) {
        /* Push call frame for builtin */
        push_call_frame(env, func->value.builtin.name);

        /* Call builtin */
        LispObject *result = func->value.builtin.func(args, env);

        /* Pop frame */
        pop_call_frame(env);

        /* If error, attach stack trace */
        if (result->type == LISP_ERROR) {
            return lisp_attach_stack_trace(result, env);
        }

        return result;
    }

    if (func->type == LISP_LAMBDA) {
        /* Create new environment with closure as parent */
        Environment *new_env = env_create(func->value.lambda.closure);
        if (env && env->call_stack != NULL) {
            new_env->call_stack = env->call_stack; /* Inherit call stack */
        }

        /* Generate lambda name for stack trace */
        char lambda_name[128];
        const char *frame_name;
        LispObject *lambda_params = func->value.lambda.params;

        /* Priority: 1) stored name, 2) lambda/param, 3) lambda */
        if (func->value.lambda.name != NULL) {
            frame_name = func->value.lambda.name;
        } else {
            if (lambda_params != NIL && lambda_params->type == LISP_CONS && lisp_car(lambda_params) != NULL &&
                lisp_car(lambda_params)->type == LISP_SYMBOL) {
                snprintf(lambda_name, sizeof(lambda_name), "lambda/%s", lisp_car(lambda_params)->value.symbol);
                frame_name = lambda_name;
            } else {
                frame_name = "lambda";
            }
        }

        /* Push call frame for lambda */
        push_call_frame(new_env, frame_name);

        /* Bind parameters */
        LispObject *params = lambda_params;
        LispObject *arg_list = args;

        while (params != NIL && params != NULL) {
            if (params->type != LISP_CONS) {
                LispObject *err = lisp_make_error_with_stack("Invalid lambda parameter list", new_env);
                pop_call_frame(new_env);
                return err;
            }

            LispObject *param = lisp_car(params);
            if (param->type != LISP_SYMBOL) {
                LispObject *err = lisp_make_error_with_stack("Lambda parameter must be a symbol", new_env);
                pop_call_frame(new_env);
                return err;
            }

            if (arg_list == NIL) {
                LispObject *err = lisp_make_error_with_stack("Too few arguments to lambda", new_env);
                pop_call_frame(new_env);
                return err;
            }

            LispObject *arg = lisp_car(arg_list);
            env_define(new_env, param->value.symbol, arg);

            params = lisp_cdr(params);
            arg_list = lisp_cdr(arg_list);
        }

        if (arg_list != NIL) {
            LispObject *err = lisp_make_error_with_stack("Too many arguments to lambda", new_env);
            pop_call_frame(new_env);
            return err;
        }

        LispObject *result = lisp_eval(func->value.lambda.body, new_env);

        /* Pop frame */
        pop_call_frame(new_env);

        /* If error, attach stack trace */
        if (result->type == LISP_ERROR) {
            return lisp_attach_stack_trace(result, env);
        }

        return result;
    }

    return lisp_make_error_with_stack("Cannot apply non-function", env);
}
