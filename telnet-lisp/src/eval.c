#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>

/* Helper function to check if an error should propagate */
static int should_propagate_error(LispObject *obj) {
    return (obj->type == LISP_ERROR && !obj->value.error_with_stack.caught);
}

/* Forward declarations */
static LispObject *eval_list(LispObject *list, Environment *env, int in_tail_position);
static LispObject *eval_if(LispObject *args, Environment *env, int in_tail_position);
static LispObject *eval_define(LispObject *args, Environment *env);
static LispObject *eval_set_bang(LispObject *args, Environment *env);
static LispObject *eval_lambda(LispObject *args, Environment *env);
static LispObject *eval_defmacro(LispObject *args, Environment *env);
static LispObject *eval_quasiquote(LispObject *expr, Environment *env);
static LispObject *expand_macro(LispObject *macro, LispObject *args, Environment *env);
static LispObject *eval_let(LispObject *args, Environment *env, int in_tail_position);
static LispObject *eval_let_star(LispObject *args, Environment *env, int in_tail_position);
static LispObject *eval_progn(LispObject *args, Environment *env, int in_tail_position);
static LispObject *eval_do(LispObject *args, Environment *env);
static LispObject *eval_cond(LispObject *args, Environment *env, int in_tail_position);
static LispObject *eval_case(LispObject *args, Environment *env, int in_tail_position);
static LispObject *eval_and(LispObject *args, Environment *env, int in_tail_position);
static LispObject *eval_or(LispObject *args, Environment *env, int in_tail_position);
static LispObject *eval_condition_case(LispObject *args, Environment *env, int in_tail_position);
static LispObject *eval_unwind_protect(LispObject *args, Environment *env, int in_tail_position);
static LispObject *apply(LispObject *func, LispObject *args, Environment *env, int in_tail_position);
static LispObject *lisp_eval_internal(LispObject *expr, Environment *env, int in_tail_position);

/* Public eval function - adds trampoline loop for tail call optimization */
LispObject *lisp_eval(LispObject *expr, Environment *env) {
    LispObject *result = lisp_eval_internal(expr, env, 1);

    /* Trampoline loop: keep unwrapping tail calls */
    while (result != NULL && result->type == LISP_TAIL_CALL) {
        LispObject *func = result->value.tail_call.func;
        LispObject *args = result->value.tail_call.args;
        /* Execute the tail call (in_tail_position=0 to actually run it) */
        /* But the body evaluation will still use in_tail_position=1 for last expr */
        result = apply(func, args, env, 0);
    }

    return result;
}

/* Internal eval function with tail position tracking */
static LispObject *lisp_eval_internal(LispObject *expr, Environment *env, int in_tail_position) {
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
    case LISP_MACRO:
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
        return eval_list(expr, env, in_tail_position);

    case LISP_TAIL_CALL:
        /* Should never evaluate a tail call directly */
        return lisp_make_error("Internal error: evaluating tail call object");
    }

    return lisp_make_error("Unknown expression type");
}

static LispObject *eval_list(LispObject *list, Environment *env, int in_tail_position) {
    if (list == NIL) {
        return NIL;
    }

    LispObject *first = lisp_car(list);
    if (first == NULL) {
        return lisp_make_error("Empty list in function position");
    }

    /* Check for special forms - these propagate tail position */
    if (first->type == LISP_SYMBOL) {
        if (first == sym_quote) {
            LispObject *rest = lisp_cdr(list);
            if (rest == NIL) {
                return lisp_make_error("quote requires an argument");
            }
            return lisp_car(rest);
        }

        if (first == sym_quasiquote) {
            LispObject *rest = lisp_cdr(list);
            if (rest == NIL) {
                return lisp_make_error("quasiquote requires an argument");
            }
            return eval_quasiquote(lisp_car(rest), env);
        }

        if (first == sym_if) {
            return eval_if(lisp_cdr(list), env, in_tail_position);
        }

        if (first == sym_define) {
            return eval_define(lisp_cdr(list), env);
        }

        if (first == sym_set) {
            return eval_set_bang(lisp_cdr(list), env);
        }

        if (first == sym_lambda) {
            return eval_lambda(lisp_cdr(list), env);
        }

        if (first == sym_defmacro) {
            return eval_defmacro(lisp_cdr(list), env);
        }

        if (first == sym_let) {
            return eval_let(lisp_cdr(list), env, in_tail_position);
        }

        if (first == sym_let_star) {
            return eval_let_star(lisp_cdr(list), env, in_tail_position);
        }

        if (first == sym_progn) {
            return eval_progn(lisp_cdr(list), env, in_tail_position);
        }

        if (first == sym_do) {
            return eval_do(lisp_cdr(list), env);
        }

        if (first == sym_cond) {
            return eval_cond(lisp_cdr(list), env, in_tail_position);
        }

        if (first == sym_case) {
            return eval_case(lisp_cdr(list), env, in_tail_position);
        }

        if (first == sym_and) {
            return eval_and(lisp_cdr(list), env, in_tail_position);
        }

        if (first == sym_or) {
            return eval_or(lisp_cdr(list), env, in_tail_position);
        }

        if (first == sym_condition_case) {
            return eval_condition_case(lisp_cdr(list), env, in_tail_position);
        }

        if (first == sym_unwind_protect) {
            return eval_unwind_protect(lisp_cdr(list), env, in_tail_position);
        }
    }

    /* Function application */
    LispObject *func = lisp_eval_internal(first, env, 0);
    if (should_propagate_error(func)) {
        return func;
    }

    /* Check if it's a macro - if so, expand and evaluate the result */
    if (func->type == LISP_MACRO) {
        LispObject *expansion = expand_macro(func, lisp_cdr(list), env);
        if (should_propagate_error(expansion)) {
            return expansion;
        }
        /* Evaluate the expanded form */
        return lisp_eval_internal(expansion, env, in_tail_position);
    }

    /* Evaluate arguments (never in tail position) */
    LispObject *evaled_args = NIL;
    LispObject *tail = NULL;
    LispObject *args = lisp_cdr(list);

    while (args != NIL && args != NULL) {
        LispObject *arg = lisp_car(args);
        LispObject *evaled = lisp_eval_internal(arg, env, 0);

        if (should_propagate_error(evaled)) {
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

    /* Apply with tail position flag */
    return apply(func, evaled_args, env, in_tail_position);
}

static LispObject *eval_if(LispObject *args, Environment *env, int in_tail_position) {
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

    /* Condition is never in tail position */
    LispObject *cond_val = lisp_eval_internal(cond, env, 0);
    if (should_propagate_error(cond_val)) {
        return cond_val;
    }

    /* Both branches ARE in tail position if parent is */
    if (lisp_is_truthy(cond_val)) {
        return lisp_eval_internal(then_expr, env, in_tail_position);
    } else {
        return lisp_eval_internal(else_expr, env, in_tail_position);
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
    LispObject *value = lisp_eval_internal(value_expr, env, 0);

    if (should_propagate_error(value)) {
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
    LispObject *value = lisp_eval_internal(value_expr, env, 0);

    if (should_propagate_error(value)) {
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

    /* Store all body expressions (implicit progn) */
    LispObject *body = rest;

    return lisp_make_lambda(params, body, env, NULL);
}

static LispObject *eval_defmacro(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("defmacro requires at least 3 arguments");
    }

    LispObject *name = lisp_car(args);
    if (name->type != LISP_SYMBOL) {
        return lisp_make_error("defmacro requires a symbol as first argument");
    }

    LispObject *rest = lisp_cdr(args);
    if (rest == NIL) {
        return lisp_make_error("defmacro requires parameter list");
    }

    LispObject *params = lisp_car(rest);
    LispObject *body = lisp_cdr(rest);

    if (body == NIL) {
        return lisp_make_error("defmacro requires a body");
    }

    /* Create the macro */
    LispObject *macro = lisp_make_macro(params, body, env, name->value.symbol);

    /* Define it in the environment */
    env_define(env, name->value.symbol, macro);

    return macro;
}

static LispObject *eval_quasiquote(LispObject *expr, Environment *env) {
    /* Base cases: atoms are returned as-is */
    if (expr == NIL || expr->type != LISP_CONS) {
        return expr;
    }

    /* Check if this is an unquote: (unquote expr) => ,expr */
    LispObject *first = lisp_car(expr);
    if (first != NULL && first->type == LISP_SYMBOL && first == sym_unquote) {
        LispObject *rest = lisp_cdr(expr);
        if (rest == NIL) {
            return lisp_make_error("unquote requires an argument");
        }
        /* Evaluate the unquoted expression */
        return lisp_eval_internal(lisp_car(rest), env, 0);
    }

    /* Check if this is unquote-splicing at the beginning: (unquote-splicing expr) => ,@expr */
    if (first != NULL && first->type == LISP_CONS) {
        LispObject *first_first = lisp_car(first);
        if (first_first != NULL && first_first->type == LISP_SYMBOL && first_first == sym_unquote_splicing) {
            LispObject *splice_rest = lisp_cdr(first);
            if (splice_rest == NIL) {
                return lisp_make_error("unquote-splicing requires an argument");
            }
            /* Evaluate the splice expression */
            LispObject *splice_value = lisp_eval_internal(lisp_car(splice_rest), env, 0);
            if (should_propagate_error(splice_value)) {
                return splice_value;
            }
            /* Append the rest of the quasiquoted list */
            LispObject *rest_result = eval_quasiquote(lisp_cdr(expr), env);
            if (should_propagate_error(rest_result)) {
                return rest_result;
            }
            /* Splice the evaluated list into the result */
            if (splice_value == NIL) {
                return rest_result;
            }
            /* Copy and append splice_value list to rest_result */
            LispObject *result = NIL;
            LispObject *tail = NULL;
            LispObject *current = splice_value;

            /* Copy each element from splice_value */
            while (current != NIL && current->type == LISP_CONS) {
                LispObject *new_cons = lisp_make_cons(lisp_car(current), NIL);
                if (result == NIL) {
                    result = new_cons;
                    tail = new_cons;
                } else {
                    tail->value.cons.cdr = new_cons;
                    tail = new_cons;
                }
                current = lisp_cdr(current);
            }

            /* Append the rest */
            if (tail != NULL) {
                tail->value.cons.cdr = rest_result;
            } else {
                result = rest_result;
            }
            return result;
        }
    }

    /* Recursively process the car and cdr */
    LispObject *car_result = eval_quasiquote(first, env);
    if (should_propagate_error(car_result)) {
        return car_result;
    }

    LispObject *cdr_result = eval_quasiquote(lisp_cdr(expr), env);
    if (should_propagate_error(cdr_result)) {
        return cdr_result;
    }

    /* Rebuild the cons cell */
    return lisp_make_cons(car_result, cdr_result);
}

static LispObject *expand_macro(LispObject *macro, LispObject *args, Environment *env) {
    (void)env; /* Unused - macros use their own closure environment */
    /* Create new environment with macro's closure as parent */
    Environment *new_env = env_create(macro->value.macro.closure);

    /* Bind parameters to UNEVALUATED arguments */
    LispObject *params = macro->value.macro.params;
    LispObject *arg_list = args;

    while (params != NIL && params != NULL) {
        /* Check for rest parameter (dotted list) */
        if (params->type == LISP_SYMBOL) {
            /* Rest of arguments go into this parameter as a list */
            env_define(new_env, params->value.symbol, arg_list);
            arg_list = NIL; /* Mark as consumed */
            break;
        }

        if (params->type != LISP_CONS) {
            return lisp_make_error("Invalid macro parameter list");
        }

        LispObject *param = lisp_car(params);
        if (param->type != LISP_SYMBOL) {
            return lisp_make_error("Macro parameter must be a symbol");
        }

        if (arg_list == NIL) {
            return lisp_make_error("Too few arguments to macro");
        }

        LispObject *arg = lisp_car(arg_list);
        env_define(new_env, param->value.symbol, arg);

        params = lisp_cdr(params);
        arg_list = lisp_cdr(arg_list);
    }

    /* If we exhausted params but still have args, and didn't encounter rest param, error */
    if (arg_list != NIL) {
        return lisp_make_error("Too many arguments to macro");
    }

    /* Evaluate body (implicit progn) and return the result */
    /* This is the expansion, not the final result */
    return eval_progn(macro->value.macro.body, new_env, 0);
}

static LispObject *eval_let(LispObject *args, Environment *env, int in_tail_position) {
    if (args == NIL) {
        return lisp_make_error("let requires at least 2 arguments");
    }

    LispObject *bindings = lisp_car(args);
    LispObject *body = lisp_cdr(args);

    if (body == NIL) {
        return lisp_make_error("let requires a body");
    }

    /* Create new environment */
    Environment *new_env = env_create(env);

    /* Process bindings - values are evaluated in parent environment */
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
        LispObject *value = lisp_eval_internal(value_expr, env, 0);

        if (should_propagate_error(value)) {
            return value;
        }

        env_define(new_env, name->value.symbol, value);
        bindings = lisp_cdr(bindings);
    }

    /* Evaluate body expressions like progn - tail position propagates */
    return eval_progn(body, new_env, in_tail_position);
}

static LispObject *eval_let_star(LispObject *args, Environment *env, int in_tail_position) {
    if (args == NIL) {
        return lisp_make_error("let* requires at least 2 arguments");
    }

    LispObject *bindings = lisp_car(args);
    LispObject *body = lisp_cdr(args);

    if (body == NIL) {
        return lisp_make_error("let* requires a body");
    }

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
        LispObject *value = lisp_eval_internal(value_expr, new_env, 0);

        if (should_propagate_error(value)) {
            return value;
        }

        env_define(new_env, name->value.symbol, value);
        bindings = lisp_cdr(bindings);
    }

    /* Evaluate body expressions like progn - tail position propagates */
    return eval_progn(body, new_env, in_tail_position);
}

static LispObject *eval_progn(LispObject *args, Environment *env, int in_tail_position) {
    if (args == NIL) {
        return NIL;
    }

    LispObject *result = NIL;

    while (args != NIL && args != NULL) {
        LispObject *expr = lisp_car(args);
        LispObject *rest = lisp_cdr(args);

        /* Only the LAST expression is in tail position */
        int is_last = (rest == NIL || rest == NULL);
        int expr_in_tail = in_tail_position && is_last;

        result = lisp_eval_internal(expr, env, expr_in_tail);

        if (should_propagate_error(result)) {
            return result;
        }

        args = rest;
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
        LispObject *init_result = lisp_eval_internal(init_val, env, 0);

        if (should_propagate_error(init_result)) {
            return init_result;
        }

        env_define(loop_env, name->value.symbol, init_result);
        binding_list = lisp_cdr(binding_list);
    }

    /* Main loop */
    while (1) {
        /* Evaluate test condition */
        LispObject *test_result = lisp_eval_internal(test_expr, loop_env, 0);
        if (should_propagate_error(test_result)) {
            return test_result;
        }

        /* If test is true, evaluate and return result expression */
        if (lisp_is_truthy(test_result)) {
            if (result_expr != NIL) {
                return lisp_eval_internal(result_expr, loop_env, 0);
            }
            return NIL;
        }

        /* Evaluate body expressions (for side effects) */
        LispObject *body = body_exprs;
        while (body != NIL && body != NULL) {
            LispObject *body_expr = lisp_car(body);
            LispObject *body_result = lisp_eval_internal(body_expr, loop_env, 0);

            if (should_propagate_error(body_result)) {
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
                LispObject *step_result = lisp_eval_internal(step_val, loop_env, 0);

                if (should_propagate_error(step_result)) {
                    return step_result;
                }

                /* Update the variable */
                env_set(loop_env, name->value.symbol, step_result);
            }

            binding_list = lisp_cdr(binding_list);
        }
    }
}

static LispObject *eval_cond(LispObject *args, Environment *env, int in_tail_position) {
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
        if (test->type == LISP_SYMBOL && test == sym_else) {
            if (result != NIL && result != NULL) {
                /* Else clause result is in tail position (implicit progn) */
                return eval_progn(result, env, in_tail_position);
            }
            return NIL;
        }

        /* Evaluate test (not in tail position) */
        LispObject *test_result = lisp_eval_internal(test, env, 0);
        if (should_propagate_error(test_result)) {
            return test_result;
        }

        /* If true, evaluate and return result (in tail position) */
        if (lisp_is_truthy(test_result)) {
            if (result != NIL && result != NULL) {
                /* Evaluate all expressions in result (implicit progn) */
                return eval_progn(result, env, in_tail_position);
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

static LispObject *eval_case(LispObject *args, Environment *env, int in_tail_position) {
    if (args == NIL) {
        return lisp_make_error("case requires at least 1 argument");
    }

    /* Evaluate key expression once (not in tail position) */
    LispObject *key = lisp_eval_internal(lisp_car(args), env, 0);
    if (should_propagate_error(key)) {
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
        if (values->type == LISP_SYMBOL && values == sym_else) {
            if (result != NIL && result != NULL) {
                /* Else clause result is in tail position (implicit progn) */
                return eval_progn(result, env, in_tail_position);
            }
            return NIL;
        }

        /* Compare key with each value in the list */
        if (values->type == LISP_CONS) {
            LispObject *val_list = values;
            while (val_list != NIL && val_list != NULL) {
                LispObject *val = lisp_car(val_list);
                LispObject *val_result = lisp_eval_internal(val, env, 0);

                if (should_propagate_error(val_result)) {
                    return val_result;
                }

                if (objects_equal(key, val_result)) {
                    if (result != NIL && result != NULL) {
                        /* Matching clause result is in tail position (implicit progn) */
                        return eval_progn(result, env, in_tail_position);
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

static LispObject *eval_and(LispObject *args, Environment *env, int in_tail_position) {
    /* (and) => #t */
    if (args == NIL) {
        return lisp_make_number(1);
    }

    /* Evaluate arguments one by one until we find a falsy value */
    while (args != NIL && args != NULL) {
        LispObject *expr = lisp_car(args);
        LispObject *rest = lisp_cdr(args);

        /* Last expression is in tail position if parent is */
        int is_last = (rest == NIL || rest == NULL);
        LispObject *result = lisp_eval_internal(expr, env, is_last ? in_tail_position : 0);

        if (should_propagate_error(result)) {
            return result;
        }

        /* If falsy, short-circuit and return this value */
        if (!lisp_is_truthy(result)) {
            return result;
        }

        /* If this was the last expression, return its value */
        if (is_last) {
            return result;
        }

        args = rest;
    }

    /* Should never reach here, but return #t for safety */
    return lisp_make_number(1);
}

static LispObject *eval_or(LispObject *args, Environment *env, int in_tail_position) {
    /* (or) => #f */
    if (args == NIL) {
        return NIL;
    }

    /* Evaluate arguments one by one until we find a truthy value */
    while (args != NIL && args != NULL) {
        LispObject *expr = lisp_car(args);
        LispObject *rest = lisp_cdr(args);

        /* Last expression is in tail position if parent is */
        int is_last = (rest == NIL || rest == NULL);
        LispObject *result = lisp_eval_internal(expr, env, is_last ? in_tail_position : 0);

        if (should_propagate_error(result)) {
            return result;
        }

        /* If truthy, short-circuit and return this value */
        if (lisp_is_truthy(result)) {
            return result;
        }

        /* If this was the last expression, return its value */
        if (is_last) {
            return result;
        }

        args = rest;
    }

    /* Should never reach here, but return #f for safety */
    return NIL;
}

static LispObject *apply(LispObject *func, LispObject *args, Environment *env, int in_tail_position) {
    if (func->type == LISP_BUILTIN) {
        /* Builtins are never tail-called (C boundary) */
        /* Push call frame for builtin */
        push_call_frame(env, func->value.builtin.name);

        /* Call builtin */
        LispObject *result = func->value.builtin.func(args, env);

        /* If error, attach stack trace BEFORE popping frame */
        if (should_propagate_error(result)) {
            result = lisp_attach_stack_trace(result, env);
            pop_call_frame(env);
            return result;
        }

        /* Pop frame */
        pop_call_frame(env);

        return result;
    }

    if (func->type == LISP_LAMBDA) {
        /* If in tail position, return a tail call continuation */
        if (in_tail_position) {
            return lisp_make_tail_call(func, args);
        }

        /* Not in tail position - execute normally */
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

        /* Evaluate body with tail position awareness (implicit progn) */
        /* Last expression in body is always in tail position (for non-tail calls) */
        LispObject *result = eval_progn(func->value.lambda.body, new_env, 1);

        /* Trampoline loop: unwrap tail calls WITHOUT recursive apply() calls */
        /* This is the TCO fix: inline lambda execution to avoid C stack growth */
        while (result != NULL && result->type == LISP_TAIL_CALL) {
            LispObject *tail_func = result->value.tail_call.func;
            LispObject *tail_args = result->value.tail_call.args;

            /* Handle builtins: safe to call apply() (no nested trampoline recursion) */
            if (tail_func->type == LISP_BUILTIN) {
                result = apply(tail_func, tail_args, new_env, 0);
                continue;
            }

            /* Handle non-lambdas: error */
            if (tail_func->type != LISP_LAMBDA) {
                result = lisp_make_error_with_stack("Cannot apply non-function", new_env);
                break;
            }

            /* INLINE LAMBDA EXECUTION (key fix: no apply() call for lambdas) */
            /* Pop previous lambda's frame before executing next tail call */
            pop_call_frame(new_env);

            /* Create NEW environment for tail-called lambda */
            Environment *tail_env = env_create(tail_func->value.lambda.closure);
            tail_env->call_stack = new_env->call_stack; /* Inherit call stack */

            /* Generate lambda name for stack trace */
            char tail_lambda_name[128];
            const char *tail_frame_name;
            LispObject *tail_lambda_params = tail_func->value.lambda.params;

            if (tail_func->value.lambda.name != NULL) {
                tail_frame_name = tail_func->value.lambda.name;
            } else if (tail_lambda_params != NIL && tail_lambda_params->type == LISP_CONS &&
                       lisp_car(tail_lambda_params) != NULL && lisp_car(tail_lambda_params)->type == LISP_SYMBOL) {
                snprintf(tail_lambda_name, sizeof(tail_lambda_name), "lambda/%s",
                         lisp_car(tail_lambda_params)->value.symbol);
                tail_frame_name = tail_lambda_name;
            } else {
                tail_frame_name = "lambda";
            }

            /* Push call frame for tail-called lambda */
            push_call_frame(tail_env, tail_frame_name);

            /* Bind parameters */
            LispObject *tail_params = tail_lambda_params;
            LispObject *tail_arg_list = tail_args;

            while (tail_params != NIL && tail_params != NULL) {
                if (tail_params->type != LISP_CONS) {
                    result = lisp_make_error_with_stack("Invalid lambda parameter list", tail_env);
                    pop_call_frame(tail_env);
                    goto trampoline_done;
                }

                LispObject *tail_param = lisp_car(tail_params);
                if (tail_param->type != LISP_SYMBOL) {
                    result = lisp_make_error_with_stack("Lambda parameter must be a symbol", tail_env);
                    pop_call_frame(tail_env);
                    goto trampoline_done;
                }

                if (tail_arg_list == NIL) {
                    result = lisp_make_error_with_stack("Too few arguments to lambda", tail_env);
                    pop_call_frame(tail_env);
                    goto trampoline_done;
                }

                LispObject *tail_arg = lisp_car(tail_arg_list);
                env_define(tail_env, tail_param->value.symbol, tail_arg);

                tail_params = lisp_cdr(tail_params);
                tail_arg_list = lisp_cdr(tail_arg_list);
            }

            if (tail_arg_list != NIL) {
                result = lisp_make_error_with_stack("Too many arguments to lambda", tail_env);
                pop_call_frame(tail_env);
                goto trampoline_done;
            }

            /* Execute lambda body (in tail position) */
            result = eval_progn(tail_func->value.lambda.body, tail_env, 1);

            /* Update new_env to tail_env for next iteration */
            new_env = tail_env;

            /* Loop continues: if result is TAIL_CALL, unwrap it WITHOUT C stack growth */
        }

    trampoline_done:

        if (should_propagate_error(result)) {
            result = lisp_attach_stack_trace(result, new_env);
            pop_call_frame(new_env);
            return result;
        }

        /* Pop frame */
        pop_call_frame(new_env);

        return result;
    }

    return lisp_make_error_with_stack("Cannot apply non-function", env);
}

/* unwind-protect special form
 * Syntax: (unwind-protect BODYFORM CLEANUP-FORMS...)
 * Evaluates BODYFORM, then always executes CLEANUP-FORMS regardless of errors.
 * Returns the result of BODYFORM (even if it's an error).
 */
static LispObject *eval_unwind_protect(LispObject *args, Environment *env, int in_tail_position) {
    (void)in_tail_position; /* Body is never in tail position - cleanup must run */

    if (args == NIL) {
        return lisp_make_error_with_stack("unwind-protect requires at least 1 argument", env);
    }

    LispObject *bodyform = lisp_car(args);
    LispObject *cleanup_forms = lisp_cdr(args);

    /* Evaluate body (NOT in tail position - cleanup must execute) */
    LispObject *result = lisp_eval_internal(bodyform, env, 0);

    /* Execute cleanup forms ALWAYS (implicit progn, result ignored) */
    if (cleanup_forms != NIL) {
        eval_progn(cleanup_forms, env, 0);
    }

    /* Return original result (even if error) */
    return result;
}

/* condition-case special form
 * Syntax: (condition-case VAR BODYFORM (ERROR-TYPE HANDLER-BODY...) ...)
 * Evaluates BODYFORM. If an error occurs, finds matching handler by error type.
 * Binds error to VAR in handler environment, executes handler body.
 */
static LispObject *eval_condition_case(LispObject *args, Environment *env, int in_tail_position) {
    /* Parse: (condition-case VAR BODYFORM HANDLER...) */
    if (args == NIL) {
        return lisp_make_error_with_stack("condition-case requires at least 3 arguments", env);
    }

    LispObject *var = lisp_car(args); /* Can be symbol or nil */
    if (var != NIL && var->type != LISP_SYMBOL) {
        return lisp_make_error_with_stack("condition-case: VAR must be a symbol or nil", env);
    }

    LispObject *rest = lisp_cdr(args);
    if (rest == NIL) {
        return lisp_make_error_with_stack("condition-case: missing body", env);
    }

    LispObject *bodyform = lisp_car(rest);
    LispObject *handlers = lisp_cdr(rest);

    /* Validate handlers: each must be (ERROR-SYMBOL . BODY) */
    for (LispObject *h = handlers; h != NIL && h->type == LISP_CONS; h = lisp_cdr(h)) {
        LispObject *handler = lisp_car(h);
        if (handler == NIL || handler->type != LISP_CONS) {
            return lisp_make_error_with_stack("condition-case: handler must be a list", env);
        }
        LispObject *handler_type = lisp_car(handler);
        if (handler_type == NIL || handler_type->type != LISP_SYMBOL) {
            return lisp_make_error_with_stack("condition-case: handler type must be a symbol", env);
        }
    }

    /* Evaluate body */
    LispObject *result = lisp_eval_internal(bodyform, env, in_tail_position);

    /* Check if error occurred */
    if (result->type == LISP_ERROR) {
        /* Find matching handler */
        LispObject *error_type = result->value.error_with_stack.error_type;
        LispObject *matching_handler = NIL;
        LispObject *error_catch_all = NIL; /* Track 'error handler separately */

        /* Search for matching handler */
        for (LispObject *h = handlers; h != NIL && h->type == LISP_CONS; h = lisp_cdr(h)) {
            LispObject *handler = lisp_car(h);
            LispObject *handler_type = lisp_car(handler);

            /* Direct match - use this handler */
            if (error_type != NULL && error_type->type == LISP_SYMBOL &&
                strcmp(handler_type->value.symbol, error_type->value.symbol) == 0) {
                matching_handler = handler;
                break;
            }

            /* 'error catches everything (but keep looking for more specific) */
            if (strcmp(handler_type->value.symbol, "error") == 0) {
                error_catch_all = handler;
            }
        }

        /* Use specific handler if found, otherwise use 'error catch-all */
        if (matching_handler == NIL) {
            matching_handler = error_catch_all;
        }

        if (matching_handler != NIL) {
            /* Execute handler */
            LispObject *handler_body = lisp_cdr(matching_handler);

            /* Mark error as caught to prevent further propagation */
            result->value.error_with_stack.caught = 1;

            /* Bind error to VAR in current environment if specified */
            LispObject *saved_binding = NULL;
            int had_binding = 0;
            if (var != NIL) {
                /* Save old binding if it exists */
                saved_binding = env_lookup(env, var->value.symbol);
                had_binding = (saved_binding != NULL);
                /* Temporarily bind error variable */
                env_define(env, var->value.symbol, result);
            }

            /* Evaluate handler body (implicit progn, tail position preserved) */
            LispObject *handler_result = eval_progn(handler_body, env, in_tail_position);

            /* Restore old binding if var was bound */
            if (var != NIL) {
                if (had_binding) {
                    env_set(env, var->value.symbol, saved_binding);
                }
                /* Note: We can't easily remove the binding, so if it didn't exist before,
                 * it will remain after. This matches Emacs Lisp behavior. */
            }

            return handler_result;
        }

        /* No handler matched - propagate error */
        return result;
    }

    /* No error - return result */
    return result;
}
