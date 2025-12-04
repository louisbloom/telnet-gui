#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>

Environment *env_create(Environment *parent) {
    Environment *env = GC_malloc(sizeof(Environment));
    env->bindings = NULL;
    env->parent = parent;
    env->call_stack = NULL;
    env->handler_stack = NULL;
    return env;
}

void env_define(Environment *env, const char *name, LispObject *value) {
    /* Check if binding already exists */
    struct Binding *binding = env->bindings;
    while (binding != NULL) {
        if (strcmp(binding->name, name) == 0) {
            binding->value = value;
            return;
        }
        binding = binding->next;
    }

    /* Create new binding */
    binding = GC_malloc(sizeof(struct Binding));
    binding->name = GC_strdup(name);
    binding->value = value;
    binding->next = env->bindings;
    env->bindings = binding;
}

LispObject *env_lookup(Environment *env, const char *name) {
    while (env != NULL) {
        struct Binding *binding = env->bindings;
        while (binding != NULL) {
            if (strcmp(binding->name, name) == 0) {
                return binding->value;
            }
            binding = binding->next;
        }
        env = env->parent;
    }
    return NULL;
}

int env_set(Environment *env, const char *name, LispObject *value) {
    /* Look for binding in current or parent environments */
    while (env != NULL) {
        struct Binding *binding = env->bindings;
        while (binding != NULL) {
            if (strcmp(binding->name, name) == 0) {
                binding->value = value;
                return 1; /* Successfully updated */
            }
            binding = binding->next;
        }
        env = env->parent;
    }
    return 0; /* Variable not found */
}

void env_free(Environment *env) {
    /* GC handles cleanup automatically */
    /* We don't need to free individual bindings or the environment */
    (void)env; /* Suppress unused parameter warning */
}

/* Call stack functions */
void push_call_frame(Environment *env, const char *function_name) {
    CallStackFrame *frame = GC_malloc(sizeof(CallStackFrame));
    frame->function_name = GC_strdup(function_name);
    frame->parent = env->call_stack;
    env->call_stack = frame;
}

void pop_call_frame(Environment *env) {
    if (env->call_stack != NULL) {
        env->call_stack = env->call_stack->parent;
    }
}

LispObject *capture_call_stack(Environment *env) {
    LispObject *stack = NIL;
    CallStackFrame *frame = env->call_stack;
    int depth = 0;
    const int MAX_STACK_DEPTH = 20;

    /* Traverse and collect frames */
    while (frame != NULL && depth < MAX_STACK_DEPTH) {
        stack = lisp_make_cons(lisp_make_string(frame->function_name), stack);
        frame = frame->parent;
        depth++;
    }

    /* Reverse the list to get chronological order */
    LispObject *reversed = NIL;
    LispObject *current = stack;
    while (current != NIL && current->type == LISP_CONS) {
        reversed = lisp_make_cons(lisp_car(current), reversed);
        current = lisp_cdr(current);
    }

    return reversed;
}

/* Forward declaration for builtin registration */
void register_builtins(Environment *env);

Environment *env_create_global(void) {
    Environment *env = env_create(NULL);
    register_builtins(env);

    /* Define the defun macro using defmacro now that we have quasiquote support */
    /* (defmacro defun (name params . body) `(define ,name (lambda ,params ,@body))) */
    const char *defun_def = "(defmacro defun (name params . body) `(define ,name (lambda ,params ,@body)))";
    const char *input = defun_def;
    LispObject *defun_expr = lisp_read(&input);
    if (defun_expr != NULL && defun_expr->type != LISP_ERROR) {
        LispObject *result = lisp_eval(defun_expr, env);
        if (result->type == LISP_ERROR) {
            /* Fallback: if quasiquote fails, we can't continue */
            return NULL;
        }
    }

    return env;
}
