#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>

Environment *env_create(Environment *parent) {
    Environment *env = GC_malloc(sizeof(Environment));
    env->bindings = NULL;
    env->parent = parent;
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

/* Forward declaration for builtin registration */
void register_builtins(Environment *env);

Environment *env_create_global(void) {
    Environment *env = env_create(NULL);
    register_builtins(env);
    return env;
}
