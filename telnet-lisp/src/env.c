#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#endif

/* Global profiling state */
ProfileState g_profile_state = {0, NULL, 0};

/* Get current time in nanoseconds */
uint64_t profile_get_time_ns(void) {
#ifdef _WIN32
    static LARGE_INTEGER freq = {0};
    if (freq.QuadPart == 0) {
        QueryPerformanceFrequency(&freq);
    }
    LARGE_INTEGER counter;
    QueryPerformanceCounter(&counter);
    return (uint64_t)(counter.QuadPart * 1000000000ULL / freq.QuadPart);
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
#endif
}

/* Find or create profile entry for function */
static ProfileEntry *profile_find_or_create(const char *function_name) {
    ProfileEntry *entry = g_profile_state.entries;
    while (entry != NULL) {
        if (strcmp(entry->function_name, function_name) == 0) {
            return entry;
        }
        entry = entry->next;
    }
    /* Create new entry */
    entry = GC_malloc(sizeof(ProfileEntry));
    entry->function_name = GC_strdup(function_name);
    entry->call_count = 0;
    entry->total_time_ns = 0;
    entry->next = g_profile_state.entries;
    g_profile_state.entries = entry;
    return entry;
}

/* Record a function call with elapsed time */
void profile_record(const char *function_name, uint64_t elapsed_ns) {
    ProfileEntry *entry = profile_find_or_create(function_name);
    entry->call_count++;
    entry->total_time_ns += elapsed_ns;
}

/* Reset profiling data */
void profile_reset(void) {
    g_profile_state.entries = NULL;
    g_profile_state.start_time_ns = 0;
}

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
    frame->entry_time_ns = 0;

    if (g_profile_state.enabled) {
        frame->entry_time_ns = profile_get_time_ns();
    }

    env->call_stack = frame;
}

void pop_call_frame(Environment *env) {
    if (env->call_stack != NULL) {
        if (g_profile_state.enabled && env->call_stack->entry_time_ns != 0) {
            uint64_t elapsed = profile_get_time_ns() - env->call_stack->entry_time_ns;
            profile_record(env->call_stack->function_name, elapsed);
        }
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

/* Standard library macros (embedded to avoid file dependency) */
static const char *stdlib_code =
    ";; defun macro\n"
    "(defmacro defun (name params . body) `(define ,name (lambda ,params ,@body)))\n"
    "\n"
    ";; when - execute body when condition is true\n"
    "(defmacro when (condition . body)\n"
    "  \"Execute BODY when CONDITION is true.\"\n"
    "  `(if ,condition (progn ,@body) nil))\n"
    "\n"
    ";; unless - execute body when condition is false\n"
    "(defmacro unless (condition . body)\n"
    "  \"Execute BODY when CONDITION is false.\"\n"
    "  `(if ,condition nil (progn ,@body)))\n"
    "\n"
    ";; defvar - define variable only if unbound\n"
    ";; Usage: (defvar name) or (defvar name value) or (defvar name value docstring)\n"
    "(defmacro defvar (name . rest)\n"
    "  \"Define NAME as a variable. Only sets value if NAME is not already bound.\"\n"
    "  (let ((value (if (null? rest) nil (car rest)))\n"
    "        (docstring (if (or (null? rest) (null? (cdr rest))) nil (car (cdr rest)))))\n"
    "    `(progn\n"
    "       (unless (bound? ',name)\n"
    "         (define ,name ,value))\n"
    "       ,(if docstring\n"
    "            `(set-documentation! ',name ,docstring)\n"
    "            nil)\n"
    "       ',name)))\n"
    "\n"
    ";; defconst - define constant (always sets value)\n"
    ";; Usage: (defconst name value) or (defconst name value docstring)\n"
    "(defmacro defconst (name value . rest)\n"
    "  \"Define NAME as a constant. Always sets value (unlike defvar).\"\n"
    "  (let ((docstring (if (null? rest) nil (car rest))))\n"
    "    `(progn\n"
    "       (define ,name ,value)\n"
    "       ,(if docstring\n"
    "            `(set-documentation! ',name ,docstring)\n"
    "            nil)\n"
    "       ',name)))\n"
    "\n"
    ";; defalias - create alias for function\n"
    ";; Usage: (defalias alias target) or (defalias alias target docstring)\n"
    "(defmacro defalias (alias target . rest)\n"
    "  \"Define ALIAS as an alias for TARGET function.\"\n"
    "  (let ((docstring (if (null? rest) nil (car rest))))\n"
    "    `(progn\n"
    "       (define ,alias ,target)\n"
    "       ,(if docstring\n"
    "            `(set-documentation! ',alias ,docstring)\n"
    "            nil)\n"
    "       ',alias)))\n"
    "\n"
    ";; Short aliases\n"
    "(defalias doc documentation \"Shorthand for `documentation`.\")\n"
    "(defalias doc-set! set-documentation! \"Shorthand for `set-documentation!`.\")\n"
    "(defalias string-append concat \"Alias for `concat`.\")\n"
    "(defalias string-split split \"Alias for `split`.\")\n"
    "(defalias string-join join \"Alias for `join`.\")\n"
    "(defalias string-length length \"Alias for `length` (also works on lists and vectors).\")\n";

/* Helper to load stdlib from embedded string */
static int load_stdlib(Environment *env) {
    const char *input = stdlib_code;
    while (*input) {
        /* Skip whitespace and comments */
        while (*input && (*input == ' ' || *input == '\t' || *input == '\n' || *input == '\r')) {
            input++;
        }
        if (*input == ';') {
            while (*input && *input != '\n')
                input++;
            continue;
        }
        if (!*input)
            break;

        LispObject *expr = lisp_read(&input);
        if (expr == NULL)
            break;
        if (expr->type == LISP_ERROR) {
            return 0; /* Parse error */
        }

        LispObject *result = lisp_eval(expr, env);
        if (result != NULL && result->type == LISP_ERROR) {
            return 0; /* Eval error */
        }
    }
    return 1; /* Success */
}

Environment *env_create_global(void) {
    Environment *env = env_create(NULL);
    register_builtins(env);

    /* Load standard library (defun, defvar, defconst, defalias, aliases) */
    if (!load_stdlib(env)) {
        return NULL;
    }

    return env;
}
