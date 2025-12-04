#ifndef LISP_H
#define LISP_H

#include <stdio.h>
#include <stddef.h>
#include <gc.h>

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>
#include "utf8.h"

/* Forward declarations */
typedef struct LispObject LispObject;
typedef struct Environment Environment;
typedef struct CallStackFrame CallStackFrame;
typedef struct HandlerContext HandlerContext;

/* Call stack frame structure */
struct CallStackFrame {
    char *function_name;
    CallStackFrame *parent;
};

/* Handler context for condition-case */
struct HandlerContext {
    LispObject *handlers;        /* Assoc list: ((ERROR-SYMBOL . HANDLER-BODY) ...) */
    LispObject *error_var_name;  /* Symbol: variable to bind error (or NIL) */
    Environment *handler_env;    /* Environment for handler evaluation */
    HandlerContext *parent;      /* Previous handler context */
};

/* Object types */
typedef enum {
    LISP_NIL,
    LISP_NUMBER,
    LISP_INTEGER,
    LISP_STRING,
    LISP_SYMBOL,
    LISP_BOOLEAN,
    LISP_CONS,
    LISP_BUILTIN,
    LISP_LAMBDA,
    LISP_MACRO,
    LISP_ERROR,
    LISP_FILE_STREAM,
    LISP_VECTOR,
    LISP_HASH_TABLE,
    LISP_TAIL_CALL
} LispType;

/* Built-in function pointer type */
typedef LispObject *(*BuiltinFunc)(LispObject *args, Environment *env);

/* Lisp object structure */
struct LispObject {
    LispType type;
    union {
        double number;
        long long integer;
        char *string;
        char *symbol;
        int boolean;
        struct {
            LispObject *car;
            LispObject *cdr;
        } cons;
        struct {
            BuiltinFunc func;
            const char *name;
        } builtin;
        struct {
            LispObject *params;
            LispObject *body;
            Environment *closure;
            char *name; /* Optional function name for debugging */
        } lambda;
        struct {
            LispObject *params;
            LispObject *body;
            Environment *closure;
            char *name; /* Optional macro name for debugging */
        } macro;
        char *error;
        FILE *file;
        struct {
            LispObject **items;
            size_t size;
            size_t capacity;
        } vector;
        struct {
            void *buckets; // Array of hash entry lists
            size_t bucket_count;
            size_t entry_count;
            size_t capacity;
        } hash_table;
        struct {
            LispObject *error_type;     /* Symbol: 'error, 'division-by-zero, etc. */
            char *message;              /* Human-readable error message */
            LispObject *data;           /* Optional arbitrary data (can be NIL) */
            LispObject *stack_trace;    /* List of function names */
            int caught;                  /* Flag: if true, error won't propagate (caught by condition-case) */
        } error_with_stack;
        struct {
            LispObject *func;  /* Function to call in tail position */
            LispObject *args;  /* Already-evaluated arguments */
        } tail_call;
    } value;
};

/* Environment structure for variable bindings */
struct Environment {
    struct Binding {
        char *name;
        LispObject *value;
        struct Binding *next;
    } *bindings;
    Environment *parent;
    CallStackFrame *call_stack; /* Current call stack */
    HandlerContext *handler_stack; /* Active condition-case handlers */
};

/* Global NIL object */
extern LispObject *NIL;

/* Simple API */
int lisp_init(void);
LispObject *lisp_eval_string(const char *code, Environment *env);
void lisp_cleanup(void);

/* Advanced API */
LispObject *lisp_read(const char **input);
LispObject *lisp_eval(LispObject *expr, Environment *env);
char *lisp_print(LispObject *obj);
void lisp_princ(LispObject *obj);
void lisp_prin1(LispObject *obj);
void lisp_print_cl(LispObject *obj);
LispObject *lisp_load_file(const char *filename, Environment *env);

/* Object creation */
LispObject *lisp_make_number(double value);
LispObject *lisp_make_integer(long long value);
LispObject *lisp_make_string(const char *value);
LispObject *lisp_make_symbol(const char *name);
LispObject *lisp_make_boolean(int value);
LispObject *lisp_make_cons(LispObject *car, LispObject *cdr);
LispObject *lisp_make_error(const char *message);
LispObject *lisp_make_builtin(BuiltinFunc func, const char *name);
LispObject *lisp_make_lambda(LispObject *params, LispObject *body, Environment *closure, const char *name);
LispObject *lisp_make_macro(LispObject *params, LispObject *body, Environment *closure, const char *name);
LispObject *lisp_make_file_stream(FILE *file);
LispObject *lisp_make_vector(size_t capacity);
LispObject *lisp_make_hash_table(void);
LispObject *lisp_make_tail_call(LispObject *func, LispObject *args);

/* Hash table entry structure */
struct HashEntry {
    char *key;
    LispObject *value;
    struct HashEntry *next;
};

/* Hash table operations */
struct HashEntry *hash_table_get_entry(LispObject *table, const char *key);
struct HashEntry *hash_table_set_entry(LispObject *table, const char *key, LispObject *value);
int hash_table_remove_entry(LispObject *table, const char *key);
void hash_table_clear(LispObject *table);

/* Object utilities */
int lisp_is_truthy(LispObject *obj);
int lisp_is_list(LispObject *obj);
size_t lisp_list_length(LispObject *list);

/* Environment functions */
Environment *env_create(Environment *parent);
void env_define(Environment *env, const char *name, LispObject *value);
LispObject *env_lookup(Environment *env, const char *name);
int env_set(Environment *env, const char *name, LispObject *value);
void env_free(Environment *env);
Environment *env_create_global(void);

/* Call stack functions */
void push_call_frame(Environment *env, const char *function_name);
void pop_call_frame(Environment *env);
LispObject *capture_call_stack(Environment *env);
LispObject *lisp_make_error_with_stack(const char *message, Environment *env);
LispObject *lisp_attach_stack_trace(LispObject *error, Environment *env);
LispObject *lisp_make_typed_error(LispObject *error_type, const char *message, LispObject *data, Environment *env);
LispObject *lisp_make_typed_error_simple(const char *error_type_name, const char *message, Environment *env);

/* List utilities */
LispObject *lisp_car(LispObject *obj);
LispObject *lisp_cdr(LispObject *obj);

/* Regex helper functions */
pcre2_code *compile_regex_pattern(const char *pattern, char **error_msg);
pcre2_match_data *execute_regex(pcre2_code *re, const char *subject);
char *extract_capture(pcre2_match_data *match_data, const char *subject, int capture_num);
int get_capture_count(pcre2_code *re);
void free_regex_resources(pcre2_code *re, pcre2_match_data *match_data);

#endif /* LISP_H */
