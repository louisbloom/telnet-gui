#ifndef LISP_H
#define LISP_H

#include <stdio.h>
#include <stddef.h>
#include <gc.h>

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

/* Forward declarations */
typedef struct LispObject LispObject;
typedef struct Environment Environment;

/* Object types */
typedef enum {
    LISP_NIL,
    LISP_NUMBER,
    LISP_STRING,
    LISP_SYMBOL,
    LISP_CONS,
    LISP_BUILTIN,
    LISP_LAMBDA,
    LISP_ERROR
} LispType;

/* Built-in function pointer type */
typedef LispObject* (*BuiltinFunc)(LispObject* args, Environment* env);

/* LISP object structure */
struct LispObject {
    LispType type;
    union {
        double number;
        char* string;
        char* symbol;
        struct {
            LispObject* car;
            LispObject* cdr;
        } cons;
        struct {
            BuiltinFunc func;
            const char* name;
        } builtin;
        struct {
            LispObject* params;
            LispObject* body;
            Environment* closure;
        } lambda;
        char* error;
    } value;
};

/* Environment structure for variable bindings */
struct Environment {
    struct Binding {
        char* name;
        LispObject* value;
        struct Binding* next;
    }* bindings;
    Environment* parent;
};

/* Global NIL object */
extern LispObject* NIL;

/* Simple API */
int lisp_init(void);
LispObject* lisp_eval_string(const char* code, Environment* env);
void lisp_cleanup(void);

/* Advanced API */
LispObject* lisp_read(const char** input);
LispObject* lisp_eval(LispObject* expr, Environment* env);
char* lisp_print(LispObject* obj);
LispObject* lisp_load_file(const char* filename, Environment* env);

/* Object creation */
LispObject* lisp_make_number(double value);
LispObject* lisp_make_string(const char* value);
LispObject* lisp_make_symbol(const char* name);
LispObject* lisp_make_cons(LispObject* car, LispObject* cdr);
LispObject* lisp_make_error(const char* message);
LispObject* lisp_make_builtin(BuiltinFunc func, const char* name);
LispObject* lisp_make_lambda(LispObject* params, LispObject* body, Environment* closure);

/* Object utilities */
int lisp_is_truthy(LispObject* obj);
int lisp_is_list(LispObject* obj);
size_t lisp_list_length(LispObject* list);

/* Environment functions */
Environment* env_create(Environment* parent);
void env_define(Environment* env, const char* name, LispObject* value);
LispObject* env_lookup(Environment* env, const char* name);
void env_free(Environment* env);
Environment* env_create_global(void);

/* List utilities */
LispObject* lisp_car(LispObject* obj);
LispObject* lisp_cdr(LispObject* obj);

/* Regex helper functions */
pcre2_code* compile_regex_pattern(const char* pattern, char** error_msg);
pcre2_match_data* execute_regex(pcre2_code* re, const char* subject);
char* extract_capture(pcre2_match_data* match_data, const char* subject, int capture_num);
int get_capture_count(pcre2_code* re);
void free_regex_resources(pcre2_code* re, pcre2_match_data* match_data);

#endif /* LISP_H */
