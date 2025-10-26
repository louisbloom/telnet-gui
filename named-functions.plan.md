# Add Named Function Support (Option 1)

## Overview

Add an optional name field to lambda objects so that functions defined with `define` can show their actual names in stack traces and error messages, improving debugging experience.

## Current State

- Lambda objects have `params`, `body`, and `closure` fields
- When `(define foo (lambda (x) ...))` is evaluated, the lambda has no knowledge of "foo"
- Stack traces show generic "lambda" or "lambda/x" instead of "foo"
- No way to identify which function caused an error in nested calls

## Goals

1. Add optional `name` field to lambda struct
2. When `define` binds a lambda to a symbol, attach that symbol name to the lambda
3. Use the lambda name in stack traces for better debugging
4. Maintain backward compatibility with anonymous lambdas
5. Lay groundwork for potential `defun` special form

## Implementation Strategy

### 1. Update Lambda Structure

**Modify `include/lisp.h`:**

- Add `char *name` field to the lambda struct in `LispObject`
- Update `lisp_make_lambda` declaration to accept optional name parameter

```c
struct {
    LispObject *params;
    LispObject *body;
    Environment *closure;
    char *name;  // NEW: Optional function name for debugging
} lambda;
```

### 2. Update Lambda Creation

**Modify `src/lisp.c`:**

- Update `lisp_make_lambda` to accept `const char *name` parameter
- Store the name (or NULL for anonymous lambdas) in the lambda object
- Use `GC_strdup` if name is provided

```c
LispObject *lisp_make_lambda(LispObject *params, LispObject *body,
                             Environment *closure, const char *name)
```

### 3. Attach Names During Define

**Modify `src/eval.c` in `eval_define`:**

- After evaluating the value expression, check if it's a lambda
- If it is a lambda and doesn't have a name yet, attach the symbol name
- This handles: `(define foo (lambda (x) ...))`

**Modify `src/eval.c` in `eval_lambda`:**

- Pass NULL as the name parameter when creating anonymous lambdas
- Keep lambdas anonymous by default

### 4. Use Names in Stack Traces

**Modify `src/eval.c` in `apply()`:**

- When pushing a call frame for a lambda, check if `func->value.lambda.name` exists
- If name exists, use it: `push_call_frame(new_env, func->value.lambda.name)`
- Otherwise fall back to current behavior: "lambda" or "lambda/param"

Priority order for lambda frame names:

1. Lambda's stored name (e.g., "foo")
2. Lambda with first param (e.g., "lambda/x")
3. Generic "lambda"

### 5. Update Lambda Printing (Optional Enhancement)

**Modify `src/print.c`:**

- When printing a lambda, show its name if available
- Change from `#<lambda>` to `#<lambda:foo>` for named functions
- Helps with debugging in REPL

### 6. Testing

**Create `examples/named_functions.lisp`:**

```lisp
; Named function
(define factorial (lambda (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1))))))

; Test error with named function
(define bad-func (lambda () (undefined-var)))
(bad-func)  ; Should show "bad-func" in stack trace

; Nested named functions
(define outer (lambda () (middle)))
(define middle (lambda () (inner)))
(define inner (lambda () (error-here)))
(outer)  ; Should show: outer -> middle -> inner

; Anonymous lambda (no name)
((lambda (x) (* x x)) 5)  ; Should still show "lambda"
```

**Test stack traces:**

- Verify named functions show their actual names
- Verify anonymous lambdas still work
- Verify nested function calls show correct call chain
- Verify `set!` doesn't rename already-named functions

## Key Files to Modify

1. `include/lisp.h` - Add name field to lambda struct, update function declaration
2. `src/lisp.c` - Update `lisp_make_lambda` to accept and store name
3. `src/eval.c` - Attach names in `eval_define`, use names in `apply()`, pass NULL in `eval_lambda`
4. `src/print.c` - (Optional) Show lambda names when printing
5. `examples/named_functions.lisp` - Test cases and examples

## Edge Cases to Handle

1. **Anonymous lambdas**: `((lambda (x) x) 5)` - should still work, name is NULL
2. **Reassignment**: `(define f (lambda () ...))` then `(set! f (lambda () ...))` - second lambda gets no name from set!
3. **Lambda as value**: `(define x (if #t (lambda () ...) nil))` - lambda gets named "x"
4. **Let bindings**: `(let ((f (lambda () ...))) ...)` - lambda stays anonymous (let doesn't use define)
5. **Returned lambdas**: `(define make-adder (lambda (x) (lambda (y) (+ x y))))` - inner lambda is anonymous

## Benefits

- **Better debugging**: Stack traces show actual function names
- **Clearer errors**: "Error in factorial" vs "Error in lambda"
- **No performance cost**: Name is only stored, not looked up during execution
- **Backward compatible**: Existing code works unchanged
- **Foundation for defun**: Makes it easy to add `defun` special form later

## Future Enhancements (Not in this plan)

- Add `defun` special form: `(defun foo (x y) body)` as sugar for `(define foo (lambda (x y) body))`
- Add Scheme-style function define: `(define (foo x y) body)` expands to `(define foo (lambda (x y) body))`
- Add introspection: `(function-name f)` returns the name of a function
