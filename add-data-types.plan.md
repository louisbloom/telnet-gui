<!-- 270209c2-f945-4c11-8cdf-3a4347af89a7 8f31255a-5971-43d5-92e9-c4b6b54f2347 -->

# Add Loop Constructs to Telnet Lisp

## Overview

Implement Scheme-style `do` loop construct with tail-call optimization to enable efficient iteration without stack overflow.

## Scheme `do` Loop Syntax

```lisp
(do ((var1 init1 step1)
     (var2 init2 step2)
     ...)
    (test result-expr)
  body-expr1
  body-expr2
  ...)
```

- **Bindings**: Each variable has an initial value and optional step expression
- **Test clause**: When test is true, evaluate result-expr and return it
- **Body**: Expressions executed on each iteration (for side effects)
- **Iteration**: After body, update variables with step expressions, then re-test

## Implementation Strategy

### 1. Add `eval_do` Function in `src/eval.c`

Add forward declaration and implement the `do` loop evaluator:

```c
static LispObject *eval_do(LispObject *args, Environment *env);
```

Implementation steps:

1. Parse bindings `((var init step) ...)`
2. Create new environment with initial variable values
3. Loop:
   - Evaluate test condition
   - If true, evaluate and return result expression
   - Evaluate body expressions (for side effects)
   - Update all variables with step expressions (in parallel)
   - Continue loop

**Key detail**: Variable updates happen in parallel (evaluate all steps in old environment, then update all at once).

### 2. Register `do` Special Form in `eval_list`

Add to the special form checks in `src/eval.c` around line 100:

```c
if (strcmp(first->value.symbol, "do") == 0) {
    return eval_do(lisp_cdr(list), env);
}
```

### 3. Tail-Call Optimization

Since `do` loops can run indefinitely, implement iteratively (not recursively) to avoid stack overflow:

- Use a `while` loop in C
- Re-evaluate test and body on each iteration
- No recursive calls to `eval_do`

This is already tail-call optimized by design since we're using iteration.

### 4. Add Examples in `examples/do_loop.lisp`

Create comprehensive examples:

- Simple counter loop
- Factorial using `do`
- List iteration
- Multiple variable iteration
- Nested loops

### 5. Update Documentation

Update `README.md`:

- Add `do` to Special Forms section
- Add examples in Language Examples section
- Update Future Enhancements (remove from high priority)

## Files to Modify

1. **src/eval.c**

   - Add `eval_do` forward declaration (line ~14)
   - Add `eval_do` implementation (after `eval_progn`, line ~362)
   - Register `do` in `eval_list` (after `progn` check, line ~102)

2. **examples/do_loop.lisp** (new file)

   - Basic counter examples
   - Factorial and fibonacci
   - Multiple variable iteration
   - Practical use cases

3. **README.md**
   - Add to Special Forms section
   - Add to Language Examples
   - Update Future Enhancements

## Example Usage

```lisp
; Count from 0 to 9
(do ((i 0 (+ i 1)))
    ((>= i 10) i))
; => 10

; Factorial
(define factorial-do
  (lambda (n)
    (do ((i n (- i 1))
         (acc 1 (* acc i)))
        ((<= i 0) acc))))

; Iterate over range with side effects
(do ((i 0 (+ i 1)))
    ((>= i 5) "done")
  (print i))
```

## Testing

Test cases to verify:

- Simple counter loop
- Early termination (test becomes true)
- Multiple variables updating in parallel
- Nested `do` loops
- `do` with no step expressions (variables stay constant)
- `do` with no body (just test and result)

### To-dos

- [x] Implement eval_do function in src/eval.c with iterative loop
- [x] Register 'do' special form in eval_list
- [x] Create examples/do_loop.lisp with comprehensive examples
- [x] Update README.md with do loop documentation and examples
- [x] Test do loop with various scenarios (counters, factorial, multiple vars)
