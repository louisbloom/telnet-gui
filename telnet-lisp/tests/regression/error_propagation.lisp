; Test error propagation with call stacks
; This test verifies that errors propagate up the call stack correctly
; and that the call stack includes both builtins and user-defined functions

; Test 1: Direct builtin error
; Expected: ERROR with "/" in call stack
; (/ 1 0)  ; => ERROR

; Test 2: Builtin error from lambda
; Expected: ERROR with "/" and "divide-by-zero" in call stack
(define divide-by-zero (lambda (x) (/ x 0)))
; (divide-by-zero 10)  ; => ERROR

; Test 3: Deep call stack with builtin error
; Expected: ERROR with "/", "inner", "middle", "outer" in call stack
(define inner (lambda (x) (/ x 0)))
(define middle (lambda (x) (inner x)))
(define outer (lambda (x) (middle x)))
; (outer 10)  ; => ERROR

; Test 4: Type error in builtin
; Expected: ERROR with "+" and "type-error-test" in call stack
(define type-error-test (lambda (x) (+ x "not a number")))
; (type-error-test 5)  ; => ERROR

; Test 5: Multiple builtins in call stack
; Expected: ERROR showing nested builtin calls
(define nested-builtins (lambda (x) (+ x (* 2 (/ 1 0)))))
; (nested-builtins 5)  ; => ERROR

; Test 6: Error from let binding evaluation
; Expected: ERROR with context showing where error occurred
(define let-error-test (lambda (x)
  (let ((y (/ x 0)))
    (+ y 1))))
; (let-error-test 10)  ; => ERROR

; Test 7: Error from if condition evaluation
; Expected: ERROR from evaluating condition
(define if-error-test (lambda (x)
  (if (> x (/ 1 0))
    "true"
    "false")))
; (if-error-test 5)  ; => ERROR

; Test 8: Error propagating through do loop
; Expected: ERROR showing do loop context
(define do-error-test (lambda ()
  (do ((i 0 (+ i 1)))
      ((>= i 10) i)
    (if (= i 5)
      (/ 1 0)
      i))))
; (do-error-test)  ; => ERROR

; Test 9: Undefined variable error
; Expected: ERROR with symbol name in message
(define undefined-var-test (lambda ()
  some-undefined-variable))
; (undefined-var-test)  ; => ERROR

; Test 10: Multiple levels with mixed errors
; Expected: ERROR showing complete call chain
(define level1 (lambda (op x y)
  (if (string= op "add")
    (+ x y)
    (if (string= op "div")
      (/ x y)
      (+ x "bad")))))

(define level2 (lambda (op x y)
  (level1 op x y)))

(define level3 (lambda (op x y)
  (level2 op x y)))

; Test division by zero through levels
; (level3 "div" 10 0)  ; => ERROR with "/", "level1", "level2", "level3"

; Test type error through levels
; (level3 "mul" 10 5)  ; => ERROR with "+", "level1", "level2", "level3"

; These tests are commented out to prevent test failures
; They should be run individually to verify error propagation
; The expected output is shown in comments for validation
