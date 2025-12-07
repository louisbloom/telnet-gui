;; Test basic macro functionality

(load "tests/test-helpers.lisp")

;; Test 1: Simple macro that returns a quoted expression
(defmacro simple-test () '(+ 1 2))
(assert-equal (simple-test) 3 "simple macro")

;; Test 2: Macro with parameters
(defmacro double (x) (list '+ x x))
(assert-equal (double 5) 10 "macro with parameters")
(assert-equal (double (+ 2 3)) 10 "macro with expression parameter")

;; Test 3: Test defun macro - single body expression
(defun add-one (x) (+ x 1))
(assert-equal (add-one 5) 6 "defun with single body expression")

;; Test 4: Test defun macro - multiple body expressions
(defun test-multi (x)
  (+ x 1)
  (+ x 2)
  (+ x 3))
(assert-equal (test-multi 10) 13 "defun with multiple body expressions")

;; Test 5: Test defun with multiple parameters
(defun add-three (a b c)
  (+ a b c))
(assert-equal (add-three 1 2 3) 6 "defun with multiple parameters")

;; Test 6: Test defun creates named functions
(defun factorial (n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))
(assert-equal (factorial 5) 120 "defun creates recursive functions")

;; Test 7: Macro that creates a conditional
(defmacro when (condition body)
  (list 'if condition body nil))
(assert-equal (when #t 42) 42 "when macro with true condition")
(assert-nil (when #f 42) "when macro with false condition")

;; Test 8: Verify macro doesn't evaluate arguments initially (but expansion is evaluated)
;; This macro receives (+ 1 2) unevaluated, returns it, then it gets evaluated
(defmacro pass-through (x) x)
(assert-equal (pass-through (+ 1 2)) 3 "pass-through macro evaluates expansion")

;; To return unevaluated form, macro must quote it
(defmacro quote-form (x) (list 'quote x))
(assert-equal (quote-form (+ 1 2)) '(+ 1 2) "macro that quotes form")
