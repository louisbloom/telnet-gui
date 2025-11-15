; Test basic macro functionality

; Test 1: Simple macro that returns a quoted expression
(defmacro simple-test () '(+ 1 2))
(simple-test)                           ; => 3

; Test 2: Macro with parameters
(defmacro double (x) (list '+ x x))
(double 5)                              ; => 10
(double (+ 2 3))                        ; => 10

; Test 3: Test defun macro - single body expression
(defun add-one (x) (+ x 1))
(add-one 5)                             ; => 6

; Test 4: Test defun macro - multiple body expressions
(defun test-multi (x)
  (+ x 1)
  (+ x 2)
  (+ x 3))
(test-multi 10)                         ; => 13

; Test 5: Test defun with multiple parameters
(defun add-three (a b c)
  (+ a b c))
(add-three 1 2 3)                       ; => 6

; Test 6: Test defun creates named functions
(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
(factorial 5)                           ; => 120

; Test 7: Macro that creates a conditional
(defmacro when (condition body)
  (list 'if condition body nil))
(when #t 42)                            ; => 42
(when #f 42)                            ; => nil

; Test 8: Verify macro doesn't evaluate arguments initially (but expansion is evaluated)
; This macro receives (+ 1 2) unevaluated, returns it, then it gets evaluated
(defmacro pass-through (x) x)
(pass-through (+ 1 2))                  ; => 3

; To return unevaluated form, macro must quote it
(defmacro quote-form (x) (list 'quote x))
(quote-form (+ 1 2))                    ; => (+ 1 2)
