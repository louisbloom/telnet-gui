; Comprehensive test of Mini LISP features

; Test 1: Arithmetic
(+ 10 20 30)

; Test 2: Variables
(define x 42)

; Test 3: Function definition and call
(define double (lambda (n) (* n 2)))
(double 21)

; Test 4: Conditionals
(if (> 10 5) 100 200)

; Test 5: String operations
(concat "Mini" " " "LISP")

; Test 6: Number comparisons
(< 5 10)
(>= 10 10)

; Test 7: Boolean operations
(and 1 2 3)
(or nil 42)
(not nil)

; Test 8: List operations
(list 1 2 3)

; Test 9: Quote
'(1 2 3)

; Test 10: Let binding
(let ((a 10) (b 20)) (+ a b))
