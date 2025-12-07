;; Integer and mixed arithmetic examples

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; Integer arithmetic
(assert-equal (+ 1 2 3) 6 "Integer addition")
(assert-equal (- 10 3) 7 "Integer subtraction")
(assert-equal (* 2 3 4) 24 "Integer multiplication")

;; Mixed integer/float
(assert-equal (+ 1 2.5) 3.5 "Mixed int+float addition")
(assert-equal (* 3 4.0) 12.0 "Mixed int*float multiplication")
(assert-equal (+ 1.5 2) 3.5 "Mixed float+int addition")

;; Division
(assert-equal (/ 10 2) 5.0 "Division always returns float")
(assert-true (< (- (/ 10 3) 3.333333) 0.00001) "Division with remainder")
(assert-equal (quotient 10 3) 3 "Integer quotient truncates")

;; Type coercion in operations
(define a 5)
(define b 2.5)
(assert-equal (+ a b) 7.5 "Integer promoted to float in mixed operation")
(assert-equal (* a 10) 50 "Integer multiplication stays integer")

;; Comparisons work with both types
(assert-true (> 5 3) "Greater than with integers")
(assert-true (> 5.0 3) "Greater than with float and integer")
(assert-true (< 2 3.5) "Less than with int and float")
(assert-true (= 5 5.0) "Equality between int and float (same value)")
(assert-true (= 5.0 5) "Equality between float and int (same value)")

;; Truthy/falsy behavior (note: 0 is truthy in this Lisp, only nil is falsy)
(define x 0)
(assert-equal x 0 "Variable holds integer 0")
(define y 5)
(assert-equal y 5 "Variable holds integer 5")
(assert-equal (if 0 "yes" "no") "yes" "0 is truthy in conditional (only nil is falsy)")
(assert-equal (if 1 "yes" "no") "yes" "Non-zero integer is truthy")
