;; Test unified length function
(load "test-helpers.lisp")

;; Test list length
(assert-equal (length '(1 2 3)) 3 "length works for lists")
(assert-equal (length '()) 0 "length works for empty list")
(assert-equal (length '(a)) 1 "length works for single element list")

;; Test string length
(assert-equal (length "hello") 5 "length works for strings")
(assert-equal (length "") 0 "length works for empty string")
(assert-equal (length "test") 4 "length works for strings")

;; Test vector length
(assert-equal (length #(1 2 3 4)) 4 "length works for vectors")
(assert-equal (length #()) 0 "length works for empty vector")
(assert-equal (length #(a b)) 2 "length works for vectors")

;; Test that unified length works for all types
(assert-equal (length '(1 2 3)) 3 "length works for lists")
(assert-equal (length "hello") 5 "length works for strings")
(assert-equal (length #(1 2 3)) 3 "length works for vectors")

(princ "All length function tests passed!\n")
