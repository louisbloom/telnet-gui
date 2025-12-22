;; List Reverse Tests

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; Basic List Reversal
(assert-nil (reverse '()) "reverse of empty list is nil")
(assert-nil (reverse nil) "reverse of nil is nil")
(assert-equal (reverse '(1)) '(1) "reverse of single element list")
(assert-equal (reverse '(1 2)) '(2 1) "reverse of two element list")
(assert-equal (reverse '(1 2 3)) '(3 2 1) "reverse of three element list")
(assert-equal (reverse '(1 2 3 4 5)) '(5 4 3 2 1) "reverse of five element list")

;; Different Element Types
(assert-equal (reverse '("a" "b" "c")) '("c" "b" "a") "reverse string list")
(assert-equal (reverse '(#t #f #t)) '(#t #f #t) "reverse boolean list")
(assert-equal (reverse '(1 "two" 3)) '(3 "two" 1) "reverse mixed type list")

;; Nested Lists
(assert-equal (reverse '((1 2) (3 4))) '((3 4) (1 2)) "reverse list of lists")
(assert-equal (reverse '(1 (2 3) 4)) '(4 (2 3) 1) "reverse list with nested list")

;; Use with Other Functions
(define lst '(1 2 3 4 5))
(assert-equal (length (reverse lst)) 5 "length of reversed list unchanged")
(assert-equal (car (reverse lst)) 5 "car of reversed list is last element")
(assert-equal (list-ref (reverse lst) 0) 5 "list-ref at index 0 of reversed list")
(assert-equal (list-ref (reverse lst) 4) 1 "list-ref at index 4 of reversed list")

;; Double reverse returns original
(assert-equal (reverse (reverse '(1 2 3))) '(1 2 3) "double reverse returns original")

;; Integration with cons and car/cdr
(assert-equal (car (reverse '(1 2 3))) 3 "car of reversed list")
(assert-equal (cdr (reverse '(1 2 3))) '(2 1) "cdr of reversed list")
