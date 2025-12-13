;;; Test short-circuiting and/or operators and list? predicate

;; Load test helper macros
(load "tests/test-helpers.lisp")

;;; Basic and tests
(assert-true (and) "and with no arguments returns true")
(assert-true (and #t) "and with single true argument returns true")
(assert-false (and #f) "and with single false argument returns false")
(assert-equal (and 1 2 3) 3 "and returns last value when all truthy")
(assert-false (and 1 #f 3) "and returns false when middle value is false")
(assert-false (and #f 1 3) "and returns false when first value is false")

;;; Basic or tests
(assert-nil (or) "or with no arguments returns nil")
(assert-false (or #f) "or with single false argument returns false")
(assert-true (or #t) "or with single true argument returns true")
(assert-equal (or #f #f 3) 3 "or returns first truthy value")
(assert-equal (or 1 2 3) 1 "or returns first value when truthy")
(assert-equal (or #f 1 3) 1 "or skips false and returns first truthy")

;;; Short-circuit tests for and
;;; These would cause errors if not short-circuited
(assert-false (and #f (/ 1 0)) "and short-circuits on false, avoiding division by zero")
(assert-false (and #f (error "should not evaluate")) "and short-circuits on false, avoiding error")

;;; Short-circuit tests for or
(assert-true (or #t (/ 1 0)) "or short-circuits on true, avoiding division by zero")
(assert-equal (or 1 (error "should not evaluate")) 1 "or short-circuits on truthy, avoiding error")

;;; Test that and/or return the actual value (not just #t/#f)
(assert-equal (and 1 2 "foo") "foo" "and returns actual last value, not just true")
(assert-equal (or #f #f "bar") "bar" "or returns actual first truthy value")
(assert-equal (or 0 "baz") 0 "or returns 0 (truthy in this Lisp)")

;;; Truthy/falsy semantics (Note: 0 and "" are TRUTHY, but '() is FALSY (same as nil))
(assert-equal (and 0 5) 5 "and with 0 (truthy) returns second value")
(assert-equal (and "" 5) 5 "and with empty string (truthy) returns second value")
(assert-nil (and '() 5) "and with empty list (falsy) returns nil")

;;; list? predicate tests
(assert-true (list? '()) "empty list is a list")
(assert-true (list? nil) "nil is a list")
(assert-true (list? '(1 2 3)) "proper list is a list")
(assert-true (list? (cons 1 2)) "cons cell is a list")
(assert-nil (list? 1) "integer is not a list")
(assert-nil (list? "string") "string is not a list")
(assert-nil (list? #(1 2 3)) "vector is not a list")
(assert-nil (list? #t) "boolean true is not a list")
(assert-true (list? #f) "boolean false (#f = nil) is a list (the empty list)")

;;; pair? predicate tests
(assert-true (pair? '(1 . 2)) "dotted pair is a pair")
(assert-true (pair? (cons 1 2)) "cons cell is a pair")
(assert-true (pair? '(a b c)) "proper list is a pair (cons cell)")
(assert-true (pair? '(1 2)) "list with elements is a pair")
(assert-nil (pair? nil) "nil is not a pair")
(assert-nil (pair? '()) "empty list is not a pair")
(assert-nil (pair? 42) "integer is not a pair")
(assert-nil (pair? "string") "string is not a pair")
(assert-nil (pair? #(1 2 3)) "vector is not a pair")
(assert-nil (pair? #t) "boolean true is not a pair")
(assert-nil (pair? #f) "boolean false is not a pair")
(assert-true (pair? (cons 'a 'b)) "cons of symbols is a pair")
(assert-true (pair? (cons 1 nil)) "cons with nil cdr is a pair")

;;; Nested and/or
(assert-equal (and (or #f 1) (or 2 #f)) 2 "nested and/or evaluates correctly")
(assert-equal (or (and #f 1) (and 2 3)) 3 "nested or/and evaluates correctly")

;;; Complex short-circuit example
(define x 0)
(assert-equal (and #t (set! x 10) (set! x 20)) 20 "and evaluates all expressions when all truthy")
(assert-equal x 20 "x was set to 20 by and expression")

(set! x 0)
(assert-false (and #f (set! x 10) (set! x 20)) "and stops on false")
(assert-equal x 0 "x was not modified by short-circuited expressions")

(set! x 0)
(assert-equal (or #f (set! x 10) (set! x 20)) 10 "or returns first truthy value")
(assert-equal x 10 "x was set to 10 by or expression")

(set! x 0)
(assert-true (or #t (set! x 10) (set! x 20)) "or returns first truthy value (true)")
(assert-equal x 0 "x was not modified by short-circuited expressions")
