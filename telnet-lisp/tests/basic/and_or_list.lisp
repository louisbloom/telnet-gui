;;; Test short-circuiting and/or operators and list? predicate

;;; Basic and tests
(and) ; => 1
(and #t) ; => 1
(and #f) ; => #f
(and 1 2 3) ; => 3
(and 1 #f 3) ; => #f
(and #f 1 3) ; => #f

;;; Basic or tests
(or) ; => nil
(or #f) ; => #f
(or #t) ; => 1
(or #f #f 3) ; => 3
(or 1 2 3) ; => 1
(or #f 1 3) ; => 1

;;; Short-circuit tests for and
;;; These would cause errors if not short-circuited
(and #f (/ 1 0)) ; => #f
(and #f (error "should not evaluate")) ; => #f

;;; Short-circuit tests for or
(or #t (/ 1 0)) ; => 1
(or 1 (error "should not evaluate")) ; => 1

;;; Test that and/or return the actual value (not just #t/#f)
(and 1 2 "foo") ; => "foo"
(or #f #f "bar") ; => "bar"
(or 0 "baz") ; => "baz"

;;; Truthy/falsy semantics
(and 0 5) ; => #f
(and "" 5) ; => #f
(and '() 5) ; => #f

;;; list? predicate tests
(list? '()) ; => 1
(list? nil) ; => 1
(list? '(1 2 3)) ; => 1
(list? (cons 1 2)) ; => 1
(list? 1) ; => nil
(list? "string") ; => nil
(list? #(1 2 3)) ; => nil
(list? #t) ; => nil
(list? #f) ; => nil

;;; Nested and/or
(and (or #f 1) (or 2 #f)) ; => 2
(or (and #f 1) (and 2 3)) ; => 3

;;; Complex short-circuit example
(define x 0) ; ignore
(and #t (set! x 10) (set! x 20)) ; => 20
x ; => 20

(set! x 0) ; ignore
(and #f (set! x 10) (set! x 20)) ; => #f
x ; => 0

(set! x 0) ; ignore
(or #f (set! x 10) (set! x 20)) ; => 10
x ; => 10

(set! x 0) ; ignore
(or #t (set! x 10) (set! x 20)) ; => 1
x ; => 0
