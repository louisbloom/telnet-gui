;; progn special form examples
;; progn evaluates multiple expressions sequentially and returns the last value

(load "tests/test-helpers.lisp")

;; Basic progn - returns last expression
(assert-equal (+ (progn (+ 1 1) (+ 2 2) 10) 5) 15 "progn returns last value")

;; progn with define operations
(assert-equal (progn
                (define x 10)
                (define y 20)
                (+ x y))
  30
  "progn with define operations")

;; progn with mixed operations building up values sequentially
(assert-equal (let ((a 5))
                (progn
                  (define b (+ a 3))
                  (define c (* b 2))
                  c))
  16
  "progn with sequential definitions")

;; Empty progn returns NIL (null value)
(assert-nil (progn) "empty progn returns nil")

;; Single expression progn returns that expression
(assert-equal (progn 42) 42 "single expression progn")

;; progn with conditional logic - evaluates all but returns last
(assert-equal (progn
                (if (> 5 3) 1 0)
                (if (< 10 5) 2 0)
                99)
  99
  "progn evaluates all expressions but returns last")
