;; let* Special Form Examples
;; Demonstrates sequential binding where later bindings can reference earlier ones

(load "tests/test-helpers.lisp")

;; let* allows bindings to reference previous bindings in the same form
;; This enables sequential computation patterns

;; Basic let* - second binding references first
(assert-equal (let* ((x 5) (y (+ x 3))) y) 8 "basic let* with dependent bindings")

;; Multiple dependent bindings building up a computation
(assert-equal (let* ((a 10)
                      (b (* a 2))
                      (c (+ b 5)))
                c)
  25
  "multiple dependent bindings")

;; Comparison: let evaluates bindings in parallel (x not in scope for y)
;; This demonstrates why let* is needed for sequential dependencies
;; let((x 5) (y (+ x 3))) y)           ; ERROR: Undefined symbol: x

;; let* works because x is already in scope when y is evaluated
(assert-equal (let* ((x 5) (y (+ x 3))) y) 8 "let* allows dependent bindings")

;; Sequential computation with multiple steps
(assert-equal (let* ((start 100)
                      (increased (* start 1.5))
                      (decreased (- increased 10)))
                decreased)
  140
  "sequential computation with multiple steps")

;; Complex example: building a greeting message step by step
(assert-equal (let* ((name "Alice")
                      (greeting (concat "Hello, " name))
                      (length (string-length greeting)))
                length)
  12
  "sequential string operations")

;; Demonstrates the sequential nature of let*
(assert-equal (let* ((x 10)
                      (y (* x 2))
                      (z (+ y 5)))
                z)
  25
  "sequential numeric operations")

;; All bindings evaluate before the body
(assert-equal (let* ((a 1)
                      (b (+ a 1))
                      (c (+ b 1)))
                (+ a b c))
  6
  "all bindings available in body")

;; ===========================================
;; let* body should behave like progn
;; ===========================================

;; Multiple expressions in body - should return last value (regression test)
(assert-equal (let* ((x 10))
                (+ x 5)
                (* x 2)
                (- x 3))
  7
  "let* body returns last expression")

;; Multiple body expressions with side effects - should evaluate all and return last
(assert-equal (let* ((count 0)
                      (value 100))
                (set! count (+ count 1))
                (set! count (+ count 1))
                (set! count (+ count 1))
                count)
  3
  "let* body evaluates all expressions")

;; Body with multiple values - should return last
(assert-equal (let* ((a 1) (b 2))
                10
                20
                30
                (+ a b))
  3
  "let* body with multiple values returns last")
