;; let* Special Form Examples
;; Demonstrates sequential binding where later bindings can reference earlier ones

;; let* allows bindings to reference previous bindings in the same form
;; This enables sequential computation patterns

;; Basic let* - second binding references first
(let* ((x 5) (y (+ x 3))) y)           ; => 8

;; Multiple dependent bindings building up a computation
(let* ((a 10)
        (b (* a 2))
        (c (+ b 5)))
  c)                                    ; => 25

;; Comparison: let evaluates bindings in parallel (x not in scope for y)
;; This demonstrates why let* is needed for sequential dependencies
;; let((x 5) (y (+ x 3))) y)           ; ERROR: Undefined symbol: x

;; let* works because x is already in scope when y is evaluated
(let* ((x 5) (y (+ x 3))) y)           ; => 8

;; Sequential computation with multiple steps
(let* ((start 100)
        (increased (* start 1.5))
        (decreased (- increased 10)))
  decreased)                             ; => 140

;; Complex example: building a greeting message step by step
(let* ((name "Alice")
        (greeting (concat "Hello, " name))
        (length (strlen greeting)))
  length)                                ; => 12

;; Demonstrates the sequential nature of let*
(let* ((x 10)
        (y (* x 2))
        (z (+ y 5)))
  z)                                     ; => 25

;; All bindings evaluate before the body
(let* ((a 1)
        (b (+ a 1))
        (c (+ b 1)))
  (+ a b c))                             ; => 6

;; ===========================================
;; let* body should behave like progn
;; ===========================================

;; Multiple expressions in body - should return last value (regression test)
(let* ((x 10))
  (+ x 5)
  (* x 2)
  (- x 3))                               ; => 7

;; Multiple body expressions with side effects - should evaluate all and return last
(let* ((count 0)
        (value 100))
  (set! count (+ count 1))
  (set! count (+ count 1))
  (set! count (+ count 1))
  count)                                 ; => 3

;; Body with multiple values - should return last
(let* ((a 1) (b 2))
  10
  20
  30
  (+ a b))                               ; => 3
