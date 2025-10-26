;; let* special form examples
;; let* allows bindings to reference previous bindings in the same form

;; Basic let* - second binding can reference first
(let* ((x 5) (y (+ x 3))) y)
;; Result: 8

;; let* with multiple dependent bindings
(let* ((a 10)
       (b (* a 2))
       (c (+ b 5)))
  c)
;; Result: 25

;; Comparison with let (which evaluates in parallel)
(let ((x 5) (y (+ x 3))) y)
;; ERROR: Undefined symbol: x

(let* ((x 5) (y (+ x 3))) y)
;; Result: 8 (works because x is in scope)

;; Using let* for sequential computation
(let* ((start 100)
       (increased (* start 1.5))
       (decreased (- increased 10)))
  decreased)
;; Result: 140

;; Complex example with mixed operations
(let* ((name "Alice")
       (greeting (concat "Hello, " name))
       (length (strlen greeting)))
  length)
;; Shows how let* helps build up values sequentially
