;; progn special form examples
;; progn evaluates multiple expressions sequentially and returns the last value

;; Basic progn - returns last expression
(+ (progn (+ 1 1) (+ 2 2) 10) 5)
;; Result: 15 (progn evaluates to 10, then adds 5)

;; progn with define operations
(progn
  (define x 10)
  (define y 20)
  (+ x y))
;; Result: 30

;; progn with mixed operations
(let ((a 5))
  (progn
    (define b (+ a 3))
    (define c (* b 2))
    c))
;; Result: 16

;; Empty progn returns NIL
(progn)

;; Single expression progn
(progn 42)
;; Result: 42

;; progn with conditional logic
(progn
  (if (> 5 3) 1 0)
  (if (< 10 5) 2 0)
  99)
;; Result: 99
