;; progn special form examples
;; progn evaluates multiple expressions sequentially and returns the last value

;; Basic progn - returns last expression
(+ (progn (+ 1 1) (+ 2 2) 10) 5)      ; => 15

;; progn with define operations
(progn
  (define x 10)
  (define y 20)
  (+ x y))                             ; => 30

;; progn with mixed operations building up values sequentially
(let ((a 5))
  (progn
    (define b (+ a 3))
    (define c (* b 2))
    c))                                ; => 16

;; Empty progn returns NIL (null value)
(progn)                                ; => nil

;; Single expression progn returns that expression
(progn 42)                             ; => 42

;; progn with conditional logic - evaluates all but returns last
(progn
  (if (> 5 3) 1 0)
  (if (< 10 5) 2 0)
  99)                                  ; => 99
