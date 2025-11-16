;; ============================================
;; Lambda Implicit Progn Test
;; ============================================
;;
;; In proper Lisp, lambda should evaluate all expressions
;; in its body and return the last value (implicit progn).
;;
;; This test verifies that lambdas with multiple body
;; expressions work correctly.

;; Simple multi-expression lambda
(define test1
  (lambda (x)
    (define y (* x 2))
    (define z (+ y 3))
    z))

(test1 5)  ; => 13

;; Lambda with side effects and return value
(define counter 0)
(define increment-and-double
  (lambda (x)
    (set! counter (+ counter 1))
    (* x 2)))

(increment-and-double 5)  ; => 10
counter  ; => 1
(increment-and-double 3)  ; => 6
counter  ; => 2

;; Lambda that evaluates multiple expressions
(define compute
  (lambda (a b)
    (define sum (+ a b))
    (define product (* a b))
    (define avg (/ sum 2))
    avg))

(compute 10 20)  ; => 15.0

;; Nested lambdas with multiple expressions
(define make-processor
  (lambda (x)
    (define temp (* x 2))
    (lambda (y)
      (define result (+ temp y))
      (define doubled (* result 2))
      doubled)))

(define proc (make-processor 5))
(proc 3)  ; => 26

;; Lambda with multiple let bindings and expressions
(define complex-calc
  (lambda (n)
    (let ((a (* n 2))
          (b (+ n 5)))
      (define c (- a b))
      (define d (* c 3))
      d)))

(complex-calc 10)  ; => 45

"Lambda implicit progn test complete"
