;; Recursion Examples
;; Demonstrates recursive function definitions and common recursion patterns

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; Factorial function - classic recursive example
(define factorial
  (lambda (n)
    (if (<= n 1)
      1
      (* n (factorial (- n 1))))))

(assert-equal (factorial 5) 120 "Factorial of 5")

;; Fibonacci sequence - demonstrates mutual recursion
(define fib
  (lambda (n)
    (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

(assert-equal (fib 10) 55 "Fibonacci of 10")
