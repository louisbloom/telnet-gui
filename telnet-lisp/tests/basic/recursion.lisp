;; Recursion Examples
;; Demonstrates recursive function definitions and common recursion patterns

;; Factorial function - classic recursive example
(define factorial
    (lambda (n)
      (if (<= n 1)
	  1
	  (* n (factorial (- n 1))))))

(factorial 5)                          ; => 120

;; Fibonacci sequence - demonstrates mutual recursion
(define fib
    (lambda (n)
      (if (< n 2)
	  n
	  (+ (fib (- n 1)) (fib (- n 2))))))

(fib 10)                               ; => 55
