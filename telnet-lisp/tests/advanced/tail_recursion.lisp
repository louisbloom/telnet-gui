;; Tail Recursion and Tail Call Optimization Tests
;; Tests that tail-recursive functions don't exhaust the stack

(load "tests/test-helpers.lisp")

;; Test 1: Tail-recursive factorial
;; Traditional recursive factorial would stack overflow at large N
(define factorial-tail
  (lambda (n acc)
    (if (<= n 1)
      acc
      (factorial-tail (- n 1) (* n acc)))))

(assert-equal (factorial-tail 5 1) 120 "tail-recursive factorial 5")
(assert-equal (factorial-tail 10 1) 3628800 "tail-recursive factorial 10")

;; Test 2: Very large tail recursion (would stack overflow without TCO)
;; Note: factorial(1000) overflows int64, resulting in overflow value
;; Integer overflow behavior is platform-specific (undefined in C), so accept either
;; minimum (-9223372036854775808) or maximum (9223372036854775807) 64-bit integer
(let ((result (factorial-tail 1000 1)))
  (assert-true (or (= result -9223372036854775808) (= result 9223372036854775807))
    "tail-recursive factorial 1000 (overflows to min or max int64)"))

;; Test 3: Tail-recursive sum (count down)
(define sum-to-n
  (lambda (n acc)
    (if (= n 0)
      acc
      (sum-to-n (- n 1) (+ n acc)))))

(assert-equal (sum-to-n 10 0) 55 "tail-recursive sum 10")
(assert-equal (sum-to-n 100 0) 5050 "tail-recursive sum 100")
(assert-equal (sum-to-n 10000 0) 50005000 "tail-recursive sum 10000")

;; Test 4: Tail-recursive length
(define length-tail
  (lambda (lst acc)
    (if (null? lst)
      acc
      (length-tail (cdr lst) (+ acc 1)))))

(assert-equal (length-tail (list 1 2 3 4 5) 0) 5 "tail-recursive length")

;; Test 5: Mutual recursion (even?/odd?) - tests general tail call optimization
(define is-even?
  (lambda (n)
    (if (= n 0)
      #t
      (is-odd? (- n 1)))))

(define is-odd?
  (lambda (n)
    (if (= n 0)
      #f
      (is-even? (- n 1)))))

(assert-true (is-even? 10) "is-even? 10")
(assert-false (is-odd? 10) "is-odd? 10")
(assert-false (is-even? 99) "is-even? 99")
(assert-true (is-odd? 99) "is-odd? 99")

;; Large mutual recursion (would stack overflow without TCO)
(assert-true (is-even? 10000) "is-even? 10000 (large)")
(assert-true (is-odd? 10001) "is-odd? 10001 (large)")

;; Test 6: Tail recursion with multiple returns
(define find-first
  (lambda (predicate lst)
    (if (null? lst)
      nil
      (if (predicate (car lst))
        (car lst)
        (find-first predicate (cdr lst))))))

(assert-equal (find-first (lambda (x) (> x 5)) (list 1 2 3 6 7 8)) 6 "find-first tail recursion")

;; Test 7: Tail recursion in let body
;; SKIPPED: Recursive let bindings not supported (requires letrec)
;; (define test-let-tail
;;   (lambda (n)
;;     (let ((helper (lambda (x acc)
;;                     (if (<= x 0)
;;                       acc
;;                       (helper (- x 1) (+ acc x))))))
;;       (helper n 0))))
;;
;; (assert-equal (test-let-tail 100) 5050 "tail recursion in let body")
;; TODO: Add letrec support or rewrite test without recursive let bindings

;; Test 8: Tail recursion through cond
(define collatz-length
  (lambda (n acc)
    (cond
      ((= n 1) acc)
      ((= (remainder n 2) 0) (collatz-length (quotient n 2) (+ acc 1)))
      (else (collatz-length (+ (* 3 n) 1) (+ acc 1))))))

(assert-equal (collatz-length 10 0) 6 "collatz length 10")
(assert-equal (collatz-length 27 0) 111 "collatz length 27")

;; Test 9: Tail recursion through case
(define count-down-case
  (lambda (n acc)
    (case n
      ((0) acc)
      (else (count-down-case (- n 1) (+ acc 1))))))

(assert-equal (count-down-case 10 0) 10 "tail recursion through case")

;; Test 10: Tail recursion through progn (last expression)
(define progn-tail
  (lambda (n acc)
    (progn
      (define temp (+ n 1))
      (if (<= n 0)
        acc
        (progn-tail (- n 1) (+ acc n))))))

(assert-equal (progn-tail 5 0) 15 "tail recursion through progn")
