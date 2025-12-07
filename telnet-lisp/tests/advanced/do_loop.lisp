;; do loop examples
;; Demonstrate the do loop construct for efficient iteration

(load "tests/test-helpers.lisp")

;; Simple counter from 0 to 9
(assert-equal (do ((i 0 (+ i 1)))
                ((>= i 10) i))
  10
  "simple counter with do loop")

;; Count up with side effects
(assert-equal (do ((i 1 (+ i 1)))
                ((> i 5) "done")
                i)
  "done"
  "do loop with side effects")

;; Multiple variables - count and accumulate sum
(assert-equal (do ((i 1 (+ i 1))
                    (sum 0))
                ((> i 10) sum)
                (set! sum (+ sum i)))
  55
  "do loop with multiple variables")

;; Compute factorial using do loop
(define factorial-do
  (lambda (n)
    (do ((i n (- i 1))
          (acc 1))
      ((<= i 0) acc)
      (set! acc (* acc i)))))

(assert-equal (factorial-do 5) 120 "factorial with do loop")

;; Count down from 10 to 1 with side effect
(assert-equal (do ((i 10 (- i 1)))
                ((<= i 0) "blastoff")
                i)
  "blastoff"
  "countdown with do loop")

;; Generate powers of 2 until > 1000
(assert-equal (do ((x 1 (* x 2)))
                ((> x 1000) x)
                x)
  1024
  "powers of 2 with do loop")

;; Pair-wise variables tracking in opposite directions
(assert-equal (do ((a 0 (+ a 1))
                    (b 10 (- b 1)))
                ((>= a 10) (list a b))
                (progn a b))
  '(10 0)
  "pair-wise variables in do loop")

;; Body is empty - just increment count until condition
(assert-equal (do ((count 0 (+ count 1)))
                ((>= count 5) count))
  5
  "do loop with empty body")

;; Sum of squares with multiple accumulators
(define sum-squares
  (lambda (n)
    (do ((i 1 (+ i 1))
          (sum 0))
      ((> i n) sum)
      (set! sum (+ sum (* i i))))))

(assert-equal (sum-squares 5) 55 "sum of squares with do loop")

;; Early exit when square equals 25
(assert-equal (do ((i 1 (+ i 1))
                    (found #f))
                ((or (> i 10) found) (if found i #f))
                (if (= (* i i) 25)
                  (set! found #t)))
  5
  "early exit with do loop")

;; Immediate return (no increment, just test)
(assert-equal (do ((x 5))
                ((= x 5) x))
  5
  "immediate return from do loop")
