;; do loop examples
;; Demonstrate the do loop construct for efficient iteration

;; Simple counter from 0 to 9
(do ((i 0 (+ i 1)))
    ((>= i 10) i))

;; Count up with side effects
(do ((i 1 (+ i 1)))
    ((> i 5) "done")
  i)

;; Multiple variables - count and accumulate
(do ((i 1 (+ i 1))
     (sum 0))
    ((> i 10) sum)
  (set! sum (+ sum i)))

;; Compute factorial using do loop
(define factorial-do
  (lambda (n)
    (do ((i n (- i 1))
         (acc 1 (* acc i)))
        ((<= i 0) acc))))

(factorial-do 5)

;; Count down from 10 to 1
(do ((i 10 (- i 1)))
    ((<= i 0) "blastoff")
  i)

;; Iterate over a range with no step (infinite loop prevention depends on test)
(do ((x 1 (* x 2)))
    ((> x 1000) x)
  x)

;; Pair-wise variables
(do ((a 0 (+ a 1))
     (b 10 (- b 1)))
    ((>= a 10) (list a b))
  (progn a b))

;; Body can be empty - just increment until condition
(do ((count 0 (+ count 1)))
    ((>= count 5) count))

;; Nested-like behavior with multiple accumulators
(define sum-squares
  (lambda (n)
    (do ((i 1 (+ i 1))
         (sum 0))
        ((> i n) sum)
      (set! sum (+ sum (* i i))))))

(sum-squares 5)

;; Early exit condition
(do ((i 1 (+ i 1))
     (found #f))
    ((or (> i 10) found) (if found i #f))
  (if (= (* i i) 25)
      (set! found #t)))

;; do with just test and result, no body
(do ((x 5))
    ((= x 5) x))

