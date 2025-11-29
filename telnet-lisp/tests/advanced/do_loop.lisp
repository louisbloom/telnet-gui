;; do loop examples
;; Demonstrate the do loop construct for efficient iteration

;; Simple counter from 0 to 9
(do ((i 0 (+ i 1)))
    ((>= i 10) i))                    ; => 10

;; Count up with side effects
(do ((i 1 (+ i 1)))
    ((> i 5) "done")
 i)                                   ; => "done"

;; Multiple variables - count and accumulate sum
(do ((i 1 (+ i 1))
     (sum 0))
    ((> i 10) sum)                      ; => 55
  (set! sum (+ sum i)))

;; Compute factorial using do loop
(define factorial-do
    (lambda (n)
      (do ((i n (- i 1))
           (acc 1 (* acc i)))
          ((<= i 0) acc))))

(factorial-do 5)                       ; => 120

;; Count down from 10 to 1 with side effect
(do ((i 10 (- i 1)))
    ((<= i 0) "blastoff")               ; => "blastoff"
 i)

;; Generate powers of 2 until > 1000
(do ((x 1 (* x 2)))
    ((> x 1000) x)                      ; => 1024
 x)

;; Pair-wise variables tracking in opposite directions
(do ((a 0 (+ a 1))
     (b 10 (- b 1)))
    ((>= a 10) (list a b))              ; => (10 0)
  (progn a b))

;; Body is empty - just increment count until condition
(do ((count 0 (+ count 1)))
    ((>= count 5) count))               ; => 5

;; Sum of squares with multiple accumulators
(define sum-squares
    (lambda (n)
      (do ((i 1 (+ i 1))
           (sum 0))
          ((> i n) sum)
	(set! sum (+ sum (* i i))))))

(sum-squares 5)                         ; => 55

;; Early exit when square equals 25
(do ((i 1 (+ i 1))
     (found #f))
    ((or (> i 10) found) (if found i #f))  ; => 5
  (if (= (* i i) 25)
      (set! found #t)))

;; Immediate return (no increment, just test)
(do ((x 5))
    ((= x 5) x))                        ; => 5
