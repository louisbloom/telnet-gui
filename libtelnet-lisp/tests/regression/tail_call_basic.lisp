;; Basic Tail Call Optimization Tests
;; Quick tests to verify TCO is working

;; Test 1: Simple tail-recursive factorial
(define factorial-tail
  (lambda (n acc)
    (if (<= n 1)
        acc
        (factorial-tail (- n 1) (* n acc)))))

(factorial-tail 5 1)                     ; => 120

;; Test 2: Tail-recursive sum
(define sum-tail
  (lambda (n acc)
    (if (= n 0)
        acc
        (sum-tail (- n 1) (+ n acc)))))

(sum-tail 10 0)                          ; => 55
(sum-tail 100 0)                         ; => 5050

;; Test 3: Deep recursion that would stack overflow without TCO
(sum-tail 5000 0)                        ; => 12502500

;; Test 4: Mutual recursion
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

(is-even? 10)                            ; => #t
(is-odd? 11)                             ; => #t
(is-even? 1000)                          ; => #t

;; Test 5: Tail call through if branches
(define abs-tail
  (lambda (n)
    (if (< n 0)
        (abs-tail (- 0 n))
        n)))

(abs-tail -42)                           ; => 42

;; Test 6: Tail call through cond
(define sign
  (lambda (n)
    (cond
      ((< n 0) -1)
      ((> n 0) 1)
      (else 0))))

(sign -5)                                ; => -1
(sign 5)                                 ; => 1
(sign 0)                                 ; => 0
