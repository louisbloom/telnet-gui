;; Minimal tail call test
(define fact-tail
  (lambda (n acc)
    (if (<= n 1)
        acc
        (fact-tail (- n 1) (* n acc)))))

(fact-tail 3 1)  ; => 6
