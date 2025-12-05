;; Integer operations: remainder, even?, odd?
;; Demonstrate the new integer utility functions

;; Remainder (modulo) operation
(remainder 17 5)    ; => 2
(remainder 10 3)    ; => 1
(remainder 20 5)    ; => 0

;; Even numbers
(even? 0)           ; => #t
(even? 2)           ; => #t
(even? 4)           ; => #t
(even? 10)          ; => #t

;; Odd numbers
(odd? 1)            ; => #t
(odd? 3)            ; => #t
(odd? 5)            ; => #t
(odd? 11)           ; => #t

;; Negative numbers
(even? -2)          ; => #t
(odd? -1)           ; => #t
(odd? -3)           ; => #t

;; Combined in logic
(if (even? 6) "yes" "no")  ; => "yes"
(if (odd? 7) "yes" "no")   ; => "yes"

;; Practical example: Filter even numbers
(define test_even (lambda (n)
                    (if (even? n)
                      "even"
                      "odd")))

(test_even 8)       ; => "even"
(test_even 9)       ; => "odd"

;; Modular arithmetic patterns
(remainder 100 3)   ; => 1
(remainder 100 4)   ; => 0
(remainder 100 7)   ; => 2

