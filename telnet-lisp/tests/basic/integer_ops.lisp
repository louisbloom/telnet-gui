;; Integer operations: remainder, even?, odd?
;; Demonstrate the new integer utility functions

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; Remainder (modulo) operation
(assert-equal (remainder 17 5) 2 "remainder of 17 divided by 5")
(assert-equal (remainder 10 3) 1 "remainder of 10 divided by 3")
(assert-equal (remainder 20 5) 0 "remainder of 20 divided by 5 is 0")

;; Even numbers
(assert-true (even? 0) "0 is even")
(assert-true (even? 2) "2 is even")
(assert-true (even? 4) "4 is even")
(assert-true (even? 10) "10 is even")

;; Odd numbers
(assert-true (odd? 1) "1 is odd")
(assert-true (odd? 3) "3 is odd")
(assert-true (odd? 5) "5 is odd")
(assert-true (odd? 11) "11 is odd")

;; Negative numbers
(assert-true (even? -2) "-2 is even")
(assert-true (odd? -1) "-1 is odd")
(assert-true (odd? -3) "-3 is odd")

;; Combined in logic
(assert-equal (if (even? 6) "yes" "no") "yes" "even? 6 in conditional")
(assert-equal (if (odd? 7) "yes" "no") "yes" "odd? 7 in conditional")

;; Practical example: Filter even numbers
(define test_even (lambda (n)
                    (if (even? n)
                      "even"
                      "odd")))

(assert-equal (test_even 8) "even" "test_even identifies 8 as even")
(assert-equal (test_even 9) "odd" "test_even identifies 9 as odd")

;; Modular arithmetic patterns
(assert-equal (remainder 100 3) 1 "100 mod 3 equals 1")
(assert-equal (remainder 100 4) 0 "100 mod 4 equals 0")
(assert-equal (remainder 100 7) 2 "100 mod 7 equals 2")
