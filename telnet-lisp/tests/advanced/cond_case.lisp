;; cond and case examples
;; Demonstrate multi-way conditionals and pattern matching

(load "tests/test-helpers.lisp")

;; ===========================================
;; cond - Multi-way conditional
;; ===========================================

;; Grade function using cond
(define grade
  (lambda (score)
    (cond
      ((>= score 90) "A")
      ((>= score 80) "B")
      ((>= score 70) "C")
      ((>= score 60) "D")
      (else "F"))))

(assert-equal (grade 95) "A" "grade A")
(assert-equal (grade 85) "B" "grade B")
(assert-equal (grade 75) "C" "grade C")
(assert-equal (grade 65) "D" "grade D")
(assert-equal (grade 50) "F" "grade F")

;; Sign of a number
(define sign
  (lambda (n)
    (cond
      ((< n 0) -1)
      ((= n 0) 0)
      (else 1))))

(assert-equal (sign -5) -1 "sign of negative")
(assert-equal (sign 0) 0 "sign of zero")
(assert-equal (sign 10) 1 "sign of positive")

;; Abs using cond
(define abs
  (lambda (n)
    (cond
      ((< n 0) (- n))
      (else n))))

(assert-equal (abs -10) 10 "absolute value of negative")
(assert-equal (abs 5) 5 "absolute value of positive")

;; Cond without else returns NIL
(assert-nil (cond
              ((< 5 3) "no")
              ((< 10 5) "nope"))
  "cond without else returns nil")

;; Empty cond
(assert-nil (cond) "empty cond returns nil")

;; Single clause cond
(assert-equal (cond
                ((= 2 2) "yes"))
  "yes"
  "single clause cond")

;; ===========================================
;; case - Pattern matching
;; ===========================================

;; Day of week
(define day-name
  (lambda (n)
    (case n
      ((1) "Monday")
      ((2) "Tuesday")
      ((3) "Wednesday")
      ((4) "Thursday")
      ((5) "Friday")
      ((6 7) "Weekend")
      (else "Invalid"))))

(assert-equal (day-name 1) "Monday" "day name Monday")
(assert-equal (day-name 3) "Wednesday" "day name Wednesday")
(assert-equal (day-name 6) "Weekend" "day 6 is weekend")
(assert-equal (day-name 7) "Weekend" "day 7 is weekend")
(assert-equal (day-name 99) "Invalid" "invalid day number")

;; Command dispatcher
(define command
  (lambda (cmd)
    (case cmd
      (("help" "h") "showing help")
      (("quit" "exit" "q") "quitting")
      (("save" "write") "saving")
      (else "unknown command"))))

(assert-equal (command "help") "showing help" "help command")
(assert-equal (command "h") "showing help" "h command")
(assert-equal (command "quit") "quitting" "quit command")
(assert-equal (command "xyz") "unknown command" "unknown command")

;; Number classifier
(define classify-number
  (lambda (x)
    (case x
      ((0) "zero")
      ((-1 -2 -3) "small negative")
      ((1 2 3) "small positive")
      ((4 5 6 7 8 9 10) "medium")
      (else "large"))))

(assert-equal (classify-number 0) "zero" "classify zero")
(assert-equal (classify-number 3) "small positive" "classify small positive")
(assert-equal (classify-number 7) "medium" "classify medium")
(assert-equal (classify-number 100) "large" "classify large")

;; Case without else returns NIL
(assert-nil (case 99
              ((1 2 3) "match"))
  "case without else returns nil")

;; ===========================================
;; Nested conditionals
;; ===========================================

;; Nested cond
(define classify
  (lambda (x)
    (cond
      ((< x 0) "negative")
      ((= x 0) "zero")
      (else
        (cond
          ((even? x) "positive even")
          (else "positive odd"))))))

(assert-equal (classify -5) "negative" "classify negative")
(assert-equal (classify 0) "zero" "classify zero")
(assert-equal (classify 4) "positive even" "classify positive even")
(assert-equal (classify 7) "positive odd" "classify positive odd")

;; Complex nested structure
(define status
  (lambda (age status)
    (case status
      (("student") "eligible for student discount")
      (("senior")
        (cond
          ((>= age 65) "senior discount")
          (else "not yet senior")))
      (else "no discount"))))

(assert-equal (status 20 "student") "eligible for student discount" "student status")
(assert-equal (status 70 "senior") "senior discount" "senior discount")
(assert-equal (status 50 "senior") "not yet senior" "not yet senior")

;; ===========================================
;; Practical examples
;; ===========================================

;; Menu selection
(define menu-action
  (lambda (choice)
    (case choice
      ((1) "Creating new file")
      ((2) "Opening file")
      ((3) "Saving file")
      ((4) "Closing file")
      ((0) "Exiting")
      (else "Invalid choice"))))

(assert-equal (menu-action 1) "Creating new file" "menu option 1")
(assert-equal (menu-action 0) "Exiting" "menu option 0")
(assert-equal (menu-action 99) "Invalid choice" "invalid menu option")

;; Conditional evaluation order (cond)
(define test-order
  (lambda (x)
    (cond
      ((> x 100) "very large")
      ((> x 50) "large")
      ((> x 10) "medium")
      ((> x 0) "small")
      (else "non-positive"))))

(assert-equal (test-order 150) "very large" "test order very large")
(assert-equal (test-order 75) "large" "test order large")
(assert-equal (test-order 25) "medium" "test order medium")
(assert-equal (test-order 5) "small" "test order small")
(assert-equal (test-order 0) "non-positive" "test order non-positive")

