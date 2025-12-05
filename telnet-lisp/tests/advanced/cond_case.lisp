;; cond and case examples
;; Demonstrate multi-way conditionals and pattern matching

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

(grade 95)  ; => "A"
(grade 85)  ; => "B"
(grade 75)  ; => "C"
(grade 65)  ; => "D"
(grade 50)  ; => "F"

;; Sign of a number
(define sign
  (lambda (n)
    (cond
      ((< n 0) -1)
      ((= n 0) 0)
      (else 1))))

(sign -5)   ; => -1
(sign 0)    ; => 0
(sign 10)   ; => 1

;; Abs using cond
(define abs
  (lambda (n)
    (cond
      ((< n 0) (- n))
      (else n))))

(abs -10)   ; => 10
(abs 5)     ; => 5

;; Cond without else returns NIL
(cond
  ((< 5 3) "no")
  ((< 10 5) "nope"))
                                        ; => NIL

;; Empty cond
(cond)
                                        ; => NIL

;; Single clause cond
(cond
  ((= 2 2) "yes"))
                                        ; => "yes"

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

(day-name 1)   ; => "Monday"
(day-name 3)   ; => "Wednesday"
(day-name 6)   ; => "Weekend"
(day-name 7)   ; => "Weekend"
(day-name 99)  ; => "Invalid"

;; Command dispatcher
(define command
  (lambda (cmd)
    (case cmd
      (("help" "h") "showing help")
      (("quit" "exit" "q") "quitting")
      (("save" "write") "saving")
      (else "unknown command"))))

(command "help")  ; => "showing help"
(command "h")     ; => "showing help"
(command "quit")  ; => "quitting"
(command "xyz")   ; => "unknown command"

;; Number classifier
(define classify-number
  (lambda (x)
    (case x
      ((0) "zero")
      ((-1 -2 -3) "small negative")
      ((1 2 3) "small positive")
      ((4 5 6 7 8 9 10) "medium")
      (else "large"))))

(classify-number 0)   ; => "zero"
(classify-number 3)   ; => "small positive"
(classify-number 7)   ; => "medium"
(classify-number 100); => "large"

;; Case without else returns NIL
(case 99
  ((1 2 3) "match"))
                                        ; => NIL

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

(classify -5)   ; => "negative"
(classify 0)    ; => "zero"
(classify 4)    ; => "positive even"
(classify 7)    ; => "positive odd"

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

(status 20 "student")  ; => "eligible for student discount"
(status 70 "senior")   ; => "senior discount"
(status 50 "senior")   ; => "not yet senior"

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

(menu-action 1)  ; => "Creating new file"
(menu-action 0)  ; => "Exiting"
(menu-action 99) ; => "Invalid choice"

;; Conditional evaluation order (cond)
(define test-order
  (lambda (x)
    (cond
      ((> x 100) "very large")
      ((> x 50) "large")
      ((> x 10) "medium")
      ((> x 0) "small")
      (else "non-positive"))))

(test-order 150) ; => "very large"
(test-order 75)  ; => "large"
(test-order 25)  ; => "medium"
(test-order 5)   ; => "small"
(test-order 0)   ; => "non-positive"

