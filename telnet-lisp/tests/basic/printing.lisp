;; Printing Functions (Common Lisp Style)
;; Tests for princ, prin1, and print functions
;; Note: These functions print to stdout and return the object being printed

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; princ - prints without quotes (human-readable), returns the object
(assert-equal (princ "Hello, World!") "Hello, World!" "princ returns string")
(assert-equal (princ 42) 42 "princ returns integer")
(assert-equal (princ 3.14) 3.14 "princ returns float")
(assert-true (princ #t) "princ returns boolean true")
(assert-false (princ #f) "princ returns boolean false")
(assert-nil (princ nil) "princ returns nil")
(assert-equal (princ '(1 2 3)) '(1 2 3) "princ returns list")
(assert-equal (princ 'symbol) 'symbol "princ returns symbol")

;; prin1 - prints with quotes (readable representation), returns the object
(assert-equal (prin1 "Hello, World!") "Hello, World!" "prin1 returns string")
(assert-equal (prin1 42) 42 "prin1 returns integer")
(assert-equal (prin1 3.14) 3.14 "prin1 returns float")
(assert-true (prin1 #t) "prin1 returns boolean true")
(assert-false (prin1 #f) "prin1 returns boolean false")
(assert-nil (prin1 nil) "prin1 returns nil")
(assert-equal (prin1 '(1 2 3)) '(1 2 3) "prin1 returns list")
(assert-equal (prin1 'symbol) 'symbol "prin1 returns symbol")

;; print - like prin1 but adds newline before and after, returns the object
(assert-equal (print "Hello, World!") "Hello, World!" "print returns string")
(assert-equal (print 42) 42 "print returns integer")
(assert-equal (print 3.14) 3.14 "print returns float")
(assert-true (print #t) "print returns boolean true")
(assert-false (print #f) "print returns boolean false")
(assert-nil (print nil) "print returns nil")
(assert-equal (print '(1 2 3)) '(1 2 3) "print returns list")
(assert-equal (print 'symbol) 'symbol "print returns symbol")

;; All functions return the object being printed (Common Lisp convention)
(define result1 (princ "test"))
(assert-equal result1 "test" "princ result assigned to variable")
(define result2 (prin1 "test"))
(assert-equal result2 "test" "prin1 result assigned to variable")
(define result3 (print "test"))
(assert-equal result3 "test" "print result assigned to variable")

;; Test with vectors
(define vec (make-vector 3))
(vector-set! vec 0 1)
(vector-set! vec 1 2)
(vector-set! vec 2 3)
(assert-equal (princ vec) vec "princ returns vector")
(assert-equal (prin1 vec) vec "prin1 returns vector")
(assert-equal (print vec) vec "print returns vector")

;; Test with nested structures
(assert-equal (princ '(1 (2 3) 4)) '(1 (2 3) 4) "princ returns nested list")
(assert-equal (prin1 '(1 (2 3) 4)) '(1 (2 3) 4) "prin1 returns nested list")
(assert-equal (print '(1 (2 3) 4)) '(1 (2 3) 4) "print returns nested list")

;; Test with empty structures
(assert-nil (princ '()) "princ returns empty list as nil")
(assert-nil (prin1 '()) "prin1 returns empty list as nil")
(assert-nil (print '()) "print returns empty list as nil")

;; Test with strings containing special characters
(assert-equal (princ "Hello\nWorld") "Hello\nWorld" "princ returns string with newline")
(assert-equal (prin1 "Hello\nWorld") "Hello\nWorld" "prin1 returns string with newline")
(assert-equal (print "Hello\nWorld") "Hello\nWorld" "print returns string with newline")
