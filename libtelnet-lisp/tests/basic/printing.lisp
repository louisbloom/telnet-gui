;; Printing Functions (Common Lisp Style)
;; Tests for princ, prin1, and print functions
;; Note: These functions print to stdout and return the object being printed

;; princ - prints without quotes (human-readable), returns the object
(princ "Hello, World!")              ; => "Hello, World!"
(princ 42)                           ; => 42
(princ 3.14)                         ; => 3.14
(princ #t)                           ; => #t
(princ #f)                           ; => #f
(princ nil)                          ; => nil
(princ '(1 2 3))                     ; => (1 2 3)
(princ 'symbol)                      ; => symbol

;; prin1 - prints with quotes (readable representation), returns the object
(prin1 "Hello, World!")              ; => "Hello, World!"
(prin1 42)                           ; => 42
(prin1 3.14)                         ; => 3.14
(prin1 #t)                           ; => #t
(prin1 #f)                           ; => #f
(prin1 nil)                          ; => nil
(prin1 '(1 2 3))                     ; => (1 2 3)
(prin1 'symbol)                      ; => symbol

;; print - like prin1 but adds newline before and after, returns the object
(print "Hello, World!")              ; => "Hello, World!"
(print 42)                           ; => 42
(print 3.14)                         ; => 3.14
(print #t)                           ; => #t
(print #f)                           ; => #f
(print nil)                          ; => nil
(print '(1 2 3))                     ; => (1 2 3)
(print 'symbol)                      ; => symbol

;; All functions return the object being printed (Common Lisp convention)
(define result1 (princ "test"))      ; => "test"
result1                               ; => "test"
(define result2 (prin1 "test"))       ; => "test"
result2                               ; => "test"
(define result3 (print "test"))       ; => "test"
result3                               ; => "test"

;; Test with vectors
(define vec (make-vector 3))
(vector-set! vec 0 1)
(vector-set! vec 1 2)
(vector-set! vec 2 3)
(princ vec)                          ; => #(1 2 3)
(prin1 vec)                          ; => #(1 2 3)
(print vec)                          ; => #(1 2 3)

;; Test with nested structures
(princ '(1 (2 3) 4))                 ; => (1 (2 3) 4)
(prin1 '(1 (2 3) 4))                 ; => (1 (2 3) 4)
(print '(1 (2 3) 4))                 ; => (1 (2 3) 4)

;; Test with empty structures
(princ '())                           ; => ()
(prin1 '())                           ; => ()
(print '())                           ; => ()

;; Test with strings containing special characters
(princ "Hello\nWorld")               ; => "Hello\nWorld"
(prin1 "Hello\nWorld")               ; => "Hello\nWorld"
(print "Hello\nWorld")               ; => "Hello\nWorld"
