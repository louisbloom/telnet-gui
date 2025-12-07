;;; Test format and terpri functions

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; Test format with nil destination (returns string)
(assert-equal (format nil "Hello, world!") "Hello, world!" "format with no directives")
(assert-equal (format nil "The answer is ~A" 42) "The answer is 42" "format with ~A directive")
(assert-equal (format nil "~A + ~A = ~A" 2 3 5) "2 + 3 = 5" "format with multiple ~A directives")

;; Test format with ~S (S-expression, with quotes)
(assert-equal (format nil "String: ~S" "hello") "String: \"hello\"" "format with ~S for string")
(assert-equal (format nil "Number: ~S" 42) "Number: 42" "format with ~S for number")
(assert-equal (format nil "List: ~S" '(1 2 3)) "List: (1 2 3)" "format with ~S for list")

;; Test format with ~A (aesthetic, no quotes)
(assert-equal (format nil "String: ~A" "hello") "String: hello" "format with ~A for string (no quotes)")
(assert-equal (format nil "Name: ~A" "Alice") "Name: Alice" "format ~A displays string without quotes")

;; Test format with ~% (newline)
(assert-equal (format nil "Line 1~%Line 2") "Line 1\nLine 2" "format with ~% newline")
(assert-equal (format nil "First~%Second~%Third") "First\nSecond\nThird" "format with multiple ~% newlines")

;; Test format with ~~ (literal tilde)
(assert-equal (format nil "Tilde: ~~") "Tilde: ~" "format with ~~ produces literal tilde")
(assert-equal (format nil "~~A is not a directive") "~A is not a directive" "format with ~~ escapes directive")

;; Test format with mixed directives
(assert-equal (format nil "Name: ~A, Age: ~A~%" "Bob" 30) "Name: Bob, Age: 30\n" "format with mixed ~A and ~%")
(assert-equal (format nil "~A: ~S" "Result" "success") "Result: \"success\"" "format with ~A and ~S together")

;; Test format with numbers
(assert-equal (format nil "Integer: ~A" 123) "Integer: 123" "format with integer")
(assert-equal (format nil "Float: ~A" 3.14) "Float: 3.14" "format with float")

;; Test format with booleans
(assert-equal (format nil "Boolean: ~A" #t) "Boolean: #t" "format with boolean true")
(assert-equal (format nil "Boolean: ~A" #f) "Boolean: nil" "format with boolean false (#f = nil)")

;; Test format with symbols
(assert-equal (format nil "Symbol: ~A" 'foo) "Symbol: foo" "format with symbol using ~A")
(assert-equal (format nil "Symbol: ~S" 'bar) "Symbol: bar" "format with symbol using ~S")

;; Test format with lists
(assert-equal (format nil "List: ~A" '(a b c)) "List: (a b c)" "format with list")
(assert-equal (format nil "Empty: ~A" '()) "Empty: nil" "format with empty list")

;; Test format with variables
(define x 100)
(assert-equal (format nil "Value of x is ~A" x) "Value of x is 100" "format with variable value")

(define name "Alice")
(assert-equal (format nil "Hello, ~A!" name) "Hello, Alice!" "format with string variable")

;; Test format building complex strings
(assert-equal (format nil "~A~A~A" "Hello" " " "World") "Hello World" "format concatenates multiple values")
(assert-equal (format nil "Result: ~A~%" "Done") "Result: Done\n" "format with string and newline")

;; Test terpri (just prints newline, returns nil)
(assert-nil (terpri) "terpri returns nil")

;; Note: format with #t destination prints to stdout and returns nil
;; (format #t "Hello!~%")  ; This would print "Hello!" followed by newline and return nil
;; We can't easily test stdout output in test format, so those are commented

;; Test case sensitivity of directives
(assert-equal (format nil "~a lowercase" 42) "42 lowercase" "format ~a directive (lowercase)")
(assert-equal (format nil "~s lowercase" "test") "\"test\" lowercase" "format ~s directive (lowercase)")
