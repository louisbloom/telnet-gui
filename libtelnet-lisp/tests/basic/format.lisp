;;; Test format and terpri functions

;; Test format with nil destination (returns string)
(format nil "Hello, world!")              ; => "Hello, world!"
(format nil "The answer is ~A" 42)        ; => "The answer is 42"
(format nil "~A + ~A = ~A" 2 3 5)         ; => "2 + 3 = 5"

;; Test format with ~S (S-expression, with quotes)
(format nil "String: ~S" "hello")         ; => "String: \"hello\""
(format nil "Number: ~S" 42)              ; => "Number: 42"
(format nil "List: ~S" '(1 2 3))          ; => "List: (1 2 3)"

;; Test format with ~A (aesthetic, no quotes)
(format nil "String: ~A" "hello")         ; => "String: hello"
(format nil "Name: ~A" "Alice")           ; => "Name: Alice"

;; Test format with ~% (newline)
(format nil "Line 1~%Line 2")             ; => "Line 1\nLine 2"
(format nil "First~%Second~%Third")       ; => "First\nSecond\nThird"

;; Test format with ~~ (literal tilde)
(format nil "Tilde: ~~")                  ; => "Tilde: ~"
(format nil "~~A is not a directive")     ; => "~A is not a directive"

;; Test format with mixed directives
(format nil "Name: ~A, Age: ~A~%" "Bob" 30)  ; => "Name: Bob, Age: 30\n"
(format nil "~A: ~S" "Result" "success")     ; => "Result: \"success\""

;; Test format with numbers
(format nil "Integer: ~A" 123)            ; => "Integer: 123"
(format nil "Float: ~A" 3.14)             ; => "Float: 3.14"

;; Test format with booleans
(format nil "Boolean: ~A" #t)             ; => "Boolean: #t"
(format nil "Boolean: ~A" #f)             ; => "Boolean: #f"

;; Test format with symbols
(format nil "Symbol: ~A" 'foo)            ; => "Symbol: foo"
(format nil "Symbol: ~S" 'bar)            ; => "Symbol: bar"

;; Test format with lists
(format nil "List: ~A" '(a b c))          ; => "List: (a b c)"
(format nil "Empty: ~A" '())              ; => "Empty: nil"

;; Test format with variables
(define x 100)                             ; ignore
(format nil "Value of x is ~A" x)         ; => "Value of x is 100"

(define name "Alice")                      ; ignore
(format nil "Hello, ~A!" name)            ; => "Hello, Alice!"

;; Test format building complex strings
(format nil "~A~A~A" "Hello" " " "World") ; => "Hello World"
(format nil "Result: ~A~%" "Done")        ; => "Result: Done\n"

;; Test terpri (just prints newline, returns nil)
(terpri)                                  ; => nil

;; Note: format with #t destination prints to stdout and returns nil
;; (format #t "Hello!~%")  ; This would print "Hello!" followed by newline and return nil
;; We can't easily test stdout output in test format, so those are commented

;; Test case sensitivity of directives
(format nil "~a lowercase" 42)            ; => "42 lowercase"
(format nil "~s lowercase" "test")        ; => "\"test\" lowercase"
