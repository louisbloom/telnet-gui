					; Advanced string operations

					; ===========================================
					; String Type Checking (regression test for string? predicate)
					; ===========================================

(string? "hello")              ; => 1
(string? "")                   ; => 1
(string? 42)                   ; => nil
(string? nil)                  ; => nil
(string? #(1 2 3))             ; => nil
(string? '(a b c))             ; => nil

					; Split test
(split "apple-banana-cherry" "-")      ; => ("apple" "banana" "cherry")

					; String contains
(string-contains? "hello world" "world") ; => 1

					; String match with wildcards
(string-match? "hello" "h*o")           ; => 1

					; String prefix
(string-prefix? "hel" "hello")          ; => 1
(string-prefix? "lis" "lisp")           ; => 1
(string-prefix? "xyz" "hello")          ; => 0

					; String transformations
(string-replace "world" "universe" "hello world")  ; => "hello universe"
(string-replace "l" "L" "hello")                   ; => "heLLo"
(string-replace "foo" "bar" "foofoofoo")          ; => "barbarbar"
(string-replace "x" "y" "x")                      ; => "y"
(string-replace "x" "y" "abc")                    ; => "abc" (no match)

					; String case conversion
(string-upcase "hello world")                     ; => "HELLO WORLD"
(string-upcase "Hello World")                     ; => "HELLO WORLD"
(string-upcase "123abc")                          ; => "123ABC"
(string-downcase "HELLO WORLD")                    ; => "hello world"
(string-downcase "Hello World")                   ; => "hello world"
(string-downcase "123ABC")                        ; => "123abc"

					; String comparisons
(string< "abc" "def")                  ; => 1
