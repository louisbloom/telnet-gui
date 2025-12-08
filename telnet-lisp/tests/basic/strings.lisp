;; Advanced string operations

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; ===========================================
;; String Type Checking (regression test for string? predicate)
;; ===========================================

(assert-true (string? "hello") "string? recognizes non-empty string")
(assert-true (string? "") "string? recognizes empty string")
(assert-nil (string? 42) "string? rejects integer")
(assert-nil (string? nil) "string? rejects nil")
(assert-nil (string? #(1 2 3)) "string? rejects vector")
(assert-nil (string? '(a b c)) "string? rejects list")

;; Split test
(assert-equal (split "apple-banana-cherry" "-") '("apple" "banana" "cherry") "split string by delimiter")

;; String contains
(assert-true (string-contains? "hello world" "world") "string-contains? finds substring")

;; String match with wildcards
(assert-true (string-match? "hello" "h*o") "string-match? with wildcard pattern")

;; String prefix
(assert-true (string-prefix? "hel" "hello") "string-prefix? matches prefix")
(assert-true (string-prefix? "lis" "lisp") "string-prefix? matches 'lis' in 'lisp'")
(assert-false (string-prefix? "xyz" "hello") "string-prefix? rejects non-matching prefix")

;; String transformations
(assert-equal (string-replace "hello world" "world" "universe") "hello universe" "string-replace changes substring")
(assert-equal (string-replace "hello" "l" "L") "heLLo" "string-replace changes all occurrences")
(assert-equal (string-replace "foofoofoo" "foo" "bar") "barbarbar" "string-replace multiple occurrences")
(assert-equal (string-replace "x" "x" "y") "y" "string-replace single character")
(assert-equal (string-replace "abc" "x" "y") "abc" "string-replace with no match returns original")

;; String case conversion
(assert-equal (string-upcase "hello world") "HELLO WORLD" "string-upcase converts to uppercase")
(assert-equal (string-upcase "Hello World") "HELLO WORLD" "string-upcase on mixed case")
(assert-equal (string-upcase "123abc") "123ABC" "string-upcase preserves numbers")
(assert-equal (string-downcase "HELLO WORLD") "hello world" "string-downcase converts to lowercase")
(assert-equal (string-downcase "Hello World") "hello world" "string-downcase on mixed case")
(assert-equal (string-downcase "123ABC") "123abc" "string-downcase preserves numbers")

;; String comparisons
(assert-true (string<? "abc" "def") "string<? compares strings")
