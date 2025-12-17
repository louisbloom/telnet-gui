;; Character Type Tests
;; Tests for the LISP_CHAR type, reader syntax, and character operations

(load "tests/test-helpers.lisp")

;; ===========================================
;; Character Literals
;; ===========================================

;; Simple ASCII characters
(assert-equal #\a #\a "char literal #\\a")
(assert-equal #\Z #\Z "char literal #\\Z")
(assert-equal #\0 #\0 "char literal #\\0")

;; Named characters
(assert-equal #\space (code-char 32) "named char #\\space")
(assert-equal #\newline (code-char 10) "named char #\\newline")
(assert-equal #\tab (code-char 9) "named char #\\tab")
(assert-equal #\return (code-char 13) "named char #\\return")
(assert-equal #\escape (code-char 27) "named char #\\escape")
(assert-equal #\null (code-char 0) "named char #\\null")
(assert-equal #\backspace (code-char 8) "named char #\\backspace")
(assert-equal #\delete (code-char 127) "named char #\\delete")

;; Hex notation
(assert-equal #\x41 #\A "hex char #\\x41 = A")
(assert-equal #\x61 #\a "hex char #\\x61 = a")
(assert-equal #\x1b #\escape "hex char #\\x1b = escape")

;; Unicode notation
(assert-equal #\u4e16 (code-char 19990) "unicode char #\\u4e16")
(assert-equal (char-code #\u4e16) 19990 "unicode char code")

;; ===========================================
;; Type Predicate
;; ===========================================

(assert-true (char? #\a) "char? on character")
(assert-false (char? "a") "char? on string")
(assert-false (char? 97) "char? on integer")
(assert-false (char? nil) "char? on nil")

;; ===========================================
;; Character/Code Conversion
;; ===========================================

(assert-equal (char-code #\a) 97 "char-code #\\a")
(assert-equal (char-code #\A) 65 "char-code #\\A")
(assert-equal (char-code #\space) 32 "char-code #\\space")
(assert-equal (char-code #\newline) 10 "char-code #\\newline")

(assert-equal (code-char 97) #\a "code-char 97")
(assert-equal (code-char 65) #\A "code-char 65")
(assert-equal (code-char 32) #\space "code-char 32")
(assert-equal (code-char 0) #\null "code-char 0")

;; ===========================================
;; String/Character Conversion
;; ===========================================

(assert-equal (char->string #\a) "a" "char->string #\\a")
(assert-equal (char->string #\space) " " "char->string #\\space")
(assert-equal (char->string #\newline) "\n" "char->string #\\newline")

(assert-equal (string->char "a") #\a "string->char \"a\"")
(assert-equal (string->char " ") #\space "string->char \" \"")
(assert-equal (string->char "\n") #\newline "string->char newline")

;; ===========================================
;; Character Comparisons
;; ===========================================

(assert-true (char=? #\a #\a) "char=? equal")
(assert-false (char=? #\a #\b) "char=? not equal")

(assert-true (char<? #\a #\b) "char<? a < b")
(assert-false (char<? #\b #\a) "char<? b < a")
(assert-false (char<? #\a #\a) "char<? a < a")

(assert-true (char>? #\b #\a) "char>? b > a")
(assert-false (char>? #\a #\b) "char>? a > b")
(assert-false (char>? #\a #\a) "char>? a > a")

(assert-true (char<=? #\a #\b) "char<=? a <= b")
(assert-true (char<=? #\a #\a) "char<=? a <= a")
(assert-false (char<=? #\b #\a) "char<=? b <= a")

(assert-true (char>=? #\b #\a) "char>=? b >= a")
(assert-true (char>=? #\a #\a) "char>=? a >= a")
(assert-false (char>=? #\a #\b) "char>=? a >= b")

;; ===========================================
;; Case Conversion
;; ===========================================

(assert-equal (char-upcase #\a) #\A "char-upcase #\\a")
(assert-equal (char-upcase #\z) #\Z "char-upcase #\\z")
(assert-equal (char-upcase #\A) #\A "char-upcase #\\A already upper")
(assert-equal (char-upcase #\1) #\1 "char-upcase #\\1 unchanged")

(assert-equal (char-downcase #\A) #\a "char-downcase #\\A")
(assert-equal (char-downcase #\Z) #\z "char-downcase #\\Z")
(assert-equal (char-downcase #\a) #\a "char-downcase #\\a already lower")
(assert-equal (char-downcase #\1) #\1 "char-downcase #\\1 unchanged")

;; ===========================================
;; Character Classification
;; ===========================================

(assert-true (char-alphabetic? #\a) "char-alphabetic? #\\a")
(assert-true (char-alphabetic? #\Z) "char-alphabetic? #\\Z")
(assert-false (char-alphabetic? #\1) "char-alphabetic? #\\1")
(assert-false (char-alphabetic? #\space) "char-alphabetic? #\\space")

(assert-true (char-numeric? #\0) "char-numeric? #\\0")
(assert-true (char-numeric? #\9) "char-numeric? #\\9")
(assert-false (char-numeric? #\a) "char-numeric? #\\a")
(assert-false (char-numeric? #\space) "char-numeric? #\\space")

(assert-true (char-whitespace? #\space) "char-whitespace? #\\space")
(assert-true (char-whitespace? #\tab) "char-whitespace? #\\tab")
(assert-true (char-whitespace? #\newline) "char-whitespace? #\\newline")
(assert-false (char-whitespace? #\a) "char-whitespace? #\\a")
(assert-false (char-whitespace? #\0) "char-whitespace? #\\0")

;; ===========================================
;; string-ref returns character
;; ===========================================

(assert-true (char? (string-ref "hello" 0)) "string-ref returns char")
(assert-equal (string-ref "hello" 0) #\h "string-ref first char")
(assert-equal (string-ref "hello" 4) #\o "string-ref last char")
(assert-equal (string-ref "a" 0) #\a "string-ref single char string")

;; ===========================================
;; string-append alias for concat
;; ===========================================

(assert-equal (string-append "hello" " " "world") "hello world" "string-append")
(assert-equal (string-append "a" "b" "c") "abc" "string-append multiple")
(assert-equal (string-append) "" "string-append empty")
