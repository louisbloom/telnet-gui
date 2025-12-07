;; Test string-index builtin function

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; Basic tests
(assert-equal (string-index "hello world" "world") 6 "find 'world' in 'hello world'")
(assert-equal (string-index "hello world" "hello") 0 "find 'hello' at start")
(assert-equal (string-index "hello world" "o") 4 "find first 'o'")
(assert-equal (string-index "hello world" " ") 5 "find space character")

;; Not found
(assert-nil (string-index "hello world" "xyz") "substring not found returns nil")
(assert-nil (string-index "hello world" "WORLD") "case-sensitive search fails")

;; Empty strings
(assert-nil (string-index "" "x") "search in empty string returns nil")
(assert-equal (string-index "hello" "") 0 "empty needle returns 0")

;; UTF-8 support
(assert-equal (string-index "Hello, 世界!" "世") 7 "find UTF-8 character")
(assert-equal (string-index "Hello, 世界!" "!") 9 "find character after UTF-8")
(assert-equal (string-index "🌍🌎🌏" "🌎") 1 "find emoji in emoji string")

;; Multi-character needles
(assert-equal (string-index "abcdef" "cd") 2 "find two-character substring")
(assert-equal (string-index "abcdef" "cde") 2 "find three-character substring")
(assert-equal (string-index "abcdef" "def") 3 "find substring at end")

;; Case sensitivity
(assert-nil (string-index "Hello" "hello") "case mismatch uppercase to lowercase")
(assert-nil (string-index "hello" "Hello") "case mismatch lowercase to uppercase")
