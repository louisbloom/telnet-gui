;; UTF-8 support test
;; Demonstrates character-based string operations

;; Test string with mixed scripts
(define test_str "Hello, 世界! 🌍")

;; String length returns character count (not bytes)
(string-length test_str)
;; Result: 15 characters (H-e-l-l-o-,- -世-界-!- -🌍)

;; Extract substring by character index
(substring test_str 7 9)
;; Result: "世界"

;; Get character at specific index
(string-ref test_str 0)
;; Result: "H"

(string-ref test_str 7)
;; Result: "世"

;; Test with emoji
(define emoji_str "Hello 🚀 World")

(string-length emoji_str)
;; Result: 13

(substring emoji_str 6 6)
;; Result: "🚀"

;; Concat still works (UTF-8 byte concatenation is correct)
(concat "Hello, " "世界!")
;; Result: "Hello, 世界!"

;; Verify substring works with single character
(substring "こんにちは" 0 1)
;; Result: "こ"

