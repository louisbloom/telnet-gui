;; UTF-8 String Operations
;; Demonstrates character-based (not byte-based) string operations with Unicode
;; The language handles multi-byte UTF-8 sequences correctly

;; Test string with mixed scripts: ASCII, Chinese, emoji
(define test_str "Hello, 世界! 🌍")

;; String length counts actual characters, not bytes
;; H-e-l-l-o-,- -世-界-!- -🌍 = 15 characters
(string-length test_str)               ; => 15

;; Extract substring by character index (not byte index)
(substring test_str 7 9)                ; => "世界"

;; Get character at specific index (returns single character string)
(string-ref test_str 0)                ; => "H"
(string-ref test_str 7)                 ; => "世"

;; Test with emoji (composed of multiple UTF-8 bytes)
(define emoji_str "Hello 🚀 World")
(string-length emoji_str)               ; => 13

;; Extract just the emoji by character index
(substring emoji_str 6 6)               ; => "🚀"

;; String concatenation works correctly with UTF-8
(concat "Hello, " "世界!")              ; => "Hello, 世界!"

;; Verify substring with Japanese characters
(substring "こんにちは" 0 1)                ; => "こ"

;; String transformations with UTF-8
;; Note: Case conversion only works for ASCII characters, Unicode is preserved
(string-upcase "hello 世界")            ; => "HELLO 世界" (Unicode preserved)
(string-downcase "HELLO 世界")         ; => "hello 世界" (Unicode preserved)
(string-replace "世界" "universe" "hello 世界")  ; => "hello universe"
(string-replace "o" "O" "hello 世界")  ; => "hellO 世界"
