;; UTF-8 String Operations
;; Demonstrates character-based (not byte-based) string operations with Unicode
;; The language handles multi-byte UTF-8 sequences correctly
(load "tests/test-helpers.lisp")

;; Test string with mixed scripts: ASCII, Chinese, emoji
(define test_str "Hello, 世界! 🌍")

;; String length counts actual characters, not bytes
;; H-e-l-l-o-,- -世-界-!- -🌍 = 12 characters
(assert-equal (length test_str) 12 "UTF-8 string length")
;; Extract substring by character index (not byte index)
(assert-equal (substring test_str 7 9) "世界" "substring with Chinese characters")
;; Get character at specific index (returns character type)
(assert-equal (string-ref test_str 0) #\H "string-ref ASCII character")
(assert-equal (string-ref test_str 7) (code-char 19990)
 "string-ref Chinese character")

;; Test with emoji (composed of multiple UTF-8 bytes)
(define emoji_str "Hello 🚀 World")

(assert-equal (length emoji_str) 13 "string length with emoji")
;; Extract just the emoji by character index
(assert-equal (substring emoji_str 6 7) "🚀" "substring with emoji")
;; String concatenation works correctly with UTF-8
(assert-equal (concat "Hello, " "世界!") "Hello, 世界!" "concat with UTF-8")
;; Verify substring with Japanese characters
(assert-equal (substring "こんにちは" 0 1) "こ" "substring with Japanese characters")
;; String transformations with UTF-8
;; Note: Case conversion only works for ASCII characters, Unicode is preserved
(assert-equal (string-upcase "hello 世界") "HELLO 世界" "upcase preserves Unicode")
(assert-equal (string-downcase "HELLO 世界") "hello 世界"
 "downcase preserves Unicode")
(assert-equal (string-replace "hello 世界" "世界" "universe") "hello universe"
 "replace Chinese characters")
(assert-equal (string-replace "hello 世界" "o" "O") "hellO 世界"
 "replace ASCII in UTF-8 string")

