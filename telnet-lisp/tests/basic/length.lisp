;; Tests for length and substring functions
;; Both work with grapheme clusters (human-visible characters)

(load "tests/test-helpers.lisp")

;; ============================================================
;; Length tests
;; ============================================================

;; Basic ASCII
(assert-equal (length "hello") 5 "ASCII string length")
(assert-equal (length "") 0 "Empty string length")
(assert-equal (string-length "test") 4 "string-length alias works")

;; CJK characters (each is one grapheme)
(assert-equal (length "世界") 2 "CJK characters")
(assert-equal (length "Hello, 世界!") 10 "Mixed ASCII and CJK")

;; Emoji without variation selector (one grapheme)
(assert-equal (length "🌍") 1 "Single emoji")
(assert-equal (length "Hello, 世界! 🌍") 12 "String with emoji")

;; Multiple emoji
(assert-equal (length "🌍🌎🌏") 3 "Multiple emoji")

;; Emoji with variation selector (U+FE0F) - should still be 1 grapheme
;; These are emoji followed by VS16 to force emoji presentation
(assert-equal (length "🌍️") 1 "Emoji with variation selector")
(assert-equal (length "⚔️") 1 "Crossed swords with VS16")
(assert-equal (length "▶️") 1 "Play button with VS16")

;; Multiple emoji with variation selectors
(assert-equal (length "🌍️🌎️🌏️") 3 "Multiple emoji with VS16")

;; Mixed: some with VS16, some without
(assert-equal (length "🌍️🌎🌏️") 3 "Mixed emoji with/without VS16")

;; Precomposed characters
(assert-equal (length "café") 4 "Precomposed café")

;; Lists (unchanged behavior)
(assert-equal (length '()) 0 "Empty list length")
(assert-equal (length '(1 2 3)) 3 "List length")
(assert-equal (length '(a b c d e)) 5 "Symbol list length")

;; Vectors (unchanged behavior)
(assert-equal (length #()) 0 "Empty vector length")
(assert-equal (length #(1 2 3)) 3 "Vector length")

;; ============================================================
;; Substring tests (must be consistent with length)
;; ============================================================

;; Basic ASCII substring
(assert-equal (substring "hello" 0 5) "hello" "Full ASCII substring")
(assert-equal (substring "hello" 1 4) "ell" "Middle ASCII substring")
(assert-equal (substring "hello" 0 0) "" "Empty substring")

;; CJK substring
(assert-equal (substring "世界" 0 2) "世界" "Full CJK substring")
(assert-equal (substring "世界" 0 1) "世" "First CJK char")
(assert-equal (substring "世界" 1 2) "界" "Second CJK char")

;; Emoji substring - variation selector must be included
(assert-equal (substring "🌍️" 0 1) "🌍️" "Emoji+VS16 as single grapheme")
(assert-equal (length (substring "🌍️" 0 1)) 1 "Substring length matches")

;; Verify substring 0 to length returns original string
(assert-equal (substring "🌍️" 0 (length "🌍️")) "🌍️" "substring 0 length = original")
(assert-equal (substring "Hello, 世界! 🌍" 0 (length "Hello, 世界! 🌍")) "Hello, 世界! 🌍" "Full string via length")

;; Mixed string substring
(assert-equal (substring "Hello, 世界! 🌍" 7 9) "世界" "CJK from mixed string")
(assert-equal (substring "Hello, 世界! 🌍" 11 12) "🌍" "Emoji from mixed string")

;; Multiple emoji with VS16
(assert-equal (substring "🌍️🌎️🌏️" 0 1) "🌍️" "First emoji+VS16")
(assert-equal (substring "🌍️🌎️🌏️" 1 2) "🌎️" "Second emoji+VS16")
(assert-equal (substring "🌍️🌎️🌏️" 2 3) "🌏️" "Third emoji+VS16")

(princ "All length and substring tests passed!\n")
