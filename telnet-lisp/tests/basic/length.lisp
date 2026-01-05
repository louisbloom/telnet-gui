;; Tests for length and substring functions
;; Both work with Unicode code points (not graphemes)
(load "tests/test-helpers.lisp")

;; ============================================================
;; Length tests
;; ============================================================
;; Basic ASCII
(assert-equal (length "hello") 5 "ASCII string length")
(assert-equal (length "") 0 "Empty string length")
(assert-equal (string-length "test") 4 "string-length alias works")
;; CJK characters (each is one code point)
(assert-equal (length "世界") 2 "CJK characters")
(assert-equal (length "Hello, 世界!") 10 "Mixed ASCII and CJK")
;; Emoji without variation selector (one code point)
(assert-equal (length "🌍") 1 "Single emoji")
(assert-equal (length "Hello, 世界! 🌍") 12 "String with emoji")
;; Multiple emoji
(assert-equal (length "🌍🌎🌏") 3 "Multiple emoji")
;; Emoji with variation selector (U+FE0F) - counts as 2 code points
;; These are emoji followed by VS16 to force emoji presentation
(assert-equal (length "🌍️") 2 "Emoji with variation selector")
(assert-equal (length "⚔️") 2 "Crossed swords with VS16")
(assert-equal (length "▶️") 2 "Play button with VS16")
;; Multiple emoji with variation selectors (3 emoji + 3 VS16 = 6 code points)
(assert-equal (length "🌍️🌎️🌏️") 6 "Multiple emoji with VS16")
;; Mixed: some with VS16, some without (2+1+2 = 5 code points)
(assert-equal (length "🌍️🌎🌏️") 5 "Mixed emoji with/without VS16")
;; Precomposed characters
(assert-equal (length "café") 4 "Precomposed café")
;; Lists (unchanged behavior)
(assert-equal (length 'nil) 0 "Empty list length")
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
;; Emoji substring - code point based indexing
;; "🌍️" has 2 code points: emoji (pos 0) and VS16 (pos 1)
(assert-equal (substring "🌍️" 0 1) "🌍" "First code point (emoji)")
(assert-equal (substring "🌍️" 1 2) "️" "Second code point (VS16)")
(assert-equal (substring "🌍️" 0 2) "🌍️" "Both code points")
(assert-equal (length (substring "🌍️" 0 1)) 1 "Substring length matches")
;; Verify substring 0 to length returns original string
(assert-equal (substring "🌍️" 0 (length "🌍️")) "🌍️"
 "substring 0 length = original")
(assert-equal (substring "Hello, 世界! 🌍" 0 (length "Hello, 世界! 🌍"))
 "Hello, 世界! 🌍" "Full string via length")
;; Mixed string substring
(assert-equal (substring "Hello, 世界! 🌍" 7 9) "世界" "CJK from mixed string")
(assert-equal (substring "Hello, 世界! 🌍" 11 12) "🌍" "Emoji from mixed string")
;; Multiple emoji with VS16 - code point indexing
;; "🌍️🌎️🌏️" = positions: 🌍(0) ️(1) 🌎(2) ️(3) 🌏(4) ️(5)
(assert-equal (substring "🌍️🌎️🌏️" 0 2) "🌍️" "First emoji+VS16")
(assert-equal (substring "🌍️🌎️🌏️" 2 4) "🌎️" "Second emoji+VS16")
(assert-equal (substring "🌍️🌎️🌏️" 4 6) "🌏️" "Third emoji+VS16")

(princ "All length and substring tests passed!\n")

