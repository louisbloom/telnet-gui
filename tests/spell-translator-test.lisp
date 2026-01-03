;;; spell-translator-test.lisp --- Tests for ROM 2.4 spell utterance translator
;;;
;;; Run with: ./build/telnet-lisp/lisp-repl tests/spell-translator-test.lisp

;; Load test helpers (includes init.lisp for hook system)
(load "test-helpers.lisp")

;; Load the spell translator
(load "../lisp/contrib/spell-translator.lisp")

(princ "\n=== Spell Translator Tests ===\n\n")

;;; ============================================================================
;;; Test: Single Character Translation
;;; ============================================================================
(princ "Testing single character translation...\n")

;; Basic character mappings
(assert-equal (translate-garbled-char #\q) #\c "q maps to c")
(assert-equal (translate-garbled-char #\z) #\e "z maps to e")
(assert-equal (translate-garbled-char #\u) #\i "u maps to i")
(assert-equal (translate-garbled-char #\o) #\g "o maps to g")
(assert-equal (translate-garbled-char #\p) #\h "p maps to h")
(assert-equal (translate-garbled-char #\y) #\f "y maps to f")
(assert-equal (translate-garbled-char #\r) #\l "r maps to l")
(assert-equal (translate-garbled-char #\w) #\m "w maps to m")
(assert-equal (translate-garbled-char #\i) #\n "i maps to n")
(assert-equal (translate-garbled-char #\g) #\s "g maps to s")
(assert-equal (translate-garbled-char #\h) #\t "h maps to t")

(princ "  Single character tests passed.\n")

;;; ============================================================================
;;; Test: Word Translation (Algorithmic)
;;; ============================================================================
(princ "Testing word translation (algorithmic)...\n")

;; Simple words without syllable patterns
(assert-equal (translate-garbled-word "yrl") "fly" "yrl translates to fly")
(assert-equal (translate-garbled-word "buie") "bind" "buie translates to bind")

;; Words with syllable patterns
(assert-equal (translate-garbled-word "yucandusbarr") "fireball" "yucandusbarr translates to fireball")
(assert-equal (translate-garbled-word "abrazak") "armor" "abrazak translates to armor")
(assert-equal (translate-garbled-word "judifgz") "curse" "judifgz translates to curse")
(assert-equal (translate-garbled-word "sraojz") "plague" "sraojz translates to plague")
(assert-equal (translate-garbled-word "zabrahpbuie") "earthbind" "zabrahpbuie translates to earthbind")
(assert-equal (translate-garbled-word "diesbuie") "lightbind" "diesbuie translates to lightbind")
(assert-equal (translate-garbled-word "noselacri") "blindness" "noselacri translates to blindness")

;; Complex words with multiple syllables
(assert-equal (translate-garbled-word "qzrzghuar") "celestial" "qzrzghuar translates to celestial")
(assert-equal (translate-garbled-word "qufqrz") "circle" "qufqrz translates to circle")
(assert-equal (translate-garbled-word "zrzwunsohar") "elemental" "zrzwunsohar translates to elemental")

(princ "  Word translation tests passed.\n")

;;; ============================================================================
;;; Test: Dictionary Overrides
;;; ============================================================================
(princ "Testing dictionary overrides...\n")

;; Built-in overrides for ambiguous translations
(assert-equal (translate-garbled-word "qaiyjcandus") "conjure"
              "qaiyjcandus uses dictionary override for conjure")
(assert-equal (translate-garbled-word "uizug") "invis"
              "uizug uses dictionary override for invis")
(assert-equal (translate-garbled-word "barh") "bolt"
              "barh uses dictionary override for bolt")
(assert-equal (translate-garbled-word "eaaf") "door"
              "eaaf uses dictionary override for door")

;; Test adding custom override
(spell-add "hzgh" "test")
(assert-equal (translate-garbled-word "hzgh") "test"
              "custom override works")

;; Test removing override
(spell-remove "hzgh")
(assert-equal (translate-garbled-word "hzgh") "test"  ; algorithmic: t-e-s-t
              "removed override falls back to algorithm")

(princ "  Dictionary override tests passed.\n")

;;; ============================================================================
;;; Test: Phrase Translation
;;; ============================================================================
(princ "Testing phrase translation...\n")

(assert-equal (translate-garbled-phrase "buie zrzwunsohar")
              "bind elemental"
              "multi-word: bind elemental")

(assert-equal (translate-garbled-phrase "qaiyjcandus zrzwunsohar")
              "conjure elemental"
              "multi-word: conjure elemental")

(assert-equal (translate-garbled-phrase "qzrzghuar qufqrz")
              "celestial circle"
              "multi-word: celestial circle")

(assert-equal (translate-garbled-phrase "waouq wuggurz")
              "magic missile"
              "multi-word: magic missile")

(assert-equal (translate-garbled-phrase "diesilla barh")
              "lightning bolt"
              "multi-word: lightning bolt")

(princ "  Phrase translation tests passed.\n")

;;; ============================================================================
;;; Test: Filter Hook
;;; ============================================================================
(princ "Testing filter hook...\n")

;; Test that filter adds annotation
(let ((input "Det utters the words, 'yrl'."))
  (let ((output (spell-translator-filter input)))
    (assert-true (regex-match? "\\[fly\\]" output)
                 "filter adds [fly] annotation")))

(let ((input "A mage merchant utters the words, 'yucandusbarr'."))
  (let ((output (spell-translator-filter input)))
    (assert-true (regex-match? "\\[fireball\\]" output)
                 "filter adds [fireball] annotation")))

;; Test non-matching text passes through unchanged
(assert-equal (spell-translator-filter "Hello world")
              "Hello world"
              "non-spell text unchanged")

(assert-equal (spell-translator-filter "You cast a spell.")
              "You cast a spell."
              "non-utter text unchanged")

(princ "  Filter hook tests passed.\n")

;;; ============================================================================
;;; Test: Edge Cases
;;; ============================================================================
(princ "Testing edge cases...\n")

;; Empty string
(assert-equal (translate-garbled-word "") "" "empty string handled")
(assert-equal (translate-garbled-phrase "") "" "empty phrase handled")

;; Single character
(assert-equal (translate-garbled-word "a") "a" "single char 'a'")
(assert-equal (translate-garbled-word "z") "e" "single char 'z'")

;; Unknown characters pass through
(assert-equal (translate-garbled-word "123") "123" "numbers pass through")

(princ "  Edge case tests passed.\n")

;;; ============================================================================
;;; Summary
;;; ============================================================================
(princ "\n=== All Spell Translator Tests Passed ===\n")
