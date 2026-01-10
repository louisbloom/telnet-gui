;;; spell-translator.lisp --- Translate garbled spell utterances (ROM 2.4 cipher)
;;;
;;; Usage:
;;;   (load "contrib/spell-translator.lisp")
;;;
;;; When someone casts a spell, you see:
;;;   Det utters the words, 'qaiyjcandus zrzwunsohar'.
;;;
;;; This script weaves in a translation right after the spell:
;;;   Det utters the words, 'qaiyjcandus zrzwunsohar' (conjure elemental).
;;;
;;; The ROM 2.4 MUD garble algorithm uses syllable substitution followed by
;;; single-character substitution. This script reverses that process.
;;;
;;; To add custom word overrides (for ambiguous translations):
;;;   (spell-add "garbled-word" "correct-word")
;;;
;;; To remove an override:
;;;   (spell-remove "garbled-word")
;;; ============================================================================
;;; Reverse Cipher Tables (ROM 2.4 magic.c)
;;; ============================================================================
;; Reverse syllable table - sorted by length (longest first for greedy matching)
;; Original ROM: ar→abra, au→kada, etc. This is the reverse.
(define *spell-reverse-syllables*
  '(("candus" . "re") ("oculo" . "de") ("sabru" . "son") ("infra" . "tect")
    ("lacri" . "ness") ("abra" . "ar") ("kada" . "au") ("fido" . "bless")
    ("nose" . "blind") ("mosa" . "bur") ("judi" . "cu") ("unso" . "en")
    ("dies" . "light") ("sido" . "move") ("illa" . "ning") ("duda" . "per")
    ("cula" . "tri") ("nofo" . "ven") ("gru" . "ra") ("ima" . "fresh")
    ("zak" . "mor") ("hi" . "lo")))

;; Reverse single-character table
;; Ambiguous mappings (a→a/o, z→e/v, y→f/j) default to most common letter
(define *spell-reverse-chars*
  '((#\a . #\a) ; ambiguous: could be 'o', defaulting to 'a'
    (#\b . #\b) (#\q . #\c) (#\e . #\d) (#\z . #\e) ; ambiguous: could be 'v', defaulting to 'e'
    (#\y . #\f) ; ambiguous: could be 'j', defaulting to 'f'
    (#\o . #\g) (#\p . #\h) (#\u . #\i) (#\t . #\k) (#\r . #\l) (#\w . #\m)
    (#\i . #\n) (#\s . #\p) (#\d . #\q) (#\f . #\r) (#\g . #\s) (#\h . #\t)
    (#\j . #\u) (#\n . #\x) (#\l . #\y) (#\k . #\z) (#\x . #\w) (#\c . #\c) ; fallback - not in original cipher
    (#\m . #\m) ; fallback - not in original cipher
    (#\v . #\v))) ; fallback - not in original cipher

;;; ============================================================================
;;; Known Spell Words (skip translation for same-class visibility)
;;; ============================================================================
;; When you're the same class as the caster, you see the real spell name.
;; If any word in the utterance matches a known spell word, skip translation.
(define *known-spell-words*
  '("acid" "align" "armor" "aura" "barrier" "benediction" "blast" "bless"
    "blind" "bolt" "breath" "burn" "call" "calm" "cancel" "cause" "chain"
    "change" "charm" "chill" "colour" "continual" "control" "create" "critical"
    "cure" "curse" "demon" "detect" "disease" "dispel" "disrupt" "door" "drain"
    "earth" "enchant" "energy" "evil" "exorcism" "explosive" "faerie"
    "farsight" "fire" "flame" "fly" "fog" "food" "frenzy" "frost" "gas" "gate"
    "giant" "good" "grasp" "hands" "harm" "haste" "heal" "heat" "hidden" "high"
    "holy" "identify" "infravision" "invis" "know" "light" "locate" "magic"
    "mass" "metal" "missile" "negative" "neuro" "nexus" "object" "pass"
    "person" "plague" "poison" "positive" "portal" "protection" "ray" "recall"
    "recharge" "refresh" "remove" "resist" "restoration" "resurrection" "rose"
    "sanctuary" "serious" "sex" "shield" "shock" "skin" "sleep" "slow" "spray"
    "spring" "stone" "strength" "summon" "tele" "touch" "truth" "ventriloquate"
    "water" "weaken" "weapon" "weather" "word" "wrath"))

;; Check if utterance contains any known spell word
(defun known-spell? (phrase)
  "Return true if phrase contains any known spell word (no translation needed)"
  (let ((lower-phrase (string-downcase phrase)))
    (known-spell-check-list lower-phrase *known-spell-words*)))

;; Recursively check if phrase contains any word from the list
(defun known-spell-check-list (phrase words)
  "Check if phrase contains any word from the list"
  (if (null? words)
    nil
    (if (string-contains? phrase (car words))
      #t
      (known-spell-check-list phrase (cdr words)))))

;;; ============================================================================
;;; Dictionary Overrides
;;; ============================================================================
;; Hash table for word overrides (garbled → correct)
;; Users can add entries with (spell-add "garbled" "correct")
(define *spell-dictionary* (make-hash-table))

;; Built-in overrides for common spells affected by ambiguity
(hash-set! *spell-dictionary* "qaiyjcandus" "conjure") ; canfure → conjure
(hash-set! *spell-dictionary* "eaaf" "door") ; daar → door
(hash-set! *spell-dictionary* "barh" "bolt") ; balt → bolt
(hash-set! *spell-dictionary* "bajroculof" "boulder") ; baulder → boulder
(hash-set! *spell-dictionary* "aiqpaf" "anchor") ; anchar → anchor
(hash-set! *spell-dictionary* "abraqpai" "archon") ; archan → archon
(hash-set! *spell-dictionary* "uizug" "invis") ; ineis → invis
(hash-set! *spell-dictionary* "aiozr" "angel") ; ansel → angel
(hash-set! *spell-dictionary* "uiygruzuguai" "infravision") ; infraeisian → infravision
(hash-set! *spell-dictionary* "zawsufuq" "vampiric") ; eampiric → vampiric
(hash-set! *spell-dictionary* "izjfahiouqar" "neurological") ; neuralogical → neurological
(hash-set! *spell-dictionary* "eugfjshuai" "disruption") ; disruptian → disruption
(hash-set! *spell-dictionary* "izoahuzz" "negative") ; negatiee → negative
(hash-set! *spell-dictionary* "saguhuzz" "positive") ; pasitiee → positive

;; Add a custom word override
(defun spell-add (garbled correct)
  "Add a dictionary override: (spell-add \"garbled\" \"correct\")"
  (hash-set! *spell-dictionary* garbled correct)
  (princ "Added: ")
  (princ garbled)
  (princ " -> ")
  (princ correct)
  (terpri)
  correct)

;; Remove a word override
(defun spell-remove (garbled)
  "Remove a dictionary override"
  (hash-remove! *spell-dictionary* garbled))

;;; ============================================================================
;;; Translation Functions
;;; ============================================================================
;; Translate a single character using the reverse cipher
(defun translate-garbled-char (c)
  "Translate a single garbled character to original"
  (let ((pair (assoc c *spell-reverse-chars*))) (if pair (cdr pair) c))) ; Unknown chars pass through unchanged

;; Helper to recursively search through syllables for a match
(defun try-match-syllable-helper (word-lower i len syllables)
  "Recursively search through syllables for first match"
  (if (null? syllables)
    nil
    (let* ((pair (car syllables))
           (garbled (car pair))
           (original (cdr pair))
           (garbled-len (length garbled)))
      (if
        (and (<= (+ i garbled-len) len)
             (string=? (substring word-lower i (+ i garbled-len)) garbled))
        (cons original garbled-len)
        (try-match-syllable-helper word-lower i len (cdr syllables))))))

;; Try to match a syllable at position i in word-lower
;; Returns (original-text . chars-consumed) or nil if no match
(defun try-match-syllable (word-lower i len)
  "Try to match a syllable pattern at position i"
  (try-match-syllable-helper word-lower i len *spell-reverse-syllables*))

;; Recursive helper for word translation
(defun translate-word-helper (word-lower i len acc)
  "Recursively translate word from position i, accumulating results in acc"
  (if (>= i len)
    ;; Done - reverse and concatenate results
    (apply string-append (reverse acc))
    ;; Try to match syllable first
    (let ((syllable-match (try-match-syllable word-lower i len)))
      (if syllable-match
        ;; Matched a syllable - advance by syllable length
        (translate-word-helper word-lower (+ i (cdr syllable-match)) len
         (cons (car syllable-match) acc))
        ;; No syllable - translate single char
        (let ((c (string-ref word-lower i)))
          (translate-word-helper word-lower (+ i 1) len
           (cons (char->string (translate-garbled-char c)) acc)))))))

;; Translate a single word using greedy syllable matching + char fallback
(defun translate-garbled-word (word)
  "Translate a garbled word to its original form"
  ;; First check dictionary override
  (let ((override (hash-ref *spell-dictionary* word)))
    (if override
      override
      ;; Apply reverse cipher algorithm using recursive helper
      (let ((word-lower (string-downcase word)))
        (translate-word-helper word-lower 0 (length word-lower) '())))))

;; Translate a phrase (multiple words)
(defun translate-garbled-phrase (phrase)
  "Translate a garbled phrase (space-separated words)"
  (let ((words (split phrase " ")))
    (join (map translate-garbled-word words) " ")))

;;; ============================================================================
;;; Filter Hook
;;; ============================================================================
;; Regex pattern for spell utterances
;; Matches: "Name utters the words, 'garbled text'." (with optional trailing whitespace/newlines)
;; Captures: (1) name, (2) garbled text
;; Note: Pattern allows trailing whitespace/newlines after the period to handle real server text
(define *spell-utter-pattern*
  "([A-Za-z' ]+) utters the words, '([^']+)'[.]?\\s*")

;; Muted magic purple color for translations
(define *spell-color* "\033[38;2;147;112;219m")

(define *spell-color-reset* "\033[0m")

;; Filter function for telnet-input-filter-hook
(defun spell-translator-filter (text)
  "Weave spell translation into utterance lines"
  ;; Strip ANSI codes for matching, but use original text for replacement
  (let* ((clean-text (strip-ansi text))
         (groups (regex-extract *spell-utter-pattern* clean-text)))
    ;; Main logic: check if pattern matched
    (if groups
      ;; Found an utterance - check if it needs translation
      (let ((garbled (string-trim (cadr groups))))
        (if (known-spell? garbled)
          ;; Already readable (same class as caster) - no translation needed
          text
          ;; Garbled text - translate and weave in after the spell
          (let ((translated (translate-garbled-phrase garbled)))
            ;; Skip if translation matches original (already readable)
            (if
              (string=? (string-downcase translated) (string-downcase garbled))
              text
              ;; Replace quote-period (and any trailing whitespace) with quote-space-translation-period
              ;; Pattern captures trailing whitespace/newlines to preserve them
              ;; $1 = captured trailing whitespace (preserved in replacement)
              (let ((replacement
                     (string-append "' " *spell-color* "(" translated ")"
                      *spell-color-reset* ".$1")))
                (let ((result (regex-replace "'\\.(\\s*)" text replacement)))
                  result))))))
      ;; No match - pass through unchanged
      text)))

;;; ============================================================================
;;; Hook Registration
;;; ============================================================================
;; Register the filter hook
(add-hook 'telnet-input-filter-hook 'spell-translator-filter)

;; Startup message
(terminal-echo
 "Spell translator loaded. Utterances will be translated automatically.\r\n")
(terminal-echo "  Add overrides: (spell-add \"garbled\" \"correct\")\r\n")
(terminal-echo "  Remove:        (spell-remove \"garbled\")\r\n")

