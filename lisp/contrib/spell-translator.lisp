;;; spell-translator.lisp --- Translate garbled spell utterances (ROM 2.4 cipher)
;;;
;;; Usage:
;;;   (load "contrib/spell-translator.lisp")
;;;
;;; When someone casts a spell, you see:
;;;   Det utters the words, 'qaiyjcandus zrzwunsohar'.
;;;
;;; This script adds a translation annotation:
;;;   Det utters the words, 'qaiyjcandus zrzwunsohar'. [conjure elemental]
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
  '(("candus" . "re")
     ("oculo" . "de")
     ("sabru" . "son")
     ("infra" . "tect")
     ("lacri" . "ness")
     ("abra" . "ar")
     ("kada" . "au")
     ("fido" . "bless")
     ("nose" . "blind")
     ("mosa" . "bur")
     ("judi" . "cu")
     ("unso" . "en")
     ("dies" . "light")
     ("sido" . "move")
     ("illa" . "ning")
     ("duda" . "per")
     ("cula" . "tri")
     ("nofo" . "ven")
     ("gru" . "ra")
     ("ima" . "fresh")
     ("zak" . "mor")
     ("hi" . "lo")))

;; Reverse single-character table
;; Ambiguous mappings (a→a/o, z→e/v, y→f/j) default to most common letter
(define *spell-reverse-chars*
  '((#\a . #\a)  ; ambiguous: could be 'o', defaulting to 'a'
     (#\b . #\b)
     (#\q . #\c)
     (#\e . #\d)
     (#\z . #\e)  ; ambiguous: could be 'v', defaulting to 'e'
     (#\y . #\f)  ; ambiguous: could be 'j', defaulting to 'f'
     (#\o . #\g)
     (#\p . #\h)
     (#\u . #\i)
     (#\t . #\k)
     (#\r . #\l)
     (#\w . #\m)
     (#\i . #\n)
     (#\s . #\p)
     (#\d . #\q)
     (#\f . #\r)
     (#\g . #\s)
     (#\h . #\t)
     (#\j . #\u)
     (#\n . #\x)
     (#\l . #\y)
     (#\k . #\z)
     (#\x . #\w)
     (#\c . #\c)  ; fallback - not in original cipher
     (#\m . #\m)  ; fallback - not in original cipher
     (#\v . #\v))); fallback - not in original cipher

;;; ============================================================================
;;; Dictionary Overrides
;;; ============================================================================

;; Hash table for word overrides (garbled → correct)
;; Users can add entries with (spell-add "garbled" "correct")
(define *spell-dictionary* (make-hash-table))

;; Built-in overrides for common spells affected by ambiguity
(hash-set! *spell-dictionary* "qaiyjcandus" "conjure")  ; canfure → conjure
(hash-set! *spell-dictionary* "eaaf" "door")            ; daar → door
(hash-set! *spell-dictionary* "barh" "bolt")            ; balt → bolt
(hash-set! *spell-dictionary* "bajroculof" "boulder")   ; baulder → boulder
(hash-set! *spell-dictionary* "aiqpaf" "anchor")        ; anchar → anchor
(hash-set! *spell-dictionary* "abraqpai" "archon")      ; archan → archon
(hash-set! *spell-dictionary* "uizug" "invis")          ; ineis → invis
(hash-set! *spell-dictionary* "aiozr" "angel")          ; ansel → angel
(hash-set! *spell-dictionary* "uiygruzuguai" "infravision") ; infraeisian → infravision
(hash-set! *spell-dictionary* "zawsufuq" "vampiric")    ; eampiric → vampiric

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
  (let ((pair (assoc c *spell-reverse-chars*)))
    (if pair
      (cdr pair)
      c)))  ; Unknown chars pass through unchanged

;; Helper to recursively search through syllables for a match
(defun try-match-syllable-helper (word-lower i len syllables)
  "Recursively search through syllables for first match"
  (if (null? syllables)
    nil
    (let* ((pair (car syllables))
            (garbled (car pair))
            (original (cdr pair))
            (garbled-len (length garbled)))
      (if (and (<= (+ i garbled-len) len)
            (string=? (substring word-lower i (+ i garbled-len))
              garbled))
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
        (translate-word-helper word-lower
          (+ i (cdr syllable-match))
          len
          (cons (car syllable-match) acc))
        ;; No syllable - translate single char
        (let ((c (string-ref word-lower i)))
          (translate-word-helper word-lower
            (+ i 1)
            len
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
;; Matches: "Name utters the words, 'garbled text'."
(define *spell-utter-pattern*
  "([A-Za-z' ]+) utters the words, '([^']+)'\\.")

;; Filter function for telnet-input-filter-hook
(defun spell-translator-filter (text)
  "Add spell translation annotations to utterance lines"
  (let ((groups (regex-extract *spell-utter-pattern* text)))
    (if groups
      ;; Found an utterance - translate and annotate
      ;; groups is (name-match garbled-match), we want the second one
      (let* ((garbled (car (cdr groups)))
              (translated (translate-garbled-phrase garbled)))
        ;; Append translation in brackets
        (string-append text " [" translated "]"))
      ;; No match - pass through unchanged
      text)))

;;; ============================================================================
;;; Hook Registration
;;; ============================================================================

;; Register the filter hook
(add-hook 'telnet-input-filter-hook spell-translator-filter)

;; Startup message
(princ "Spell translator loaded. Utterances will be translated automatically.\n")
(princ "  Add overrides: (spell-add \"garbled\" \"correct\")\n")
(princ "  Remove:        (spell-remove \"garbled\")\n")
