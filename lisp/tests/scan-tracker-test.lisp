;; Tests for lisp/contrib/scan-tracker.lisp
;;
;; Run from project root:
;;   ./build/telnet-lisp/lisp-repl lisp/tests/scan-tracker-test.lisp

;; Load base test assertions (without init.lisp which has GUI dependencies)
(load "telnet-lisp/tests/test-helpers.lisp")

;; ============================================================================
;; MOCKS (normally provided by init.lisp and tintin.lisp)
;; ============================================================================

;; Mock *tintin-variables* hash table
(define *tintin-variables* (make-hash-table))

;; Mock add-hook (captures registrations)
(defvar *test-hooks* '())
(defun add-hook (hook-name fn-symbol)
  (set! *test-hooks* (cons (cons hook-name fn-symbol) *test-hooks*))
  nil)

;; Mock terminal-echo (captures output for testing)
(defvar *test-terminal-output* '())
(defun terminal-echo (text)
  (set! *test-terminal-output* (cons text *test-terminal-output*))
  nil)

;; Mock run-at-time (executes callback immediately for testing)
(defun run-at-time (delay repeat callback &rest args)
  (apply callback args)
  nil)

;; string-trim is now a builtin in telnet-lisp

;; Load the scan-tracker from contrib
(load "lisp/contrib/scan-tracker.lisp")

;; ============================================================================
;; HELPER FUNCTION TESTS
;; ============================================================================

(princ "Testing helper functions...\n")

;; Test direction abbreviation mapping
(assert-equal (scan-tracker-get-direction-abbrev "north") "n"
  "north -> n")
(assert-equal (scan-tracker-get-direction-abbrev "south") "s"
  "south -> s")
(assert-equal (scan-tracker-get-direction-abbrev "east") "e"
  "east -> e")
(assert-equal (scan-tracker-get-direction-abbrev "west") "w"
  "west -> w")
(assert-equal (scan-tracker-get-direction-abbrev "up") "u"
  "up -> u")
(assert-equal (scan-tracker-get-direction-abbrev "down") "d"
  "down -> d")
(assert-nil (scan-tracker-get-direction-abbrev "invalid")
  "invalid direction returns nil")

;; Test case-insensitive contains
(assert-true (scan-tracker-string-contains-ci "Djurden is here." "djurden")
  "case-insensitive match lowercase needle")
(assert-true (scan-tracker-string-contains-ci "djurden is here." "Djurden")
  "case-insensitive match uppercase needle")
(assert-true (scan-tracker-string-contains-ci "DJURDEN IS HERE." "djurden")
  "case-insensitive match all caps haystack")
(assert-false (scan-tracker-string-contains-ci "A zombie is here." "djurden")
  "no match when target not present")

;; ============================================================================
;; STATE RESET HELPER
;; ============================================================================

(defun reset-scan-state ()
  "Reset all scan state for clean test"
  (set! *scan-state* 'idle)
  (set! *scan-direction* nil)
  (set! *scan-range* 0)
  (set! *scan-best-path* nil)
  (set! *scan-best-range* 999)
  (hash-set! *tintin-variables* "target" nil)
  (hash-set! *tintin-variables* "target_path" nil))

;; ============================================================================
;; DIRECTION PARSING TESTS
;; ============================================================================

(princ "Testing direction parsing...\n")

(reset-scan-state)
(scan-tracker-process-line "You scan north.")
(assert-equal *scan-state* 'scanning "state becomes scanning after 'You scan'")
(assert-equal *scan-direction* "n" "direction set to 'n' for north")

(reset-scan-state)
(scan-tracker-process-line "You scan south.")
(assert-equal *scan-direction* "s" "direction set to 's' for south")

(reset-scan-state)
(scan-tracker-process-line "You scan east.")
(assert-equal *scan-direction* "e" "direction set to 'e' for east")

(reset-scan-state)
(scan-tracker-process-line "You scan west.")
(assert-equal *scan-direction* "w" "direction set to 'w' for west")

(reset-scan-state)
(scan-tracker-process-line "You scan up.")
(assert-equal *scan-direction* "u" "direction set to 'u' for up")

(reset-scan-state)
(scan-tracker-process-line "You scan down.")
(assert-equal *scan-direction* "d" "direction set to 'd' for down")

;; ============================================================================
;; RANGE PARSING TESTS
;; ============================================================================

(princ "Testing range parsing...\n")

(reset-scan-state)
(scan-tracker-process-line "You scan north.")
(scan-tracker-process-line "***** Range 1 *****")
(assert-equal *scan-range* 1 "range set to 1")

(scan-tracker-process-line "***** Range 2 *****")
(assert-equal *scan-range* 2 "range updated to 2")

(scan-tracker-process-line "***** Range 4 *****")
(assert-equal *scan-range* 4 "range updated to 4")

;; Range should not change when not scanning
(reset-scan-state)
(scan-tracker-process-line "***** Range 3 *****")
(assert-equal *scan-range* 0 "range not set when idle")

;; ============================================================================
;; TARGET MATCHING TESTS
;; ============================================================================

(princ "Testing target matching...\n")

(reset-scan-state)
(hash-set! *tintin-variables* "target" "Djurden")
(scan-tracker-process-line "You scan west.")
(scan-tracker-process-line "***** Range 2 *****")
(scan-tracker-process-line "Djurden is here.")

(assert-equal *scan-best-path* "2w" "target found at range 2 west")
(assert-equal *scan-best-range* 2 "best range is 2")

;; Test case-insensitive matching
(reset-scan-state)
(hash-set! *tintin-variables* "target" "djurden")
(scan-tracker-process-line "You scan north.")
(scan-tracker-process-line "***** Range 3 *****")
(scan-tracker-process-line "DJURDEN is resting here.")

(assert-equal *scan-best-path* "3n" "case-insensitive match works")

;; Test that closer target wins
(reset-scan-state)
(hash-set! *tintin-variables* "target" "Djurden")
(scan-tracker-process-line "You scan north.")
(scan-tracker-process-line "***** Range 4 *****")
(scan-tracker-process-line "Djurden is here.")
(assert-equal *scan-best-path* "4n" "first match at range 4")

(scan-tracker-process-line "You scan south.")
(scan-tracker-process-line "***** Range 2 *****")
(scan-tracker-process-line "Djurden is here.")
(assert-equal *scan-best-path* "2s" "closer match at range 2 replaces")

(scan-tracker-process-line "You scan east.")
(scan-tracker-process-line "***** Range 3 *****")
(scan-tracker-process-line "Djurden is here.")
(assert-equal *scan-best-path* "2s" "farther match at range 3 does not replace")

;; ============================================================================
;; FINALIZATION TESTS
;; ============================================================================

(princ "Testing finalization...\n")

;; Test prompt ends scan and sets $target_path
(reset-scan-state)
(hash-set! *tintin-variables* "target" "Djurden")
(scan-tracker-process-line "You scan west.")
(scan-tracker-process-line "***** Range 2 *****")
(scan-tracker-process-line "Djurden is here.")
(scan-tracker-process-line "civilized <100%hp 100%m 100%mv>")

(assert-equal *scan-state* 'idle "state reset to idle after prompt")
(assert-equal (hash-ref *tintin-variables* "target_path") "2w"
  "$target_path set to best path")

;; Test no match sets empty string
(reset-scan-state)
(hash-set! *tintin-variables* "target" "Djurden")
(scan-tracker-process-line "You scan north.")
(scan-tracker-process-line "***** Range 1 *****")
(scan-tracker-process-line "A zombie is here.")
(scan-tracker-process-line "You scan south.")
(scan-tracker-process-line "civilized <50%hp 50%m 50%mv>")

(assert-equal (hash-ref *tintin-variables* "target_path") ""
  "$target_path is empty when target not found")

;; ============================================================================
;; FULL SCAN SIMULATION
;; ============================================================================

(princ "Testing full scan simulation...\n")

(reset-scan-state)
(hash-set! *tintin-variables* "target" "Djurden")

;; Simulate a full "scan all" output
(scan-tracker-hook "You scan north.
***** Range 1 *****
A pale, withered man stands here, staring at you with immense hatred.

You scan south.
***** Range 2 *****
A horribly disgusting zombie is here, looking to devour the living.

***** Range 3 *****
Djurden is here.

You scan east.
You scan west.
***** Range 1 *****
A zombie is here.

***** Range 2 *****
Djurden is here.

You scan up.
You scan down.

civilized <83%hp 71%m 49%mv 3568tnl (69.50%) 3 AM>")

(assert-equal (hash-ref *tintin-variables* "target_path") "2w"
  "full scan finds closest path (2w closer than 3s)")

;; ============================================================================
;; EDGE CASES
;; ============================================================================

(princ "Testing edge cases...\n")

;; Empty target
(reset-scan-state)
(hash-set! *tintin-variables* "target" "")
(scan-tracker-process-line "You scan north.")
(scan-tracker-process-line "***** Range 1 *****")
(scan-tracker-process-line "Djurden is here.")
(scan-tracker-process-line "civilized <100%hp>")
(assert-equal (hash-ref *tintin-variables* "target_path") ""
  "empty target finds nothing")

;; No target variable set
(reset-scan-state)
(hash-set! *tintin-variables* "target" nil)
(scan-tracker-process-line "You scan north.")
(scan-tracker-process-line "***** Range 1 *****")
(scan-tracker-process-line "Djurden is here.")
(scan-tracker-process-line "civilized <100%hp>")
(assert-equal (hash-ref *tintin-variables* "target_path") ""
  "nil target finds nothing")

;; Target in mob description (partial match)
(reset-scan-state)
(hash-set! *tintin-variables* "target" "zombie")
(scan-tracker-process-line "You scan south.")
(scan-tracker-process-line "***** Range 3 *****")
(scan-tracker-process-line "A horribly disgusting zombie is here, looking to devour the living.")
(scan-tracker-process-line "civilized <100%hp>")
(assert-equal (hash-ref *tintin-variables* "target_path") "3s"
  "partial match in mob description works")

;; ============================================================================
;; DONE
;; ============================================================================

(princ "\nAll scan-tracker tests passed!\n")
