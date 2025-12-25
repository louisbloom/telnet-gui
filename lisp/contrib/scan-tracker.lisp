;; scan-tracker.lisp - Track targets in MUD scan output
;;
;; This script was created for Carrion Fields MUD (https://carrionfields.net/)
;; It parses "scan all" or "scan <direction>" output to find a target
;; and generate a speedwalk path stored in TinTin++ variable $target_path.
;;
;; Usage:
;;   (load "lisp/contrib/scan-tracker.lisp")
;;   or: #load lisp/contrib/scan-tracker.lisp
;;
;;   1. Set target: #variable target Djurden
;;   2. Run scan: scan all (or scan n, scan s, etc.)
;;   3. Result stored in $target_path (e.g., "2w" for Range 2 West)
;;
;; Scan output format (Carrion Fields):
;;   You scan north.
;;   ***** Range 1 *****
;;   A zombie is here.
;;   ***** Range 2 *****
;;   Djurden is here.
;;   You scan south.
;;   ...
;;   <prompt ends scan>

;; ============================================================================
;; STATE VARIABLES
;; ============================================================================

(defvar *scan-state* 'idle
  "Current scan state: 'idle or 'scanning")

(defvar *scan-direction* nil
  "Current scan direction: \"n\", \"s\", \"e\", \"w\", \"u\", \"d\"")

(defvar *scan-range* 0
  "Current range number from scan output")

(defvar *scan-best-path* nil
  "Best (shortest) path found to target so far")

(defvar *scan-best-range* 999
  "Best (lowest) range found to target so far")

;; ============================================================================
;; DIRECTION MAPPING
;; ============================================================================

(defvar *scan-direction-map*
  '(("north" . "n") ("south" . "s") ("east" . "e")
     ("west" . "w") ("up" . "u") ("down" . "d"))
  "Map full direction names to speedwalk abbreviations")

;; ============================================================================
;; HELPER FUNCTIONS
;; ============================================================================

(defun scan-tracker-get-direction-abbrev (full-name)
  "Convert full direction name to abbreviation (e.g., \"north\" -> \"n\")"
  (let ((entry (assoc full-name *scan-direction-map*)))
    (if entry (cdr entry) nil)))

(defun scan-tracker-string-contains-ci (haystack needle)
  "Case-insensitive string contains check"
  (string-contains? (string-downcase haystack) (string-downcase needle)))

;; ============================================================================
;; LINE PROCESSING
;; ============================================================================

(defun scan-tracker-process-line (line)
  "Process a single line of scan output"
  (cond
    ;; "You scan <direction>." - start/continue scanning
    ((regex-match? "^You scan (north|south|east|west|up|down)\\." line)
      (let ((captures (regex-extract "^You scan (\\w+)\\." line)))
	(when captures
          (set! *scan-state* 'scanning)
          (set! *scan-direction* (scan-tracker-get-direction-abbrev (car captures)))
          (set! *scan-range* 0))))

    ;; "***** Range N *****" - update range
    ((regex-match? "^\\*+ Range (\\d+) \\*+$" line)
      (when (eq? *scan-state* 'scanning)
	(let ((captures (regex-extract "Range (\\d+)" line)))
          (when captures
            (set! *scan-range* (string->number (car captures)))))))

    ;; Prompt line - end scan, finalize result
    ((regex-match? "<\\d+%hp" line)
      (when (eq? *scan-state* 'scanning)
	(scan-tracker-finalize)))

    ;; Any other line while scanning - check for target
    ((eq? *scan-state* 'scanning)
      (scan-tracker-check-target line))))

;; ============================================================================
;; TARGET CHECKING
;; ============================================================================

(defun scan-tracker-check-target (line)
  "Check if line contains the target, record path if found"
  (let ((target (hash-ref *tintin-variables* "target")))
    (when (and target
            (string? target)
            (> (length target) 0)
            *scan-direction*
            (> *scan-range* 0)
            (scan-tracker-string-contains-ci line target))
      ;; Found target - record if this is the closest so far
      (when (< *scan-range* *scan-best-range*)
        (set! *scan-best-range* *scan-range*)
        (set! *scan-best-path*
          (concat (number->string *scan-range*) *scan-direction*))))))

;; ============================================================================
;; FINALIZATION
;; ============================================================================

(defun scan-tracker-finalize ()
  "Finalize scan: set $target_path, echo result, and reset state"
  (let ((target (hash-ref *tintin-variables* "target"))
         (path *scan-best-path*))
    ;; Set $target_path to best path found (or empty string if not found)
    (hash-set! *tintin-variables* "target_path" (if path path ""))
    ;; Schedule pretty print result after telnet input is displayed
    (when (and target (string? target) (> (length target) 0))
      (run-at-time 0 nil
        (lambda ()
          (if path
            ;; Found - green text
            (terminal-echo (concat "\033[32m[Scan] \033[1m" target
                             "\033[0m\033[32m found: \033[1;33m"
                             path "\033[0m\n"))
            ;; Not found - red text
            (terminal-echo (concat "\033[31m[Scan] \033[1m" target
                             "\033[0m\033[31m not found\033[0m\n")))))))
  ;; Reset state for next scan
  (set! *scan-state* 'idle)
  (set! *scan-direction* nil)
  (set! *scan-range* 0)
  (set! *scan-best-path* nil)
  (set! *scan-best-range* 999))

;; ============================================================================
;; MAIN HOOK
;; ============================================================================

(defun scan-tracker-hook (text)
  "Process telnet input looking for scan output to track target.
Called by telnet-input-hook for each chunk of server output."
  ;; Split text into lines and process each
  (let ((lines (regex-split "[\r\n]+" text)))
    (do ((remaining lines (cdr remaining)))
      ((null? remaining))
      (let ((trimmed (string-trim (car remaining))))
        (when (> (length trimmed) 0)
          (scan-tracker-process-line trimmed))))))

;; Register the hook
(add-hook 'telnet-input-hook 'scan-tracker-hook)
