;; TinTin++ Implementation - Test-Driven Development
;; Building incrementally, one test at a time

;; ============================================================================
;; USAGE AS USER-INPUT-HOOK
;; ============================================================================
;;
;; To use TinTin++ in telnet-gui, simply load this file:
;;
;;    ./build/telnet-gui/telnet-gui.exe -l telnet-gui/tintin.lisp <host> <port>
;;
;; TinTin++ automatically activates when loaded! You can use:
;;    - Command separation: n;s;e;w
;;    - Speedwalk: 3n2e (expands to n;n;n;e;e)
;;    - Aliases: #alias {k} {kill %1}
;;    - Variables: #variable {target} {orc}
;;    - Pattern matching: #alias {attack %1 with %2} {kill %1;wield %2}
;;    - Partial matching: #al {k} {kill %1} or #var {target} {orc}
;;    - Case-insensitive: #ALIAS, #Alias, #alias all work
;;
;; All # commands support:
;;    - Partial prefix matching (e.g., #al → #alias, #v → #variable)
;;    - Case-insensitive matching (e.g., #ALIAS, #Alias, #alias)
;;    - Error messages for unknown commands (never sent to telnet server)
;;
;; Toggle TinTin++ on/off:
;;    (tintin-toggle!)   ; Toggle on/off
;;    (tintin-enable!)   ; Enable
;;    (tintin-disable!)  ; Disable
;;
;; Example configuration file (save as my-config.lisp):
;;
;;   ;; Load TinTin++ (automatically activates)
;;   (load "tintin.lisp")
;;
;;   ;; Pre-define common directional aliases
;;   (tintin-process-command "#alias {n} {north}")
;;   (tintin-process-command "#alias {s} {south}")
;;   (tintin-process-command "#alias {e} {east}")
;;   (tintin-process-command "#alias {w} {west}")
;;   (tintin-process-command "#alias {u} {up}")
;;   (tintin-process-command "#alias {d} {down}")
;;
;;   ;; Set up combat aliases
;;   (tintin-process-command "#alias {k} {kill %1}")
;;   (tintin-process-command "#alias {ka} {kill all.%1}")
;;
;;   ;; Set up variables
;;   (tintin-process-command "#variable {target} {orc}")
;;   (tintin-process-command "#variable {weapon} {sword}")
;;
;;   ;; Print confirmation
;;   (terminal-echo "TinTin++ configuration loaded!\r\n")
;;
;; Then run: ./telnet-gui.exe -l my-config.lisp <host> <port>
;;
;; ============================================================================
;; DATA STRUCTURES
;; ============================================================================

(define *tintin-aliases* (make-hash-table))
(define *tintin-variables* (make-hash-table))
(define *tintin-highlights* (make-hash-table))
(define *tintin-actions* (make-hash-table))
(define *tintin-action-executing* #f)
(define *tintin-speedwalk-enabled* #t)
(define *tintin-speedwalk-diagonals* #f)
(define *tintin-enabled* #t)
(define *tintin-max-alias-depth* 10)

;; TinTin++ command registry with metadata
;; Each entry: (handler-fn arg-count syntax-help)
;; Registry is populated after handlers are defined (see COMMAND HANDLERS section)
(define *tintin-commands* (make-hash-table))

;; ============================================================================
;; STUB DEFINITIONS (for standalone use in lisp-repl)
;; ============================================================================

;; Define terminal-echo as printing to stdout if not already defined
(if (not (condition-case err
           (progn terminal-echo #t)
           (error #f)))
  (define terminal-echo (lambda (text) (print text) nil)))

;; Define telnet-send as printing to stderr if not already defined
(if (not (condition-case err
           (progn telnet-send #t)
           (error #f)))
  (define telnet-send (lambda (text) (eprint text) nil)))

;; ============================================================================
;; UTILITY FUNCTIONS
;; NOTE: string->number and reverse are now native built-in functions
;; ============================================================================


;; ============================================================================
;; HIGHLIGHT COLOR PARSING SYSTEM
;; ============================================================================

;; TinTin++ Color Name Mappings
;; Standard ANSI colors (30-37 for FG, 40-47 for BG)
(define *tintin-colors-fg*
  '(("black" . "30") ("red" . "31") ("green" . "32") ("yellow" . "33")
     ("blue" . "34") ("magenta" . "35") ("cyan" . "36") ("white" . "37")))

(define *tintin-colors-bg*
  '(("black" . "40") ("red" . "41") ("green" . "42") ("yellow" . "43")
     ("blue" . "44") ("magenta" . "45") ("cyan" . "46") ("white" . "47")))

;; Bright/light colors (90-97 for FG, 100-107 for BG)
(define *tintin-colors-bright-fg*
  '(("light black" . "90") ("light red" . "91") ("light green" . "92")
     ("light yellow" . "93") ("light blue" . "94") ("light magenta" . "95")
     ("light cyan" . "96") ("light white" . "97")))

(define *tintin-colors-bright-bg*
  '(("light black" . "100") ("light red" . "101") ("light green" . "102")
     ("light yellow" . "103") ("light blue" . "104") ("light magenta" . "105")
     ("light cyan" . "106") ("light white" . "107")))

;; Tertiary colors (from TinTin++ docs) - map to RGB equivalents
(define *tintin-tertiary-colors*
  '(("azure" . "acf") ("ebony" . "000") ("jade" . "afc") ("lime" . "cfa")
     ("orange" . "fc8") ("pink" . "fca") ("silver" . "ccc") ("tan" . "ca8")
     ("violet" . "fac") ("white" . "fff")))

;; Text attributes
(define *tintin-attributes*
  '(("reset" . "0") ("bold" . "1") ("dim" . "2") ("italic" . "3")
     ("underscore" . "4") ("underline" . "4") ("blink" . "5")
     ("reverse" . "7") ("strikethrough" . "9")))

;; Helper: Convert hex character to decimal (0-15)
(defun tintin-hex-to-dec (hex-char)
  (let ((ch (string-downcase hex-char)))
    (cond
      ((string=? ch "0") 0) ((string=? ch "1") 1) ((string=? ch "2") 2)
      ((string=? ch "3") 3) ((string=? ch "4") 4) ((string=? ch "5") 5)
      ((string=? ch "6") 6) ((string=? ch "7") 7) ((string=? ch "8") 8)
      ((string=? ch "9") 9) ((string=? ch "a") 10) ((string=? ch "b") 11)
      ((string=? ch "c") 12) ((string=? ch "d") 13) ((string=? ch "e") 14)
      ((string=? ch "f") 15)
      (#t 0))))

;; Helper: Expand 3-char RGB to full RGB values
;; Example: "abc" → (170 187 204)
(defun tintin-expand-rgb (rgb-str)
  (let ((len (string-length rgb-str)))
    (if (= len 3)
      ;; 3-char: each char represents 0-255 in 16 steps (multiply by 17)
      (list (* (tintin-hex-to-dec (substring rgb-str 0 1)) 17)
        (* (tintin-hex-to-dec (substring rgb-str 1 2)) 17)
        (* (tintin-hex-to-dec (substring rgb-str 2 3)) 17))
      ;; 6-char: parse as two-digit hex pairs
      (if (= len 6)
        (list (+ (* (tintin-hex-to-dec (substring rgb-str 0 1)) 16)
                (tintin-hex-to-dec (substring rgb-str 1 2)))
          (+ (* (tintin-hex-to-dec (substring rgb-str 2 3)) 16)
            (tintin-hex-to-dec (substring rgb-str 3 4)))
          (+ (* (tintin-hex-to-dec (substring rgb-str 4 5)) 16)
            (tintin-hex-to-dec (substring rgb-str 5 6))))
        ;; Invalid length - return black
        (list 0 0 0)))))

;; Helper: Convert RGB values to ANSI 24-bit color code
;; is-bg: #t for background (48;2), #f for foreground (38;2)
(defun tintin-rgb-to-ansi (r g b is-bg)
  (concat (if is-bg "48;2;" "38;2;")
    (number->string r) ";"
    (number->string g) ";"
    (number->string b)))

;; Parse RGB color code <rgb>, <Frgb>, or <Frrggbb>
;; Returns ANSI code string or nil
(defun tintin-parse-rgb-color (rgb-string is-bg)
  (if (and (> (string-length rgb-string) 2)
        (string=? (substring rgb-string 0 1) "<")
        (string=? (substring rgb-string (- (string-length rgb-string) 1)
                    (string-length rgb-string)) ">"))
    ;; Extract content between < and >
    (let ((content (substring rgb-string 1 (- (string-length rgb-string) 1)))
           (len (- (string-length rgb-string) 2)))
      (cond
        ;; <rgb> format (3 chars)
        ((= len 3)
          (let ((rgb (tintin-expand-rgb content)))
            (tintin-rgb-to-ansi (list-ref rgb 0) (list-ref rgb 1) (list-ref rgb 2) is-bg)))
        ;; <Frgb> format (4 chars) - ignore F, use last 3
        ((= len 4)
          (let ((rgb (tintin-expand-rgb (substring content 1 4))))
            (tintin-rgb-to-ansi (list-ref rgb 0) (list-ref rgb 1) (list-ref rgb 2) is-bg)))
        ;; <Frrggbb> format (7 chars) - ignore F, use last 6
        ((= len 7)
          (let ((rgb (tintin-expand-rgb (substring content 1 7))))
            (tintin-rgb-to-ansi (list-ref rgb 0) (list-ref rgb 1) (list-ref rgb 2) is-bg)))
        (#t nil)))
    nil))

;; Look up named color in association list
(defun tintin-lookup-color (name alist)
  (if (or (null? alist) (not (list? alist)))
    nil
    (let ((pair (assoc name alist)))
      (if pair (cdr pair) nil))))

;; Strip attribute keywords from text (bold, dim, italic, etc.)
;; Returns text with attribute keywords removed
(defun tintin-strip-attributes (text)
  (let* ((text-lower (string-downcase text))
          (result text-lower))
    ;; Remove each attribute keyword
    (do ((i 0 (+ i 1)))
      ((>= i (list-length *tintin-attributes*)) result)
      (let ((keyword (car (list-ref *tintin-attributes* i))))
        (set! result (string-replace keyword "" result))))
    ;; Trim whitespace
    (tintin-trim result)))

;; Parse named color (e.g., "red", "light blue")
;; Returns ANSI code string or nil
(defun tintin-parse-named-color (name is-bg)
  (let ((name-lower (string-downcase (tintin-trim name))))
    ;; Try tertiary colors first (convert to RGB)
    (let ((tertiary (tintin-lookup-color name-lower *tintin-tertiary-colors*)))
      (if tertiary
        (tintin-parse-rgb-color (concat "<" tertiary ">") is-bg)
        ;; Try light/bright colors
        (let ((bright (tintin-lookup-color name-lower
                        (if is-bg *tintin-colors-bright-bg* *tintin-colors-bright-fg*))))
          (if bright
            bright
            ;; Try standard colors
            (tintin-lookup-color name-lower
              (if is-bg *tintin-colors-bg* *tintin-colors-fg*))))))))

;; Parse attributes from text (bold, underscore, etc.)
;; Returns list of ANSI attribute codes
(defun tintin-parse-attributes (text)
  (let ((text-lower (string-downcase text))
         (attrs '()))
    ;; Check each attribute keyword
    (do ((i 0 (+ i 1)))
      ((>= i (list-length *tintin-attributes*)) attrs)
      (let* ((pair (list-ref *tintin-attributes* i))
              (keyword (car pair))
              (code (cdr pair)))
        (if (string-contains? text-lower keyword)
          (set! attrs (cons code attrs)))))))

;; Helper: Find first occurrence of character in string
;; Returns position or nil if not found
(defun tintin-string-find-char (str ch)
  (let ((len (string-length str))
         (pos 0)
         (found nil))
    (do ()
      ((or (>= pos len) found) found)
      (if (string=? (string-ref str pos) ch)
        (set! found pos)
        (set! pos (+ pos 1))))))

;; Split color spec on colon (FG:BG separator)
;; Returns (fg-part bg-part) or (fg-part nil)
(defun tintin-split-fg-bg (spec)
  (let ((colon-pos (tintin-string-find-char spec ":")))
    (if colon-pos
      (list (substring spec 0 colon-pos)
        (substring spec (+ colon-pos 1) (string-length spec)))
      (list spec nil))))

;; Parse single color component (foreground or background)
;; Returns ANSI code string (may include attributes)
(defun tintin-parse-color-component (text is-bg)
  (if (or (not text) (string=? text ""))
    nil
    (let ((text-trimmed (tintin-trim text))
           (codes '()))
      ;; Extract attributes first
      (let ((attr-codes (tintin-parse-attributes text-trimmed)))
        (set! codes attr-codes))

      ;; Try RGB color format
      (let ((start-bracket (string-index text-trimmed "<")))
        (if start-bracket
          (let ((end-bracket (string-index text-trimmed ">")))
            (if end-bracket
              (let ((rgb-str (substring text-trimmed start-bracket (+ end-bracket 1))))
                (let ((rgb-code (tintin-parse-rgb-color rgb-str is-bg)))
                  (if rgb-code
                    (set! codes (cons rgb-code codes)))))))))

      ;; If no RGB found, try named colors
      (if (and (not (string-contains? text-trimmed "<"))
            (or (string-contains? text-trimmed "black")
              (string-contains? text-trimmed "red")
              (string-contains? text-trimmed "green")
              (string-contains? text-trimmed "yellow")
              (string-contains? text-trimmed "blue")
              (string-contains? text-trimmed "magenta")
              (string-contains? text-trimmed "cyan")
              (string-contains? text-trimmed "white")
              (string-contains? text-trimmed "azure")
              (string-contains? text-trimmed "jade")
              (string-contains? text-trimmed "violet")
              (string-contains? text-trimmed "lime")
              (string-contains? text-trimmed "pink")
              (string-contains? text-trimmed "orange")))
        (let ((color-only (tintin-strip-attributes text-trimmed)))
          (let ((named-code (tintin-parse-named-color color-only is-bg)))
            (if named-code
              (set! codes (cons named-code codes))))))

      ;; Combine codes with semicolons
      (if (eq? codes '())
        nil
        (let ((result "")
               (first #t))
          (do ((remaining (reverse codes) (cdr remaining)))
            ((null? remaining) result)
            (if first
              (set! first #f)
              (set! result (concat result ";")))
            (set! result (concat result (car remaining)))))))))

;; Build ANSI escape sequence from fg and bg codes
;; Returns complete \033[...m sequence
(defun tintin-build-ansi-code (fg-codes bg-codes)
  (let ((codes '()))
    (if fg-codes
      (set! codes (cons fg-codes codes)))
    (if bg-codes
      (set! codes (cons bg-codes codes)))
    (if (eq? codes '())
      ""
      (let ((combined "")
             (first #t))
        (do ((remaining (reverse codes) (cdr remaining)))
          ((null? remaining))
          (if first
            (set! first #f)
            (set! combined (concat combined ";")))
          (set! combined (concat combined (car remaining))))
        (concat "\033[" combined "m")))))

;; Main color spec parser
;; Parses TinTin++ color specification and returns ANSI escape code
;; Examples:
;;   "red" → "\033[31m"
;;   "<fff>" → "\033[38;2;255;255;255m"
;;   "bold red:blue" → "\033[1;31;44m"
;;   "light red" → "\033[91m"
(defun tintin-parse-color-spec (spec)
  (if (or (not spec) (string=? spec ""))
    (list nil nil)
    (let ((parts (tintin-split-fg-bg spec)))
      (let ((fg-part (list-ref parts 0))
             (bg-part (list-ref parts 1)))
        (list (tintin-parse-color-component fg-part #f)
          (if bg-part (tintin-parse-color-component bg-part #t) nil))))))

;; ============================================================================
;; HIGHLIGHT PATTERN MATCHING SYSTEM
;; ============================================================================

;; Check if character needs regex escaping
(defun tintin-regex-special-char? (ch)
  (or (string=? ch ".")
    (string=? ch "*")
    (string=? ch "+")
    (string=? ch "?")
    (string=? ch "[")
    (string=? ch "]")
    (string=? ch "{")
    (string=? ch "}")
    (string=? ch "(")
    (string=? ch ")")
    (string=? ch "|")
    (string=? ch "\\")
    (string=? ch "^")
    (string=? ch "$")))

;; Convert TinTin++ pattern to PCRE2 regex
;; Pattern translation:
;;   %* or %1-%99 → (.*?) (non-greedy capture)
;;   ^ at start → ^ (line anchor)
;;   Other chars → escaped for regex
;; Examples:
;;   "You hit %*" → "You hit (.*?)"
;;   "^Health: %1" → "^Health: (.*?)"
;;   "Valgar" → "Valgar"
(defun tintin-pattern-to-regex (pattern)
  (if (not (string? pattern))
    ""
    (let ((len (string-length pattern))
           (pos 0)
           (result ""))
      (do ()
        ((>= pos len) result)
        (let ((ch (string-ref pattern pos)))
          (cond
            ;; Handle % placeholders
            ((string=? ch "%")
              (if (< (+ pos 1) len)
                (let ((next-ch (string-ref pattern (+ pos 1))))
                  (if (string=? next-ch "*")
                    ;; %* → (.*?)
                    (progn
                      (set! result (concat result "(.*?)"))
                      (set! pos (+ pos 2)))
                    ;; Check if it's %1-%99
                    (if (and (string>=? next-ch "0") (string<=? next-ch "9"))
                      (let ((digit-end (+ pos 2)))
                        ;; Consume second digit if present
                        (if (and (< digit-end len)
                              (string>=? (string-ref pattern digit-end) "0")
                              (string<=? (string-ref pattern digit-end) "9"))
                          (set! digit-end (+ digit-end 1)))
                        ;; %N or %NN → (.*?)
                        (set! result (concat result "(.*?)"))
                        (set! pos digit-end))
                      ;; Not %* or %N - literal %
                      (progn
                        (set! result (concat result "\\%"))
                        (set! pos (+ pos 1))))))
                ;; % at end of string - literal
                (progn
                  (set! result (concat result "\\%"))
                  (set! pos (+ pos 1)))))

            ;; Handle ^ at start (line anchor)
            ((and (string=? ch "^") (= pos 0))
              (set! result (concat result "^"))
              (set! pos (+ pos 1)))

            ;; Escape regex special characters
            ((tintin-regex-special-char? ch)
              (set! result (concat result "\\" ch))
              (set! pos (+ pos 1)))

            ;; Regular character - no escaping needed
            (#t
              (set! result (concat result ch))
              (set! pos (+ pos 1)))))))))

;; Test if TinTin++ pattern matches text using regex
;; Returns #t if match found, #f otherwise
(defun tintin-match-highlight-pattern (pattern text)
  (if (or (not (string? pattern)) (not (string? text)))
    #f
    (let ((regex-pattern (tintin-pattern-to-regex pattern)))
      (if (string=? regex-pattern "")
        #f
        ;; Use regex-match to test if pattern matches
        (let ((match-result (regex-match regex-pattern text)))
          (if match-result #t #f))))))

;; Sort highlight entries by priority (descending - higher priority first)
;; Input: list of (pattern . (fg bg priority)) pairs
;; Output: sorted list by priority (highest first)
(defun tintin-sort-highlights-by-priority (highlight-list)
  (if (or (null? highlight-list) (= (list-length highlight-list) 0))
    '()
    ;; Simple insertion sort by priority
    (let ((sorted '()))
      (do ((remaining highlight-list (cdr remaining)))
        ((null? remaining) sorted)
        (let* ((entry (car remaining))
                (priority (car (cdr (cdr (cdr entry))))))
          ;; Insert entry in sorted position
          (set! sorted (tintin-insert-by-priority entry priority sorted)))))))

;; Helper: Insert entry into sorted list by priority, then by pattern length
(defun tintin-insert-by-priority (entry priority sorted-list)
  (if (null? sorted-list)
    (list entry)
    (let ((first-entry (car sorted-list))
           (first-priority (car (cdr (cdr (cdr (car sorted-list)))))))
      (if (> priority first-priority)
        ;; Higher priority - insert at head
        (cons entry sorted-list)
        (if (= priority first-priority)
          ;; Same priority - use pattern length as tiebreaker (longer first)
          (let ((entry-pattern (car entry))
                 (first-pattern (car first-entry)))
            (if (>= (string-length entry-pattern) (string-length first-pattern))
              (cons entry sorted-list)
              (cons first-entry
                (tintin-insert-by-priority entry priority (cdr sorted-list)))))
          ;; Lower priority - insert later
          (cons first-entry
            (tintin-insert-by-priority entry priority (cdr sorted-list))))))))

;; ============================================================================
;; ACTION PRIORITY SORTING
;; ============================================================================

;; Sort action entries by priority (ascending - lower priority first)
;; Input: list of (pattern . (commands-string priority)) pairs
;; Output: sorted list by priority (lowest first)
(defun tintin-sort-actions-by-priority (action-list)
  (if (or (null? action-list) (= (list-length action-list) 0))
    '()
    ;; Simple insertion sort by priority
    (let ((sorted '()))
      (do ((remaining action-list (cdr remaining)))
        ((null? remaining) sorted)
        (let* ((entry (car remaining))
                (priority (car (cdr (cdr entry)))))
          ;; Insert entry in sorted position
          (set! sorted (tintin-insert-action-by-priority entry priority sorted)))))))

;; Helper: Insert action entry into sorted list by priority, then by pattern length
(defun tintin-insert-action-by-priority (entry priority sorted-list)
  (if (null? sorted-list)
    (list entry)
    (let ((first-entry (car sorted-list))
           (first-priority (car (cdr (cdr (car sorted-list))))))
      (if (< priority first-priority)
        ;; Lower priority - insert at head (actions use ascending order)
        (cons entry sorted-list)
        (if (= priority first-priority)
          ;; Same priority - use pattern length as tiebreaker (longer first)
          (let ((entry-pattern (car entry))
                 (first-pattern (car first-entry)))
            (if (>= (string-length entry-pattern) (string-length first-pattern))
              (cons entry sorted-list)
              (cons first-entry
                (tintin-insert-action-by-priority entry priority (cdr sorted-list)))))
          ;; Higher priority - insert later
          (cons first-entry
            (tintin-insert-action-by-priority entry priority (cdr sorted-list))))))))

;; ============================================================================
;; HIGHLIGHT APPLICATION
;; ============================================================================

;; Split text into lines, preserving line endings
;; Returns list of lines with their line endings intact
(defun tintin-split-lines (text)
  (if (not (string? text))
    '()
    (let ((len (string-length text))
           (pos 0)
           (line-start 0)
           (lines '()))
      (do ()
        ((>= pos len)
          ;; Add final line if any
          (if (< line-start len)
            (reverse (cons (substring text line-start len) lines))
            (reverse lines)))
        (let ((ch (string-ref text pos)))
          (if (string=? ch "\n")
            ;; Found line ending - add line including \n
            (progn
              (set! lines (cons (substring text line-start (+ pos 1)) lines))
              (set! pos (+ pos 1))
              (set! line-start pos))
            ;; Regular character - continue
            (set! pos (+ pos 1))))))))

;; ============================================================================
;; ANSI STATE TRACKING (for nested/overlapping highlights)
;; ============================================================================

;; Extract the most recent (closest) ANSI escape sequence before a position
;; This represents the "active formatting state" at that position
;; Returns the ANSI sequence string or "" if none found or if reset encountered
(defun tintin-find-active-ansi-before (text pos)
  (if (or (not (string? text)) (<= pos 0))
    ""
    (let ((scan-pos (- pos 1))
           (found-ansi ""))
      ;; Scan backwards looking for the FIRST (most recent) ANSI sequence
      (do ()
        ((or (< scan-pos 0) (not (string=? found-ansi ""))) found-ansi)
        (if (and (>= scan-pos 0)
              (string=? (substring text scan-pos (+ scan-pos 1)) "\033")
              (< (+ scan-pos 1) (string-length text))
              (string=? (substring text (+ scan-pos 1) (+ scan-pos 2)) "["))
          ;; Found ESC[ - extract the complete sequence
          (let ((seq-end (+ scan-pos 2)))
            ;; Find the 'm' terminator
            (do ()
              ((or (>= seq-end (string-length text))
                 (string=? (substring text seq-end (+ seq-end 1)) "m")))
              (set! seq-end (+ seq-end 1)))
            ;; Check if we found a complete sequence
            (if (and (< seq-end (string-length text))
                  (string=? (substring text seq-end (+ seq-end 1)) "m"))
              (let ((sequence (substring text scan-pos (+ seq-end 1))))
                ;; Check if this is a reset code (ESC[0m or ESC[m)
                (if (or (string=? sequence "\033[0m")
                      (string=? sequence "\033[m"))
                  ;; Reset code - return empty (no active formatting)
                  (set! found-ansi "reset")  ; Special marker to exit and return ""
                  ;; Non-reset code - this is the active state
                  (set! found-ansi sequence))
                ;; Don't continue scanning - we found what we need
                (set! scan-pos -1))
              (set! scan-pos (- scan-pos 1))))
          ;; Not an ANSI sequence, continue backwards
          (set! scan-pos (- scan-pos 1))))
      ;; Return empty string if we found a reset, otherwise return the sequence
      (if (string=? found-ansi "reset") "" found-ansi))))

;; Find the position where matched text starts in the line
;; Returns position or -1 if not found
(defun tintin-find-match-position (line matched-text)
  (if (or (not (string? line)) (not (string? matched-text)))
    -1
    (let ((pos (string-index line matched-text)))
      (if pos pos -1))))

;; Check what comes immediately after a position:
;; Returns: 'reset if reset code found, 'ansi if non-reset ANSI found, 'text if regular text
(defun tintin-check-after-match (text pos)
  (if (or (not (string? text)) (>= pos (string-length text)))
    'text
    (let ((len (string-length text))
           (scan-pos pos))
      ;; Check if there's an ANSI code immediately after
      (if (and (< (+ scan-pos 1) len)
            (string=? (substring text scan-pos (+ scan-pos 1)) "\033")
            (< (+ scan-pos 1) len)
            (string=? (substring text (+ scan-pos 1) (+ scan-pos 2)) "["))
        ;; Found ESC[ - check what kind
        (let ((seq-end (+ scan-pos 2)))
          ;; Find the 'm' terminator
          (do ()
            ((or (>= seq-end len)
               (string=? (substring text seq-end (+ seq-end 1)) "m")))
            (set! seq-end (+ seq-end 1)))
          ;; Check if complete sequence
          (if (and (< seq-end len)
                (string=? (substring text seq-end (+ seq-end 1)) "m"))
            (let ((sequence (substring text scan-pos (+ seq-end 1))))
              (if (or (string=? sequence "\033[0m")
                    (string=? sequence "\033[m"))
                'reset  ; Reset code follows
                'ansi)) ; Non-reset ANSI code follows
            'text)) ; Incomplete sequence, treat as text
        ;; No ANSI code immediately after
        'text))))

;; Wrap matched pattern in line with ANSI color codes
;; Returns line with highlight applied or original line if no match
;; Now with ANSI state tracking: restores previous state unless reset follows
(defun tintin-wrap-match (line pattern fg-color bg-color)
  (if (not (string? line))
    line
    (let ((regex-pattern (tintin-pattern-to-regex pattern)))
      (if (string=? regex-pattern "")
        line
        ;; Parse color spec to get ANSI codes
        (let ((fg-ansi (if fg-color
                         (tintin-parse-color-component fg-color #f)
                         nil))
               (bg-ansi (if bg-color
                          (tintin-parse-color-component bg-color #t)
                          nil)))
          ;; Build opening ANSI sequence
          (let ((ansi-open (tintin-build-ansi-code fg-ansi bg-ansi)))
            (if (string=? ansi-open "")
              line
              ;; Find the matched text
              (let ((matched-text (regex-find regex-pattern line)))
                (if matched-text
                  ;; Find where the match occurs in the line
                  (let ((match-pos (tintin-find-match-position line matched-text)))
                    (if (< match-pos 0)
                      line
                      (let ((match-end-pos (+ match-pos (string-length matched-text))))
                        ;; Check what comes immediately after the match
                        (let ((after-type (tintin-check-after-match line match-end-pos)))
                          (let ((prev-state (tintin-find-active-ansi-before line match-pos)))
                            (let ((ansi-close
                                    (cond
                                      ;; Reset follows: use reset to close cleanly
                                      ((eq? after-type 'reset) "\033[0m")
                                      ;; Another ANSI code follows: restore previous state if any, else reset
                                      ;; This preserves outer highlights when nested
                                      ((eq? after-type 'ansi)
                                        (if (string=? prev-state "")
                                          "\033[0m"
                                          ;; Reset first to clear all attributes, then restore prev state
                                          (concat "\033[0m" prev-state)))
                                      ;; Regular text follows: restore previous state or reset
                                      (#t (if (string=? prev-state "")
                                            "\033[0m"
                                            ;; Reset first to clear all attributes, then restore prev state
                                            (concat "\033[0m" prev-state))))))
                              (string-replace matched-text
                                (concat ansi-open matched-text ansi-close)
                                line)))))))
                  line)))))))))

;; Apply highlights to a single line
;; Returns highlighted line or original line if no highlights match
(defun tintin-highlight-line (line)
  (if (or (not (string? line)) (= (hash-count *tintin-highlights*) 0))
    line
    ;; Get all highlights sorted by priority (highest first)
    (let ((highlight-entries (hash-entries *tintin-highlights*)))
      (let ((sorted (tintin-sort-highlights-by-priority highlight-entries)))
        ;; Try all patterns and apply all that match
        (let ((result line))
          (do ((i 0 (+ i 1)))
            ((>= i (list-length sorted))
              result)
            (let* ((entry (list-ref sorted i))
                    (pattern (car entry))
                    (data (cdr entry))
                    (fg-color (car data))
                    (bg-color (car (cdr data))))
              ;; Check if pattern matches the current result
              (if (tintin-match-highlight-pattern pattern result)
                ;; Apply highlight to current result (allows multiple highlights)
                (set! result (tintin-wrap-match result pattern fg-color bg-color))))))))))

;; Main entry point: Apply highlights to incoming text
;; Splits text into lines, highlights each line, returns transformed text
(defun tintin-apply-highlights (text)
  (if (or (not (string? text)) (= (hash-count *tintin-highlights*) 0))
    text
    ;; Split into lines
    (let ((lines (tintin-split-lines text)))
      (if (null? lines)
        text
        ;; Highlight each line
        (let ((highlighted '()))
          (do ((i 0 (+ i 1)))
            ((>= i (list-length lines))
              ;; Join highlighted lines back together
              (let ((result ""))
                (do ((j 0 (+ j 1)))
                  ((>= j (list-length highlighted)) result)
                  (set! result (concat result (list-ref highlighted j))))))
            (let ((line (list-ref lines i)))
              (set! highlighted (cons (tintin-highlight-line line) highlighted))))
          ;; Need to reverse since we cons'd in reverse order
          (set! highlighted (reverse highlighted))
          ;; Join lines
          (let ((result ""))
            (do ((k 0 (+ k 1)))
              ((>= k (list-length highlighted)) result)
              (set! result (concat result (list-ref highlighted k))))))))))

;; ============================================================================
;; TEST 1: COMMAND SEPARATOR
;; ============================================================================

;; Helper: Trim leading and trailing whitespace from string
(defun tintin-trim (str)
  (if (not (string? str))
    ""
    (let ((len (string-length str)))
      (if (= len 0)
        ""
        ;; Find first non-whitespace character
        (let ((start (tintin-find-first-non-ws str 0 len)))
          (if (>= start len)
            ""  ; All whitespace
            ;; Find last non-whitespace character
            (let ((end (tintin-find-last-non-ws str (- len 1))))
              (substring str start (+ end 1)))))))))

;; Helper: Find first non-whitespace character index
(defun tintin-find-first-non-ws (str pos len)
  (if (>= pos len)
    pos
    (let ((ch (string-ref str pos)))
      (if (or (string=? ch " ")
            (string=? ch "\t")
            (string=? ch "\r")
            (string=? ch "\n"))
        (tintin-find-first-non-ws str (+ pos 1) len)
        pos))))

;; Helper: Find last non-whitespace character index
(defun tintin-find-last-non-ws (str pos)
  (if (< pos 0)
    -1
    (let ((ch (string-ref str pos)))
      (if (or (string=? ch " ")
            (string=? ch "\t")
            (string=? ch "\r")
            (string=? ch "\n"))
        (tintin-find-last-non-ws str (- pos 1))
        pos))))

;; Recursive helper for splitting commands
(defun tintin-split-loop (str pos len depth current results)
  (if (>= pos len)
    ;; Done - add final command if any and return reversed list
    (if (not (string=? current ""))
      (reverse (cons current results))
      (reverse results))
    ;; Process current character
    (let ((ch (string-ref str pos)))
      (cond
        ((string=? ch "{")
          (tintin-split-loop str (+ pos 1) len (+ depth 1) (concat current ch) results))
        ((string=? ch "}")
          (tintin-split-loop str (+ pos 1) len (- depth 1) (concat current ch) results))
        ((and (string=? ch ";") (= depth 0))
          (tintin-split-loop str (+ pos 1) len depth "" (cons current results)))
        (#t
          (tintin-split-loop str (+ pos 1) len depth (concat current ch) results))))))

(defun tintin-split-commands (str)
  (if (not (string? str))
    '()
    ;; Split commands and trim whitespace from each
    (map tintin-trim (tintin-split-loop str 0 (string-length str) 0 "" '()))))

;; ============================================================================
;; TEST 2: SPEEDWALK
;; ============================================================================

;; Check if character is a digit
(defun tintin-is-digit? (ch)
  (and (string? ch)
    (= (string-length ch) 1)
    (or (string=? ch "0") (string=? ch "1") (string=? ch "2") (string=? ch "3")
      (string=? ch "4") (string=? ch "5") (string=? ch "6") (string=? ch "7")
      (string=? ch "8") (string=? ch "9"))))

;; Check if a string is a valid direction
(defun tintin-is-direction? (str)
  (or (string=? str "n") (string=? str "e") (string=? str "s") (string=? str "w")
    (string=? str "u") (string=? str "d")
    (and *tintin-speedwalk-diagonals*
      (or (string=? str "ne") (string=? str "nw")
        (string=? str "se") (string=? str "sw")))))

;; Expand speedwalk string like "3n2e" to "n;n;n;e;e"
(defun tintin-expand-speedwalk (input)
  (if (or (not (string? input)) (not *tintin-speedwalk-enabled*))
    input
    (let ((len (string-length input))
           (pos 0)
           (result '())
           (valid #t))  ; Track if entire input is valid speedwalk
      (do ()
        ((>= pos len))
        (let ((count-str "")
               (direction ""))
          ;; Collect digits for count
          (do ()
            ((or (>= pos len) (not (tintin-is-digit? (substring input pos (+ pos 1)))))
              nil)
            (set! count-str (concat count-str (substring input pos (+ pos 1))))
            (set! pos (+ pos 1)))

          ;; Get direction (1 or 2 characters)
          (if (< pos len)
            (let ((ch1 (substring input pos (+ pos 1))))
              ;; Try 2-char direction first (only if diagonals enabled)
              (if (and *tintin-speedwalk-diagonals*
                    (< (+ pos 1) len)
                    (tintin-is-direction? (concat ch1 (substring input (+ pos 1) (+ pos 2)))))
                (progn
                  (set! direction (concat ch1 (substring input (+ pos 1) (+ pos 2))))
                  (set! pos (+ pos 2)))
                ;; Try 1-char direction
                (if (tintin-is-direction? ch1)
                  (progn
                    (set! direction ch1)
                    (set! pos (+ pos 1)))
                  ;; Not a valid direction - mark as invalid
                  (progn
                    (set! valid #f)
                    (set! pos (+ pos 1)))))))

          ;; Expand direction N times (only if we found a valid direction)
          (if (not (string=? direction ""))
            (let ((count (if (string=? count-str "") 1 (string->number count-str))))
              (do ((i 0 (+ i 1)))
                ((>= i count))
                (set! result (cons direction result)))))))

      ;; Return original input if any part was invalid, otherwise return expanded
      (if (not valid)
        input
        ;; Join results with semicolons
        (let ((reversed (reverse result))
               (output ""))
          (do ((i 0 (+ i 1)))
            ((>= i (list-length reversed)) output)
            (set! output (concat output
                           (if (> i 0) ";" "")
                           (list-ref reversed i)))))))))

;; ============================================================================
;; TEST 3: ALIAS CREATION
;; ============================================================================

;; Extract braced argument (including braces)
;; Returns (braced-text . next-pos) or nil if no braced text found
;; Example: "{hello {world}}" → ("{hello {world}}" . position-after-closing-brace)
(defun tintin-extract-braced (str start-pos)
  (if (>= start-pos (string-length str))
    nil
    (let ((pos start-pos)
           (len (string-length str)))
      ;; Find opening brace
      (do ()
        ((or (>= pos len) (string=? (string-ref str pos) "{"))
          (if (>= pos len)
            nil
            ;; Extract including braces - track depth for nested braces
            (let ((depth 1)
                   (brace-start pos)  ; Start at opening brace
                   (end-pos (+ pos 1)))
              (do ()
                ((or (>= end-pos len) (= depth 0))
                  (if (= depth 0)
                    ;; Return text INCLUDING braces (from brace-start to end-pos)
                    (cons (substring str brace-start end-pos) end-pos)
                    nil))
                (let ((ch (string-ref str end-pos)))
                  (if (string=? ch "{")
                    (set! depth (+ depth 1))
                    (if (string=? ch "}")
                      (set! depth (- depth 1))))
                  (set! end-pos (+ end-pos 1)))))))
        (set! pos (+ pos 1))))))

;; Extract space-delimited token starting at pos
;; Returns (token . next-pos) or nil if no token found
;; Example: (tintin-extract-token "#load Det" 6) => ("Det" . 9)
(defun tintin-extract-token (str start-pos)
  (if (>= start-pos (string-length str))
    nil
    (let ((len (string-length str))
           (pos start-pos))
      ;; Skip leading whitespace
      (do ()
        ((or (>= pos len) (not (string=? (string-ref str pos) " "))))
        (set! pos (+ pos 1)))
      ;; Check if we have any characters left
      (if (>= pos len)
        nil
        ;; Find end of token (space or end of string)
        (let ((start pos)
               (end pos))
          (do ()
            ((or (>= end len) (string=? (string-ref str end) " ")))
            (set! end (+ end 1)))
          ;; Return token and position
          (if (= start end)
            nil
            (cons (substring str start end) end)))))))

;; Extract from start-pos to end of string (for last argument in unbraced format)
;; Returns: (string . end-pos) or nil
(defun tintin-extract-to-end (str start-pos)
  (if (>= start-pos (string-length str))
    nil
    (let ((len (string-length str))
           (pos start-pos))
      ;; Skip leading whitespace
      (do ()
        ((or (>= pos len) (not (string=? (string-ref str pos) " "))))
        (set! pos (+ pos 1)))
      ;; Check if we have any characters left
      (if (>= pos len)
        nil
        ;; Return from pos to end of string
        (cons (substring str pos len) len)))))

;; Parse N arguments from command string (mixed format: braced or unbraced)
;; Returns: list of N strings or nil if parsing fails
;; Each argument can be independently braced or unbraced
;; Braced arguments preserve braces: {text} → "{text}"
;; Example: (tintin-parse-arguments "#alias bag {kill %1}" 2) => ("bag" "{kill %1}")
;;          (tintin-parse-arguments "#load Det" 1) => ("Det")
(defun tintin-parse-arguments (input n)
  (let ((start-pos 1)       ; Start after #
         (args '())
         (success #t))
    ;; Step 1: Skip whitespace after #
    (do ()
      ((or (>= start-pos (string-length input))
         (not (string=? (string-ref input start-pos) " "))))
      (set! start-pos (+ start-pos 1)))

    ;; Step 2: Skip past command name (until space, {, or end)
    (do ()
      ((or (>= start-pos (string-length input))
         (string=? (string-ref input start-pos) " ")
         (string=? (string-ref input start-pos) "{")))
      (set! start-pos (+ start-pos 1)))

    ;; Step 3: Parse N arguments using mixed format
    ;; Each argument can be braced or unbraced independently
    (do ((i 0 (+ i 1)))
      ((or (>= i n) (not success))
        (if success (reverse args) nil))
      ;; Skip whitespace before this argument
      (do ()
        ((or (>= start-pos (string-length input))
           (not (string=? (string-ref input start-pos) " "))))
        (set! start-pos (+ start-pos 1)))

      ;; Check if we have more input
      (if (>= start-pos (string-length input))
        (set! success #f)  ; Ran out of input before getting N arguments
        ;; Check if this argument is braced or unbraced
        (let ((is-braced (string=? (string-ref input start-pos) "{")))
          (if is-braced
            ;; Extract braced argument (preserves braces)
            (let ((arg-data (tintin-extract-braced input start-pos)))
              (if arg-data
                (progn
                  (set! args (cons (car arg-data) args))
                  (set! start-pos (cdr arg-data)))
                (set! success #f)))
            ;; Extract unbraced token
            ;; For the last argument, read to end of string instead of stopping at space
            (let ((is-last-arg (= i (- n 1))))
              (let ((token-data (if is-last-arg
                                  (tintin-extract-to-end input start-pos)
                                  (tintin-extract-token input start-pos))))
                (if token-data
                  (progn
                    (set! args (cons (car token-data) args))
                    (set! start-pos (cdr token-data)))
                  (set! success #f))))))))))


;; Match a pattern against input and extract placeholder values
;; Returns list of extracted values or nil if no match
;; Example: (tintin-match-pattern "k %1 with %2" "k orc with sword") => ("orc" "sword")
(defun tintin-match-pattern (pattern input)
  (let ((pattern-parts (split pattern " "))
         (input-parts (split input " ")))
    (if (not (= (list-length pattern-parts) (list-length input-parts)))
      nil
      (let ((matches '())
             (success #t))
        (do ((i 0 (+ i 1)))
          ((or (>= i (list-length pattern-parts)) (not success))
            (if success (reverse matches) nil))
          (let ((p-part (list-ref pattern-parts i))
                 (i-part (list-ref input-parts i)))
            (if (string-prefix? "%" p-part)
              ;; Placeholder - capture the value
              (set! matches (cons i-part matches))
              ;; Literal - must match exactly
              (if (and (string? p-part) (string? i-part) (not (string=? p-part i-part)))
                (set! success #f)))))))))

;; Check if character is valid in variable name: [a-zA-Z0-9_-]
(defun tintin-is-varname-char? (ch)
  (or (and (string>=? ch "a") (string<=? ch "z"))
    (and (string>=? ch "A") (string<=? ch "Z"))
    (and (string>=? ch "0") (string<=? ch "9"))
    (string=? ch "_")
    (string=? ch "-")))

;; Expand $variable references in a string
(defun tintin-expand-variables (str)
  (if (not (string? str))
    str
    (let ((result str)
           (var-names (hash-keys *tintin-variables*)))
      ;; Replace each variable
      (do ((i 0 (+ i 1)))
        ((>= i (list-length var-names)) result)
        (let* ((var-name (list-ref var-names i))
                (var-value (hash-ref *tintin-variables* var-name)))
          (set! result (string-replace (concat "$" var-name) var-value result)))))))

;; Expand $variable references in a string (optimized O(m) single-pass)
(defun tintin-expand-variables-fast (str)
  (if (not (string? str))
    str
    (let ((len (string-length str))
           (pos 0)
           (result ""))
      (do ()
        ((>= pos len) result)
        (let ((ch (string-ref str pos)))
          (if (string=? ch "$")
            ;; Extract variable name
            (let ((var-start (+ pos 1))
                   (var-end (+ pos 1)))
              ;; Find end of variable name
              (do ()
                ((or (>= var-end len)
                   (not (tintin-is-varname-char? (string-ref str var-end)))))
                (set! var-end (+ var-end 1)))

              (if (= var-start var-end)
                ;; No variable name after $, keep literal $
                (progn
                  (set! result (concat result "$"))
                  (set! pos (+ pos 1)))
                ;; Variable name found, try to expand
                (let* ((var-name (substring str var-start var-end))
                        (var-value (hash-ref *tintin-variables* var-name)))
                  (if var-value
                    (set! result (concat result var-value))
                    (set! result (concat result "$" var-name)))
                  (set! pos var-end))))
            ;; Regular character
            (progn
              (set! result (concat result ch))
              (set! pos (+ pos 1)))))))))

;; ============================================================================
;; ACTION CAPTURE EXTRACTION
;; ============================================================================

;; Extract %1-%99 capture groups from pattern match
;; Uses regex-extract builtin to get capture values from matched text
;; Returns: List of captured strings or empty list if no match
;; Example: pattern="You hit %1 for %2 damage", text="You hit orc for 15 damage"
;;          → ("orc" "15")
(defun tintin-extract-captures (pattern text)
  (if (or (not (string? pattern)) (not (string? text)))
    '()
    (let ((regex-pattern (tintin-pattern-to-regex pattern)))
      (if (string=? regex-pattern "")
        '()
        ;; Use regex-extract to get all capture groups
        (let ((captures (regex-extract regex-pattern text)))
          (if captures captures '()))))))

;; Replace %1-%99 in template with capture values
;; Iterates through captures list, replacing each placeholder
;; Returns: Template with placeholders replaced
;; Example: template="say %1 took %2!", captures=("orc" "15")
;;          → "say orc took 15!"
(defun tintin-substitute-captures (template captures)
  (if (or (not (string? template)) (not (list? captures)))
    template
    (let ((result template))
      ;; Replace each capture group placeholder (%1, %2, ..., %99)
      (do ((i 0 (+ i 1)))
        ((>= i (list-length captures)) result)
        (let ((placeholder (concat "%" (number->string (+ i 1))))
               (value (list-ref captures i)))
          (if (string? value)
            (set! result (string-replace placeholder value result))))))))

;; ============================================================================
;; SAVE/LOAD UTILITY FUNCTIONS
;; ============================================================================

;; Escape string for Lisp syntax (backslashes first, then quotes)
(defun tintin-escape-string (str)
  (let ((result str))
    (set! result (string-replace "\\" "\\\\" result))  ; Escape backslashes first
    (set! result (string-replace "\"" "\\\"" result))  ; Then escape quotes
    result))


;; Save TinTin++ state to file
(defun tintin-save-state (filename)
  (let ((file (open filename "w")))
    ;; Write header
    (write-line file ";; TinTin++ State File")
    (write-line file ";; Generated by #save command")
    (write-line file "")

    ;; Write aliases
    (write-line file ";; Aliases")
    (let ((alias-entries (hash-entries *tintin-aliases*)))
      (do ((i 0 (+ i 1)))
        ((>= i (list-length alias-entries)))
        (let* ((entry (list-ref alias-entries i))
                (name (car entry))
                (value (cdr entry))
                (commands (car value))
                (priority (car (cdr value))))
          (write-line file (concat "(hash-set! *tintin-aliases* "
                             "\"" (tintin-escape-string name) "\" "
                             "(list \"" (tintin-escape-string commands) "\" "
                             (number->string priority) "))")))))
    (write-line file "")

    ;; Write variables
    (write-line file ";; Variables")
    (let ((var-entries (hash-entries *tintin-variables*)))
      (do ((i 0 (+ i 1)))
        ((>= i (list-length var-entries)))
        (let* ((entry (list-ref var-entries i))
                (name (car entry))
                (value (cdr entry)))
          (write-line file (concat "(hash-set! *tintin-variables* "
                             "\"" (tintin-escape-string name) "\" "
                             "\"" (tintin-escape-string value) "\")")))))
    (write-line file "")

    ;; Write highlights
    (write-line file ";; Highlights")
    (let ((highlight-entries (hash-entries *tintin-highlights*)))
      (do ((i 0 (+ i 1)))
        ((>= i (list-length highlight-entries)))
        (let* ((entry (list-ref highlight-entries i))
                (pattern (car entry))
                (data (cdr entry))
                (fg-color (car data))
                (bg-color (car (cdr data)))
                (priority (car (cdr (cdr data)))))
          (write-line file (concat "(hash-set! *tintin-highlights* "
                             "\"" (tintin-escape-string pattern) "\" "
                             "(list "
                             (if fg-color
                               (concat "\"" (tintin-escape-string fg-color) "\"")
                               "nil")
                             " "
                             (if bg-color
                               (concat "\"" (tintin-escape-string bg-color) "\"")
                               "nil")
                             " "
                             (number->string priority) "))")))))
    (write-line file "")

    ;; Write actions
    (write-line file ";; Actions")
    (let ((action-entries (hash-entries *tintin-actions*)))
      (do ((i 0 (+ i 1)))
        ((>= i (list-length action-entries)))
        (let* ((entry (list-ref action-entries i))
                (pattern (car entry))
                (data (cdr entry))
                (commands (car data))
                (priority (car (cdr data))))
          (write-line file (concat "(hash-set! *tintin-actions* "
                             "\"" (tintin-escape-string pattern) "\" "
                             "(list \"" (tintin-escape-string commands) "\" "
                             (number->string priority) "))")))))
    (write-line file "")

    ;; Write settings
    (write-line file ";; Settings")
    (write-line file (concat "(set! *tintin-speedwalk-enabled* "
                       (if *tintin-speedwalk-enabled* "#t" "#f") ")"))
    (write-line file (concat "(set! *tintin-speedwalk-diagonals* "
                       (if *tintin-speedwalk-diagonals* "#t" "#f") ")"))
    (write-line file (concat "(set! *tintin-enabled* "
                       (if *tintin-enabled* "#t" "#f") ")"))

    ;; Close file
    (close file)
    filename))

;; ============================================================================
;; TINTIN++ COMMAND HELPERS
;; ============================================================================

;; Check if a string is a TinTin++ command (starts with #)
(defun tintin-is-command? (str)
  (and (string? str)
    (> (string-length str) 0)
    (string=? (string-ref str 0) "#")))

;; Extract command name from TinTin++ command string
;; Example: "#alias {k} {kill}" → "alias"
;;          "#var{x}" → "var"
;;          "# " → nil
(defun tintin-extract-command-name (str)
  (if (not (tintin-is-command? str))
    nil
    (let ((len (string-length str))
           (pos 1))  ; Start after #
      ;; Skip any whitespace after #
      (do ()
        ((or (>= pos len) (not (string=? (string-ref str pos) " "))))
        (set! pos (+ pos 1)))
      ;; Check if we have any characters left
      (if (>= pos len)
        nil
        ;; Find end of command word (space, {, or end of string)
        (let ((start pos)
               (end pos))
          (do ()
            ((or (>= end len)
               (string=? (string-ref str end) " ")
               (string=? (string-ref str end) "{")))
            (set! end (+ end 1)))
          ;; Extract and lowercase the command name
          (if (= start end)
            nil
            (string-downcase (substring str start end))))))))

;; Find a TinTin++ command by partial prefix match
;; Returns the full command name or nil if no match
;; Example: "al" → "alias", "var" → "variable"
(defun tintin-find-command (prefix)
  (if (not (string? prefix))
    nil
    (let ((prefix-lower (string-downcase prefix))
           (commands (hash-keys *tintin-commands*))
           (result nil))
      (do ((i 0 (+ i 1)))
        ((or (>= i (list-length commands)) result) result)
        (let ((cmd (list-ref commands i)))
          (if (string-prefix? prefix-lower cmd)
            (set! result cmd)))))))

;; Match first word against alias hash table
;; Returns: (alias-entry . args) or nil
(defun tintin-match-simple-alias (cmd)
  (let ((words (split cmd " ")))
    (if (or (null? words) (= (list-length words) 0))
      nil
      (let ((first-word (car words))
             (args (cdr words)))
        (let ((alias-entry (hash-ref *tintin-aliases* first-word)))
          (if alias-entry
            (cons alias-entry args)
            nil))))))

;; Linear search for pattern aliases
;; Returns: (pattern . match-values) or nil
(defun tintin-match-pattern-alias (cmd)
  (let ((alias-names (hash-keys *tintin-aliases*))
         (matched nil))
    (if (or (null? alias-names) (= (list-length alias-names) 0))
      nil
      (do ((i 0 (+ i 1)))
        ((or (>= i (list-length alias-names)) matched) matched)
        (let* ((pattern (list-ref alias-names i))
                (match-values (tintin-match-pattern pattern cmd)))
          (if match-values
            (set! matched (cons pattern match-values))))))))

;; Replace %0, %1, %2... in template with args or match-values
;; Returns: template with placeholders replaced + unused args appended
(defun tintin-substitute-template (template args match-values)
  (let* ((arg-vals (or match-values args '()))
          (result template)
          (used-args (make-vector (list-length arg-vals) #f)))

    ;; Replace %0 with all arguments
    (if (> (list-length arg-vals) 0)
      (let ((all-args "")
             (old-result result))
        (do ((i 0 (+ i 1)))
          ((>= i (list-length arg-vals)))
          (set! all-args (concat all-args
                           (if (> i 0) " " "")
                           (list-ref arg-vals i))))
        (set! result (string-replace "%0" all-args result))
        ;; Mark all args as used if %0 was replaced
        (if (not (string=? result old-result))
          (do ((i 0 (+ i 1)))
            ((>= i (list-length arg-vals)))
            (vector-set! used-args i #t)))))

    ;; Replace %1, %2, etc.
    (do ((i 0 (+ i 1)))
      ((>= i (list-length arg-vals)))
      (let ((placeholder (concat "%" (number->string (+ i 1))))
             (old-result result))
        (set! result (string-replace placeholder (list-ref arg-vals i) result))
        (if (not (string=? result old-result))
          (vector-set! used-args i #t))))

    ;; Append unused arguments (only for simple aliases with args)
    (if args
      (let ((unused-list '()))
        (do ((j 0 (+ j 1)))
          ((>= j (list-length arg-vals)))
          (if (not (vector-ref used-args j))
            (set! unused-list (cons (list-ref arg-vals j) unused-list))))
        (if (not (eq? unused-list '()))
          (let ((unused-args "")
                 (reversed (reverse unused-list)))
            (do ((k 0 (+ k 1)))
              ((>= k (list-length reversed)))
              (set! unused-args (concat unused-args
                                  (if (> k 0) " " "")
                                  (list-ref reversed k))))
            (set! result (concat result " " unused-args))))))

    result))

;; Expand speedwalk, split by semicolons, recursively process
;; Returns: fully expanded and joined commands
;; KEY: This eliminates ~70 lines of duplication
(defun tintin-expand-and-recurse (result depth)
  ;; Check depth limit (circular alias detection)
  (if (>= depth *tintin-max-alias-depth*)
    (progn
      (tintin-echo (concat "Error: Circular alias detected or depth limit ("
                     (number->string *tintin-max-alias-depth*)
                     ") exceeded\r\n"))
      result)  ; Return unexpanded to stop recursion

    ;; Expand speedwalk only (variables expand per-command for just-in-time evaluation)
    (let ((expanded (tintin-expand-speedwalk result)))
      ;; Split by semicolon
      (let ((split-commands (tintin-split-commands expanded)))
        (if (> (list-length split-commands) 1)
          ;; Multiple commands - recursively process each
          (let ((sub-results '()))
            (do ((j 0 (+ j 1)))
              ((>= j (list-length split-commands)))
              (let ((subcmd (list-ref split-commands j)))
                (if (and (string? subcmd) (not (string=? subcmd "")))
                  ;; Expand variables for THIS command only (just-in-time)
                  (let* ((cmd-with-vars (tintin-expand-variables-fast subcmd))
                          (result (tintin-process-command-internal cmd-with-vars (+ depth 1))))
                    (if (and (string? result) (not (string=? result "")))
                      (set! sub-results (cons result sub-results)))))))
            ;; Join with semicolons
            (if (eq? sub-results '())
              ""
              (let ((reversed (reverse sub-results))
                     (output ""))
                (do ((k 0 (+ k 1)))
                  ((>= k (list-length reversed)) output)
                  (set! output (concat output
                                 (if (> k 0) ";" "")
                                 (list-ref reversed k)))))))
          ;; Single command - recursively process
          (if (> (list-length split-commands) 0)
            (let ((cmd-with-vars (tintin-expand-variables-fast (list-ref split-commands 0))))
              (tintin-process-command-internal cmd-with-vars (+ depth 1)))
            ""))))))

;; ============================================================================
;; ACTION EXECUTION
;; ============================================================================

;; Execute action commands with circular execution detection
;; Sets *tintin-action-executing* flag to prevent infinite loops
;; Processes commands via tintin-process-input and sends each via telnet-send
;; Returns: nil (side effect only)
(defun tintin-execute-action (commands)
  (if (not (string? commands))
    nil
    ;; Check circular execution flag
    (if *tintin-action-executing*
      (progn
        (tintin-echo "Warning: Action triggered during action execution (skipped)\r\n")
        nil)
      (progn
        ;; Set flag to prevent recursion
        (set! *tintin-action-executing* #t)
        ;; Process and send commands
        (condition-case err
          (progn
            (let ((processed (tintin-process-input commands)))
              (if (and (string? processed) (not (string=? processed "")))
                (let ((cmd-list (tintin-split-commands processed)))
                  (do ((i 0 (+ i 1)))
                    ((>= i (list-length cmd-list)))
                    (let ((cmd (list-ref cmd-list i)))
                      (if (and (string? cmd) (not (string=? cmd "")))
                        (condition-case send-err
                          (telnet-send (concat cmd "\r\n"))
                          (error
                            (tintin-echo (concat "Action send failed: "
                                           (error-message send-err) "\r\n"))))))))))
            ;; Clear flag after execution
            (set! *tintin-action-executing* #f))
          (error
            ;; Clear flag on error
            (set! *tintin-action-executing* #f)
            (tintin-echo (concat "Action execution error: "
                           (error-message err) "\r\n"))))))))

;; Test all action patterns against line and execute matches
;; Processes ALL matching actions in priority order (low to high)
;; For each match: extract captures → substitute → expand vars → execute
(defun tintin-trigger-actions-for-line (line)
  (if (or (not (string? line)) (= (hash-count *tintin-actions*) 0))
    nil
    ;; Get all actions sorted by priority (low to high)
    (let ((action-entries (hash-entries *tintin-actions*)))
      (let ((sorted (tintin-sort-actions-by-priority action-entries)))
        ;; Try all patterns and execute all that match
        (do ((i 0 (+ i 1)))
          ((>= i (list-length sorted)))
          (let* ((entry (list-ref sorted i))
                  (pattern (car entry))
                  (data (cdr entry))
                  (commands (car data))
                  (priority (car (cdr data))))
            ;; Check if pattern matches the line
            (if (tintin-match-highlight-pattern pattern line)
              ;; Pattern matches - extract captures and execute
              (let ((captures (tintin-extract-captures pattern line)))
                ;; Substitute captures in commands
                (let ((substituted (tintin-substitute-captures commands captures)))
                  ;; Expand variables
                  (let ((expanded (tintin-expand-variables-fast substituted)))
                    ;; Execute the action
                    (tintin-execute-action expanded)))))))))))

;; Orchestrate alias matching and expansion
;; Returns: expanded command (may contain semicolons)
(defun tintin-expand-alias (cmd depth)
  (let ((expanded-cmd (tintin-expand-variables-fast cmd)))
    ;; Try simple alias match
    (let ((simple-match (tintin-match-simple-alias expanded-cmd)))
      (if simple-match
        ;; Simple alias found
        (let* ((alias-entry (car simple-match))
                (args (cdr simple-match))
                (template (car alias-entry))
                (result (tintin-substitute-template template args nil)))
          (tintin-expand-and-recurse result depth))

        ;; Try pattern alias match
        (let ((pattern-match (tintin-match-pattern-alias expanded-cmd)))
          (if pattern-match
            ;; Pattern alias found
            (let* ((pattern (car pattern-match))
                    (match-values (cdr pattern-match))
                    (alias-data (hash-ref *tintin-aliases* pattern))
                    (template (car alias-data))
                    (result (tintin-substitute-template template nil match-values)))
              (tintin-expand-and-recurse result depth))

            ;; No alias match - just expand speedwalk
            (tintin-expand-speedwalk expanded-cmd)))))))

;; Main command router with depth tracking (internal)
(defun tintin-process-command-internal (cmd depth)
  (if (or (not (string? cmd)) (string=? cmd ""))
    ""
    ;; Check if it's a # command
    (if (tintin-is-command? cmd)
      (progn
        ;; Echo # command
        (tintin-echo (concat cmd "\r\n"))
        ;; Extract and dispatch
        (let ((cmd-name (tintin-extract-command-name cmd)))
          (if (not cmd-name)
            (progn
              (tintin-echo (concat "Invalid TinTin++ command format: " cmd "\r\n"))
              "")
            (let ((matched (tintin-find-command cmd-name)))
              (if (not matched)
                (progn
                  (tintin-echo (concat "Unknown TinTin++ command: #" cmd-name "\r\n"))
                  "")
                (tintin-dispatch-command matched cmd))))))
      ;; Regular command - expand aliases
      (tintin-expand-alias cmd depth))))

;; Process a single command (public wrapper, always starts at depth 0)
(defun tintin-process-command (cmd)
  (tintin-process-command-internal cmd 0))

;; ============================================================================
;; TEST 7: FULL INPUT PROCESSING
;; ============================================================================

;; Process a full input line (split by semicolons, process each command)
(defun tintin-process-input (input)
  (if (not (string? input))
    ""
    (let ((commands (tintin-split-commands input))
           (results '()))
      ;; Process each command and collect results
      (do ((i 0 (+ i 1)))
        ((>= i (list-length commands)))
        (let ((processed (tintin-process-command (list-ref commands i))))
          (if (and (string? processed) (not (string=? processed "")))
            (set! results (cons processed results)))))
      ;; Reverse and join results with semicolons
      (let ((reversed-results (reverse results))
             (output ""))
        (do ((i 0 (+ i 1)))
          ((>= i (list-length reversed-results)) output)
          (set! output (concat output
                         (if (> i 0) ";" "")
                         (list-ref reversed-results i))))))))

;; ============================================================================
;; USER-INPUT-HOOK INTEGRATION
;; ============================================================================

;; Hook function for user-input-hook integration
;; Signature: (lambda (text cursor-pos) -> string|nil)
;; - text: User input text
;; - cursor-pos: Cursor position (ignored for TinTin++ processing)
;; Returns: nil (hook handles echo/send) or text (when disabled)
;;
;; Hook Contract: Returns nil to indicate all echo/send handled by hook.
;; This hook processes TinTin++ commands and sends each one separately.
;; For example, "s;s" becomes two separate telnet sends: "s" and "s"
(defun tintin-user-input-hook (text cursor-pos)
  (if (not *tintin-enabled*)
    text
    (progn
      ;; Echo original input FIRST (unless it's a # command)
      (if (not (tintin-is-command? text))
        (tintin-echo (concat text "\r\n")))
      (let ((processed (tintin-process-input text))
             (commands nil))
        ;; Split processed output by semicolons
        (set! commands (tintin-split-commands processed))
        ;; Send each command separately
        (do ((i 0 (+ i 1)))
          ((>= i (list-length commands)))
          (let ((cmd (list-ref commands i)))
            (if (and (string? cmd) (not (string=? cmd "")))
              (progn
                ;; Echo expanded command to terminal (if different from original)
                (if (and (string? cmd) (string? text) (not (string=? cmd text)))
                  (tintin-echo (concat cmd "\r\n")))
                ;; Send to telnet server with error handling
                (condition-case err
                  (progn
                    ;; Check if we can send (connected or test mode)
                    (let ((can-send
                            (condition-case err2
                              ;; Try to check connection mode
                              (or (eq? *connection-mode* 'conn)
                                ;; If *connection-mode* undefined (test mode), check if telnet-send exists
                                (and (symbol? 'telnet-send) #t))
                              ;; If *connection-mode* not defined, we're in test mode
                              (error #t))))
                      (if can-send
                        ;; Send the command
                        (telnet-send (concat cmd "\r\n"))
                        ;; Not connected
                        (tintin-echo "\r\n*** Not connected ***\r\n"))))
                  ;; Catch any send errors
                  (error
                    (tintin-echo (concat "\r\n*** Send failed: "
                                   (error-message err) " ***\r\n")))))))))
      ;; Return nil to indicate hook handled everything (proper contract)
      ())))
;; Toggle TinTin++ processing on/off
(defun tintin-toggle! ()
  (set! *tintin-enabled* (not *tintin-enabled*))
  (tintin-echo (concat "TinTin++ "
                 (if *tintin-enabled* "enabled" "disabled")
                 "\r\n"))
  *tintin-enabled*)

;; Enable TinTin++ processing
(defun tintin-enable! ()
  (set! *tintin-enabled* #t)
  (tintin-echo "TinTin++ enabled\r\n")
  #t)

;; Disable TinTin++ processing
(defun tintin-disable! ()
  (set! *tintin-enabled* #f)
  (tintin-echo "TinTin++ disabled\r\n")
  #f)

;; ============================================================================
;; UTILITY FUNCTIONS (REFACTORED)
;; ============================================================================

;; Echo text to terminal if available
;; Used to centralize terminal output across all command handlers
(defun tintin-echo (text)
  (if (symbol? 'terminal-echo)
    (terminal-echo text))
  nil)

;; Report syntax error for a command
;; Returns empty string to maintain handler contract
(defun tintin-syntax-error (syntax-help)
  (tintin-echo (concat "Syntax error: " syntax-help "\r\n"))
  "")

;; Strip outer braces from a string if present
;; Example: "{text}" → "text", "text" → "text", "{a{b}c}" → "a{b}c"
(defun tintin-strip-braces (str)
  (if (not (string? str))
    str
    (let ((len (string-length str)))
      (if (and (> len 1)
            (string=? (string-ref str 0) "{")
            (string=? (string-ref str (- len 1)) "}"))
        (substring str 1 (- len 1))
        str))))

;; Sort alias entries alphabetically by name
;; Input: list of (name . (commands priority)) pairs
;; Output: sorted list alphabetically by name
(defun tintin-sort-aliases-alphabetically (alias-list)
  (if (or (null? alias-list) (= (list-length alias-list) 0))
    '()
    ;; Simple insertion sort by name
    (let ((sorted '()))
      (do ((remaining alias-list (cdr remaining)))
        ((null? remaining) sorted)
        (let ((entry (car remaining))
               (name (car (car remaining))))
          ;; Insert entry in alphabetically sorted position
          (set! sorted (tintin-insert-alias-alphabetically entry name sorted)))))))

;; Helper: Insert alias entry into sorted list alphabetically by name
(defun tintin-insert-alias-alphabetically (entry name sorted-list)
  (if (null? sorted-list)
    (list entry)
    (let ((first-entry (car sorted-list))
           (first-name (car (car sorted-list))))
      (if (string<? name first-name)
        ;; Insert before first entry
        (cons entry sorted-list)
        ;; Insert later in list
        (cons first-entry
          (tintin-insert-alias-alphabetically entry name (cdr sorted-list)))))))

;; Sort highlight entries alphabetically by pattern
;; Input: list of (pattern . (fg-color bg-color priority)) pairs
;; Output: sorted list alphabetically by pattern
(defun tintin-sort-highlights-alphabetically (highlight-list)
  (if (or (null? highlight-list) (= (list-length highlight-list) 0))
    '()
    ;; Simple insertion sort by pattern
    (let ((sorted '()))
      (do ((remaining highlight-list (cdr remaining)))
        ((null? remaining) sorted)
        (let ((entry (car remaining))
               (pattern (car (car remaining))))
          ;; Insert entry in alphabetically sorted position
          (set! sorted (tintin-insert-highlight-alphabetically entry pattern sorted)))))))

;; Helper: Insert highlight entry into sorted list alphabetically by pattern
(defun tintin-insert-highlight-alphabetically (entry pattern sorted-list)
  (if (null? sorted-list)
    (list entry)
    (let ((first-entry (car sorted-list))
           (first-pattern (car (car sorted-list))))
      (if (string<? pattern first-pattern)
        ;; Insert before first entry
        (cons entry sorted-list)
        ;; Insert later in list
        (cons first-entry
          (tintin-insert-highlight-alphabetically entry pattern (cdr sorted-list)))))))

;; Sort action entries alphabetically by pattern
;; Input: list of (pattern . (commands-string priority)) pairs
;; Output: sorted list alphabetically by pattern
(defun tintin-sort-actions-alphabetically (action-list)
  (if (or (null? action-list) (= (list-length action-list) 0))
    '()
    ;; Simple insertion sort by pattern
    (let ((sorted '()))
      (do ((remaining action-list (cdr remaining)))
        ((null? remaining) sorted)
        (let ((entry (car remaining))
               (pattern (car (car remaining))))
          ;; Insert entry in alphabetically sorted position
          (set! sorted (tintin-insert-action-alphabetically entry pattern sorted)))))))

;; Helper: Insert action entry into sorted list alphabetically by pattern
(defun tintin-insert-action-alphabetically (entry pattern sorted-list)
  (if (null? sorted-list)
    (list entry)
    (let ((first-entry (car sorted-list))
           (first-pattern (car (car sorted-list))))
      (if (string<? pattern first-pattern)
        ;; Insert before first entry
        (cons entry sorted-list)
        ;; Insert later in list
        (cons first-entry
          (tintin-insert-action-alphabetically entry pattern (cdr sorted-list)))))))

;; ============================================================================
;; TABLE FORMATTING UTILITIES
;; ============================================================================

;; Pad string to specified width with spaces
(defun tintin-pad-string (str width)
  (if (not (string? str))
    ""
    (let ((str-len (string-length str)))
      (let ((padding-needed (- width str-len)))
        (if (<= padding-needed 0)
          str
          (let ((result str))
            (do ((i 0 (+ i 1)))
              ((>= i padding-needed) result)
              (set! result (concat result " ")))))))))

;; Repeat a string N times
(defun tintin-repeat-string (str count)
  (let ((result ""))
    (do ((i 0 (+ i 1)))
      ((>= i count) result)
      (set! result (concat result str)))))

;; Get visual length of string, excluding ANSI escape sequences
;; This is critical for table alignment with colored text
(defun tintin-visual-length (str)
  (if (not (string? str))
    0
    (let ((ansi-pattern "\\x1b\\[[0-9;]*m"))
      (string-length (regex-replace-all ansi-pattern str "")))))

;; Find best position to break text near width boundary
;; Returns position to break at (searches backwards for space/hyphen)
(defun tintin-find-break-point (text width)
  (if (<= (tintin-visual-length text) width)
    (string-length text)
    (let* ((text-len (string-length text))
            (start-pos (if (< width text-len) width (- text-len 1))))
      ;; Search backwards from width (or text end) for space or hyphen
      (do ((i start-pos (- i 1)))
        ((or (< i 0)
           (and (< i text-len)  ; Bounds check
             (let ((ch (string-ref text i)))
               (or (char=? ch #\space)
                 (char=? ch #\-)
                 (char=? ch #\newline)))))
          (if (< i 0)
            (if (< width text-len) width text-len)  ; Hard break at width or text end
            (+ i 1)))))))  ; Break after space/hyphen

;; Wrap text to fit within width, returning list of lines
(defun tintin-wrap-text (text width)
  (if (or (not (string? text)) (= (tintin-visual-length text) 0))
    '("")
    ;; Guard against invalid width
    (if (<= width 0)
      (list text)  ; Return as-is if width is too small
      (if (<= (tintin-visual-length text) width)
        (list text)
        ;; Find break point and split
        (let* ((break-pos (tintin-find-break-point text width))
                ;; Ensure we always make progress (at least 1 char)
                (safe-break-pos (if (<= break-pos 0) 1 break-pos))
                (line1 (substring text 0 safe-break-pos))
                (rest-start safe-break-pos)
                ;; Skip leading space in rest
                (rest-start-adj (if (and (< rest-start (string-length text))
                                      (char=? (string-ref text rest-start) #\space))
                                  (+ rest-start 1)
                                  rest-start)))
          (if (>= rest-start-adj (string-length text))
            (list line1)
            (let ((rest (substring text rest-start-adj (string-length text))))
              (cons line1 (tintin-wrap-text rest width)))))))))

;; Draw generic table border for any number of columns
;; widths: list of column widths
;; position: 'top, 'middle, or 'bottom
;; Returns: border string with Unicode box-drawing characters
(defun tintin-draw-border (widths position)
  (if (or (null? widths) (= (list-length widths) 0))
    ""
    (let ((chars (cond ((eq? position 'top) '("┌" "┬" "┐"))
                   ((eq? position 'middle) '("├" "┼" "┤"))
                   ((eq? position 'bottom) '("└" "┴" "┘"))
                   (#t '("├" "┼" "┤"))))  ; default to middle
           (left (car chars))
           (middle (cadr chars))
           (right (caddr chars))
           (line left))
      ;; Build border: left + (─*width1) + middle + (─*width2) + ... + right
      (do ((i 0 (+ i 1)))
        ((>= i (list-length widths)))
        (let ((width (list-ref widths i)))
          ;; Add horizontal line segment
          (set! line (concat line "─" (tintin-repeat-string "─" width) "─"))
          ;; Add junction or right cap
          (if (< (+ i 1) (list-length widths))
            (set! line (concat line middle))
            (set! line (concat line right)))))
      (concat line "\r\n"))))

;; DEPRECATED: Old function kept for compatibility
;; Use tintin-draw-border instead
(defun tintin-draw-table-border (widths style)
  (tintin-draw-border widths style))

;; Draw table row with wrapping support
;; cells: list of cell values (strings)
;; widths: list of column widths
;; Returns: list of display lines for this logical row
(defun tintin-draw-row (cells widths)
  (if (or (null? cells) (null? widths))
    '("")
    (let ((wrapped-cells '())
           (max-lines 0))
      ;; Step 1: Wrap each cell to its column width
      (do ((i 0 (+ i 1)))
        ((>= i (list-length cells)))
        (let ((cell (list-ref cells i))
               (width (list-ref widths i)))
          (let ((wrapped (tintin-wrap-text cell width)))
            (set! wrapped-cells (cons wrapped wrapped-cells))
            ;; Track max lines needed
            (if (> (list-length wrapped) max-lines)
              (set! max-lines (list-length wrapped))))))

      ;; Reverse to restore original order
      (set! wrapped-cells (reverse wrapped-cells))

      ;; Step 2: Build each display line
      (let ((result '()))
        (do ((line-idx 0 (+ line-idx 1)))
          ((>= line-idx max-lines) (reverse result))
          (let ((line "│ "))
            ;; Build this display line from all columns
            (do ((col-idx 0 (+ col-idx 1)))
              ((>= col-idx (list-length cells)))
              (let* ((wrapped-cell (list-ref wrapped-cells col-idx))
                      (width (list-ref widths col-idx))
                      ;; Get text for this line (or empty if this cell has fewer lines)
                      (text (if (< line-idx (list-length wrapped-cell))
                              (list-ref wrapped-cell line-idx)
                              ""))
                      (padded (tintin-pad-string text width)))
                (set! line (concat line padded))
                ;; Add separator or end cap
                (if (< (+ col-idx 1) (list-length cells))
                  (set! line (concat line " │ "))
                  (set! line (concat line " │\r\n")))))
            (set! result (cons line result))))))))

;; DEPRECATED: Old function kept for compatibility
;; Use tintin-draw-row instead
(defun tintin-draw-table-row (values widths)
  (let ((lines (tintin-draw-row values widths)))
    ;; Return first line only (no wrapping in old version)
    (if (null? lines)
      ""
      (car lines))))

;; Calculate optimal column widths that fit within max-width
;; data: list of lists (rows x columns)
;; max-width: terminal width in characters (from terminal-info)
;; min-col-width: minimum width per column (default 8)
;; Returns: list of column widths that fit within max-width
(defun tintin-calculate-optimal-widths (data max-width min-col-width)
  (if (or (null? data) (= (list-length data) 0))
    '()
    (let ((num-cols (list-length (car data)))
           (col-maxes (make-vector (list-length (car data)) 0)))
      ;; Step 1: Find max visual width for each column
      (do ((i 0 (+ i 1)))
        ((>= i (list-length data)))
        (let ((row (list-ref data i)))
          (do ((j 0 (+ j 1)))
            ((>= j (list-length row)))
            (let ((cell (list-ref row j)))
              (let ((cell-width (tintin-visual-length cell))
                     (current-max (vector-ref col-maxes j)))
                (if (> cell-width current-max)
                  (vector-set! col-maxes j cell-width)))))))

      ;; Step 2: Calculate total needed width
      ;; formula: sum(widths) + (num-cols + 1) + (num-cols - 1) * 3
      (let ((content-width 0))
        ;; Sum up column widths
        (do ((k 0 (+ k 1)))
          ((>= k num-cols))
          (set! content-width (+ content-width (vector-ref col-maxes k))))

        (let* ((border-width (+ num-cols 1))
                (separator-width (* (- num-cols 1) 3))
                (total-width (+ content-width border-width separator-width)))

          ;; Step 3: If total > max-width, scale down proportionally
          (if (<= total-width max-width)
            ;; Fits within terminal, return as-is
            (let ((result '()))
              (do ((k 0 (+ k 1)))
                ((>= k num-cols) (reverse result))
                (set! result (cons (vector-ref col-maxes k) result))))
            ;; Needs scaling: available = max-width - borders - separators
            (let ((available (- max-width border-width separator-width))
                   (result '()))
              (do ((k 0 (+ k 1)))
                ((>= k num-cols) (reverse result))
                (let* ((col-width (vector-ref col-maxes k))
                        ;; Proportional scaling: (width * available) / content-width
                        (scaled-width (quotient (* col-width available) content-width))
                        ;; Ensure at least min-col-width
                        (final-width (if (< scaled-width min-col-width)
                                       min-col-width
                                       scaled-width)))
                  (set! result (cons final-width result)))))))))))

;; DEPRECATED: Old function kept for compatibility
;; Use tintin-calculate-optimal-widths instead
(defun tintin-calculate-column-widths (rows)
  (tintin-calculate-optimal-widths rows 80 8))

;; ============================================================================
;; GENERIC TABLE PRINTER
;; ============================================================================

;; Print formatted table from list of lists
;; data: ((header1 header2 ...) (row1-col1 row1-col2 ...) ...)
;; First list is treated as headers (rendered in bold)
;; Automatically detects terminal width and optimizes column layout
(defun tintin-print-table (data)
  (if (or (null? data) (= (list-length data) 0))
    (tintin-echo "Error: Table data cannot be empty")
    (let* ((term-info (terminal-info))
            (term-cols (cdr (assoc 'cols term-info)))
            (min-col-width 8)
            (headers (car data))
            (rows (cdr data))
            (all-rows data))

      ;; Validate that we have at least headers
      (if (or (null? headers) (= (list-length headers) 0))
        (tintin-echo "Error: Table must have at least header row")
        (let ((widths (tintin-calculate-optimal-widths all-rows term-cols min-col-width)))

          ;; Draw top border
          (tintin-echo (tintin-draw-border widths 'top))

          ;; Draw header row (with bold formatting)
          (let ((bold-headers '()))
            ;; Add bold formatting to each header
            (do ((i 0 (+ i 1)))
              ((>= i (list-length headers)))
              (let ((header (list-ref headers i)))
                (set! bold-headers (cons (concat "\x1b[1m" header "\x1b[0m") bold-headers))))
            (set! bold-headers (reverse bold-headers))

            ;; Draw header lines
            (let ((header-lines (tintin-draw-row bold-headers widths)))
              (do ((i 0 (+ i 1)))
                ((>= i (list-length header-lines)))
                (tintin-echo (list-ref header-lines i)))))

          ;; Draw middle border
          (tintin-echo (tintin-draw-border widths 'middle))

          ;; Draw data rows (with wrapping if needed)
          (do ((row-idx 0 (+ row-idx 1)))
            ((>= row-idx (list-length rows)))
            (let* ((row (list-ref rows row-idx))
                    (row-lines (tintin-draw-row row widths)))
              (do ((line-idx 0 (+ line-idx 1)))
                ((>= line-idx (list-length row-lines)))
                (tintin-echo (list-ref row-lines line-idx)))))

          ;; Draw bottom border
          (tintin-echo (tintin-draw-border widths 'bottom)))))))

;; ============================================================================
;; LIST COMMANDS (using generic table printer)
;; ============================================================================

;; List all defined aliases
(defun tintin-list-aliases ()
  (let ((alias-entries (hash-entries *tintin-aliases*))
         (count (hash-count *tintin-aliases*)))
    (if (= count 0)
      (progn
        (tintin-echo "No aliases defined.\r\n")
        "")
      (progn
        (tintin-echo (concat "Aliases (" (number->string count) "):\r\n"))
        ;; Sort aliases alphabetically
        (let ((sorted (tintin-sort-aliases-alphabetically alias-entries)))
          ;; Build data structure: headers + data rows
          (let ((data (list (list "Name" "Commands" "Priority"))))
            ;; Add data rows
            (do ((i 0 (+ i 1)))
              ((>= i (list-length sorted)))
              (let* ((entry (list-ref sorted i))
                      (name (car entry))
                      (value (cdr entry))
                      (commands (car value))
                      (priority (car (cdr value)))
                      (priority-str (if (= priority 5) "" (number->string priority))))
                (set! data (append data (list (list name commands priority-str))))))
            ;; Print table using generic printer
            (tintin-print-table data)))
        ""))))

;; List all defined variables
(defun tintin-list-variables ()
  (let ((var-entries (hash-entries *tintin-variables*))
         (count (hash-count *tintin-variables*)))
    (if (= count 0)
      (progn
        (tintin-echo "No variables defined.\r\n")
        "")
      (progn
        (tintin-echo (concat "Variables (" (number->string count) "):\r\n"))
        ;; Build data structure: headers + data rows
        (let ((data (list (list "Variable" "Value"))))
          ;; Add data rows
          (do ((i 0 (+ i 1)))
            ((>= i (list-length var-entries)))
            (let* ((entry (list-ref var-entries i))
                    (name (car entry))
                    (value (cdr entry)))
              (set! data (append data (list (list name value))))))
          ;; Print table using generic printer
          (tintin-print-table data))
        ""))))

;; List all defined highlights (sorted alphabetically)
(defun tintin-list-highlights ()
  (let ((highlight-entries (hash-entries *tintin-highlights*))
         (count (hash-count *tintin-highlights*)))
    (if (= count 0)
      (progn
        (tintin-echo "No highlights defined.\r\n")
        "")
      (progn
        (tintin-echo (concat "Highlights (" (number->string count) "):\r\n"))
        ;; Sort alphabetically before displaying
        (let ((sorted (tintin-sort-highlights-alphabetically highlight-entries)))
          ;; Build data structure: headers + data rows
          (let ((data (list (list "Pattern" "Color" "Priority"))))
            ;; Add data rows
            (do ((i 0 (+ i 1)))
              ((>= i (list-length sorted)))
              (let* ((entry (list-ref sorted i))
                      (pattern (car entry))
                      (entry-data (cdr entry))
                      (fg-color (car entry-data))
                      (bg-color (car (cdr entry-data)))
                      (priority (car (cdr (cdr entry-data))))
                      (color-str (concat (if fg-color fg-color "")
                                   (if (and fg-color bg-color) ":" "")
                                   (if bg-color bg-color "")))
                      (priority-str (if (= priority 5) "" (number->string priority))))
                (set! data (append data (list (list pattern color-str priority-str))))))
            ;; Print table using generic printer
            (tintin-print-table data)))
        ""))))

;; List all defined actions (sorted alphabetically)
(defun tintin-list-actions ()
  (let ((action-entries (hash-entries *tintin-actions*))
         (count (hash-count *tintin-actions*)))
    (if (= count 0)
      (progn
        (tintin-echo "No actions defined.\r\n")
        "")
      (progn
        (tintin-echo (concat "Actions (" (number->string count) "):\r\n"))
        ;; Sort alphabetically before displaying
        (let ((sorted (tintin-sort-actions-alphabetically action-entries)))
          ;; Build data structure: headers + data rows
          (let ((data (list (list "Pattern" "Commands" "Priority"))))
            ;; Add data rows
            (do ((i 0 (+ i 1)))
              ((>= i (list-length sorted)))
              (let* ((entry (list-ref sorted i))
                      (pattern (car entry))
                      (entry-data (cdr entry))
                      (commands (car entry-data))
                      (priority (car (cdr entry-data)))
                      (priority-str (if (= priority 5) "" (number->string priority))))
                (set! data (append data (list (list pattern commands priority-str))))))
            ;; Print table using generic printer
            (tintin-print-table data)))
        ""))))

;; ============================================================================
;; COMMAND HANDLERS (REFACTORED)
;; ============================================================================

;; Handle #alias command
;; args: (), (name), or (name commands)
(defun tintin-handle-alias (args)
  (cond
    ;; No arguments - list all aliases
    ((or (null? args) (= 0 (list-length args)))
      (tintin-list-aliases))
    ;; One argument - show specific alias
    ((= 1 (list-length args))
      (let ((name (tintin-strip-braces (list-ref args 0))))
        (let ((alias-data (hash-ref *tintin-aliases* name)))
          (if alias-data
            (let ((commands (car alias-data))
                   (priority (car (cdr alias-data))))
              (tintin-echo (concat "Alias '" name "': " name " → " commands
                             (if (= priority 5)
                               ""
                               (concat " (priority: " (number->string priority) ")"))
                             "\r\n"))
              "")
            (progn
              (tintin-echo (concat "Alias '" name "' not found\r\n"))
              "")))))
    ;; Two arguments - create alias
    (#t
      (let ((name (tintin-strip-braces (list-ref args 0)))
             (commands (tintin-strip-braces (list-ref args 1)))
             (priority 5))  ; Default priority
        (hash-set! *tintin-aliases* name (list commands priority))
        (tintin-echo (concat "Alias '" name "' created: " name " → " commands
                       (if (= priority 5)
                         ""
                         (concat " (priority: " (number->string priority) ")"))
                       "\r\n"))
        ""))))

;; Handle #variable command
;; args: (), (name), or (name value)
(defun tintin-handle-variable (args)
  (cond
    ;; No arguments - list all variables
    ((or (null? args) (= 0 (list-length args)))
      (tintin-list-variables))
    ;; One argument - show specific variable
    ((= 1 (list-length args))
      (let ((name (tintin-strip-braces (list-ref args 0))))
        (let ((value (hash-ref *tintin-variables* name)))
          (if value
            (progn
              (tintin-echo (concat "Variable '" name "': " name " = " value "\r\n"))
              "")
            (progn
              (tintin-echo (concat "Variable '" name "' not found\r\n"))
              "")))))
    ;; Two arguments - create variable
    (#t
      (let ((name (tintin-strip-braces (list-ref args 0)))
             (value (tintin-strip-braces (list-ref args 1))))
        (hash-set! *tintin-variables* name value)
        (tintin-echo (concat "Variable '" name "' set to '" value "'\r\n"))
        ""))))

;; Handle #unalias command
;; args: (name)
(defun tintin-handle-unalias (args)
  (let ((name (tintin-strip-braces (list-ref args 0))))
    (if (hash-ref *tintin-aliases* name)
      (progn
        (hash-remove! *tintin-aliases* name)
        (tintin-echo (concat "Alias '" name "' removed\r\n"))
        "")
      (progn
        (tintin-echo (concat "Alias '" name "' not found\r\n"))
        ""))))

;; Handle #highlight command
;; args: (), (pattern), (pattern color), or (pattern color priority)
;; Color spec format: "fg", "fg:bg", "<rgb>", "bold red", etc.
;; Entry format: pattern → (fg-color bg-color priority)
(defun tintin-handle-highlight (args)
  (cond
    ;; No arguments - list all highlights
    ((or (null? args) (= 0 (list-length args)))
      (tintin-list-highlights))
    ;; One argument - show specific highlight
    ((= 1 (list-length args))
      (let ((pattern (tintin-strip-braces (list-ref args 0))))
        (let ((highlight-data (hash-ref *tintin-highlights* pattern)))
          (if highlight-data
            (let ((fg-color (car highlight-data))
                   (bg-color (car (cdr highlight-data)))
                   (priority (car (cdr (cdr highlight-data)))))
              (tintin-echo (concat "Highlight '" pattern "': " pattern " → "
                             (if fg-color fg-color "")
                             (if (and fg-color bg-color) ":" "")
                             (if bg-color bg-color "")
                             (if (= priority 5)
                               ""
                               (concat " (priority: " (number->string priority) ")"))
                             "\r\n"))
              "")
            (progn
              (tintin-echo (concat "Highlight '" pattern "' not found\r\n"))
              "")))))
    ;; Two or three arguments - create highlight
    (#t
      (let* ((pattern (tintin-strip-braces (list-ref args 0)))
              (color-spec (tintin-strip-braces (list-ref args 1)))
              (priority (if (>= (list-length args) 3)
                          (string->number (tintin-strip-braces (list-ref args 2)))
                          5)))  ; Default priority
        ;; Parse color spec into FG and BG components
        (let ((parts (tintin-split-fg-bg color-spec)))
          (let ((fg-part (list-ref parts 0))
                 (bg-part (list-ref parts 1)))
            ;; Store as (fg-color bg-color priority)
            (hash-set! *tintin-highlights* pattern
              (list (if (string=? fg-part "") nil fg-part)
                bg-part
                priority))
            (tintin-echo (concat "Highlight '" pattern "' created: "
                           pattern " → " color-spec
                           (if (= priority 5)
                             ""
                             (concat " (priority: " (number->string priority) ")"))
                           "\r\n"))
            ""))))))

;; Handle #unhighlight command
;; args: (pattern)
(defun tintin-handle-unhighlight (args)
  (let ((pattern (tintin-strip-braces (list-ref args 0))))
    (if (hash-ref *tintin-highlights* pattern)
      (progn
        (hash-remove! *tintin-highlights* pattern)
        (tintin-echo (concat "Highlight '" pattern "' removed\r\n"))
        "")
      (progn
        (tintin-echo (concat "Highlight '" pattern "' not found\r\n"))
        ""))))

;; Handle #action command
;; args: (), (pattern), (pattern commands), or (pattern commands priority)
;; Entry format: pattern → (commands-string priority)
(defun tintin-handle-action (args)
  (cond
    ;; No arguments - list all actions
    ((or (null? args) (= 0 (list-length args)))
      (tintin-list-actions))
    ;; One argument - show specific action
    ((= 1 (list-length args))
      (let ((pattern (tintin-strip-braces (list-ref args 0))))
        (let ((action-data (hash-ref *tintin-actions* pattern)))
          (if action-data
            (let ((commands (car action-data))
                   (priority (car (cdr action-data))))
              (tintin-echo (concat "Action '" pattern "': " pattern " → " commands
                             (if (= priority 5)
                               ""
                               (concat " (priority: " (number->string priority) ")"))
                             "\r\n"))
              "")
            (progn
              (tintin-echo (concat "Action '" pattern "' not found\r\n"))
              "")))))
    ;; Two or three arguments - create action
    (#t
      (let* ((pattern (tintin-strip-braces (list-ref args 0)))
              (commands (tintin-strip-braces (list-ref args 1)))
              (priority (if (>= (list-length args) 3)
                          (string->number (tintin-strip-braces (list-ref args 2)))
                          5)))  ; Default priority
        ;; Store as (commands-string priority)
        (hash-set! *tintin-actions* pattern (list commands priority))
        (tintin-echo (concat "Action '" pattern "' created: "
                       pattern " → " commands
                       (if (= priority 5)
                         ""
                         (concat " (priority: " (number->string priority) ")"))
                       "\r\n"))
        ""))))

;; Handle #unaction command
;; args: (pattern)
(defun tintin-handle-unaction (args)
  (let ((pattern (tintin-strip-braces (list-ref args 0))))
    (if (hash-ref *tintin-actions* pattern)
      (progn
        (hash-remove! *tintin-actions* pattern)
        (tintin-echo (concat "Action '" pattern "' removed\r\n"))
        "")
      (progn
        (tintin-echo (concat "Action '" pattern "' not found\r\n"))
        ""))))

;; Handle #save command
;; args: (filename)
(defun tintin-handle-save (args)
  (let ((filename (tintin-strip-braces (list-ref args 0))))
    ;; Expand ~/path if present
    (set! filename (expand-path filename))
    (tintin-save-state filename)
    (tintin-echo (concat "State saved to '" filename "'\r\n"))
    ""))

;; Handle #load command
;; args: (filename)
(defun tintin-handle-load (args)
  (let ((filename (tintin-strip-braces (list-ref args 0))))
    ;; Expand ~/path if present
    (set! filename (expand-path filename))
    ;; Try to load the file, catching errors
    (if (condition-case err
          (progn (load filename) #t)
          (error #f))
      ;; Success case
      (progn
        (tintin-echo (concat "State loaded from '" filename "'\r\n"))
        "")
      ;; Error case
      (progn
        (tintin-echo (concat "Failed to load '" filename "': file not found or invalid\r\n"))
        ""))))

;; Register commands with metadata (now that handlers are defined)
(hash-set! *tintin-commands* "alias"
  (list tintin-handle-alias 2 "#alias or #alias {name} or #alias {name} {commands}"))
(hash-set! *tintin-commands* "unalias"
  (list tintin-handle-unalias 1 "#unalias {name}"))
(hash-set! *tintin-commands* "variable"
  (list tintin-handle-variable 2 "#variable or #variable {name} or #variable {name} {value}"))
(hash-set! *tintin-commands* "highlight"
  (list tintin-handle-highlight 2 "#highlight or #highlight {pattern} or #highlight {pattern} {color}"))
(hash-set! *tintin-commands* "unhighlight"
  (list tintin-handle-unhighlight 1 "#unhighlight {pattern}"))
(hash-set! *tintin-commands* "save"
  (list tintin-handle-save 1 "#save {filename}"))
(hash-set! *tintin-commands* "load"
  (list tintin-handle-load 1 "#load {filename}"))
(hash-set! *tintin-commands* "action"
  (list tintin-handle-action 3 "#action or #action {pattern} or #action {pattern} {commands} [priority]"))
(hash-set! *tintin-commands* "unaction"
  (list tintin-handle-unaction 1 "#unaction {pattern}"))

;; ============================================================================
;; GENERIC COMMAND DISPATCHER (REFACTORED)
;; ============================================================================

;; Check if a TinTin++ command has any arguments
;; Returns #t if arguments present, #f if just command name
(defun tintin-has-arguments? (input)
  (let ((len (string-length input))
         (pos 1))  ; Start after #
    ;; Skip whitespace after #
    (do ()
      ((or (>= pos len) (not (string=? (string-ref input pos) " "))))
      (set! pos (+ pos 1)))
    ;; Skip command name
    (do ()
      ((or (>= pos len)
         (string=? (string-ref input pos) " ")
         (string=? (string-ref input pos) "{")))
      (set! pos (+ pos 1)))
    ;; Skip whitespace after command name
    (do ()
      ((or (>= pos len) (not (string=? (string-ref input pos) " "))))
      (set! pos (+ pos 1)))
    ;; If we have more characters, there are arguments
    (< pos len)))

;; Try parsing with progressively fewer arguments (for variable-arg commands)
;; Returns parsed args list or nil if all attempts fail
(defun tintin-try-parse-arguments (input max-count)
  (if (<= max-count 0)
    nil
    (let ((args (tintin-parse-arguments input max-count)))
      (if args
        args
        ;; Try with one fewer argument
        (tintin-try-parse-arguments input (- max-count 1))))))

;; Dispatch a TinTin++ command using metadata-driven approach
;; cmd-name: matched command name (e.g., "alias")
;; input: original input string (e.g., "#alias {k} {kill %1}")
(defun tintin-dispatch-command (cmd-name input)
  (let ((cmd-data (hash-ref *tintin-commands* cmd-name)))
    (if (not cmd-data)
      ;; Should never happen (tintin-find-command validated it)
      ""
      (let ((handler (list-ref cmd-data 0))
             (arg-count (list-ref cmd-data 1))
             (syntax-help (list-ref cmd-data 2)))
        ;; Check if input has any arguments after command name
        (let ((has-args (tintin-has-arguments? input)))
          (if (not has-args)
            ;; No arguments - call handler with empty list
            (handler '())
            ;; Has arguments - try parsing with max count down to 1
            (let ((args (tintin-try-parse-arguments input arg-count)))
              (if args
                (handler args)
                (tintin-syntax-error syntax-help)))))))))

;; ============================================================================
;; AUTO-ACTIVATION
;; ============================================================================
;; Automatically activate TinTin++ when this file is loaded

(define user-input-hook tintin-user-input-hook)

;; Hook function for telnet-input-filter-hook integration
;; Signature: (lambda (text) -> string)
;; - text: Incoming telnet data (may contain ANSI codes)
;; Returns: Transformed text with highlights applied
;;
;; This hook receives data from the telnet server before it's displayed
;; in the terminal. We apply highlight patterns to colorize matching text.
(defun tintin-telnet-input-filter (text)
  (if (and *tintin-enabled* (> (hash-count *tintin-highlights*) 0))
    (tintin-apply-highlights text)
    text))

;; Install telnet-input-filter-hook
(define telnet-input-filter-hook tintin-telnet-input-filter)

;; Override telnet-input-hook to add action triggering + word collection
;; This hook is called when data arrives from the telnet server
;; It sees stripped text (no ANSI codes), better for pattern matching
(defun telnet-input-hook (text)
  ;; Step 1: Collect words for completions (preserve default behavior from bootstrap.lisp)
  (collect-words-from-text text)

  ;; Step 2: Trigger actions (if TinTin++ enabled)
  (if (and *tintin-enabled*
        (not *tintin-action-executing*)
        (> (hash-count *tintin-actions*) 0))
    (let ((lines (tintin-split-lines text)))
      (do ((i 0 (+ i 1)))
        ((>= i (list-length lines)))
        (tintin-trigger-actions-for-line (list-ref lines i))))))

;; Announce activation (terminal is ready when this file loads via -l)
(tintin-echo "TinTin++ loaded and activated\r\n")
