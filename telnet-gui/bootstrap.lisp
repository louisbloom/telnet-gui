;; Bootstrap Lisp file for telnet-gui
;; This file is always loaded on startup before any user-provided Lisp file
;; All variables defined here can be overridden in your custom Lisp configuration file

;; ============================================================================
;; COMPLETION PATTERN CONFIGURATION
;; ============================================================================
;; *completion-pattern*: PCRE2 regex pattern that matches the text to complete
;;
;; The pattern is applied to the current line, and the matched text is passed
;; to the completion-hook function. The pattern should match the word or
;; command prefix that the user wants to complete.
;;
;; Examples:
;;   "\\S+$"          - Match any non-whitespace characters at end of line
;;                      (default: completes any word at cursor position)
;;   "[a-zA-Z_][a-zA-Z0-9_]*$" - Match identifiers (letters, digits, underscore)
;;                      (good for command completion)
;;   "[^\\s]+$"       - Match any non-space characters at end of line
;;                      (similar to default but excludes tabs)
;;   "\\w+$"          - Match word characters (letters, digits, underscore)
;;                      (simpler than identifier pattern)
;;
;; Pattern matching details:
;;   - The pattern is anchored to the end of the line ($)
;;   - Only the matched portion is passed to completion-hook
;;   - Use capturing groups if you need to extract specific parts
;;   - PCRE2 syntax: \\S = non-whitespace, \\w = word char, \\s = whitespace
(define *completion-pattern* "\\S+$")

;; ============================================================================
;; WORD STORE CONFIGURATION
;; ============================================================================
;; *completion-word-store-size*: Maximum number of words to store for completions
;;
;; This controls the maximum size of the word store used for completion
;; suggestions. When the store reaches this limit, older words are removed
;; to make room for new ones (FIFO - First In, First Out).
;;
;; Examples:
;;   500  - Small store, good for limited memory
;;   1000 - Default, balanced for most use cases
;;   5000 - Large store, good for extensive word collection
;;   10000 - Very large store, may use more memory
(define *completion-word-store-size* 10000)

;; Max number of completion candidates to return per prefix
(define *completion-max-results* 64)

;; Initialize word store (hash table for fast lookups)
(define *completion-word-store* (make-hash-table))

;; Initialize word order (vector for FIFO bounded storage)
;; Initialize with nil so vector has correct size (not just capacity)
(define *completion-word-order* (make-vector *completion-word-store-size* nil))

;; Current position in the order vector (circular buffer index)
(define *completion-word-order-index* 0)

;; ============================================================================
;; TELNET I/O LOGGING CONFIGURATION
;; ============================================================================

;; Enable/disable telnet I/O logging (both send and receive)
(define *enable-telnet-logging* #t)

;; Directory for telnet log files (supports ~/ expansion)
;; Log filename format: telnet-<host>-<port>-<timestamp>.log
(define *telnet-log-directory* "~/telnet-logs")

;; ============================================================================
;; WORD STORE HELPER FUNCTIONS
;; ============================================================================

;; Helper function to trim punctuation from word boundaries
;; Uses regex-replace to remove leading and trailing punctuation
(defun trim-punctuation (word)
  (if (not (and (string? word) (> (string-length word) 0)))
    ""
    (let* ((no-trailing (regex-replace "[.,!?;:()\\[\\]{}'\"\\-]+$" "" word))
            (cleaned (regex-replace "^[.,!?;:()\\[\\]{}'\"\\-]+" "" no-trailing)))
      cleaned)))

;; Extract words from text as consecutive non-whitespace characters
;; Returns a list of words (strings)
;; Splits on whitespace only (spaces, tabs, newlines)
;; Removes punctuation from word boundaries (preceded or followed by whitespace)
;; Helper: Clean a single word (trim punctuation, validate)
;; Returns cleaned word string or empty string if invalid
(defun clean-word (word)
  (if (and (string? word) (> (string-length word) 0))
    (trim-punctuation word)
    ""))

;; Helper: Check if a cleaned word is valid for storage
(defun valid-word? (cleaned)
  (and (string? cleaned)
    (> (string-length cleaned) 0)))

;; Helper: Process word list and filter valid words
;; Preserves left-to-right order so leftmost words are added first (oldest)
;; and rightmost words are added last (newest)
(defun filter-valid-words (words)
  (let ((filtered '()))
    (do ((remaining words (cdr remaining)))
      ((null? remaining) (reverse filtered))
      (let ((cleaned (clean-word (car remaining))))
        ;; Early continue if word is invalid
        (if (not (valid-word? cleaned))
          ()
          (set! filtered (cons cleaned filtered)))))))

(defun extract-words (text)
  (if (not (string? text))
    '()
    (let ((words (regex-split "\\s+" text)))
      (if (null? words)
        '()
        (filter-valid-words words)))))

;; Helper: Normalize circular buffer index to valid range
(defun normalize-order-index (idx vec-size)
  (if (>= idx vec-size)
    0
    idx))

;; Helper: Advance circular buffer index
(defun advance-order-index (vec-size)
  (set! *completion-word-order-index* (+ *completion-word-order-index* 1))
  (if (>= *completion-word-order-index* vec-size)
    (set! *completion-word-order-index* 0)))

;; Helper: Evict old word from store slot and insert new word
;; Uses reference counting to track word occurrences in circular buffer
;; Accepts explicit vector and store to avoid capturing stale globals
(defun insert-word-into-slot! (vec store slot old-word new-word)
  ;; Decrement count for old word (if different from new word)
  (if (string? old-word)
    (if (not (and (string? new-word) (string=? old-word new-word)))
      (let ((count (hash-ref store old-word)))
        (if (and (not (null? count)) (> count 1))
          (hash-set! store old-word (- count 1))
          (hash-remove! store old-word)))))
  ;; Set new word in slot
  (vector-set! vec slot new-word)
  ;; Increment count for new word
  (let ((count (hash-ref store new-word)))
    (if (null? count)
      (hash-set! store new-word 1)
      (hash-set! store new-word (+ count 1)))))

;; Helper: Check if word is valid for storage (length >= 3)
(defun word-valid-for-store? (word)
  (and (string? word)
    (>= (string-length word) 3)))

;; Add a word to the store (bounded by *completion-word-store-size*)
;; If store is full, removes oldest word (FIFO)
;; Words shorter than 3 characters are not stored
;; Words are always added at newest position, even if they already exist
;; This ensures most recently seen words appear first in completions
(defun add-word-to-store (word)
  ;; Early return for invalid words
  (if (not (word-valid-for-store? word))
    0
    (let* ((vec *completion-word-order*)
            (vec-size (vector-length vec)))
      ;; Normalize index BEFORE any vector-ref to avoid OOB
      (if (>= *completion-word-order-index* vec-size)
        (set! *completion-word-order-index* 0))
      ;; Always add word at newest position (even if duplicate)
      (let* ((slot (normalize-order-index *completion-word-order-index* vec-size))
              (old (vector-ref vec slot)))
        (insert-word-into-slot! vec *completion-word-store* slot old word)
        (advance-order-index vec-size)
        1))))

;; Collect all words from text and add them to the store
(defun collect-words-from-text (text)
  (let ((words (extract-words text)))
    (if (null? words)
      ()
      (do ((remaining words (cdr remaining)))
        ((null? remaining))
        (add-word-to-store (car remaining))))))

;; Helper: Compute circular buffer index from position
(defun compute-circular-index (pos vec-size)
  (if (< pos 0)
    (+ pos vec-size)
    pos))

;; Helper: Check if word matches prefix (case-insensitive) and not seen
(defun word-matches-prefix? (word prefix-lower seen)
  (and (string? word)
    (string-prefix? prefix-lower (string-downcase word))
    (null? (hash-ref seen word))))

;; Helper: Scan circular buffer for matching words (newest to oldest)
;; Returns (cons acc count) where acc is newest-first list of matches
(defun scan-circular-buffer (vec vec-size start prefix-lower seen max-results)
  (let ((acc '())
         (count 0))
    (do ((i 0 (+ i 1)))
      ;; Reverse acc since we cons in reverse order (newest scanned = first in list)
      ((or (>= i vec-size) (>= count max-results)) (cons (reverse acc) count))
      (let* ((pos (- start 1 i))
              (idx (compute-circular-index pos vec-size))
              (k (vector-ref vec idx)))
        ;; Skip if word is nil or doesn't match
        (if (not (string? k))
          ()
          (if (not (word-matches-prefix? k prefix-lower seen))
            ()
            (progn
              (hash-set! seen k 1)
              (set! acc (cons k acc))
              (set! count (+ count 1)))))))))

;; Helper: Fallback scan of hash keys for remaining matches
(defun scan-hash-keys (store prefix-lower seen acc count max-results)
  (let ((keys (hash-keys store)))
    ;; Early return if no keys
    (if (null? keys)
      acc
      (do ((remaining keys (cdr remaining)))
        ((or (null? remaining) (>= count max-results)) acc)
        (let ((k (car remaining)))
          ;; Skip if word doesn't match
          (if (not (word-matches-prefix? k prefix-lower seen))
            ()
            (progn
              (hash-set! seen k 1)
              (set! acc (cons k acc))
              (set! count (+ count 1)))))))))

;; Get all words from store that match a prefix (case-insensitive)
;; Returns a list of matching words (strings)
(defun get-completions-from-store (prefix)
  ;; Early return for invalid prefix
  (if (not (and (string? prefix) (> (string-length prefix) 0)))
    '()
    (let* ((p (string-downcase prefix))
            (vec *completion-word-order*)
            (vec-size (vector-length vec))
            (start *completion-word-order-index*)
            (seen (make-hash-table))
            (result (scan-circular-buffer vec vec-size start p seen *completion-max-results*))
            (acc (car result))
            (count (cdr result)))
      ;; Return early if we found matches in circular buffer
      (if (> count 0)
        acc
        ;; Otherwise fallback to hash scan
        (scan-hash-keys *completion-word-store* p seen acc count *completion-max-results*)))))

;; ============================================================================
;; COMPLETION HOOK CONFIGURATION
;; ============================================================================
;; completion-hook: Function called when user requests tab completion
;;
;; Signature: (lambda (text) -> list)
;;   - text: The matched text from *completion-pattern*
;;   - Returns: List of completion candidates (strings)
;;
;; The completion system will:
;;   1. Match *completion-pattern* against the current line
;;   2. Call completion-hook with the matched text
;;   3. Display the returned completions in a popup menu
;;   4. User can select a completion with arrow keys and Enter
;;
;; This implementation uses a word store that automatically collects words
;; from telnet server output. Words are stored in a circular buffer with
;; FIFO eviction. Only words >= 3 characters are stored. Completion matching
;; is case-insensitive and returns results in newest-first order.
;;
;; Examples:
;;
;; Simple command completion:
;;   (define completion-hook
;;     (lambda (text)
;;       (cond
;;         ((string-prefix? "hel" text) '("help" "hello" "helix"))
;;         ((string-prefix? "lis" text) '("list" "lisp" "listen"))
;;         (#t '()))))
;;
;; File path completion (simplified):
;;   (define completion-hook
;;     (lambda (text)
;;       (if (string-prefix? "/" text)
;;           '("/home/user/" "/usr/bin/" "/etc/")
;;           '("file1.txt" "file2.lisp" "file3.c"))))
;;
;; Debug completion (prints what it receives):
;;   (define completion-hook
;;     (lambda (text)
;;       (progn
;;         (princ "Completing: ")
;;         (princ text)
;;         (princ "\n")
;;         '("option1" "option2" "option3"))))
;;
;; Word store completion (uses collected words - DEFAULT):
;;   (defun completion-hook (text)
;;     (get-completions-from-store text))
;;
;; No completions:
;;   (defun completion-hook (text) ())
(defun completion-hook (text)
  (if (and (string? text) (> (string-length text) 0))
    (get-completions-from-store text)
    '()))

;; ============================================================================
;; TELNET INPUT HOOK CONFIGURATION
;; ============================================================================
;; telnet-input-hook: Function called when text is received from telnet server
;;
;; Signature: (lambda (text) -> any)
;;   - text: Plain text from server (ANSI codes already stripped)
;;   - Returns: Ignored (hook is side-effect only)
;;
;; This hook receives all text output from the telnet server AFTER ANSI
;; escape codes have been removed. It's called for every chunk of text
;; received, allowing you to monitor, log, or process server output.
;;
;; DEFAULT BEHAVIOR: The default implementation automatically collects words
;; from server output for tab completion. Words >= 3 characters are stored
;; in a bounded circular buffer (see *completion-word-store-size*).
;;
;; Important notes:
;;   - This hook is SIDE-EFFECT ONLY - it cannot modify the data flow
;;   - Text is already stripped of ANSI codes before reaching this hook
;;   - Hook is called synchronously during telnet processing
;;   - For filtering/modifying input, use telnet-input-filter
;;
;; Examples:
;;
;; Simple logging to console:
;;   (define telnet-input-hook
;;     (lambda (text)
;;       (progn
;;         (princ "[TELNET] ")
;;         (princ text))))
;;
;; Log to file (requires file I/O functions):
;;   (define telnet-input-hook
;;     (lambda (text)
;;       (write-to-file "telnet.log" text)))
;;
;; Multi-function processing:
;;   (define telnet-input-hook
;;     (lambda (text)
;;       (progn
;;         (log-to-file text)        ; Log everything
;;         (check-for-alerts text)   ; Check for important messages
;;         (update-status-bar text)  ; Update UI status
;;         ())))                     ; Return nil
;;
;; Conditional processing:
;;   (define telnet-input-hook
;;     (lambda (text)
;;       (if (string-contains? "ERROR" text)
;;           (progn
;;             (princ "*** ERROR DETECTED ***\n")
;;             (princ text)
;;             (princ "\n"))
;;           ())))
;;
;; No processing (silent):
;;   (define telnet-input-hook (lambda (text) ()))
;;
;; Word collection for completions (DEFAULT - collects words for tab completion):
;;   (defun telnet-input-hook (text)
;;     (collect-words-from-text text))
(defun telnet-input-hook (text)
  (collect-words-from-text text))

;; ============================================================================
;; TELNET INPUT FILTER CONFIGURATION
;; ============================================================================
;; telnet-input-filter: Function called to transform telnet data before displaying in terminal
;;
;; Signature: (lambda (text) -> string)
;;   - text: Raw telnet data from server (with ANSI codes preserved)
;;   - Returns: Transformed text to display in terminal (string), or original if not string
;;
;; This hook is called BEFORE displaying text in the terminal, allowing you to
;; transform, filter, or replace the telnet server's output. The returned string
;; is what actually gets displayed in the terminal.
;;
;; Important notes:
;;   - Hook MUST return a string (or original text is used)
;;   - Receives raw telnet data including ANSI escape codes
;;   - Hook is called for every chunk of data received from telnet server
;;   - Can modify, filter, or replace text before it enters the terminal
;;   - Unlike telnet-input-hook, this hook TRANSFORMS the data flow
;;   - telnet-input-filter and telnet-input-hook can both be used together
;;
;; Examples:
;;
;; Pass-through (default - no transformation):
;;   (define telnet-input-filter (lambda (text) text))
;;
;; Remove all ANSI color codes (simple version):
;;   (define telnet-input-filter
;;     (lambda (text)
;;       (string-replace-all text "\033[" "")))
;;
;; Filter out specific patterns:
;;   (define telnet-input-filter
;;     (lambda (text)
;;       (if (string-contains? "PASSWORD" text)
;;           ""
;;           text)))
;;
;; Replace text patterns:
;;   (define telnet-input-filter
;;     (lambda (text)
;;       (string-replace-all text "ERROR" "***ERROR***")))
;;
;; Add prefix to all output:
;;   (define telnet-input-filter
;;     (lambda (text)
;;       (string-append "[SERVER] " text)))
;;
;; Convert to uppercase:
;;   (define telnet-input-filter
;;     (lambda (text)
;;       (string-upcase text)))
;;
;; Conditional transformation:
;;   (define telnet-input-filter
;;     (lambda (text)
;;       (if (string-prefix? ">" text)
;;           (string-append "PROMPT: " text)
;;           text)))
(define telnet-input-filter (lambda (text) text))

;; ============================================================================
;; USER INPUT HOOK CONFIGURATION
;; ============================================================================
;; user-input-hook: Function called when user sends text to telnet server
;;
;; Signature: (lambda (text cursor-pos) -> string|nil)
;;   - text: The text user typed in input area
;;   - cursor-pos: Cursor position in input area (integer)
;;   - Returns: Transformed text to send (string), or nil if hook handled everything
;;
;; Hook Contract:
;;   - Return string: C code echoes and sends the returned text (with CRLF appended)
;;   - Return nil (or non-string): Hook has handled echo/send itself (proper way)
;;   - Return "" (empty string): Same as nil, hook handled everything
;;
;; This hook is called BEFORE sending text to the telnet server, allowing you
;; to transform, filter, or replace the user's input. You can either:
;;   1. Return transformed string for C code to echo/send
;;   2. Return nil after handling echo/send yourself (via terminal-echo/telnet-send)
;;
;; Important notes:
;;   - Proper way to handle everything: return nil (not empty string)
;;   - If you call terminal-echo and telnet-send yourself, return nil
;;   - Hook is called for every Enter keypress (even empty input)
;;
;; Examples:
;;
;; Pass-through (default - no transformation):
;;   (define user-input-hook (lambda (text cursor-pos) text))
;;
;; Convert to uppercase:
;;   (define user-input-hook
;;     (lambda (text cursor-pos)
;;       (string-upcase text)))
;;
;; Add prefix to all commands:
;;   (define user-input-hook
;;     (lambda (text cursor-pos)
;;       (string-append ">" text)))
;;
;; Command aliases:
;;   (define user-input-hook
;;     (lambda (text cursor-pos)
;;       (cond
;;         ((string=? text "n") "north")
;;         ((string=? text "s") "south")
;;         ((string=? text "e") "east")
;;         ((string=? text "w") "west")
;;         (#t text))))
;;
;; Cursor position aware (only transform if cursor at end):
;;   (define user-input-hook
;;     (lambda (text cursor-pos)
;;       (if (= cursor-pos (string-length text))
;;           (string-upcase text)
;;           text)))
;;
;; Suppress echo/send for specific commands (return nil):
;;   (define user-input-hook
;;     (lambda (text cursor-pos)
;;       (if (string-prefix? "password " text)
;;           ()  ; nil - hook handles it (or suppresses it)
;;           text)))
(define user-input-hook (lambda (text cursor-pos) text))

;; ============================================================================
;; MOUSE WHEEL SCROLLING CONFIGURATION
;; ============================================================================
;; *scroll-lines-per-click*: Number of terminal lines to scroll per mouse wheel click
;;
;; This controls how much the terminal scrolls when you use the mouse wheel.
;; Higher values scroll more per click, lower values provide finer control.
;;
;; Examples:
;;   1  - Very fine control, one line at a time (good for precise reading)
;;   3  - Default, balanced scrolling (good for most users)
;;   5  - Faster scrolling, good for long documents
;;   10 - Very fast scrolling, good for quickly jumping through output
;;
;; Note: This only affects discrete wheel clicks. For smooth scrolling
;;       (trackpads), see *smooth-scrolling-enabled* below.
(define *scroll-lines-per-click* 3)

;; ============================================================================
;; SMOOTH SCROLLING CONFIGURATION
;; ============================================================================
;; *smooth-scrolling-enabled*: Enable smooth scrolling for high-resolution trackpads
;;
;; When enabled (#t), the terminal uses smooth pixel-based scrolling that
;; responds to trackpad gestures with sub-line precision. This provides a
;; natural, fluid scrolling experience similar to modern text editors.
;;
;; When disabled (#f), the terminal uses discrete line-based scrolling that
;; jumps by whole lines. This is more traditional and may be preferred for
;; some use cases.
;;
;; Examples:
;;   #t - Enable smooth scrolling (default, recommended for trackpads)
;;   #f - Disable smooth scrolling (use discrete line jumps)
;;
;; Use cases:
;;   - Enable (#t) if you have a trackpad or high-resolution mouse
;;   - Enable (#t) for a modern, fluid user experience
;;   - Disable (#f) if you prefer traditional terminal scrolling
;;   - Disable (#f) if smooth scrolling feels laggy on your system
(define *smooth-scrolling-enabled* #t)

;; ============================================================================
;; SCROLLBACK BUFFER CONFIGURATION
;; ============================================================================
;; *max-scrollback-lines*: Maximum number of scrollback lines to keep in memory
;;
;; This controls how many lines of terminal output history are retained for
;; scrolling back through past output. When the buffer is full, oldest lines
;; are automatically evicted to make room for new output (FIFO circular buffer).
;;
;; Values:
;;   0     - Unbounded (default, limited to 10000 lines for safety)
;;   1000  - Small buffer, uses less memory
;;   5000  - Medium buffer, good balance
;;   10000 - Large buffer, maximum scrollback
;;
;; Note: Set to 0 for unbounded scrollback (limited by SCROLLBACK_MAX_LINES = 10000
;;       in code for safety). Setting a smaller value can reduce memory usage for
;;       very long sessions with lots of output.
;;
;; Memory usage: Each line uses ~100-500 bytes depending on content
;;   1000 lines  ≈ 100KB - 500KB
;;   5000 lines  ≈ 500KB - 2.5MB
;;   10000 lines ≈ 1MB - 5MB
(define *max-scrollback-lines* 0)

;; ============================================================================
;; AUTO-SCROLL TO BOTTOM CONFIGURATION
;; ============================================================================
;; *scroll-to-bottom-on-user-input*: Auto-scroll to bottom when user sends input
;;
;; When enabled (#t), the terminal automatically scrolls to the bottom (current
;; output) whenever you send a command or text to the server. This ensures you
;; always see the response to your commands.
;;
;; Values:
;;   #t - Auto-scroll on user input (default, recommended)
;;   #f - Don't auto-scroll, stay at current scroll position
;;
;; Use cases:
;;   - Enable (#t) for normal interactive use (default)
;;   - Disable (#f) if you want to read scrollback while typing commands
(define *scroll-to-bottom-on-user-input* #t)

;; *scroll-to-bottom-on-telnet-input*: Auto-scroll to bottom when server sends data
;;
;; When enabled (#t), the terminal automatically scrolls to the bottom whenever
;; new data arrives from the server. This keeps you following the live output.
;; When disabled (#f), you can scroll up to read history while new data continues
;; to arrive in the background.
;;
;; Values:
;;   #t - Auto-scroll on server output (follows live output)
;;   #f - Don't auto-scroll, stay at current scroll position (default)
;;
;; Use cases:
;;   - Enable (#t) to always follow live output
;;   - Disable (#f) to read scrollback while server continues to send data
;;   - Disable (#f) for high-traffic servers where you want to review past output
(define *scroll-to-bottom-on-telnet-input* #f)

;; ============================================================================
;; INPUT HISTORY CONFIGURATION
;; ============================================================================
;; *input-history-size*: Maximum number of input history entries to keep
;;
;; This controls how many previous input commands are stored in history
;; that you can navigate through with Up/Down arrow keys. When the buffer
;; is full, oldest entries are automatically removed to make room for new
;; commands (FIFO circular buffer).
;;
;; Values:
;;   10   - Minimal history, uses very little memory
;;   50   - Small history, good for simple sessions
;;   100  - Medium history, default and recommended
;;   200  - Large history, good for complex sessions
;;   500  - Very large history, maximum recommended
;;
;; Memory usage: Each entry uses INPUT_AREA_MAX_LENGTH bytes
;;   50 entries  ≈ 100KB (assuming 2KB per entry)
;;   100 entries ≈ 200KB
;;   200 entries ≈ 400KB
;;   500 entries ≈ 1MB
;;
;; Note: Very large history sizes may impact performance when navigating
;;       through history, especially on slower systems.
(define *input-history-size* 100)

;; ============================================================================
;; COLOR CONFIGURATION
;; ============================================================================
;; All colors are specified as RGB lists (r g b) where each component is 0-255

;; Terminal default colors (used when no ANSI color codes are present)
(define *terminal-fg-color* '(255 255 255))  ; White text
(define *terminal-bg-color* '(0 0 0))        ; Black background

;; Input area colors
(define *input-area-fg-color* '(255 255 0))  ; Yellow text
(define *input-area-bg-color* '(25 40 60))   ; Dark blue background

;; Text selection colors
(define *selection-fg-color* '(0 0 0))       ; Black text on selection
(define *selection-bg-color* '(255 140 0))   ; Orange selection background

;; Cursor/caret color
(define *cursor-color* '(200 200 200))       ; Light gray vertical line

;; Separator line between terminal and input area
(define *input-separator-color* '(100 100 100))  ; Gray separator line

;; Window chrome colors (titlebar, resize bars)
(define *resize-bar-color* '(30 70 50))   ; Even darker greenish color for titlebar and resize bars
