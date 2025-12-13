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
  "Remove leading and trailing punctuation from word.

  ## Parameters
  - `word` - String to clean

  ## Returns
  String with punctuation removed from both ends, or empty string if input invalid.

  ## Description
  Strips common punctuation characters from the beginning and end of a word,
  preserving punctuation in the middle. Used for cleaning words extracted from
  server output before adding to completion store.

  **Removed punctuation:** `. , ! ? ; : ( ) [ ] { } ' \" -`

  ## Examples
  ```lisp
  (trim-punctuation \"hello\")
  ; => \"hello\"

  (trim-punctuation \"hello!\")
  ; => \"hello\"

  (trim-punctuation \"(world)\")
  ; => \"world\"

  (trim-punctuation \"don't\")
  ; => \"don't\"  ; Preserves internal apostrophe

  (trim-punctuation \"--test--\")
  ; => \"test\"

  (trim-punctuation \"...\")
  ; => \"\"  ; All punctuation

  (trim-punctuation \"\")
  ; => \"\"
  ```

  ## Notes
  - Internal punctuation preserved (e.g., contractions, hyphenated words)
  - Multiple consecutive punctuation marks removed
  - Returns empty string for invalid input

  ## See Also
  - `clean-word` - Wrapper that validates and trims
  - `collect-words-from-text` - Uses this for word extraction"
  (if (not (and (string? word) (> (string-length word) 0)))
    ""
    (let* ((no-trailing (regex-replace "[.,!?;:()\\[\\]{}'\"\\-]+$" word ""))
            (cleaned (regex-replace "^[.,!?;:()\\[\\]{}'\"\\-]+" no-trailing "")))
      cleaned)))

;; Extract words from text as consecutive non-whitespace characters
;; Returns a list of words (strings)
;; Splits on whitespace only (spaces, tabs, newlines)
;; Removes punctuation from word boundaries (preceded or followed by whitespace)
;; Helper: Clean a single word (trim punctuation, validate)
;; Returns cleaned word string or empty string if invalid
(defun clean-word (word)
  "Clean a word by trimming punctuation.

  ## Parameters
  - `word` - String to clean

  ## Returns
  Cleaned word string (punctuation removed from ends), or empty string if invalid.

  ## Description
  Validates input and delegates to `trim-punctuation` for cleaning. This is a
  convenience wrapper used by word extraction functions.

  ## Examples
  ```lisp
  (clean-word \"hello!\")
  ; => \"hello\"

  (clean-word nil)
  ; => \"\"

  (clean-word \"\")
  ; => \"\"
  ```

  ## See Also
  - `trim-punctuation` - Performs actual punctuation removal
  - `valid-word?` - Check if cleaned word is valid"
  (if (and (string? word) (> (string-length word) 0))
    (trim-punctuation word)
    ""))

;; Helper: Check if a cleaned word is valid for storage
(defun valid-word? (cleaned)
  "Check if a cleaned word is valid for storage.

  ## Parameters
  - `cleaned` - String to validate (should be pre-cleaned)

  ## Returns
  `#t` if word is valid (non-empty string), `#f` otherwise.

  ## Description
  Validates that a word is suitable for adding to the completion store.
  Used after cleaning to filter out empty results.

  ## Examples
  ```lisp
  (valid-word? \"hello\")
  ; => #t

  (valid-word? \"\")
  ; => #f

  (valid-word? nil)
  ; => #f
  ```

  ## See Also
  - `clean-word` - Cleans words before validation
  - `word-valid-for-store?` - Checks minimum length requirement"
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
  "Add a word to the completion store with FIFO eviction.

  ## Parameters
  - `word` - Word to add (string)

  ## Returns
  - `1` if word was added
  - `0` if word was rejected (< 3 characters or invalid)

  ## Description
  Adds a word to the circular buffer completion store. If the store is full,
  evicts the oldest word (FIFO). Words are always added at the newest position,
  even if they already exist in the store, ensuring most recently seen words
  appear first in completion results.

  **Storage Rules:**
  - Minimum word length: 3 characters
  - Store capacity: `*completion-word-store-size*` (default: 1000)
  - Eviction policy: FIFO (oldest word removed when full)
  - Duplicates allowed: Same word can appear multiple times
  - Reference counting: Tracks occurrences for proper eviction

  ## Examples
  ```lisp
  (add-word-to-store \"hello\")
  ; => 1  ; Added

  (add-word-to-store \"hi\")
  ; => 0  ; Too short (< 3 chars)

  (add-word-to-store \"\")
  ; => 0  ; Invalid

  ; Adding duplicate moves it to newest position
  (add-word-to-store \"dragon\")  ; Added at position N
  (add-word-to-store \"wizard\")  ; Added at position N+1
  (add-word-to-store \"dragon\")  ; Added again at position N+2 (newest)
  ; \"dragon\" now appears twice in store
  ```

  ## Notes
  - Words < 3 characters silently rejected
  - Duplicates increase recency (newest wins in completions)
  - Reference counting prevents premature removal
  - Circular buffer wraps around when full
  - Thread-safe for single-threaded Lisp environment

  ## Configuration Variables
  - `*completion-word-store-size*` - Max words in store (default: 1000)

  ## See Also
  - `collect-words-from-text` - Extracts and adds multiple words
  - `get-completions-from-store` - Retrieves matching words
  - `word-valid-for-store?` - Validation predicate"
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
  "Extract words from text and add them to completion store.

  ## Parameters
  - `text` - Text to extract words from (typically server output)

  ## Returns
  `nil` - This function is side-effect only.

  ## Description
  Extracts all words from text by splitting on whitespace, cleans them by
  removing punctuation, and adds valid words (>= 3 characters) to the
  completion word store. This is the primary function used by `telnet-input-hook`
  to populate the tab completion system.

  **Process:**
  1. Split text on whitespace (spaces, tabs, newlines)
  2. Remove leading/trailing punctuation from each word
  3. Filter words < 3 characters
  4. Add remaining words to circular buffer store (FIFO eviction)

  ## Examples
  ```lisp
  ; Extract from server output
  (collect-words-from-text \"The dragon guards the treasure!\")
  ; Adds: \"The\", \"dragon\", \"guards\", \"the\", \"treasure\" to store

  ; Short words filtered out
  (collect-words-from-text \"Go to the inn\")
  ; Only adds: \"the\", \"inn\" (\"Go\" and \"to\" are too short)

  ; Punctuation removed
  (collect-words-from-text \"Hello, world!\")
  ; Adds: \"Hello\", \"world\"

  ; Empty input
  (collect-words-from-text \"\")
  ; No effect
  ```

  ## Notes
  - Minimum word length: 3 characters (after cleaning)
  - Punctuation stripped: `. , ! ? ; : ( ) [ ] { } ' \" -`
  - Internal punctuation preserved (e.g., \"don't\" keeps apostrophe)
  - Store bounded by `*completion-word-store-size*` (default: 1000)
  - Oldest words evicted when store is full (FIFO)
  - Side-effect only: return value ignored

  ## Configuration Variables
  - `*completion-word-store-size*` - Max words in store (default: 1000)

  ## See Also
  - `telnet-input-hook` - Calls this function automatically
  - `add-word-to-store` - Adds individual words to store
  - `get-completions-from-store` - Retrieves matching words
  - `completion-hook` - Uses word store for tab completion"
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
  "Retrieve words from store matching a prefix (case-insensitive).

  ## Parameters
  - `prefix` - Prefix to match (string)

  ## Returns
  List of matching words (strings), newest first, or empty list `()` if no matches.

  ## Description
  Searches the completion word store for all words matching the given prefix.
  Matching is case-insensitive (\"hel\" matches \"Hello\", \"HELP\", \"helmet\").
  Results are returned in newest-first order with duplicates removed.

  **Search Strategy:**
  1. **Primary**: Scan circular buffer from newest to oldest
  2. **Fallback**: If no matches found, scan hash table keys
  3. **Deduplication**: Uses temporary hash to track seen words
  4. **Limit**: Returns up to `*completion-max-results*` matches (default: 50)

  **Match Priority:**
  - Newest occurrences appear first (most recently seen in server output)
  - Earlier results prioritized over later ones
  - Circular buffer scan faster than hash scan (preferred)

  ## Examples
  ```lisp
  ; Assuming store contains: \"hello\", \"help\", \"helmet\", \"world\"
  (get-completions-from-store \"hel\")
  ; => (\"helmet\" \"help\" \"hello\")  ; Newest first

  (get-completions-from-store \"wor\")
  ; => (\"world\")

  (get-completions-from-store \"xyz\")
  ; => ()  ; No matches

  (get-completions-from-store \"H\")
  ; => (\"helmet\" \"help\" \"hello\")  ; Case-insensitive

  (get-completions-from-store \"\")
  ; => ()  ; Empty prefix
  ```

  ## Notes
  - **Case-insensitive**: \"HEL\" matches \"hello\"
  - **Prefix only**: \"lo\" does NOT match \"hello\" (not prefix)
  - **Newest first**: Most recent server output prioritized
  - **Deduplication**: Same word appears only once in results
  - **Performance**: O(n) where n = store size (typically 1000)
  - **Limited results**: Returns max `*completion-max-results*` matches

  ## Configuration Variables
  - `*completion-max-results*` - Max completions returned (default: 50)
  - `*completion-word-store-size*` - Total store capacity (default: 1000)

  ## See Also
  - `completion-hook` - Calls this function for tab completion
  - `add-word-to-store` - Adds words to store
  - `collect-words-from-text` - Populates store from text"
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
  "Provide tab completion candidates from word store.

  ## Parameters
  - `text` - Partial text to complete (matched by `*completion-pattern*` regex)

  ## Returns
  List of completion candidate strings, newest matches first. Returns empty
  list `()` if no matches found or text is invalid.

  ## Description
  Searches the word store for words matching the given prefix (case-insensitive).
  The word store is automatically populated from telnet server output by
  `telnet-input-hook`. Only words >= 3 characters are stored in the circular
  buffer (bounded by `*completion-word-store-size*`).

  **Completion Process:**
  1. User presses TAB in input area
  2. C code extracts text matching `*completion-pattern*` regex (default: `\\S+$`)
  3. Calls this hook with extracted text
  4. Hook searches word store for prefix matches
  5. Returns list of candidates for C code to display

  **Match Priority:**
  - Newest words first (most recently seen in server output)
  - Case-insensitive matching (\"hel\" matches \"Hello\", \"HELP\", \"helmet\")
  - Duplicates automatically removed
  - Limited to `*completion-max-results*` entries (default: 50)

  ## Examples
  ```lisp
  ; Assuming word store contains: hello, help, helmet, world
  (completion-hook \"hel\")
  ; => (\"helmet\" \"help\" \"hello\")  ; Newest first

  (completion-hook \"wo\")
  ; => (\"world\")

  (completion-hook \"xyz\")
  ; => ()  ; No matches

  (completion-hook \"\")
  ; => ()  ; Empty prefix
  ```

  ## Notes
  - Called by C code during TAB completion (user never calls directly)
  - Word store populated automatically by `telnet-input-hook`
  - Override this function to implement custom completion logic
  - Can return any list of strings (not limited to word store)

  ## Configuration Variables
  - `*completion-pattern*` - Regex for matching partial text (default: `\\S+$`)
  - `*completion-word-store-size*` - Max words in store (default: 1000)
  - `*completion-max-results*` - Max completions returned (default: 50)

  ## See Also
  - `telnet-input-hook` - Populates word store from server output
  - `get-completions-from-store` - Internal function that performs search
  - `add-word-to-store` - Adds words to completion store"
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
  "Process incoming telnet server output for word collection (side-effect only).

  ## Parameters
  - `text` - Plain text from server (ANSI escape codes already stripped)

  ## Returns
  `nil` - This hook is side-effect only; return value is ignored by C code.

  ## Description
  Called automatically when text arrives from the telnet server, AFTER ANSI
  escape codes have been removed. The default implementation extracts words
  from server output and adds them to the completion word store for tab
  completion support.

  **Default Behavior:**
  - Extracts words by splitting on whitespace
  - Strips punctuation from word boundaries
  - Only stores words >= 3 characters
  - Uses circular buffer with FIFO eviction (bounded by `*completion-word-store-size*`)
  - Preserves newest-first ordering for completion results

  **Word Extraction Rules:**
  - Split text on whitespace (spaces, tabs, newlines)
  - Remove leading/trailing punctuation: `.,!?;:()[]{}'\"'-`
  - Minimum word length: 3 characters
  - Case-preserved in store (matching is case-insensitive)

  ## Examples
  ```lisp
  ; Default implementation (collects words for completion)
  (defun telnet-input-hook (text)
    (collect-words-from-text text))

  ; Custom: Log all server output to file
  (defun telnet-input-hook (text)
    (progn
      (collect-words-from-text text)  ; Keep default word collection
      (let ((file (open \"server.log\" \"a\")))
        (write-line file text)
        (close file))))

  ; Custom: Check for alerts and trigger actions
  (defun telnet-input-hook (text)
    (progn
      (collect-words-from-text text)
      (if (string-contains? text \"You have new mail\")
        (terminal-echo \"\\r\\n*** ALERT: New mail received ***\\r\\n\"))))

  ; Custom: Silent (no word collection)
  (defun telnet-input-hook (text)
    ())
  ```

  ## Important Notes
  - **Side-effect only**: Return value is ignored by C code
  - **Cannot modify data flow**: Text is already processed before hook
  - **ANSI codes removed**: Text is plain (for filtering ANSI, use `telnet-input-filter-hook`)
  - **Synchronous**: Called during telnet processing (avoid long operations)
  - **Called frequently**: Invoked for every chunk of server output

  ## Use Cases
  - Word collection for tab completion (default)
  - Logging server output to files
  - Triggering alerts on specific patterns
  - Updating UI elements based on server state
  - Statistical analysis of game output

  ## Configuration Variables
  - `*completion-word-store-size*` - Max words in store (default: 1000)

  ## See Also
  - `completion-hook` - Uses word store for tab completion
  - `telnet-input-filter-hook` - For modifying ANSI output before display
  - `collect-words-from-text` - Internal word extraction function
  - `add-word-to-store` - Adds individual words to completion store"
  (collect-words-from-text text))

;; ============================================================================
;; TELNET INPUT FILTER HOOK CONFIGURATION
;; ============================================================================
;; telnet-input-filter-hook: Function called to transform telnet data before displaying in terminal
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
;;   - telnet-input-filter-hook and telnet-input-hook can both be used together
;;
;; Examples:
;;
;; Pass-through (default - no transformation):
;;   (define telnet-input-filter-hook (lambda (text) text))
;;
;; Remove all ANSI color codes (simple version):
;;   (define telnet-input-filter-hook
;;     (lambda (text)
;;       (string-replace-all text "\033[" "")))
;;
;; Filter out specific patterns:
;;   (define telnet-input-filter-hook
;;     (lambda (text)
;;       (if (string-contains? "PASSWORD" text)
;;           ""
;;           text)))
;;
;; Replace text patterns:
;;   (define telnet-input-filter-hook
;;     (lambda (text)
;;       (string-replace-all text "ERROR" "***ERROR***")))
;;
;; Add prefix to all output:
;;   (define telnet-input-filter-hook
;;     (lambda (text)
;;       (string-append "[SERVER] " text)))
;;
;; Convert to uppercase:
;;   (define telnet-input-filter-hook
;;     (lambda (text)
;;       (string-upcase text)))
;;
;; Conditional transformation:
;;   (define telnet-input-filter-hook
;;     (lambda (text)
;;       (if (string-prefix? ">" text)
;;           (string-append "PROMPT: " text)
;;           text)))
(defun telnet-input-filter-hook (text)
  "Transform telnet server output before displaying in terminal.

  ## Parameters
  - `text` - Raw telnet data from server (includes ANSI escape codes)

  ## Returns
  String to display in terminal. If non-string returned, original text is used.

  ## Description
  Called BEFORE displaying text in the terminal, allowing transformation,
  filtering, or replacement of telnet server output. The returned string is
  what actually gets displayed. This hook receives raw ANSI-encoded data and
  can modify the visual output before rendering.

  **Default Behavior:**
  - Pass-through: Returns text unchanged (identity function)

  **Data Flow:**
  1. Server sends data → telnet.c receives
  2. **This hook transforms data** (with ANSI codes intact)
  3. Data displayed in terminal (libvterm processes ANSI)
  4. `telnet-input-hook` processes stripped text (side-effects only)

  ## Examples
  ```lisp
  ; Default (no transformation)
  (defun telnet-input-filter-hook (text)
    text)

  ; Remove ANSI color codes (strip all formatting)
  (defun telnet-input-filter-hook (text)
    (strip-ansi text))

  ; Filter out sensitive patterns
  (defun telnet-input-filter-hook (text)
    (if (string-contains? text \"PASSWORD\")
      \"\"  ; Suppress output
      text))

  ; Highlight errors in red
  (defun telnet-input-filter-hook (text)
    (string-replace text \"ERROR\"
      \"\\033[1;31mERROR\\033[0m\"))  ; Bold red

  ; Add timestamp prefix to all output
  (defun telnet-input-filter-hook (text)
    (concat \"[\" (current-time-string) \"] \" text))

  ; Conditional transformation based on content
  (defun telnet-input-filter-hook (text)
    (cond
      ((string-prefix? \">\" text)
        (concat \"\\033[1;32m\" text \"\\033[0m\"))  ; Green prompts
      ((string-contains? text \"You died\")
        (concat \"\\033[1;31m\" text \"\\033[0m\"))  ; Red death messages
      (#t text)))  ; Default: unchanged
  ```

  ## Important Notes
  - **Must return string**: Non-string returns use original text
  - **ANSI codes preserved**: Text includes raw escape sequences
  - **Transforms data flow**: Changes what user sees (unlike `telnet-input-hook`)
  - **Called frequently**: Invoked for every chunk from server
  - **Synchronous**: Avoid long operations (blocks rendering)
  - **Can combine with telnet-input-hook**: Both hooks work together

  ## Use Cases
  - Color highlighting and formatting
  - Filtering sensitive information
  - Pattern-based text replacement
  - Adding visual markers or prefixes
  - ANSI color removal (accessibility)
  - Custom text styling

  ## See Also
  - `telnet-input-hook` - Side-effect hook (word collection, logging)
  - `strip-ansi` - Remove ANSI escape codes from text
  - `terminal-echo` - Echo text directly to terminal
  - TinTin++ highlight system - Pattern-based coloring"
  text)

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
(defun user-input-hook (text cursor-pos)
  "Transform user input before sending to telnet server.

  ## Parameters
  - `text` - Text user typed in input area
  - `cursor-pos` - Cursor position in input area (integer, 0-based)

  ## Returns
  - String: C code echoes and sends this text (with CRLF appended)
  - `nil` or non-string: Hook handled echo/send itself (proper way)
  - Empty string `\"\"`: Same as `nil` (hook handled everything)

  ## Description
  Called BEFORE sending text to the telnet server, allowing transformation,
  filtering, or replacement of user input. You can either:
  1. Return transformed string for C code to echo/send
  2. Return `nil` after handling echo/send yourself (via `terminal-echo`/`telnet-send`)

  **Default Behavior:**
  - Pass-through: Returns text unchanged

  **Hook Contract:**
  - If you call `terminal-echo` and `telnet-send` yourself, return `nil`
  - If you return a string, C code handles echo/send
  - Called for every Enter keypress (even empty input)

  ## Examples
  ```lisp
  ; Default (no transformation)
  (defun user-input-hook (text cursor-pos)
    text)

  ; Convert to uppercase
  (defun user-input-hook (text cursor-pos)
    (string-upcase text))

  ; Command aliases (simple)
  (defun user-input-hook (text cursor-pos)
    (cond
      ((string=? text \"n\") \"north\")
      ((string=? text \"s\") \"south\")
      ((string=? text \"e\") \"east\")
      ((string=? text \"w\") \"west\")
      (#t text)))

  ; Cursor position aware (only transform if cursor at end)
  (defun user-input-hook (text cursor-pos)
    (if (= cursor-pos (string-length text))
      (string-upcase text)  ; Cursor at end - transform
      text))  ; Cursor in middle - don't transform

  ; Add prefix to all commands
  (defun user-input-hook (text cursor-pos)
    (concat \">\" text))

  ; Suppress specific commands (handle manually)
  (defun user-input-hook (text cursor-pos)
    (if (string-prefix? \"password \" text)
      (progn
        ; Don't echo password, just send it
        (telnet-send (concat text \"\\r\\n\"))
        ())  ; Return nil - we handled it
      text))  ; Normal commands - let C code handle

  ; TinTin++ integration (command expansion)
  (defun user-input-hook (text cursor-pos)
    (progn
      ; Echo original input
      (terminal-echo (concat text \"\\r\\n\"))
      ; Process through TinTin++ (expands aliases, speedwalk)
      (let ((processed (tintin-process-input text)))
        (if (string? processed)
          (progn
            ; Echo expanded command if different
            (if (not (string=? text processed))
              (terminal-echo (concat processed \"\\r\\n\")))
            ; Send to server
            (telnet-send (concat processed \"\\r\\n\"))
            ())  ; Return nil - we handled it
          ()))))  ; TinTin++ handled it
  ```

  ## Important Notes
  - **Proper way to handle everything**: Return `nil` (not empty string)
  - **If you call terminal-echo/telnet-send**: You MUST return `nil`
  - **Called for every Enter press**: Even on empty input
  - **Cursor position useful**: Check if user is at end or editing middle
  - **Synchronous**: Avoid long operations (blocks input)

  ## Use Cases
  - Command aliases and shortcuts
  - Input transformation (uppercase, prefix, etc.)
  - TinTin++ scripting integration
  - Password handling (no echo)
  - Input validation and filtering
  - Command history manipulation
  - Cursor-aware transformations

  ## See Also
  - `terminal-echo` - Echo text to terminal (for manual handling)
  - `telnet-send` - Send text to server (for manual handling)
  - `tintin-process-input` - TinTin++ command processing
  - `telnet-input-filter-hook` - Transform server output
  - `telnet-input-hook` - Process server output (side-effects)"
  text)

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
;; UTILITY FUNCTIONS
;; ============================================================================

;; Helper function to build indent string
(defun build-indent (level)
  (let ((result ""))
    (do ((i 0 (+ i 1)))
      ((>= i level) result)
      (set! result (concat result "  ")))))

;; Helper function to convert objects to strings
(defun obj-to-string (obj)
  "Convert an object to its string representation."
  (cond
    ((symbol? obj) (symbol->string obj))
    ((string? obj) obj)
    ((eq? obj #t) "#t")
    ((eq? obj #f) "#f")
    (#t (format nil "~A" obj))))

;; Helper function to check if a value is a nested alist
(defun is-nested-alist? (value)
  "Check if a value is a nested association list."
  (and (list? value)
    (not (null? value))
    (pair? (car value))
    (not (list? (car (car value))))))

;; Helper function to format a string value with quotes
(defun format-string-value (str)
  "Format a string value by wrapping it in quotes."
  (concat "\"" str "\"\n"))

;; Helper function to format a regular (non-string) value
(defun format-regular-value (value)
  "Format a non-string value."
  (concat (obj-to-string value) "\n"))

;; ANSI true color escape sequence helpers
(defun ansi-fg-rgb (r g b)
  "Generate ANSI true color foreground escape sequence."
  (concat "\033[38;2;" (number->string r) ";" (number->string g) ";" (number->string b) "m"))

(defun ansi-bold-fg-rgb (r g b)
  "Generate ANSI bold + true color foreground escape sequence."
  (concat "\033[1;38;2;" (number->string r) ";" (number->string g) ";" (number->string b) "m"))

(define *ansi-reset* "\033[0m")

(defun pretty-print-alist (alist)
  "Pretty-print an association list to the terminal with ANSI true color formatting.

  ## Parameters
  - `alist` - Association list to print (list of (key . value) pairs)

  ## Returns
  `nil` (output is echoed directly to terminal)

  ## Description
  Formats and displays an association list (alist) directly to the terminal
  with ANSI true color formatting. Each key-value pair is printed on its own
  line with proper indentation and color coding.

  **Color scheme:**
  - Keys: Bold cyan
  - Colon separator: Gray
  - String values: Green
  - Numeric/boolean values: Yellow
  - List values: Magenta
  - Errors: Bold red

  **Format:**
  - Key-value pairs printed as: `key: value`
  - Nested alists are indented
  - Lists are printed in readable format
  - Strings are printed with quotes
  - Booleans printed as `#t` or `#f`

  ## Examples
  ```lisp
  (pretty-print-alist '((name . \"John\") (age . 30) (active . #t)))
  ; Outputs to terminal with colors:
  ; name: \"John\"
  ; age: 30
  ; active: #t

  (pretty-print-alist '((user . ((name . \"Alice\") (id . 123)))))
  ; Outputs nested structure with indentation
  ```

  ## Notes
  - Outputs directly to terminal (returns nil)
  - Uses ANSI true color (24-bit) escape sequences
  - Handles nested alists with indentation
  - Works with any alist structure

  ## See Also
  - `terminal-echo` - Echo text to terminal
  - `hash-keys` - Get keys from hash table"
  ;; Color definitions using true color RGB
  (let* ((key-color (ansi-bold-fg-rgb 100 200 255))      ; Bold cyan for keys
          (colon-color (ansi-fg-rgb 150 150 150))          ; Gray for separator
          (string-color (ansi-fg-rgb 150 255 150))         ; Green for strings
          (value-color (ansi-fg-rgb 255 255 150))          ; Yellow for numbers/bools
          (list-color (ansi-fg-rgb 255 150 255))           ; Magenta for lists
          (error-color (ansi-bold-fg-rgb 255 100 100))     ; Bold red for errors
          (reset *ansi-reset*)
          (format-nested-alist
            (lambda (nested-alist indent indent-str)
              (terminal-echo "\r\n")
              (do ((remaining nested-alist (cdr remaining)))
		((null? remaining))
		(let ((pair (car remaining)))
                  (if (pair? pair)
                    (print-pair (car pair) (cdr pair) (+ indent 1))
                    (progn
                      (terminal-echo indent-str)
                      (terminal-echo "  ")
                      (terminal-echo (concat value-color (obj-to-string pair) reset))
                      (terminal-echo "\r\n")))))))
          (print-pair
            (lambda (key value indent)
              (let ((indent-str (build-indent indent)))
		(terminal-echo indent-str)
		(terminal-echo (concat key-color (obj-to-string key) reset))
		(terminal-echo (concat colon-color ": " reset))
		(cond
                  ((list? value)
                    (if (is-nested-alist? value)
                      (format-nested-alist value indent indent-str)
                      (terminal-echo (concat list-color (obj-to-string value) reset "\r\n"))))
                  ((string? value)
                    (terminal-echo (concat string-color "\"" value "\"" reset "\r\n")))
                  (#t
                    (terminal-echo (concat value-color (obj-to-string value) reset "\r\n"))))))))
    (if (not (list? alist))
      (progn
        (terminal-echo (concat error-color "Error: pretty-print-alist expects a list" reset "\r\n"))
        nil)
      (if (null? alist)
        (progn
          (terminal-echo (concat value-color "(empty alist)" reset "\r\n"))
          nil)
        (progn
          (do ((remaining alist (cdr remaining)))
            ((null? remaining))
            (let ((pair (car remaining)))
              (if (pair? pair)
                (print-pair (car pair) (cdr pair) 0)
                (progn
                  (terminal-echo (concat error-color "Invalid pair: " (obj-to-string pair) reset "\r\n"))))))
          nil)))))

;; ============================================================================
;; COLOR CONFIGURATION
;; ============================================================================
;; All colors are specified as RGB lists (r g b) where each component is 0-255

;; Terminal default colors (used when no ANSI color codes are present)
(define *terminal-fg-color* '(255 255 255))  ; White text
(define *terminal-bg-color* '(0 0 0))        ; Black background

;; Text selection colors
(define *selection-fg-color* '(0 0 0))       ; Black text on selection
(define *selection-bg-color* '(255 140 0))   ; Orange selection background

;; Cursor/caret color
(define *cursor-color* '(200 200 200))       ; Light gray vertical line

;; Separator line between terminal and input area
(define *input-separator-color* '(100 100 100))  ; Gray separator line

;; ============================================================================
;; TERMINAL LINE HEIGHT CONFIGURATION
;; ============================================================================
;; *terminal-line-height*: Multiplier for terminal line spacing
;;
;; This controls the vertical spacing between terminal lines. The value is a
;; multiplier applied to the base cell height calculated from the font metrics.
;;
;; Values:
;;   1.2  - Default spacing (20% more than font metrics, better readability)
;;   1.0  - Normal spacing (matches font metrics)
;;   1.5  - 50% more spacing between lines
;;   2.0  - Double spacing
;;   0.5  - Half spacing (tighter, minimum recommended)
;;
;; Examples:
;;   1.2  - Default, comfortable spacing for readability
;;   1.0  - Compact terminal appearance (matches font metrics)
;;   1.25 - Slightly more spacing for readability
;;   1.5  - Comfortable spacing for long reading sessions
;;   2.0  - Very loose spacing, maximum recommended
;;
;; Note: Values are clamped to 0.5-3.0 range for safety. The multiplier affects
;;       vertical spacing between rows, window height calculations, and mouse
;;       coordinate conversion, but does not stretch glyphs (they maintain their
;;       original size from the font).
(define *terminal-line-height* 1.2)
