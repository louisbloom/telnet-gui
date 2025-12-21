;; Bootstrap Lisp file for telnet-gui
;; This file is always loaded on startup before any user-provided Lisp file
;; All variables defined here can be overridden in your custom Lisp configuration file

;; ============================================================================
;; COMPLETION PATTERN CONFIGURATION
;; ============================================================================

(defvar *completion-pattern* "\\S+$"
  "PCRE2 regex pattern that matches the text to complete.

The pattern is applied to the current line, and the matched text is passed
to the completion-hook function. The pattern should match the word or
command prefix that the user wants to complete.

## Examples
- `\\\\S+$` - Match any non-whitespace characters at end of line
  (default: completes any word at cursor position)
- `[a-zA-Z_][a-zA-Z0-9_]*$` - Match identifiers (letters, digits, underscore)
  (good for command completion)
- `[^\\\\s]+$` - Match any non-space characters at end of line
  (similar to default but excludes tabs)
- `\\\\w+$` - Match word characters (letters, digits, underscore)
  (simpler than identifier pattern)

## Pattern Matching Details
- The pattern is anchored to the end of the line ($)
- Only the matched portion is passed to completion-hook
- Use capturing groups if you need to extract specific parts
- PCRE2 syntax: `\\\\S` = non-whitespace, `\\\\w` = word char, `\\\\s` = whitespace")

;; ============================================================================
;; WORD STORE CONFIGURATION
;; ============================================================================

(defvar *completion-word-store-size* 10000
  "Maximum number of words to store for completions.

This controls the maximum size of the word store used for completion
suggestions. When the store reaches this limit, older words are removed
to make room for new ones (FIFO - First In, First Out).

## Examples
- 500 - Small store, good for limited memory
- 1000 - Balanced for most use cases
- 5000 - Large store, good for extensive word collection
- 10000 - Very large store (default), may use more memory")

(defvar *completion-max-results* 64
  "Maximum number of completion candidates to return per prefix.")

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

(defvar *enable-telnet-logging* #t
  "Enable/disable telnet I/O logging (both send and receive).")

(defvar *telnet-log-directory* "~/telnet-logs"
  "Directory for telnet log files (supports ~/ expansion).
Log filename format: telnet-<host>-<port>-<timestamp>.log")

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
;; EXTENSIBLE HOOK SYSTEM
;; ============================================================================
;; Emacs-style hook system allowing multiple functions per hook.
;; Hooks are stored as alists: ((hook-name . (sym1 sym2 ...)) ...)
;; Functions are identified by symbol, allowing reload without duplicates.

(defun memq (item lst)
  "Return sublist starting at first eq? match, or nil if not found."
  (cond
    ((null? lst) nil)
    ((eq? item (car lst)) lst)
    (#t (memq item (cdr lst)))))

(defvar *hooks* '()
  "Global registry of hooks.

Format: ((hook-name . (fn1 fn2 fn3 ...)) ...)

Each hook name maps to a list of functions that will be called in order
when the hook is run.

## Example
```lisp
*hooks*
; => ((telnet-input-hook . (#<lambda> #<lambda>))
;     (user-input-hook . (#<lambda>)))
```

## See Also
- `add-hook` - Add a function to a hook
- `remove-hook` - Remove a function from a hook
- `run-hook` - Run all functions in a hook")

(defun add-hook (hook-name fn-symbol)
  "Add a function to a hook by symbol name.

## Parameters
- `hook-name` - Symbol identifying the hook (e.g., 'telnet-input-hook)
- `fn-symbol` - Symbol naming the function to add (e.g., 'my-handler)

## Returns
`nil`

## Description
Adds a function symbol to the hook's list. If the symbol is already present,
does nothing (idempotent). Functions are called in the order they were added.

Using symbols instead of function objects allows scripts to be reloaded
without creating duplicate hook entries.

## Examples
```lisp
;; Define a handler function
(defun my-telnet-logger (text)
  (log-to-file text))

;; Add it to the hook by symbol
(add-hook 'telnet-input-hook 'my-telnet-logger)

;; Reloading the script won't create duplicates
```

## See Also
- `remove-hook` - Remove a function from a hook
- `run-hook` - Run all functions in a hook"
  (let ((entry (assoc hook-name *hooks*)))
    (if entry
      ;; Hook exists - add symbol if not already present
      (unless (memq fn-symbol (cdr entry))
        (set! *hooks*
          (map (lambda (e)
                 (if (eq? (car e) hook-name)
                   (cons hook-name (append (cdr e) (list fn-symbol)))
                   e))
            *hooks*)))
      ;; New hook - create entry
      (set! *hooks* (cons (cons hook-name (list fn-symbol)) *hooks*))))
  nil)

(defun remove-hook (hook-name fn-symbol)
  "Remove a function from a hook by symbol name.

## Parameters
- `hook-name` - Symbol identifying the hook
- `fn-symbol` - Symbol of the function to remove

## Returns
`nil`

## Description
Removes the function symbol from the hook's list.

## Examples
```lisp
;; Add a handler
(add-hook 'telnet-input-hook 'my-logger)

;; Later, remove it
(remove-hook 'telnet-input-hook 'my-logger)
```

## See Also
- `add-hook` - Add a function to a hook
- `run-hook` - Run all functions in a hook"
  (let ((entry (assoc hook-name *hooks*)))
    (when entry
      ;; Rebuild *hooks* with the symbol removed from this hook's list
      (set! *hooks*
        (map (lambda (e)
               (if (eq? (car e) hook-name)
                 (cons hook-name (filter (lambda (s) (not (eq? s fn-symbol))) (cdr e)))
                 e))
          *hooks*))))
  nil)

(defun run-hook (hook-name &rest args)
  "Run all functions in a hook with given arguments.

## Parameters
- `hook-name` - Symbol identifying the hook
- `args` - Arguments to pass to each function

## Returns
`nil` (side-effects only)

## Description
Looks up each function symbol and calls it with the provided arguments.
Functions are called in the order they were added. Return values are
ignored - this is for side-effect-only hooks like `telnet-input-hook`.

If the hook doesn't exist or has no functions, does nothing.

## Examples
```lisp
;; Manually run a hook (normally called by C code)
(run-hook 'telnet-input-hook \"Hello from server\")

;; Multiple arguments
(run-hook 'user-input-hook \"hello\" 5)
```

## See Also
- `add-hook` - Add a function to a hook
- `remove-hook` - Remove a function from a hook"
  (let ((entry (assoc hook-name *hooks*)))
    (when entry
      (do ((syms (cdr entry) (cdr syms)))
        ((null? syms))
        (apply (eval (car syms)) args))))
  nil)

;; ============================================================================
;; TELNET-GUI HOOK IMPLEMENTATIONS
;; ============================================================================
;; These hooks are called by C code at specific points. The C code defines
;; no-op stubs which are overridden here with implementations that dispatch
;; to the extensible hook system.

(defun telnet-input-hook (text)
  "Process telnet server output through registered hooks.

## Parameters
- `text` - Plain text from server (ANSI codes already stripped)

## Returns
`nil` (side-effect only hook)

## Description
Called by C code when text is received from the telnet server. Dispatches
to all functions registered with `(add-hook 'telnet-input-hook ...)`.

This hook is for side effects only (logging, word collection, etc.).
To transform the data before display, use `telnet-input-filter-hook`.

## See Also
- `add-hook` - Register a handler
- `telnet-input-filter-hook` - Transform data before display"
  (run-hook 'telnet-input-hook text)
  nil)

;; User input hook state variables
(defvar *user-input-handled* nil
  "Set to #t by a user-input-hook handler to indicate it handled the input.")

(defvar *user-input-result* nil
  "Result to return when *user-input-handled* is #t.")

(defun run-user-input-hooks (text cursor-pos)
  "Run all functions in 'user-input-hook. First hook to set *user-input-handled* wins."
  (let ((entry (assoc 'user-input-hook *hooks*)))
    (when entry
      (do ((syms (cdr entry) (cdr syms)))
        ((or (null? syms) *user-input-handled*))
        ((eval (car syms)) text cursor-pos)))))

(defun user-input-hook (text cursor-pos)
  "Transform user input before sending to telnet server.

## Parameters
- `text` - The text user typed in input area
- `cursor-pos` - Cursor position in input area (integer)

## Returns
- String: C code echoes and sends the returned text
- `nil` or empty string: Hook handled everything (no further action)

## Description
Called by C code when user presses Enter. Dispatches to all functions
registered with `(add-hook 'user-input-hook ...)`.

Handlers can set `*user-input-handled*` to `#t` and `*user-input-result*`
to control the return value. First handler to set these wins.

## See Also
- `add-hook` - Register a handler
- `*user-input-handled*` - Flag to indicate input was handled
- `*user-input-result*` - Result when handled"
  (set! *user-input-handled* nil)
  (set! *user-input-result* nil)
  (run-user-input-hooks text cursor-pos)
  (if *user-input-handled* *user-input-result* text))

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
;;   (add-hook 'telnet-input-hook
;;     (lambda (text) (collect-words-from-text text)))
;;
;; NOTE: telnet-input-hook is defined above (in TELNET-GUI HOOK IMPLEMENTATIONS)
;; and dispatches to the extensible hook system. Multiple handlers can be added
;; via add-hook. The C code calls (telnet-input-hook text).

;; Default handler: Collect words for tab completion
(defun default-word-collector (text)
  "Default telnet-input-hook handler that collects words for tab completion."
  (collect-words-from-text text))

(add-hook 'telnet-input-hook 'default-word-collector)


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
;;
;; NOTE: user-input-hook is defined above (in TELNET-GUI HOOK IMPLEMENTATIONS)
;; and dispatches to the extensible hook system. Handlers can set
;; *user-input-handled* and *user-input-result* to control the return value.
;; The C code calls (user-input-hook text cursor-pos).

;; ============================================================================
;; MOUSE WHEEL SCROLLING CONFIGURATION
;; ============================================================================

(defvar *scroll-lines-per-click* 3
  "Number of terminal lines to scroll per mouse wheel click.

This controls how much the terminal scrolls when you use the mouse wheel.
Higher values scroll more per click, lower values provide finer control.

## Examples
- 1 - Very fine control, one line at a time (good for precise reading)
- 3 - Default, balanced scrolling (good for most users)
- 5 - Faster scrolling, good for long documents
- 10 - Very fast scrolling, good for quickly jumping through output

Note: This only affects discrete wheel clicks. For smooth scrolling
(trackpads), see `*smooth-scrolling-enabled*`.")

;; ============================================================================
;; SMOOTH SCROLLING CONFIGURATION
;; ============================================================================

(defvar *smooth-scrolling-enabled* #t
  "Enable smooth scrolling for high-resolution trackpads.

When enabled (`#t`), the terminal uses smooth pixel-based scrolling that
responds to trackpad gestures with sub-line precision. This provides a
natural, fluid scrolling experience similar to modern text editors.

When disabled (`#f`), the terminal uses discrete line-based scrolling that
jumps by whole lines. This is more traditional and may be preferred for
some use cases.

## Use Cases
- Enable (`#t`) if you have a trackpad or high-resolution mouse
- Enable (`#t`) for a modern, fluid user experience
- Disable (`#f`) if you prefer traditional terminal scrolling
- Disable (`#f`) if smooth scrolling feels laggy on your system")

;; ============================================================================
;; SCROLLBACK BUFFER CONFIGURATION
;; ============================================================================

(defvar *max-scrollback-lines* 0
  "Maximum number of scrollback lines to keep in memory.

This controls how many lines of terminal output history are retained for
scrolling back through past output. When the buffer is full, oldest lines
are automatically evicted to make room for new output (FIFO circular buffer).

## Values
- 0 - Unbounded (default, limited to 10000 lines for safety)
- 1000 - Small buffer, uses less memory
- 5000 - Medium buffer, good balance
- 10000 - Large buffer, maximum scrollback

Note: Set to 0 for unbounded scrollback (limited by SCROLLBACK_MAX_LINES = 10000
in code for safety). Setting a smaller value can reduce memory usage for
very long sessions with lots of output.

## Memory Usage
Each line uses ~100-500 bytes depending on content:
- 1000 lines ≈ 100KB - 500KB
- 5000 lines ≈ 500KB - 2.5MB
- 10000 lines ≈ 1MB - 5MB")

;; ============================================================================
;; AUTO-SCROLL TO BOTTOM CONFIGURATION
;; ============================================================================

(defvar *scroll-to-bottom-on-user-input* #t
  "Auto-scroll to bottom when user sends input.

When enabled (`#t`), the terminal automatically scrolls to the bottom (current
output) whenever you send a command or text to the server. This ensures you
always see the response to your commands.

## Values
- `#t` - Auto-scroll on user input (default, recommended)
- `#f` - Don't auto-scroll, stay at current scroll position

## Use Cases
- Enable (`#t`) for normal interactive use (default)
- Disable (`#f`) if you want to read scrollback while typing commands")

;; ============================================================================
;; INPUT HISTORY CONFIGURATION
;; ============================================================================

(defvar *input-history-size* 100
  "Maximum number of input history entries to keep.

This controls how many previous input commands are stored in history
that you can navigate through with Up/Down arrow keys. When the buffer
is full, oldest entries are automatically removed to make room for new
commands (FIFO circular buffer).

## Values
- 10 - Minimal history, uses very little memory
- 50 - Small history, good for simple sessions
- 100 - Medium history, default and recommended
- 200 - Large history, good for complex sessions
- 500 - Very large history, maximum recommended

## Memory Usage
Each entry uses INPUT_AREA_MAX_LENGTH bytes:
- 50 entries ≈ 100KB (assuming 2KB per entry)
- 100 entries ≈ 200KB
- 200 entries ≈ 400KB
- 500 entries ≈ 1MB

Note: Very large history sizes may impact performance when navigating
through history, especially on slower systems.")

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

(defconst *ansi-reset* "\033[0m"
  "ANSI escape sequence to reset all text attributes.

Resets foreground color, background color, bold, underline, and all other
ANSI text attributes to terminal defaults.")

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
;; All colors are specified as RGB lists (r g b) where each component is 0-255.
;; Override any color in your config.lisp file loaded with -l option.

;; ----------------------------------------------------------------------------
;; Terminal Default Colors
;; ----------------------------------------------------------------------------

(defvar *terminal-fg-color* '(255 255 255)
  "Default terminal foreground (text) color.

## Format
RGB list: (R G B) where each component is 0-255.

## Default
White (255, 255, 255)

## Description
Used for terminal text when no ANSI color codes are present. Server output
with ANSI color codes will override this for specific text.

## Examples
```lisp
(define *terminal-fg-color* '(255 255 255))  ; White (default)
(define *terminal-fg-color* '(200 200 200))  ; Light gray
(define *terminal-fg-color* '(0 255 0))      ; Green (retro terminal)
```")

(defvar *terminal-bg-color* '(0 0 0)
  "Default terminal background color.

## Format
RGB list: (R G B) where each component is 0-255.

## Default
Black (0, 0, 0)

## Description
Used for terminal background, input area background, and divider background.
Server output with ANSI background colors will override this for specific cells.

## Examples
```lisp
(define *terminal-bg-color* '(0 0 0))        ; Black (default)
(define *terminal-bg-color* '(30 30 30))     ; Dark gray
(define *terminal-bg-color* '(0 0 40))       ; Dark blue
```")

;; ----------------------------------------------------------------------------
;; Text Selection Colors
;; ----------------------------------------------------------------------------

(defvar *selection-fg-color* '(0 0 0)
  "Foreground (text) color for selected text.

## Format
RGB list: (R G B) where each component is 0-255.

## Default
Black (0, 0, 0)

## Description
Text color when cells are selected/highlighted. Should contrast well with
`*selection-bg-color*` for readability.

## Examples
```lisp
(define *selection-fg-color* '(0 0 0))        ; Black (default)
(define *selection-fg-color* '(255 255 255))  ; White
```")

(defvar *selection-bg-color* '(0 180 180)
  "Background color for selected text.

## Format
RGB list: (R G B) where each component is 0-255.

## Default
Cyan (0, 180, 180)

## Description
Background color of selected text regions. Should contrast well with
`*selection-fg-color*` and be visible against `*terminal-bg-color*`.

## Examples
```lisp
(define *selection-bg-color* '(0 180 180))    ; Cyan (default)
(define *selection-bg-color* '(255 140 0))    ; Orange
(define *selection-bg-color* '(100 149 237))  ; Cornflower blue
```")

;; ----------------------------------------------------------------------------
;; Cursor Color
;; ----------------------------------------------------------------------------

(defvar *terminal-cursor-color* '(140 120 150)
  "Cursor (caret) background color in input area.

## Format
RGB list: (R G B) where each component is 0-255.

## Default
Muted purple (140, 120, 150)

## Description
Background color of the cursor rectangle in the input area. The character
under the cursor is rendered with this background color.

## Examples
```lisp
(define *terminal-cursor-color* '(140 120 150))  ; Muted purple (default)
(define *terminal-cursor-color* '(150 140 120))  ; Warm gray
(define *terminal-cursor-color* '(255 255 255))  ; White block cursor
```")

;; ----------------------------------------------------------------------------
;; Divider Colors
;; ----------------------------------------------------------------------------

(defvar *divider-connected-color* '(128 150 150)
  "Divider line color when connected to server.

## Format
RGB list: (R G B) where each component is 0-255.

## Default
Gray with green/blue tint (128, 150, 150)

## Description
Color of the box-drawing divider lines (top and bottom of input area) when
a telnet connection is active. Provides visual feedback of connection status.

## Examples
```lisp
(define *divider-connected-color* '(128 150 150))  ; Gray with green/blue tint (default)
(define *divider-connected-color* '(80 200 120))   ; Saturated green
(define *divider-connected-color* '(0 180 180))    ; Cyan (match theme)
```")

(defvar *divider-disconnected-color* '(128 128 128)
  "Divider line color when disconnected from server.

## Format
RGB list: (R G B) where each component is 0-255.

## Default
Gray (128, 128, 128)

## Description
Color of the box-drawing divider lines when no telnet connection is active.
A neutral color indicating disconnected state.

## Examples
```lisp
(define *divider-disconnected-color* '(128 128 128))  ; Gray (default)
(define *divider-disconnected-color* '(100 100 100))  ; Darker gray
(define *divider-disconnected-color* '(255 100 100))  ; Red (warning)
```")

;; ----------------------------------------------------------------------------
;; User Input Echo Color
;; ----------------------------------------------------------------------------

(defvar *user-input-echo-color* '(255 215 0)
  "Color for echoing user input to terminal.

## Format
RGB list: (R G B) where each component is 0-255.

## Default
Gold (255, 215, 0)

## Description
When you press Enter, your input is echoed to the terminal in this color
before being sent to the server. Helps distinguish your commands from
server output.

## Examples
```lisp
(define *user-input-echo-color* '(255 215 0))    ; Gold (default)
(define *user-input-echo-color* '(255 220 100))  ; Light yellow
(define *user-input-echo-color* '(0 255 255))    ; Cyan
(define *user-input-echo-color* '(255 255 255))  ; White (same as terminal)
```")

;; ============================================================================
;; TERMINAL LINE HEIGHT CONFIGURATION
;; ============================================================================

(defvar *terminal-line-height* 1.2
  "Multiplier for terminal line spacing.

This controls the vertical spacing between terminal lines. The value is a
multiplier applied to the base cell height calculated from the font metrics.

## Values
- 1.2 - Default spacing (20% more than font metrics, better readability)
- 1.0 - Normal spacing (matches font metrics)
- 1.5 - 50% more spacing between lines
- 2.0 - Double spacing
- 0.5 - Half spacing (tighter, minimum recommended)

## Examples
- 1.2 - Default, comfortable spacing for readability
- 1.0 - Compact terminal appearance (matches font metrics)
- 1.25 - Slightly more spacing for readability
- 1.5 - Comfortable spacing for long reading sessions
- 2.0 - Very loose spacing, maximum recommended

Note: Values are clamped to 0.5-3.0 range for safety. The multiplier affects
vertical spacing between rows, window height calculations, and mouse
coordinate conversion, but does not stretch glyphs (they maintain their
original size from the font).")


;; ============================================================================
;; DIVIDER MODE INDICATORS
;; ============================================================================
;; Mode indicators displayed on the divider line (e.g., ⚡ for eval mode)
;; Modes are stored as an alist: ((priority . (symbol . "display")) ...)
;; Lower priority = displayed first (leftmost)

(defvar *divider-modes* '()
  "Association list of divider mode indicators.

Format: ((priority . (symbol . \"display\")) ...)
- priority: Integer, lower values displayed first (leftmost)
- symbol: Mode identifier (e.g., 'eval, 'animation)
- display: String to show on divider (e.g., \"⚡\", \"▶\")

## Built-in Modes
- eval (priority 10): ⚡ - Shown when in Lisp eval mode (Shift+Tab)
- animation (priority 90): ▶ - Shown when animation is playing

## Example
```lisp
*divider-modes*
; => ((10 . (eval . \"⚡\")) (90 . (animation . \"▶\")))
```")

;; Note: divider-mode-set and divider-mode-remove are C builtins defined in lisp.c
;; They automatically trigger a redraw when the mode list is modified.

;; ============================================================================
;; TIMER SYSTEM
;; ============================================================================
;; Emacs-style timer system for scheduling Lisp code to run after delays.
;;
;; API:
;;   (run-at-time TIME REPEAT FUNCTION &rest ARGS) - Schedule a timer
;;   (cancel-timer TIMER) - Cancel a timer
;;   (cancel-function-timers FUNCTION) - Cancel all timers for a function
;;   (list-timers) - List all active timers
;;
;; Example:
;;   (run-at-time 5 nil (lambda () (terminal-echo "Hello!\r\n")))  ; after 5 sec
;;   (run-at-time 60 60 (lambda () (telnet-send "PING")))  ; every 60 sec

(defvar *timer-list* '()
  "List of active timers. Each timer is a list:
   (id fire-time-ms repeat-ms callback args)")

(defvar *timer-next-id* 1
  "Next timer ID to assign.")

(defun run-at-time (time repeat function &rest args)
  "Schedule FUNCTION to run after TIME seconds.

## Parameters
- `time` - Seconds until first execution (number)
- `repeat` - Seconds between executions, or nil for one-shot
- `function` - Callback function to invoke
- `args` - Optional arguments passed to callback

## Returns
Timer object (vector) for use with `cancel-timer`.

## Description
Creates a timer that will call FUNCTION after TIME seconds. If REPEAT
is non-nil, the timer will repeat every REPEAT seconds after the first
execution.

## Examples
```lisp
;; One-shot timer after 2 seconds
(run-at-time 2 nil (lambda () (terminal-echo \"Hello!\\r\\n\")))

;; Repeating timer every 60 seconds
(define keepalive (run-at-time 60 60 (lambda () (telnet-send \"PING\"))))

;; Timer with arguments
(run-at-time 5 nil (lambda (msg) (terminal-echo msg)) \"Delayed!\\r\\n\")
```

## See Also
- `cancel-timer` - Cancel a timer
- `cancel-function-timers` - Cancel all timers for a function
- `list-timers` - List active timers"
  (let* ((delay-ms (* time 1000))
          (repeat-ms (if repeat (* repeat 1000) 0))
          (fire-time (+ (current-time-ms) delay-ms))
          (id *timer-next-id*)
          (timer (list id fire-time repeat-ms function args)))
    (set! *timer-next-id* (+ *timer-next-id* 1))
    (set! *timer-list* (cons timer *timer-list*))
    timer))

(defun cancel-timer (timer)
  "Cancel TIMER.

## Parameters
- `timer` - Timer object returned by `run-at-time`

## Returns
`#t` if timer was found and cancelled, `nil` otherwise.

## Examples
```lisp
(define my-timer (run-at-time 60 nil my-callback))
(cancel-timer my-timer)  ; => #t
(cancel-timer my-timer)  ; => nil (already cancelled)
```

## See Also
- `run-at-time` - Create a timer
- `cancel-function-timers` - Cancel by function"
  (let ((found #f))
    (set! *timer-list*
      (filter (lambda (t)
                (if (eq? t timer)
                  (progn (set! found #t) #f)
                  #t))
        *timer-list*))
    found))

(defun cancel-function-timers (function)
  "Cancel all timers with FUNCTION as callback.

## Parameters
- `function` - Function whose timers to cancel

## Returns
Number of timers cancelled.

## Description
Useful when you've lost the timer reference but know the callback function.

## Examples
```lisp
(defun my-ping () (telnet-send \"PING\"))
(run-at-time 30 30 my-ping)
(run-at-time 60 60 my-ping)
(cancel-function-timers my-ping)  ; => 2
```

## See Also
- `cancel-timer` - Cancel by timer object
- `list-timers` - View active timers"
  (let ((count 0))
    (set! *timer-list*
      (filter (lambda (t)
                (if (eq? (list-ref t 3) function)
                  (progn (set! count (+ count 1)) #f)
                  #t))
        *timer-list*))
    count))

(defun list-timers ()
  "Return list of active timers.

## Returns
List of timer lists, each containing:
  - [0] id - Timer ID
  - [1] fire-time-ms - When timer fires (milliseconds)
  - [2] repeat-ms - Repeat interval (0 for one-shot)
  - [3] callback - Function to call
  - [4] args - Arguments for callback

## Examples
```lisp
(list-timers)
; => ((1 45000 0 #<lambda> ()) (2 60000 60000 #<lambda> ()))
```

## See Also
- `run-at-time` - Create timers
- `cancel-timer` - Cancel timers"
  *timer-list*)

(defun run-timers ()
  "Run all due timers. Called each frame by C code.

## Description
This function is called automatically by the main event loop.
It checks all timers in `*timer-list*` and fires any that are due.
Recurring timers are rescheduled; one-shot timers are removed.

Do not call this function directly."
  (when (not (null? *timer-list*))
    (let ((now (current-time-ms))
           (new-list '()))
      (do ((remaining *timer-list* (cdr remaining)))
        ((null? remaining))
        (let* ((timer (car remaining))
                (fire-time (list-ref timer 1))
                (repeat-ms (list-ref timer 2))
                (callback (list-ref timer 3))
                (args (list-ref timer 4)))
          (if (>= now fire-time)
            (progn
              ;; Timer is due - call it
              (apply callback args)
              ;; Reschedule if recurring (mutate fire-time in place to preserve identity)
              (when (> repeat-ms 0)
                (set-car! (cdr timer) (+ now repeat-ms))
                (set! new-list (cons timer new-list))))
            ;; Not due yet - keep it
            (set! new-list (cons timer new-list)))))
      (set! *timer-list* new-list))))

