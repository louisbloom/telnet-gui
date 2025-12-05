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
            (let ((token-data (tintin-extract-token input start-pos)))
              (if token-data
                (progn
                  (set! args (cons (car token-data) args))
                  (set! start-pos (cdr token-data)))
                (set! success #f)))))))))


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
  (or (and (string>= ch "a") (string<= ch "z"))
    (and (string>= ch "A") (string<= ch "Z"))
    (and (string>= ch "0") (string<= ch "9"))
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

    ;; Expand variables, then speedwalk
    (let ((expanded (tintin-expand-speedwalk
                      (tintin-expand-variables-fast result))))
      ;; Split by semicolon
      (let ((split-commands (tintin-split-commands expanded)))
        (if (> (list-length split-commands) 1)
          ;; Multiple commands - recursively process each
          (let ((sub-results '()))
            (do ((j 0 (+ j 1)))
              ((>= j (list-length split-commands)))
              (let ((subcmd (list-ref split-commands j)))
                (if (and (string? subcmd) (not (string=? subcmd "")))
                  (let ((result (tintin-process-command-internal subcmd (+ depth 1))))
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
            (tintin-process-command-internal (list-ref split-commands 0) (+ depth 1))
            ""))))))

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

;; ============================================================================
;; COMMAND HANDLERS (REFACTORED)
;; ============================================================================

;; Handle #alias command
;; args: (name commands)
(defun tintin-handle-alias (args)
  (let ((name (tintin-strip-braces (list-ref args 0)))
         (commands (tintin-strip-braces (list-ref args 1)))
         (priority 5))  ; Default priority
    (hash-set! *tintin-aliases* name (list commands priority))
    (tintin-echo (concat "Alias '" name "' created (priority: "
                   (number->string priority) ")\r\n"))
    ""))

;; Handle #variable command
;; args: (name value)
(defun tintin-handle-variable (args)
  (let ((name (tintin-strip-braces (list-ref args 0)))
         (value (tintin-strip-braces (list-ref args 1))))
    (hash-set! *tintin-variables* name value)
    (tintin-echo (concat "Variable '" name "' set to '" value "'\r\n"))
    ""))

;; Handle #save command
;; args: (filename)
(defun tintin-handle-save (args)
  (let ((filename (tintin-strip-braces (list-ref args 0))))
    (tintin-save-state filename)
    (tintin-echo (concat "State saved to '" filename "'\r\n"))
    ""))

;; Handle #load command
;; args: (filename)
(defun tintin-handle-load (args)
  (let ((filename (tintin-strip-braces (list-ref args 0))))
    (load filename)
    (tintin-echo (concat "State loaded from '" filename "'\r\n"))
    ""))

;; Register commands with metadata (now that handlers are defined)
(hash-set! *tintin-commands* "alias"
  (list tintin-handle-alias 2 "#alias {name} {commands}"))
(hash-set! *tintin-commands* "variable"
  (list tintin-handle-variable 2 "#variable {name} {value}"))
(hash-set! *tintin-commands* "save"
  (list tintin-handle-save 1 "#save {filename}"))
(hash-set! *tintin-commands* "load"
  (list tintin-handle-load 1 "#load {filename}"))

;; ============================================================================
;; GENERIC COMMAND DISPATCHER (REFACTORED)
;; ============================================================================

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
        ;; Parse arguments
        (let ((args (tintin-parse-arguments input arg-count)))
          (if args
            ;; Success: call handler
            (handler args)
            ;; Parse failed: show syntax error
            (tintin-syntax-error syntax-help)))))))

;; ============================================================================
;; AUTO-ACTIVATION
;; ============================================================================
;; Automatically activate TinTin++ when this file is loaded

(define user-input-hook tintin-user-input-hook)

;; Announce activation (terminal is ready when this file loads via -l)
(tintin-echo "TinTin++ loaded and activated\r\n")
