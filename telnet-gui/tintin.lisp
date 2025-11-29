;; TinTin++ Layer for telnet-lisp
;; Provides TinTin++-compatible alias and command processing

;; ============================================================================
;; CORE DATA STRUCTURES
;; ============================================================================

;; Alias storage: hash table mapping alias names to (commands priority) lists
(define *tintin-aliases* (make-hash-table))

;; Default priority for aliases (lower number = higher priority)
(define *tintin-default-priority* 5)

;; Speedwalk enabled flag (default: enabled)
(define *tintin-speedwalk-enabled* #t)

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================

;; Reverse a list
(defun reverse (lst)
  (if (null? lst)
      '()
      (do ((remaining lst (cdr remaining))
           (result '()))
          ((null? remaining) result)
        (set! result (cons (car remaining) result)))))

;; ============================================================================
;; HELPER FUNCTIONS
;; ============================================================================

;; Extract text between { and } with nested brace handling
;; Returns: (extracted-text . next-position) or nil
(defun tintin-extract-braced-arg (str pos)
  (if (not (and (string? str) (>= pos 0) (< pos (string-length str))))
      nil
      (let ((len (string-length str))
            (start -1)
            (depth 0)
            (i pos))
        ;; Skip whitespace and find opening brace
        (do ((found #f))
            ((or found (>= i len)) (if found i -1))
          (if (and (< i len) (or (string= (string-ref str i) " ") (string= (string-ref str i) "\t")))
              (set! i (+ i 1))
              (if (and (< i len) (string= (string-ref str i) "{"))
                  (progn
                    (set! start (+ i 1))
                    (set! depth 1)
                    (set! i (+ i 1))
                    (set! found #t))
                  (set! found #t))))
        ;; If no opening brace found, return nil
        (if (< start 0)
            nil
            ;; Find matching closing brace
            (do ((found #f))
                ((or found (>= i len)) (if found (cons (substring str start i) (+ i 1)) nil))
              (if (>= i len)
                  (set! found #t)
                  (let ((ch (string-ref str i)))
                    (if (string= ch "{")
                        (progn
                          (set! depth (+ depth 1))
                          (set! i (+ i 1)))
                        (if (string= ch "}")
                            (if (= depth 1)
                                (set! found #t)
                                (progn
                                  (set! depth (- depth 1))
                                  (set! i (+ i 1))))
                            (set! i (+ i 1)))))))))))

;; Parse all braced arguments from a string
;; Returns: list of extracted strings
(defun tintin-parse-braced-args (str)
  (if (not (string? str))
      '()
      (let ((result '())
            (pos 0)
            (len (string-length str)))
        (do ((done #f))
            (done result)
          (let ((extracted (tintin-extract-braced-arg str pos)))
            (if extracted
                (progn
                  (set! result (cons (car extracted) result))
                  (set! pos (cdr extracted)))
                (set! done #t))))
        (reverse result))))

;; Split string into words (by whitespace)
;; Returns: list of word strings
(defun tintin-split-words (str)
  (if (not (string? str))
      '()
      (let ((parts (regex-split "\\s+" str))
            (result '()))
        (do ((remaining parts (cdr remaining)))
            ((null? remaining) result)
          (let ((part (car remaining)))
            (if (and (string? part) (> (string-length part) 0))
                (set! result (cons part result)))))
        (reverse result))))

;; Split input by ; separator, filter empty commands
;; Returns: list of non-empty command strings
(defun tintin-split-commands (input)
  (if (not (string? input))
      '()
      (let ((parts (split input ";"))
            (result '()))
        (do ((remaining parts (cdr remaining)))
            ((null? remaining) result)
          (let ((part (car remaining)))
            (if (string? part)
                (let ((trimmed (regex-replace "^\\s+" "" (regex-replace "\\s+$" "" part))))
                  (if (> (string-length trimmed) 0)
                      (set! result (cons trimmed result)))))))
        (reverse result))))

;; Convert string to number (simple integer parser)
;; Returns: number or nil if invalid
(defun tintin-string-to-number (str)
  (if (not (string? str))
      nil
      (let* ((trimmed (regex-replace "^\\s+" "" (regex-replace "\\s+$" "" str)))
             (len (string-length trimmed))
             (i 0)
             (sign 1)
             (value 0))
        (if (= len 0)
            nil
            (progn
              ;; Check for sign
              (if (string= (string-ref trimmed 0) "-")
                  (progn
                    (set! sign -1)
                    (set! i 1)))
              (if (and (< i len) (string= (string-ref trimmed i) "+"))
                  (set! i (+ i 1)))
              ;; Parse digits
              (do ((valid #t))
                  ((or (not valid) (>= i len)) (if valid (* sign value) nil))
                (let ((ch (string-ref trimmed i))
                      (digit -1))
                  ;; Convert single character to digit
                  (cond
                   ((string= ch "0") (set! digit 0))
                   ((string= ch "1") (set! digit 1))
                   ((string= ch "2") (set! digit 2))
                   ((string= ch "3") (set! digit 3))
                   ((string= ch "4") (set! digit 4))
                   ((string= ch "5") (set! digit 5))
                   ((string= ch "6") (set! digit 6))
                   ((string= ch "7") (set! digit 7))
                   ((string= ch "8") (set! digit 8))
                   ((string= ch "9") (set! digit 9))
                   (#t (set! valid #f)))
                  (if (>= digit 0)
                      (progn
                        (set! value (+ (* value 10) digit))
                        (set! i (+ i 1)))
                      (set! valid #f)))))))))

;; ============================================================================
;; SPEEDWALK EXPANSION
;; ============================================================================

;; Check if a character is a valid direction (single char)
(defun tintin-is-direction-char? (ch)
  (if (not (string? ch))
      #f
      (or (string= ch "n") (string= ch "s") (string= ch "e") (string= ch "w")
          (string= ch "u") (string= ch "d"))))

;; Check if a string is a valid direction (single or double char)
(defun tintin-is-direction? (str)
  (if (not (string? str))
      #f
      (let ((len (string-length str)))
        (if (= len 1)
            (tintin-is-direction-char? str)
            (if (= len 2)
                (or (string= str "ne") (string= str "nw") (string= str "se") (string= str "sw"))
                #f)))))

;; Expand speedwalk notation in a command string
;; Example: "2s5w3s3w2nw" -> "s;s;w;w;w;w;w;s;s;s;w;w;w;n;n;w"
;; Example: "nesw" -> "n;e;s;w"
(defun tintin-expand-speedwalk (input)
  (if (not (string? input))
      ""
      (if (not *tintin-speedwalk-enabled*)
          input
          (let ((len (string-length input))
                (result '())
                (i 0))
            (do ((done #f))
                (done (if (null? result)
                          ""
                          (let ((reversed-result (reverse result)))
                            (let ((joined (car reversed-result)))
                              (do ((rest (cdr reversed-result) (cdr rest)))
                                  ((null? rest) joined)
                                (set! joined (concat joined ";" (car rest))))))))
              (if (>= i len)
                  (set! done #t)
                  (let ((ch (string-ref input i))
                        (num-str "")
                        (num 1)
                        (dir ""))
                    ;; Parse optional number prefix
                    (do ((j i (+ j 1)))
                        ((if (>= j len)
                             #t
                             (let ((digit-ch (string-ref input j)))
                               (not (and (string>= digit-ch "0") (string<= digit-ch "9")))))
                         (if (> j i)
                             (progn
                               (set! num-str (substring input i j))
                               (let ((parsed (tintin-string-to-number num-str)))
                                 (if parsed
                                     (set! num parsed)
                                     (set! num 1)))
                               (set! i j))
                             (set! i j))))
                    ;; Parse direction (1 or 2 chars)
                    (if (< i len)
                        (let ((ch1 (string-ref input i)))
                          (if (tintin-is-direction-char? ch1)
                              (if (< (+ i 1) len)
                                  ;; Check if next char forms a two-char direction
                                  (let ((ch2 (string-ref input (+ i 1))))
                                    (if (and (tintin-is-direction-char? ch2)
                                             (tintin-is-direction? (concat ch1 ch2)))
                                        ;; Two-char direction
                                        (progn
                                          (set! dir (concat ch1 ch2))
                                          (set! i (+ i 2)))
                                        ;; Single-char direction
                                        (progn
                                          (set! dir ch1)
                                          (set! i (+ i 1)))))
                                  ;; Single-char direction (no next char)
                                  (progn
                                    (set! dir ch1)
                                    (set! i (+ i 1))))
                              ;; Not a direction - copy as-is
                              (progn
                                (set! result (cons ch1 result))
                                (set! i (+ i 1))
                                (set! num 0)))))
                    ;; Expand direction if we have one
                    (if (> (string-length dir) 0)
                        (do ((j 0 (+ j 1)))
                            ((>= j num) ())
                          (set! result (cons dir result)))))))))))

;; ============================================================================
;; VARIABLE SUBSTITUTION
;; ============================================================================

;; Expand variables in command template
;; Input: template (string), arg-words (list), all-args (string for %0)
;; Returns: expanded command string
(defun tintin-expand-variables (template arg-words all-args)
  (if (not (string? template))
      ""
      (let ((result template))
        ;; Replace %99 down to %1 (to avoid partial matches)
        (do ((i 99 (- i 1)))
            ((< i 1) result)
          (let ((var (concat "%" (number->string i)))
                (replacement (if (and (not (null? arg-words)) (>= (list-length arg-words) i))
                                 (list-ref arg-words (- i 1))
                                 "")))
            (if (string? replacement)
                (set! result (string-replace var replacement result)))))
        ;; Replace %0 last
        (let ((all-args-str (if (string? all-args) all-args "")))
          (string-replace "%0" all-args-str result)))))

;; ============================================================================
;; ALIAS MATCHING
;; ============================================================================

;; Match pattern against input and extract variables
;; Pattern: "k %1 with %2", Input: "k sword with shield"
;; Returns: (extracted-args-list) or nil if no match
(defun tintin-match-pattern (pattern input)
  (if (not (and (string? pattern) (string? input)))
      nil
      (let ((pattern-parts (tintin-split-words pattern))
            (input-parts (tintin-split-words input))
            (extracted '())
            (match #t))
        (if (not (= (list-length pattern-parts) (list-length input-parts)))
            nil
            (progn
              (do ((p-remaining pattern-parts (cdr p-remaining))
                   (i-remaining input-parts (cdr i-remaining)))
                  ((or (null? p-remaining) (not match)) (if match (reverse extracted) nil))
                (let ((p-part (car p-remaining))
                      (i-part (car i-remaining)))
                  (if (string-prefix? "%" p-part)
                      ;; Variable - extract value
                      (set! extracted (cons i-part extracted))
                      ;; Literal - must match exactly
                      (if (not (string= p-part i-part))
                          (set! match #f))))))))))

;; Find matching alias for input
;; Returns: (alias-name commands priority . extracted-args) or nil
(defun tintin-match-alias (input)
  (if (not (string? input))
      nil
      (let* ((words (tintin-split-words input))
             (first-word (if (and (not (null? words)) (> (list-length words) 0))
                             (list-ref words 0)
                             ""))
             (exact-match nil)
             (pattern-matches '())
             (catch-all nil))
        ;; Check exact match first
        (if (> (string-length first-word) 0)
            (let ((alias-data (hash-ref *tintin-aliases* first-word)))
              (if alias-data
                  (set! exact-match (cons first-word alias-data)))))
        ;; If exact match found, return it immediately
        (if exact-match
            (let* ((alias-name (car exact-match))
                   (alias-data (cdr exact-match))
                   (commands (car alias-data))
                   (priority (car (cdr alias-data)))
                   (args (if (> (list-length words) 1)
                            (cdr words)
                            '())))
              (cons alias-name (cons commands (cons priority args))))
            ;; No exact match - check pattern matches
            (progn
              ;; Iterate through all aliases to find pattern matches
              (let ((entries (hash-entries *tintin-aliases*)))
                (do ((remaining entries (cdr remaining)))
                    ((null? remaining)
                     ;; After checking all patterns, select best match
                     (if (null? pattern-matches)
                         ;; No pattern match - check catch-all
                         (let ((catch-all-data (hash-ref *tintin-aliases* "%*")))
                           (if catch-all-data
                               (let ((commands (car catch-all-data))
                                     (priority (car (cdr catch-all-data))))
                                 (cons "%*" (cons commands (cons priority (list input)))))
                               nil))
                         ;; Sort matches by priority (lower = higher), then alphabetically
                         (let ((sorted (do ((matches-remaining pattern-matches (cdr matches-remaining))
                                            (result '()))
                                           ((null? matches-remaining) result)
                                         (let ((match (car matches-remaining))
                                               (match-priority (car (cdr (cdr match))))
                                               (match-name (car match))
                                               (inserted #f))
                                           ;; Insert in sorted order
                                           (do ((r-remaining result (cdr r-remaining))
                                                (new-result '()))
                                               ((or (null? r-remaining) inserted)
                                                (if inserted
                                                    (reverse new-result)
                                                    (reverse (cons match new-result))))
                                             (let ((other (car r-remaining))
                                                   (other-priority (car (cdr (cdr other))))
                                                   (other-name (car other)))
                                               (if (or (< match-priority other-priority)
                                                       (and (= match-priority other-priority)
                                                            (string< match-name other-name)))
                                                   (progn
                                                     (set! new-result (cons other (cons match new-result)))
                                                     (set! inserted #t))
                                                   (set! new-result (cons other new-result)))))))))
                           ;; Return first (best) match
                           (if (null? sorted)
                               nil
                               (car sorted))))))
                  (let ((entry (car remaining)))
                    (let* ((alias-name (car entry))
                           (alias-data (cdr entry))
                           (commands (car alias-data))
                           (priority (car (cdr alias-data))))
                      ;; Skip catch-all for now (handle separately)
                      (if (not (string= alias-name "%*"))
                          ;; Try pattern matching
                          (let ((extracted (tintin-match-pattern alias-name input)))
                            (if extracted
                                (set! pattern-matches (cons (cons alias-name (cons commands (cons priority extracted))) pattern-matches)))))))))))))

;; ============================================================================
;; COMMAND PARSING
;; ============================================================================

;; Parse #alias command
;; Returns: (name commands priority) or nil on error
(defun tintin-parse-alias-command (input)
  (if (not (string? input))
      nil
      (let ((trimmed (regex-replace "^\\s+" "" (regex-replace "\\s+$" "" input))))
        (if (not (string-prefix? "#alias" trimmed))
            nil
            (let ((args (tintin-parse-braced-args trimmed)))
              (if (< (list-length args) 2)
                  nil
                  (let ((name (list-ref args 0))
                        (commands (list-ref args 1))
                        (priority-str (if (>= (list-length args) 3)
                                          (list-ref args 2)
                                          nil))
                        (priority (if priority-str
                                      (let ((p (tintin-string-to-number priority-str)))
                                        (if p p *tintin-default-priority*))
                                      *tintin-default-priority*)))
                    (if (and (string? name) (> (string-length name) 0)
                             (string? commands) (> (string-length commands) 0))
                        (list name commands priority)
                        nil))))))))

;; Parse #unalias command
;; Returns: name string or nil on error
(defun tintin-parse-unalias-command (input)
  (if (not (string? input))
      nil
      (let ((trimmed (regex-replace "^\\s+" "" (regex-replace "\\s+$" "" input))))
        (if (not (string-prefix? "#unalias" trimmed))
            nil
            (let ((rest (substring trimmed 8 (string-length trimmed)))
                  (trimmed-rest (regex-replace "^\\s+" "" (regex-replace "\\s+$" "" rest))))
              ;; Check if name is in braces
              (let ((braced (tintin-extract-braced-arg trimmed-rest 0)))
                (if braced
                    (car braced)
                    ;; Otherwise, take first word
                    (let ((words (tintin-split-words trimmed-rest)))
                      (if (> (list-length words) 0)
                          (list-ref words 0)
                          nil)))))))))

;; Check if input is #list command
;; Returns: #t if it's a list command, #f otherwise
(defun tintin-is-list-command? (input)
  (if (not (string? input))
      #f
      (let ((trimmed (regex-replace "^\\s+" "" (regex-replace "\\s+$" "" input))))
        (string-prefix? "#list" trimmed))))

;; Parse #config command
;; Returns: (key value) or nil on error
(defun tintin-parse-config-command (input)
  (if (not (string? input))
      nil
      (let ((trimmed (regex-replace "^\\s+" "" (regex-replace "\\s+$" "" input))))
        (if (not (string-prefix? "#config" trimmed))
            nil
            (let ((args (tintin-parse-braced-args trimmed)))
              (if (< (list-length args) 2)
                  nil
                  (let ((key (list-ref args 0))
                        (value (list-ref args 1)))
                    (if (and (string? key) (> (string-length key) 0)
                             (string? value) (> (string-length value) 0))
                        (list key value)
                        nil))))))))

;; Apply config setting
(defun tintin-apply-config (key value)
  (let ((key-lower (string-downcase key))
        (value-lower (string-downcase value)))
    (if (string= key-lower "speedwalk")
        (progn
          (if (or (string= value-lower "on") (string= value-lower "1") (string= value-lower "#t"))
              (set! *tintin-speedwalk-enabled* #t)
              (if (or (string= value-lower "off") (string= value-lower "0") (string= value-lower "#f"))
                  (set! *tintin-speedwalk-enabled* #f)
                  ()))
          (terminal-echo (concat "Speedwalk " (if *tintin-speedwalk-enabled* "enabled" "disabled") ".\r\n")))
        (terminal-echo (concat "Unknown config key: " key "\r\n")))))

;; List all aliases
;; Echoes formatted list to terminal
(defun tintin-list-aliases (input)
  (let ((entries (hash-entries *tintin-aliases*))
        (count (hash-count *tintin-aliases*)))
    (if (= count 0)
        (terminal-echo "No aliases defined.\r\n")
        (progn
          (terminal-echo (concat "Aliases (" (number->string count) "):\r\n"))
          ;; Sort entries by name for consistent output
          (let ((sorted-entries (do ((entries-remaining entries (cdr entries-remaining))
                                     (result '()))
                                    ((null? entries-remaining) result)
                                  (let ((entry (car entries-remaining))
                                        (inserted #f))
                                    ;; Insert in alphabetical order
                                    (do ((r-remaining result (cdr r-remaining))
                                         (new-result '()))
                                        ((or (null? r-remaining) inserted)
                                         (if inserted
                                             (reverse new-result)
                                             (reverse (cons entry new-result))))
                                      (let ((other (car r-remaining))
                                            (entry-name (car entry))
                                            (other-name (car other)))
                                        (if (string< entry-name other-name)
                                            (progn
                                              (set! new-result (cons entry (cons other new-result)))
                                              (set! inserted #t))
                                            (set! new-result (cons other new-result)))))))))
            (do ((remaining sorted-entries (cdr remaining)))
                ((null? remaining) ())
              (let ((entry (car remaining)))
                (let* ((name (car entry))
                       (alias-data (cdr entry))
                       (commands (car alias-data))
                       (priority (car (cdr alias-data))))
                  (terminal-echo (concat "  " name " -> " commands " (priority: " (number->string priority) ")\r\n"))))))))))

;; ============================================================================
;; COMMAND PROCESSING
;; ============================================================================

;; Process single command (speedwalk expansion, then alias expansion)
;; Returns: processed command string or original if no match
(defun tintin-process-command (cmd)
  (if (not (string? cmd))
      ""
      (let ((trimmed (regex-replace "^\\s+" "" (regex-replace "\\s+$" "" cmd))))
        (if (= (string-length trimmed) 0)
            ""
            ;; Expand speedwalk first (if enabled)
            (let ((expanded (tintin-expand-speedwalk trimmed)))
              ;; Check for #list command
              (if (tintin-is-list-command? expanded)
                  (progn
                    (tintin-list-aliases expanded)
                    "")
                  ;; Check for #config command
                  (let ((config-data (tintin-parse-config-command expanded)))
                    (if config-data
                        (progn
                          (tintin-apply-config (list-ref config-data 0) (list-ref config-data 1))
                          "")
                          ;; Check for #alias command
                          (let ((alias-data (tintin-parse-alias-command expanded)))
                  (if alias-data
                      (progn
                        (let ((name (list-ref alias-data 0))
                              (commands (list-ref alias-data 1))
                              (priority (list-ref alias-data 2)))
                          (hash-set! *tintin-aliases* name (list commands priority))
                          (terminal-echo (concat "Alias '" name "' created (priority: " (number->string priority) ")\r\n")))
                        "")
                      ;; Check for #unalias command
                      (let ((name (tintin-parse-unalias-command expanded)))
                        (if name
                            (progn
                              (if (hash-ref *tintin-aliases* name)
                                  (progn
                                    (hash-remove! *tintin-aliases* name)
                                    (terminal-echo (concat "Alias '" name "' removed\r\n")))
                                  (terminal-echo (concat "Alias '" name "' not found\r\n")))
                              "")
                              ;; Regular command - try alias expansion (on speedwalk-expanded string)
                              (let ((match (tintin-match-alias expanded)))
                                (if match
                                    (let ((alias-name (car match))
                                          (commands (car (cdr match)))
                                          (priority (car (cdr (cdr match))))
                                          (extracted-args (cdr (cdr (cdr match)))))
                                      ;; Extract all-args string for %0 substitution
                                      (let ((all-args (if (string= alias-name "%*")
                                                          expanded
                                                          (let ((words (tintin-split-words expanded))
                                                                (first-word-len (if (> (list-length words) 0)
                                                                                    (string-length (list-ref words 0))
                                                                                    0)))
                                                            (if (> (list-length words) 1)
                                                                (let ((start-pos (+ first-word-len 1)))
                                                                  (if (< start-pos (string-length expanded))
                                                                      (substring expanded start-pos (string-length expanded))
                                                                      ""))
                                                                (if (> (list-length extracted-args) 0)
                                                                    ;; For pattern matches, join extracted args
                                                                    (do ((args-remaining extracted-args (cdr args-remaining))
                                                                         (result ""))
                                                                        ((null? args-remaining) result)
                                                                      (if (string= result "")
                                                                          (set! result (car args-remaining))
                                                                          (set! result (concat result " " (car args-remaining)))))
                                                                    ""))))))
                                        (tintin-expand-variables commands extracted-args all-args)))
                                    expanded))))))))))))))

;; Process full input (handles ; separator)
;; Returns: processed string with commands joined by \r\n
(defun tintin-process-input (input)
  (if (not (string? input))
      ""
      (let ((commands (tintin-split-commands input))
            (results '()))
        (do ((remaining commands (cdr remaining)))
            ((null? remaining)
             (if (null? results)
                 ""
                 (let ((joined (car results)))
                   (do ((rest (cdr results) (cdr rest)))
                       ((null? rest) joined)
                     (set! joined (concat joined "\r\n" (car rest)))))))
          (let ((processed (tintin-process-command (car remaining))))
            (if (and (string? processed) (> (string-length processed) 0))
                (set! results (cons processed results))))))))

;; ============================================================================
;; INTEGRATION HOOK
;; ============================================================================

;; Hook function for user-input-hook
(defun tintin-input-hook (text cursor-pos)
  (tintin-process-input text))

;; ============================================================================
;; TESTS
;; ============================================================================

;; Comprehensive test suite for TinTin++ layer
;; Uncomment sections to run tests:

;; Test 1: Command separator
;; (progn
;;   (tintin-split-commands "cmd1;cmd2")      ; => ("cmd1" "cmd2")
;;   (tintin-split-commands "cmd1;;cmd2")     ; => ("cmd1" "cmd2") (empty filtered)
;;   (tintin-split-commands ";cmd1")          ; => ("cmd1") (leading ; ignored)
;;   (tintin-split-commands "cmd1;")          ; => ("cmd1") (trailing ; ignored)
;;   (tintin-split-commands "cmd1;cmd2;cmd3") ; => ("cmd1" "cmd2" "cmd3")
;;   )

;; Test 2: Basic alias creation and expansion
;; (progn
;;   (tintin-process-command "#alias {k} {kill %1}")
;;   (tintin-process-command "k orc")         ; => "kill orc"
;;   )

;; Test 3: Variable substitution
;; (progn
;;   (tintin-process-command "#alias {test} {echo %0}")
;;   (tintin-process-command "test a b c")    ; => "echo a b c"
;;   (tintin-process-command "#alias {test2} {a=%1 b=%2}")
;;   (tintin-process-command "test2 x y")     ; => "a=x b=y"
;;   (tintin-process-command "test2 x")       ; => "a=x b=" (missing var)
;;   (tintin-process-command "#alias {t} {%1 %1 %1}")
;;   (tintin-process-command "t x")           ; => "x x x" (multiple occurrences)
;;   )

;; Test 4: Priority ordering
;; (progn
;;   (tintin-process-command "#alias {test} {low} {10}")
;;   (tintin-process-command "#alias {test} {high} {1}")
;;   ;; Should use higher priority (lower number)
;;   )

;; Test 5: Catch-all alias
;; (progn
;;   (tintin-process-command "#alias {%*} {echo caught: %0}")
;;   (tintin-process-command "unknown command") ; => "echo caught: unknown command"
;;   )

;; Test 6: Multi-word pattern matching
;; (progn
;;   (tintin-process-command "#alias {k %1 with %2} {draw %2;attack %1}")
;;   (tintin-process-command "k orc with sword") ; => "draw sword;attack orc"
;;   )

;; Test 7: #unalias command
;; (progn
;;   (tintin-process-command "#alias {test} {echo test}")
;;   (tintin-process-command "#unalias {test}")  ; Should echo "removed"
;;   (tintin-process-command "#unalias {nonexistent}") ; Should echo "not found"
;;   )

;; Test 10: #list command
;; (progn
;;   (tintin-process-command "#alias {k} {kill %1}")
;;   (tintin-process-command "#alias {n} {north}")
;;   (tintin-process-command "#list")  ; Should list all aliases
;;   )

;; Test 8: Edge cases
;; (progn
;;   (tintin-process-input "")                ; => "" (empty input)
;;   (tintin-process-input "unknown")         ; => "unknown" (no match)
;;   (tintin-process-input "#alias {bad}")    ; => "" (invalid syntax, error echoed)
;;   (tintin-process-input "cmd1;cmd2;cmd3")  ; => "cmd1\r\ncmd2\r\ncmd3"
;;   )

;; Test 9: Full integration test
;; (progn
;;   ;; Create aliases
;;   (tintin-process-command "#alias {n} {north}")
;;   (tintin-process-command "#alias {s} {south}")
;;   (tintin-process-command "#alias {k %1} {kill %1;kick}")
;;   ;; Use them
;;   (tintin-process-input "n;s;k orc")       ; => "north\r\nsouth\r\nkill orc;kick"
;;   )
