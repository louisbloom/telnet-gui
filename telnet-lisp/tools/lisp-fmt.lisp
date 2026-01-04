;;; lisp-fmt.lisp --- Lisp source code formatter
;;;
;;; Usage:
;;;   lisp-repl telnet-lisp/tools/lisp-fmt.lisp file.lisp       # Format to stdout
;;;   lisp-repl telnet-lisp/tools/lisp-fmt.lisp -i file.lisp    # In-place edit
;;;
;;; Formatting rules:
;;;   - Line width: 79 characters
;;;   - Indent: 2 spaces
;;;   - List continuations align with first element
;;;   - Comments are preserved (leading and inline)

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(define *max-column* 79)
(define *indent-size* 2)

;;; Named constants for form-specific formatting
(define *let-binding-offset* 3)    ; Characters after "(let* " before first binding
(define *defun-inline-args* 2)     ; Max args to keep on first line for defun
(define *max-cached-indent* 80)    ; Maximum indent level to cache

;;; ============================================================================
;;; Comment-Preserving Reader
;;; ============================================================================
;;; A pure Lisp reader that preserves comments and attaches them to expressions.
;;; Replaces read-sexp for the formatter to enable comment-aware formatting.

;;; ----------------------------------------------------------------------------
;;; File Reading Utility
;;; ----------------------------------------------------------------------------

(defun read-file-to-string (filename)
  "Read entire file contents into a string."
  (let ((file (open filename "r"))
         (lines '())
         (line nil))
    (unwind-protect
      (progn
        (do ()
          ((null? (set! line (read-line file))))
          (set! lines (cons line lines)))
        (join (reverse lines) "\n"))
      (close file))))

;;; ----------------------------------------------------------------------------
;;; Reader State Management
;;; ----------------------------------------------------------------------------
;;; State: (source position line column)

(defun make-reader-state (source)
  "Create a reader state for parsing SOURCE string."
  (list source 0 1 1))

(defun rs-source (state) (car state))
(defun rs-pos (state) (car (cdr state)))
(defun rs-line (state) (car (cdr (cdr state))))
(defun rs-col (state) (car (cdr (cdr (cdr state)))))

(defun rs-set-pos! (state pos) (set-car! (cdr state) pos))
(defun rs-set-line! (state line) (set-car! (cdr (cdr state)) line))
(defun rs-set-col! (state col) (set-car! (cdr (cdr (cdr state))) col))

(defun reader-peek (state)
  "Return current character without advancing, or nil at EOF."
  (let ((pos (rs-pos state))
         (src (rs-source state)))
    (if (>= pos (length src))
      nil
      (string-ref src pos))))

(defun reader-advance! (state)
  "Advance position by one character, updating line/column. Returns char."
  (let ((ch (reader-peek state)))
    (when ch
      (rs-set-pos! state (+ (rs-pos state) 1))
      (if (char=? ch #\newline)
        (progn
          (rs-set-line! state (+ (rs-line state) 1))
          (rs-set-col! state 1))
        (rs-set-col! state (+ (rs-col state) 1))))
    ch))

(defun reader-at-eof? (state)
  "Check if reader is at end of input."
  (null? (reader-peek state)))

;;; ----------------------------------------------------------------------------
;;; Character Utilities
;;; ----------------------------------------------------------------------------

(defun delimiter? (ch)
  "Check if character is a token delimiter."
  (or (null? ch)
    (char-whitespace? ch)
    (char=? ch #\()
    (char=? ch #\))
    (char=? ch #\;)
    (char=? ch #\")))

;;; ----------------------------------------------------------------------------
;;; Token Data Structure
;;; ----------------------------------------------------------------------------
;;; Token: (type value leading-comment line column)

(defun make-token (type value leading-comment line col)
  "Create a token with type, value, leading comment, and position."
  (list type value leading-comment line col))

(defun token-type (tok) (car tok))
(defun token-value (tok) (car (cdr tok)))
(defun token-comment (tok) (car (cdr (cdr tok))))
(defun token-line (tok) (car (cdr (cdr (cdr tok)))))
(defun token-col (tok) (car (cdr (cdr (cdr (cdr tok))))))

;;; ----------------------------------------------------------------------------
;;; Comment Reading
;;; ----------------------------------------------------------------------------

(defun read-line-comment (state)
  "Read a semicolon comment to end of line. Returns comment string including ;."
  (let ((start (rs-pos state))
         (src (rs-source state)))
    (do ()
      ((let ((ch (reader-peek state)))
         (or (null? ch) (char=? ch #\newline))))
      (reader-advance! state))
    (substring src start (rs-pos state))))

(defun skip-whitespace-collect-comments (state)
  "Skip whitespace and collect leading comments. Returns comment string or nil."
  (let ((comments '()))
    (do ()
      ((let ((ch (reader-peek state)))
         (not (or (and ch (char-whitespace? ch))
                (and ch (char=? ch #\;))))))
      (let ((ch (reader-peek state)))
        (cond
          ((char=? ch #\;)
            (set! comments (cons (read-line-comment state) comments)))
          (#t
            (reader-advance! state)))))
    (if (null? comments)
      nil
      (join (reverse comments) "\n"))))

(defun collect-inline-comment (state expr-end-line)
  "Check for inline comment on EXPR-END-LINE. Returns comment string or nil."
  (let ((found-comment nil))
    (do ()
      ((or found-comment
         (let ((ch (reader-peek state)))
           (or (null? ch)
             (char=? ch #\newline)
             (not (or (char=? ch #\space)
                    (char=? ch #\tab)
                    (char=? ch #\;)))))))
      (let ((ch (reader-peek state)))
        (cond
          ((char=? ch #\;)
            (if (= (rs-line state) expr-end-line)
              (set! found-comment (read-line-comment state))
              ;; Comment on different line - don't consume it
              nil))
          (#t
            (reader-advance! state)))))
    found-comment))

;;; ----------------------------------------------------------------------------
;;; String Token Reading
;;; ----------------------------------------------------------------------------

(defun read-octal-escape (state first-digit)
  "Read an octal escape sequence starting with FIRST-DIGIT."
  (let ((value (- (char-code first-digit) (char-code #\0)))
         (count 1))
    (do ()
      ((or (>= count 3)
         (let ((ch (reader-peek state)))
           (or (null? ch)
             (not (and (char>=? ch #\0) (char<=? ch #\7)))))))
      (let ((ch (reader-advance! state)))
        (set! value (+ (* value 8) (- (char-code ch) (char-code #\0))))
        (set! count (+ count 1))))
    (code-char value)))

(defun chars-to-string (char-list)
  "Convert list of characters to string efficiently using join.
   This is O(n) instead of O(n²) repeated concatenation."
  (let ((parts '()))
    (do ((remaining char-list (cdr remaining)))
      ((null? remaining))
      (set! parts (cons (char->string (car remaining)) parts)))
    (join (reverse parts) "")))

(defun read-string-token (state)
  "Read a string literal, handling escape sequences."
  (reader-advance! state) ; skip opening quote
  (let ((chars '()))  ; Accumulate characters in a list (O(n) total)
    (do ()
      ((let ((ch (reader-peek state)))
         (or (null? ch) (char=? ch #\"))))
      (let ((ch (reader-advance! state)))
        (if (char=? ch #\\)
          (let ((esc (reader-advance! state)))
            (set! chars
              (cons (cond
                      ((char=? esc #\n) #\newline)
                      ((char=? esc #\t) #\tab)
                      ((char=? esc #\r) #\return)
                      ((char=? esc #\\) #\\)
                      ((char=? esc #\") #\")
                      ((and (char>=? esc #\0) (char<=? esc #\7))
                        (read-octal-escape state esc))
                      (#t esc))
                chars)))
          (set! chars (cons ch chars)))))
    (reader-advance! state) ; skip closing quote
    (chars-to-string (reverse chars))))

;;; ----------------------------------------------------------------------------
;;; Atom Token Reading
;;; ----------------------------------------------------------------------------

(defun read-atom-token (state)
  "Read an atom (symbol or number) token."
  (let ((start (rs-pos state))
         (src (rs-source state)))
    (do ()
      ((delimiter? (reader-peek state)))
      (reader-advance! state))
    (substring src start (rs-pos state))))

;;; ----------------------------------------------------------------------------
;;; Character Literal Reading
;;; ----------------------------------------------------------------------------

(defun read-hex-digits (state count)
  "Read COUNT hex digits and return their numeric value."
  (let ((value 0)
         (i 0))
    (do ()
      ((>= i count))
      (let ((ch (reader-advance! state)))
        (cond
          ((and (char>=? ch #\0) (char<=? ch #\9))
            (set! value (+ (* value 16) (- (char-code ch) (char-code #\0)))))
          ((and (char>=? ch #\a) (char<=? ch #\f))
            (set! value (+ (* value 16) (+ 10 (- (char-code ch) (char-code #\a))))))
          ((and (char>=? ch #\A) (char<=? ch #\F))
            (set! value (+ (* value 16) (+ 10 (- (char-code ch) (char-code #\A)))))))
        (set! i (+ i 1))))
    value))

(defun parse-character-name (name)
  "Parse a character name like 'newline' or 'a' into a character."
  (cond
    ((string=? name "newline") #\newline)
    ((string=? name "space") #\space)
    ((string=? name "tab") #\tab)
    ((string=? name "return") #\return)
    ((= (length name) 1) (string-ref name 0))
    (#t (error (concat "Unknown character name: " name)))))

(defun read-character-literal (state)
  "Read a character literal like #\\a or #\\newline.
   Note: Called after # has been consumed. Consumes the backslash and character name."
  (reader-advance! state) ; skip backslash
  (let ((ch (reader-peek state)))
    (cond
      ((null? ch)
        (error "Unexpected EOF in character literal"))
      ((char=? ch #\x)
        (reader-advance! state)
        (code-char (read-hex-digits state 2)))
      ((char=? ch #\u)
        (reader-advance! state)
        (code-char (read-hex-digits state 4)))
      (#t
        ;; Read at least one character, then continue if not a delimiter
        (let ((start (rs-pos state))
               (src (rs-source state)))
          (reader-advance! state)  ; Read the first character
          ;; Continue reading for named characters like "newline"
          (do ()
            ((delimiter? (reader-peek state)))
            (reader-advance! state))
          (parse-character-name (substring src start (rs-pos state))))))))

;;; ----------------------------------------------------------------------------
;;; Hash Token Reading (#t, #f, #\, #()
;;; ----------------------------------------------------------------------------

(defun read-hash-token (state leading line col)
  "Read a # prefixed token."
  (reader-advance! state) ; skip #
  (let ((ch (reader-peek state)))
    (cond
      ((char=? ch #\t)
        (reader-advance! state)
        (make-token 'boolean #t leading line col))
      ((char=? ch #\f)
        (reader-advance! state)
        (make-token 'boolean #f leading line col))
      ((char=? ch #\()
        (reader-advance! state)
        (make-token 'vector-start nil leading line col))
      ((char=? ch #\\)
        (let ((char-val (read-character-literal state)))
          (make-token 'char char-val leading line col)))
      (#t
        (error (format nil "Unknown # syntax: #~A at line ~A" ch line))))))

;;; ----------------------------------------------------------------------------
;;; Main Tokenizer
;;; ----------------------------------------------------------------------------

(defun read-token (state)
  "Read the next token with any leading comments."
  (let* ((leading (skip-whitespace-collect-comments state))
          (line (rs-line state))
          (col (rs-col state))
          (ch (reader-peek state)))
    (cond
      ((null? ch)
        (make-token 'eof nil leading line col))
      ((char=? ch #\()
        (reader-advance! state)
        (make-token 'lparen nil leading line col))
      ((char=? ch #\))
        (reader-advance! state)
        (make-token 'rparen nil leading line col))
      ((char=? ch #\')
        (reader-advance! state)
        (make-token 'quote nil leading line col))
      ((char=? ch #\`)
        (reader-advance! state)
        (make-token 'backquote nil leading line col))
      ((char=? ch #\,)
        (reader-advance! state)
        (if (and (reader-peek state) (char=? (reader-peek state) #\@))
          (progn
            (reader-advance! state)
            (make-token 'unquote-splicing nil leading line col))
          (make-token 'unquote nil leading line col)))
      ((char=? ch #\")
        (let ((str-val (read-string-token state)))
          (make-token 'string str-val leading line col)))
      ((char=? ch #\#)
        (read-hash-token state leading line col))
      ((char=? ch #\.)
        (let ((next-pos (+ (rs-pos state) 1))
               (src (rs-source state)))
          (if (or (>= next-pos (length src))
                (delimiter? (string-ref src next-pos)))
            (progn
              (reader-advance! state)
              (make-token 'dot nil leading line col))
            (let ((atom-val (read-atom-token state)))
              (make-token 'atom atom-val leading line col)))))
      (#t
        (let ((atom-val (read-atom-token state)))
          (make-token 'atom atom-val leading line col))))))

;;; ----------------------------------------------------------------------------
;;; Annotated Expression Data Structure
;;; ----------------------------------------------------------------------------
;;; Wraps an S-expression with comment metadata.
;;; Structure: (annotated value comment-before comment-after)

(defun make-annotated (value before after)
  "Create an annotated expression with comments."
  (list 'annotated value before after))

(defun annotated? (obj)
  "Check if OBJ is an annotated expression."
  (and (pair? obj) (eq? (car obj) 'annotated)))

(defun ann-value (ann) (car (cdr ann)))
(defun ann-before (ann) (car (cdr (cdr ann))))
(defun ann-after (ann) (car (cdr (cdr (cdr ann)))))

(defun ann-set-after! (ann comment)
  "Set the inline comment on an annotated expression."
  (set-car! (cdr (cdr (cdr ann))) comment))

;;; Comment access functions for annotated expressions
;;; These are called by the formatter

(defun comment-before (sexp)
  "Get leading comment from expression (works with annotated or plain)."
  (if (annotated? sexp)
    (ann-before sexp)
    nil))

(defun comment-after (sexp)
  "Get inline comment from expression (works with annotated or plain)."
  (if (annotated? sexp)
    (ann-after sexp)
    nil))

;;; ----------------------------------------------------------------------------
;;; Parser State
;;; ----------------------------------------------------------------------------
;;; Parser uses one-token lookahead.

(defun make-parser-state (reader-state)
  "Create parser state with one-token lookahead.
   State: (reader-state current-token last-inline-comment)"
  (let ((first-token (read-token reader-state)))
    (list reader-state first-token nil)))

(defun parser-reader (pstate) (car pstate))
(defun parser-current (pstate) (car (cdr pstate)))
(defun parser-last-inline (pstate) (car (cdr (cdr pstate))))
(defun parser-set-last-inline! (pstate val) (set-car! (cdr (cdr pstate)) val))

(defun parser-advance! (pstate)
  "Consume current token and read next. Returns consumed token.
   Also collects any inline comment on the same line and stores it."
  (let* ((current (parser-current pstate))
          (end-line (rs-line (parser-reader pstate))))
    ;; Collect inline comment BEFORE read-token consumes it
    (parser-set-last-inline! pstate
      (collect-inline-comment (parser-reader pstate) end-line))
    ;; Now read next token
    (set-car! (cdr pstate) (read-token (parser-reader pstate)))
    current))

;;; ----------------------------------------------------------------------------
;;; Atom Value Parsing
;;; ----------------------------------------------------------------------------

(defun parse-atom-value (str)
  "Convert atom string to appropriate Lisp value."
  (cond
    ((string=? str "nil") nil)
    ((string=? str "#t") #t)
    ((string=? str "#f") #f)
    (#t
      (let ((num (string->number str)))
        (if num num (string->symbol str))))))

;;; ----------------------------------------------------------------------------
;;; Expression Parser
;;; ----------------------------------------------------------------------------

(defun parse-expression (pstate)
  "Parse one expression, returning annotated result."
  (let* ((tok (parser-current pstate))
          (type (token-type tok))
          (leading (token-comment tok)))
    (cond
      ((eq? type 'eof)
        nil)
      ((eq? type 'lparen)
        (parse-list pstate leading))
      ((eq? type 'quote)
        (parse-quoted pstate 'quote leading))
      ((eq? type 'backquote)
        (parse-quoted pstate 'quasiquote leading))
      ((eq? type 'unquote)
        (parse-quoted pstate 'unquote leading))
      ((eq? type 'unquote-splicing)
        (parse-quoted pstate 'unquote-splicing leading))
      ((eq? type 'vector-start)
        (parse-vector pstate leading))
      ((eq? type 'atom)
        (parser-advance! pstate)  ; Collects inline comment
        (make-annotated (parse-atom-value (token-value tok)) leading
          (parser-last-inline pstate)))
      ((eq? type 'string)
        (parser-advance! pstate)
        (make-annotated (token-value tok) leading (parser-last-inline pstate)))
      ((eq? type 'boolean)
        (parser-advance! pstate)
        (make-annotated (token-value tok) leading (parser-last-inline pstate)))
      ((eq? type 'char)
        (parser-advance! pstate)
        (make-annotated (token-value tok) leading (parser-last-inline pstate)))
      (#t
        (error (format nil "Unexpected token type: ~A at line ~A"
                 type (token-line tok)))))))

(defun parse-list (pstate leading-comment)
  "Parse a list expression (a b c) or dotted pair (a . b)."
  (parser-advance! pstate) ; consume lparen
  (let ((elements '())
         (dotted-tail nil))
    (do ()
      ((let ((tok (parser-current pstate)))
         (or (eq? (token-type tok) 'rparen)
           (eq? (token-type tok) 'dot)
           (eq? (token-type tok) 'eof))))
      (set! elements (cons (parse-expression pstate) elements)))
    ;; Check for dotted pair
    (when (eq? (token-type (parser-current pstate)) 'dot)
      (parser-advance! pstate) ; consume dot
      (set! dotted-tail (parse-expression pstate)))
    ;; Expect closing paren
    (if (eq? (token-type (parser-current pstate)) 'rparen)
      (parser-advance! pstate)  ; This collects inline comment into parser state
      (error (format nil "Expected ) at line ~A"
               (token-line (parser-current pstate)))))
    ;; Build the list from annotated elements
    ;; Note: elements are in reverse order from parsing, build-list-from-annotated handles this
    (let* ((inline (parser-last-inline pstate))  ; Get inline comment from parser state
            (result (build-list-from-annotated elements dotted-tail)))
      (make-annotated result leading-comment inline))))

(defun build-list-from-annotated (elements dotted-tail)
  "Build a list structure from annotated elements.
   Elements come in reversed order (last element first)."
  (let ((result (if dotted-tail (ann-value dotted-tail) nil)))
    ;; Elements are in reversed order, so just cons them
    (do ((remaining elements (cdr remaining)))
      ((null? remaining) result)
      (set! result (cons (car remaining) result)))))

(defun parse-quoted (pstate quote-sym leading-comment)
  "Parse 'expr, `expr, ,expr, or ,@expr."
  (parser-advance! pstate) ; consume quote token
  (let ((inner (parse-expression pstate)))
    (make-annotated (list quote-sym inner) leading-comment nil)))

(defun list-to-vector (lst)
  "Convert a list to a vector."
  (let* ((len (length lst))
          (vec (make-vector len nil))
          (i 0))
    (do ((remaining lst (cdr remaining)))
      ((null? remaining) vec)
      (vector-set! vec i (car remaining))
      (set! i (+ i 1)))))

(defun parse-vector (pstate leading-comment)
  "Parse #(elements...)."
  (parser-advance! pstate) ; consume vector-start
  (let ((elements '()))
    (do ()
      ((eq? (token-type (parser-current pstate)) 'rparen))
      (set! elements (cons (parse-expression pstate) elements)))
    (parser-advance! pstate) ; consume rparen - collects inline comment
    (let ((inline (parser-last-inline pstate)))
      (make-annotated (list-to-vector (reverse elements)) leading-comment inline))))

;;; ----------------------------------------------------------------------------
;;; Top-Level Reader
;;; ----------------------------------------------------------------------------

(defun read-file-with-comments (filename)
  "Read all expressions from file with comment preservation."
  (let* ((source (read-file-to-string filename))
          (rstate (make-reader-state source))
          (pstate (make-parser-state rstate))
          (expressions '()))
    (do ()
      ((eq? (token-type (parser-current pstate)) 'eof))
      (let ((expr (parse-expression pstate)))
        (when expr
          (set! expressions (cons expr expressions)))))
    (reverse expressions)))

;;; ============================================================================
;;; Form-Specific Rules Table
;;; ============================================================================
;;; Rules define how different forms should be formatted.
;;; Each rule is an alist with properties:
;;;   handler   - Symbol of function to call for formatting
;;;   inline    - Number of elements to keep inline (for special-form)
;;;   body-indent - Indent increment for body (default: *indent-size*)

(define *form-rules*
  '((cond . ((handler . format-cond-form)))
     (case . ((handler . format-cond-form)))
     (if . ((handler . format-if-form)))))

(defun get-form-rule (sym prop)
  "Get a property from the form rules table for SYM."
  (let ((form-entry (assoc sym *form-rules*)))
    (if form-entry
      (let ((prop-entry (assoc prop (cdr form-entry))))
        (if prop-entry (cdr prop-entry) nil))
      nil)))

;;; ============================================================================
;;; Indent String Caching
;;; ============================================================================
;;; Memoize indent strings to avoid O(n) string building on every call.

(define *indent-cache* (make-vector *max-cached-indent* nil))

(defun build-indent-string (n)
  "Build an indent string of n spaces (uncached, O(n))."
  (let ((result ""))
    (do ((i 0 (+ i 1)))
      ((>= i n) result)
      (set! result (concat result " ")))))

;;; ============================================================================
;;; Comment Formatting Helpers
;;; ============================================================================

(defun format-leading-comment (comment-text indent)
  "Format leading comment with proper indentation.
   Handles multi-line comments - first line has no indent (caller provides it),
   subsequent lines are indented."
  (if (null? comment-text)
    ""
    (let* ((indent-str (make-indent indent))
            (lines (string-split comment-text "\n"))
            (formatted-lines '())
            (first-line #t))
      ;; First line has no indent (caller provides it), subsequent lines are indented
      (do ((remaining lines (cdr remaining)))
        ((null? remaining))
        (let ((line (car remaining)))
          (if first-line
            (progn
              (set! formatted-lines (cons line formatted-lines))
              (set! first-line #f))
            (set! formatted-lines
              (cons (concat indent-str line) formatted-lines)))))
      ;; Join and add trailing newline
      (concat (join (reverse formatted-lines) "\n") "\n"))))

(defun format-inline-comment (comment-text)
  "Format inline comment (adds space before if needed).
   Comment-text already includes the semicolon."
  (if (null? comment-text)
    ""
    (concat " " comment-text)))

;;; ============================================================================
;;; Line Builder Abstraction
;;; ============================================================================
;;; A line builder accumulates formatted output efficiently using lists
;;; instead of repeated string concatenation (O(n) vs O(n²)).
;;;
;;; State: (base-indent completed-lines current-parts current-col)

(defun lb-create (indent)
  "Create a new line builder starting at given indent."
  (list indent '() '() indent))

(defun lb-base-indent (lb) (car lb))
(defun lb-lines (lb) (car (cdr lb)))
(defun lb-parts (lb) (car (cdr (cdr lb))))
(defun lb-col (lb) (car (cdr (cdr (cdr lb)))))

(defun lb-set-lines! (lb val) (set-car! (cdr lb) val))
(defun lb-set-parts! (lb val) (set-car! (cdr (cdr lb)) val))
(defun lb-set-col! (lb val) (set-car! (cdr (cdr (cdr lb))) val))

(defun lb-append! (lb str)
  "Append a string to the current line."
  (lb-set-parts! lb (cons str (lb-parts lb)))
  (lb-set-col! lb (+ (lb-col lb) (length str))))

(defun lb-append-space! (lb)
  "Append a space to the current line."
  (lb-append! lb " "))

(defun lb-newline! (lb &optional new-indent)
  "Finish current line and start a new one at optional indent."
  (let ((indent (if new-indent new-indent (lb-base-indent lb)))
         (line (join (reverse (lb-parts lb)) "")))
    (lb-set-lines! lb (cons line (lb-lines lb)))
    (lb-set-parts! lb (list (make-indent indent)))
    (lb-set-col! lb indent)))

(defun lb-finish (lb)
  "Return the final formatted string."
  (let* ((parts (lb-parts lb))
          (final-line (if (null? parts) "" (join (reverse parts) "")))
          (lines (lb-lines lb))
          (all-lines (if (and (null? lines) (string=? final-line ""))
                       '("")
                       (reverse (cons final-line lines)))))
    (join all-lines "\n")))

;;; ============================================================================
;;; List Traversal Helpers
;;; ============================================================================

(defun list-has-dotted-tail? (lst)
  "Check if list has a dotted (improper) tail."
  (cond
    ((null? lst) #f)
    ((not (pair? lst)) #t)
    (#t (list-has-dotted-tail? (cdr lst)))))

(defun get-dotted-tail (lst)
  "Get the dotted tail of an improper list, or nil."
  (cond
    ((null? lst) nil)
    ((not (pair? lst)) lst)
    (#t (get-dotted-tail (cdr lst)))))

;;; ============================================================================
;;; S-expression to string conversion (single line)
;;; ============================================================================

(defun quote-shorthand? (sexp sym)
  "Check if sexp is (SYM x) - a two-element list starting with SYM."
  (and (pair? sexp)
    (symbol? (car sexp))
    (eq? (car sexp) sym)
    (pair? (cdr sexp))
    (null? (cdr (cdr sexp)))))

(defun sexp-to-string (sexp)
  "Convert s-expression to single-line string representation."
  (cond
    ((annotated? sexp) (sexp-to-string (ann-value sexp)))
    ((null? sexp) "nil")
    ((eq? sexp #t) "#t")
    ((eq? sexp #f) "#f")
    ((number? sexp) (number->string sexp))
    ((string? sexp) (format nil "~S" sexp))
    ((symbol? sexp) (symbol->string sexp))
    ((char? sexp) (format nil "~S" sexp))
    ((vector? sexp) (vector-to-string sexp))
    ;; Quote/quasiquote shorthand - check before generic list handling
    ((quote-shorthand? sexp 'quote)
      (concat "'" (sexp-to-string (car (cdr sexp)))))
    ((quote-shorthand? sexp 'quasiquote)
      (concat "`" (sexp-to-string (car (cdr sexp)))))
    ((quote-shorthand? sexp 'unquote)
      (concat "," (sexp-to-string (car (cdr sexp)))))
    ((quote-shorthand? sexp 'unquote-splicing)
      (concat ",@" (sexp-to-string (car (cdr sexp)))))
    ((list? sexp) (list-to-string sexp))
    ((pair? sexp) (list-to-string sexp))
    (#t (format nil "~A" sexp))))

(defun vector-to-string (vec)
  "Convert vector to string like #(a b c)."
  (let ((parts '())
         (len (length vec)))
    (do ((i 0 (+ i 1)))
      ((>= i len))
      (set! parts (cons (sexp-to-string (vector-ref vec i)) parts)))
    (concat "#(" (join (reverse parts) " ") ")")))

(defun list-to-string (lst)
  "Convert list to string like (a b c)."
  (cond
    ((null? lst) "()")
    ((not (pair? lst))
      ;; Not actually a list - just convert to string
      (sexp-to-string lst))
    (#t
      (let ((parts '())
             (current lst))
        ;; Handle proper list
        (do ()
          ((not (pair? current)))
          (set! parts (cons (sexp-to-string (car current)) parts))
          (set! current (cdr current)))
        ;; Handle dotted pair
        (if (not (null? current))
          (concat "(" (join (reverse parts) " ") " . " (sexp-to-string current) ")")
          (concat "(" (join (reverse parts) " ") ")"))))))

;;; ============================================================================
;;; Length calculation
;;; ============================================================================

(defun sexp-length (sexp)
  "Calculate the string length of an s-expression."
  (length (sexp-to-string sexp)))

;;; ============================================================================
;;; Pretty printing with alignment
;;; ============================================================================

(defun make-indent (n)
  "Create a string of n spaces (cached for common values)."
  (cond
    ((< n 0) "")
    ((< n *max-cached-indent*)
      (let ((cached (vector-ref *indent-cache* n)))
        (if cached
          cached
          (let ((s (build-indent-string n)))
            (vector-set! *indent-cache* n s)
            s))))
    (#t (build-indent-string n))))

(defun has-nested-comments? (sexp)
  "Check if sexp or any nested element has comments attached."
  (cond
    ((null? sexp) #f)
    ((annotated? sexp)
      ;; Annotated - check comments on this and recurse into value
      (or (ann-before sexp)
        (ann-after sexp)
        (has-nested-comments? (ann-value sexp))))
    ((not (pair? sexp))
      ;; Plain atom - no comments possible
      #f)
    (#t
      ;; Plain list - recurse on elements
      (or (has-nested-comments? (car sexp))
        (has-nested-comments? (cdr sexp))))))

(defun fits-on-line? (sexp col)
  "Check if sexp fits on line starting at column col."
  (<= (+ col (sexp-length sexp)) *max-column*))

(defun special-form? (sym)
  "Check if symbol is a special form requiring body indentation."
  (member sym '(define defun defvar defconst defmacro lambda
                 if cond case when unless progn do unwind-protect
                 condition-case)))

(defun let-form? (sym)
  "Check if symbol is a let/let* form requiring special binding alignment."
  (member sym '(let let*)))

(defun format-sexp-inner (sexp indent)
  "Format s-expression without comment handling. Returns string."
  (cond
    ;; Atoms - just convert to string
    ((not (pair? sexp))
      (sexp-to-string sexp))
    ;; Empty list
    ((null? sexp)
      "()")
    ;; If has nested comments, must use multi-line to preserve them
    ((has-nested-comments? sexp)
      (format-list-multiline sexp indent))
    ;; Try single line if no nested comments and fits
    ((fits-on-line? sexp indent)
      (sexp-to-string sexp))
    ;; Multi-line formatting needed
    (#t
      (format-list-multiline sexp indent))))

(defun unwrap-value (sexp)
  "Get the actual value from a possibly-annotated expression."
  (if (annotated? sexp)
    (ann-value sexp)
    sexp))

(defun format-sexp (sexp indent)
  "Format s-expression with given base indentation. Returns string.
   Preserves leading and inline comments attached to the expression."
  (let* ((before (comment-before sexp))
          (after (comment-after sexp))
          (value (unwrap-value sexp))
          (formatted (format-sexp-inner value indent))
          (leading-comment (format-leading-comment before indent))
          (inline-comment (format-inline-comment after)))
    ;; Build result: leading comment + indentation + expression + inline comment
    (let ((result formatted))
      ;; Add inline comment if present
      (when after
        (set! result (concat result inline-comment)))
      ;; Add leading comment if present (with proper indentation)
      (when (and before (not (string=? leading-comment "")))
        (set! result (concat leading-comment (make-indent indent) result)))
      result)))

(defun format-list-multiline (lst indent)
  "Format a list across multiple lines."
  (let* ((head-raw (car lst))
          (head (unwrap-value head-raw))
          (head-is-symbol (symbol? head))
          (is-special (and head-is-symbol (special-form? head)))
          (is-let (and head-is-symbol (let-form? head)))
          ;; Check form rules table for custom handler
          (form-handler (and head-is-symbol (get-form-rule head 'handler))))
    (cond
      ;; Custom handler from form rules table
      (form-handler
        ((eval form-handler) lst indent))
      ;; Let forms: (let ((var val) ...) body...)
      (is-let
        (format-let-form lst indent))
      ;; Special forms: (define name ...) or (defun name (args) ...)
      (is-special
        (format-special-form lst indent))
      ;; Quoted list: '(...)
      ((and head-is-symbol (string=? (symbol->string head) "quote"))
        (format-quoted-list lst indent))
      ;; Quasiquoted list: `(...)
      ((and head-is-symbol (string=? (symbol->string head) "quasiquote"))
        (format-quasiquoted-expr lst indent))
      ;; Unquoted expression: ,expr
      ((and head-is-symbol (string=? (symbol->string head) "unquote"))
        (format-unquoted-expr lst indent ","))
      ;; Spliced expression: ,@expr
      ((and head-is-symbol (string=? (symbol->string head) "unquote-splicing"))
        (format-unquoted-expr lst indent ",@"))
      ;; Regular list - align elements under first element
      (#t
        (format-aligned-list lst indent)))))

(defun format-let-form (lst indent)
  "Format let/let* forms with proper binding alignment.
   (let* ((var1 val1)
          (var2 val2))
     body)"
  (let* ((head (unwrap-value (car lst)))
          (head-str (symbol->string head))
          (rest (cdr lst))
          (bindings-raw (if (pair? rest) (car rest) '()))
          (bindings (unwrap-value bindings-raw))
          (body (if (pair? rest) (cdr rest) '()))
          (body-indent (+ indent *indent-size*))
          ;; Bindings start after "(let* ("
          (binding-indent (+ indent (length head-str) *let-binding-offset*))
          (lb (lb-create indent)))
    ;; Start: (let* (
    (lb-append! lb "(")
    (lb-append! lb head-str)
    (lb-append! lb " (")
    ;; Format bindings - each on its own line for clarity
    (if (pair? bindings)
      (let ((first-binding #t))
        (do ((remaining bindings (cdr remaining)))
          ((not (pair? remaining)))
          (let ((binding-str (format-sexp (car remaining) binding-indent)))
            (if first-binding
              (progn
                (lb-append! lb binding-str)
                (set! first-binding #f))
              (progn
                (lb-newline! lb binding-indent)
                (lb-append! lb binding-str)))))
        (lb-append! lb ")")
        (lb-newline! lb body-indent))
      ;; Empty bindings
      (progn
        (lb-append! lb ")")
        (lb-newline! lb body-indent)))
    ;; Format body forms
    (do ((remaining body (cdr remaining))
          (first-body #t #f))
      ((not (pair? remaining)))
      (unless first-body
        (lb-newline! lb body-indent))
      (lb-append! lb (format-sexp (car remaining) body-indent)))
    ;; Close let form
    (lb-append! lb ")")
    (lb-finish lb)))

(defun format-special-form (lst indent)
  "Format special forms like define, defun, let, etc."
  (let* ((head (unwrap-value (car lst)))
          (head-str (if (symbol? head) (symbol->string head) (sexp-to-string head)))
          (rest (cdr lst))
          (body-indent (+ indent *indent-size*))
          (lb (lb-create body-indent))
          (elem-count 0))
    ;; Start with opening paren and head
    (lb-append! lb "(")
    (lb-append! lb head-str)
    ;; Add elements
    (do ((remaining rest (if (pair? remaining) (cdr remaining) nil)))
      ((not (pair? remaining))
	;; Handle dotted tail
	(when (not (null? remaining))
          (lb-append! lb " . ")
          (lb-append! lb (sexp-to-string remaining))))
      (let* ((elem (car remaining))
              (elem-single-len (sexp-length elem))
              (try-len (+ (lb-col lb) 1 elem-single-len)))
        (set! elem-count (+ elem-count 1))
        ;; First *defun-inline-args* elements try to stay on same line
        (if (and (<= elem-count *defun-inline-args*) (<= try-len *max-column*))
          (progn
            (lb-append-space! lb)
            (lb-append! lb (format-sexp elem (lb-col lb))))
          ;; New line with body indentation
          (progn
            (lb-newline! lb body-indent)
            (lb-append! lb (format-sexp elem body-indent))))))
    ;; Close form
    (lb-append! lb ")")
    (lb-finish lb)))

(defun format-quoted-list (lst indent)
  "Format quoted list like '(a b c ...)."
  (let* ((inner (car (cdr lst)))
          ;; Format with indent+1 to account for the quote character
          (inner-fmt (format-sexp inner (+ indent 1))))
    ;; Always keep quote attached to content
    (concat "'" inner-fmt)))

(defun format-quasiquoted-expr (lst indent)
  "Format quasiquoted expression like `(...)."
  (let* ((inner (car (cdr lst)))
          ;; Format with indent+1 to account for the backtick character
          (inner-fmt (format-sexp inner (+ indent 1))))
    (concat "`" inner-fmt)))

(defun format-unquoted-expr (lst indent prefix)
  "Format unquoted expression with PREFIX (, or ,@)."
  (let* ((inner (car (cdr lst)))
          ;; Format with indent + prefix length
          (inner-fmt (format-sexp inner (+ indent (length prefix)))))
    (concat prefix inner-fmt)))

(defun format-cond-form (lst indent)
  "Format cond/case forms with each clause on its own line.
   (cond
     ((test1) result1)
     ((test2) result2)
     (#t default))"
  (let* ((head (unwrap-value (car lst)))
          (head-str (symbol->string head))
          (clauses (cdr lst))
          (clause-indent (+ indent *indent-size*))
          (lb (lb-create indent)))
    ;; Start: (cond
    (lb-append! lb "(")
    (lb-append! lb head-str)
    ;; Format each clause on its own line
    (do ((remaining clauses (cdr remaining)))
      ((not (pair? remaining)))
      (lb-newline! lb clause-indent)
      (lb-append! lb (format-sexp (car remaining) clause-indent)))
    ;; Close form
    (lb-append! lb ")")
    (lb-finish lb)))

(defun format-if-form (lst indent)
  "Format if forms with condition inline, then/else on separate lines if needed.
   (if condition
     then-expr
     else-expr)"
  (let* ((head (car lst))
          (rest (cdr lst))
          ;; Track existence of parts, not just values (nil is a valid value!)
          (has-condition (pair? rest))
          (has-then (and has-condition (pair? (cdr rest))))
          (has-else (and has-then (pair? (cdr (cdr rest)))))
          (condition (if has-condition (car rest) nil))
          (then-part (if has-then (car (cdr rest)) nil))
          (else-part (if has-else (car (cdr (cdr rest))) nil))
          (body-indent (+ indent *indent-size*))
          (lb (lb-create indent)))
    ;; Start: (if
    (lb-append! lb "(if")
    ;; Condition - try to keep inline if short enough
    (when has-condition
      (let ((cond-str (format-sexp condition (lb-col lb))))
        (if (<= (+ (lb-col lb) 1 (length cond-str)) *max-column*)
          (progn
            (lb-append-space! lb)
            (lb-append! lb cond-str))
          (progn
            (lb-newline! lb body-indent)
            (lb-append! lb (format-sexp condition body-indent))))))
    ;; Then part
    (when has-then
      (lb-newline! lb body-indent)
      (lb-append! lb (format-sexp then-part body-indent)))
    ;; Else part (if present)
    (when has-else
      (lb-newline! lb body-indent)
      (lb-append! lb (format-sexp else-part body-indent)))
    ;; Close form
    (lb-append! lb ")")
    (lb-finish lb)))

(defun format-aligned-list (lst indent)
  "Format list with elements aligned under first element."
  (let* ((elem-indent (+ indent 1))  ; After opening paren
          (lb (lb-create elem-indent))
          (dotted-tail nil))
    ;; Start with opening paren
    (lb-set-parts! lb '("("))
    (lb-set-col! lb (+ indent 1))
    ;; Process each element
    (do ((remaining lst (if (pair? remaining) (cdr remaining) nil))
          (first-elem #t #f))
      ((not (pair? remaining))
	(if (not (null? remaining))
          (set! dotted-tail remaining)))
      (let* ((elem (car remaining))
              (elem-single-len (sexp-length elem))
              (space-needed (if first-elem 0 1))
              (try-col (if first-elem (lb-col lb) (+ (lb-col lb) 1)))
              (elem-str (format-sexp elem try-col))
              (is-multiline (string-contains? elem-str "\n")))
        (if (and (not first-elem)
              (or (> (+ (lb-col lb) space-needed elem-single-len) *max-column*)
                is-multiline))
          ;; New line needed
          (let ((elem-str-aligned (if is-multiline
                                    (format-sexp elem elem-indent)
                                    elem-str)))
            (lb-newline! lb elem-indent)
            (lb-append! lb elem-str-aligned)
            ;; Track column after first line of multi-line element
            (let ((first-newline (string-index elem-str-aligned "\n")))
              (when first-newline
                (lb-set-col! lb (+ elem-indent first-newline)))))
          ;; Fits on current line
          (progn
            (unless first-elem (lb-append-space! lb))
            (lb-append! lb elem-str)))))
    ;; Handle dotted tail
    (when dotted-tail
      (let* ((tail-single-len (sexp-length dotted-tail))
              (tail-needed (+ 3 tail-single-len)))
        (if (> (+ (lb-col lb) tail-needed 1) *max-column*)
          (progn
            (lb-newline! lb elem-indent)
            (lb-append! lb ". ")
            (lb-append! lb (format-sexp dotted-tail elem-indent)))
          (progn
            (lb-append! lb " . ")
            (lb-append! lb (format-sexp dotted-tail (lb-col lb)))))))
    ;; Close and finish
    (lb-append! lb ")")
    (lb-finish lb)))

;;; ============================================================================
;;; File processing
;;; ============================================================================

(defun looks-like-expr-list? (sexps)
  "Check if sexps looks like a list of multiple expressions.
   Returns #t if sexps is a list where first element is also a list
   starting with a symbol (common pattern for top-level forms)."
  (and (pair? sexps)
    (pair? (car sexps))
    (symbol? (car (car sexps)))))

(defun format-file (filename)
  "Format a Lisp file and return formatted content as string."
  (let ((sexps (read-file-with-comments filename))
         (results '())
         (expr-num 0))
    ;; read-file-with-comments returns list of annotated expressions
    ;; Format each top-level expression
    (do ((remaining sexps (cdr remaining)))
      ((null? remaining))
      (set! expr-num (+ expr-num 1))
      (condition-case err
        (set! results (cons (format-sexp (car remaining) 0) results))
        (error
          (princ "Error formatting expression #")
          (princ expr-num)
          (princ ": ")
          (princ (error-message err))
          (terpri)
          (princ "Expression head: ")
          (let ((expr (car remaining)))
            (if (annotated? expr)
              (let ((val (ann-value expr)))
                (if (pair? val)
                  (princ (car val))
                  (princ val)))
              (if (pair? expr)
                (princ (car expr))
                (princ expr))))
          (terpri)
          (signal 'format-error (error-message err)))))
    ;; Join with blank lines between top-level forms
    (concat (join (reverse results) "\n\n") "\n")))

(defun format-file-inplace (filename)
  "Format a Lisp file in-place."
  (let ((content (format-file filename))
         (file (open filename "w")))
    (write-line file content)
    (close file)
    (princ "Formatted: ")
    (princ filename)
    (terpri)))

;;; ============================================================================
;;; Main entry point
;;; ============================================================================

(defun main ()
  "Main entry point - process command line arguments."
  (let ((args *command-line-args*)
         (inplace #f)
         (files '()))
    ;; Parse arguments
    (do ((remaining args (cdr remaining)))
      ((null? remaining))
      (let ((arg (car remaining)))
        (cond
          ((string=? arg "-i") (set! inplace #t))
          ((string=? arg "--inplace") (set! inplace #t))
          (#t (set! files (cons arg files))))))
    ;; Process files
    (set! files (reverse files))
    (if (null? files)
      (progn
        (princ "Usage: lisp-repl tools/lisp-fmt.lisp [-i] file.lisp ...\n")
        (princ "  -i, --inplace  Edit files in place\n"))
      (do ((remaining files (cdr remaining)))
        ((null? remaining))
        (let ((file (car remaining)))
          (if inplace
            (format-file-inplace file)
            (princ (format-file file))))))))

;; Run main only if *command-line-args* is defined (script mode)
(if (bound? '*command-line-args*)
  (main))
