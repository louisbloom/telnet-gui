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

(defun sexp-to-string (sexp)
  "Convert s-expression to single-line string representation."
  (cond
    ((null? sexp) "nil")
    ((eq? sexp #t) "#t")
    ((eq? sexp #f) "#f")
    ((number? sexp) (number->string sexp))
    ((string? sexp) (format nil "~S" sexp))
    ((symbol? sexp) (symbol->string sexp))
    ((char? sexp) (format nil "~S" sexp))
    ((vector? sexp) (vector-to-string sexp))
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
    ((not (pair? sexp))
      ;; Atom - check for comments on this object
      (or (comment-before sexp) (comment-after sexp)))
    (#t
      ;; List - check this cons cell and recurse
      (or (comment-before sexp)
          (comment-after sexp)
          (has-nested-comments? (car sexp))
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

(defun format-sexp (sexp indent)
  "Format s-expression with given base indentation. Returns string.
   Preserves leading comments attached to the expression."
  (let* ((before (comment-before sexp))
         (formatted (format-sexp-inner sexp indent))
         (leading-comment (format-leading-comment before indent)))
    ;; When there's a leading comment, we need to add indentation before the expression
    ;; because the comment ends with a newline
    ;; Note: Inline comments (comment-after) are not yet supported due to complexity
    (if (and before (not (string=? leading-comment "")))
      (concat leading-comment (make-indent indent) formatted)
      formatted)))

(defun format-list-multiline (lst indent)
  "Format a list across multiple lines."
  (let* ((head (car lst))
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
      ;; Regular list - align elements under first element
      (#t
        (format-aligned-list lst indent)))))

(defun format-let-form (lst indent)
  "Format let/let* forms with proper binding alignment.
   (let* ((var1 val1)
          (var2 val2))
     body)"
  (let* ((head (car lst))
         (head-str (symbol->string head))
         (rest (cdr lst))
         (bindings (if (pair? rest) (car rest) '()))
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
  (let* ((head (car lst))
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

(defun format-cond-form (lst indent)
  "Format cond/case forms with each clause on its own line.
   (cond
     ((test1) result1)
     ((test2) result2)
     (#t default))"
  (let* ((head (car lst))
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
  (let ((sexps (read-sexp filename))
        (results '())
        (expr-num 0))
    ;; read-sexp returns list of expressions from file
    ;; Detect if we got a list of expressions or a single expression
    (unless (looks-like-expr-list? sexps)
      (set! sexps (list sexps)))
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
          (if (pair? (car remaining))
            (princ (car (car remaining)))
            (princ (car remaining)))
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
