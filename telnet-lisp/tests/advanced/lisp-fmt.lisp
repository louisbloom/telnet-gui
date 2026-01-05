;;; Lisp Source Code Formatter Tests
;;; Tests for telnet-lisp/lisp/lisp-fmt.lisp
(load "tests/test-helpers.lisp")
(load "lisp/lisp-fmt.lisp")

;; ============================================================
;; Basic Atom Formatting
;; ============================================================
(assert-equal (sexp-to-string 42) "42" "integer formatting")
(assert-equal (sexp-to-string 3.14) "3.14" "float formatting")
(assert-equal (sexp-to-string "hello") "\"hello\"" "string formatting")
(assert-equal (sexp-to-string 'foo) "foo" "symbol formatting")
(assert-equal (sexp-to-string #t) "#t" "true formatting")
;; Note: In this Lisp, #f and nil are the same value
(assert-equal (sexp-to-string nil) "nil" "nil/false formatting")
;; ============================================================
;; Simple List Formatting
;; ============================================================
;; Note: '() equals nil in this Lisp, which formats as "nil"
(assert-equal (sexp-to-string 'nil) "nil" "empty list is nil")
(assert-equal (sexp-to-string '(a)) "(a)" "single element list")
(assert-equal (sexp-to-string '(a b c)) "(a b c)" "simple list")
(assert-equal (sexp-to-string '(+ 1 2)) "(+ 1 2)" "function call list")
(assert-equal (sexp-to-string '((a b) (c d))) "((a b) (c d))" "nested lists")
;; ============================================================
;; Vector Formatting
;; ============================================================
(assert-equal (sexp-to-string #()) "#()" "empty vector")
(assert-equal (sexp-to-string #(1 2 3)) "#(1 2 3)" "simple vector")
;; ============================================================
;; Quote/Quasiquote Shorthand
;; ============================================================
;; Quote shorthand
(assert-equal (sexp-to-string '(quote x)) "'x" "quote outputs as apostrophe")
(assert-equal (sexp-to-string '(quote (a b c))) "'(a b c)" "quoted list")
(assert-equal (format-sexp '(quote (a b c)) 0) "'(a b c)"
 "format-sexp uses quote shorthand")
;; Quasiquote shorthand
(assert-equal (sexp-to-string '(quasiquote x)) "`x"
 "quasiquote outputs as backtick")
(assert-equal (sexp-to-string '(quasiquote (a b c))) "`(a b c)"
 "quasiquoted list")
;; Unquote shorthand
(assert-equal (sexp-to-string '(unquote x)) ",x" "unquote outputs as comma")
(assert-equal (sexp-to-string '(unquote-splicing x)) ",@x"
 "unquote-splicing outputs as comma-at")
;; Nested quasiquote with unquote
(assert-equal (sexp-to-string '(quasiquote (a (unquote b) c))) "`(a ,b c)"
 "quasiquote with nested unquote")
;; Complex quasiquote
(assert-equal
 (sexp-to-string '(quasiquote (list (unquote a) (unquote-splicing b))))
 "`(list ,a ,@b)" "quasiquote with unquote and unquote-splicing")
;; ============================================================
;; Format-sexp Tests (with indentation)
;; ============================================================
;; Simple expressions stay on one line
(assert-equal (format-sexp '(+ 1 2) 0) "(+ 1 2)" "simple expr on one line")
;; Nested short expressions
(assert-equal (format-sexp '(+ (* 2 3) 4) 0) "(+ (* 2 3) 4)"
 "nested expr on one line")
;; ============================================================
;; Special Form Indentation
;; ============================================================
;; Short defun stays on one line
(assert-equal (format-sexp '(defun foo (x) (+ x 1)) 0)
 "(defun foo (x) (+ x 1))" "short defun on one line")
;; simple define
(assert-equal (format-sexp '(define x 42) 0) "(define x 42)"
 "simple define on one line")
;; Longer defun uses multi-line (via format-special-form)
;; Build a defun that's too long for one line
(assert-equal
 (format-sexp
  '(defun calculate-something-complex (first-param second-param)
     (+ first-param second-param)) 0)
 "(defun calculate-something-complex (first-param second-param)\n  (+ first-param second-param))"
 "long defun gets body indent")
;; ============================================================
;; Let Form Formatting
;; ============================================================
;; Short let stays on one line
(assert-equal (format-sexp '(let ((x 1)) x) 0) "(let ((x 1)) x)"
 "short let on one line")
;; Longer let* forces multi-line
(assert-equal
 (format-sexp
  '(let* ((first-variable 100)
          (second-variable 200))
     (+ first-variable second-variable)) 0)
 "(let* ((first-variable 100)\n       (second-variable 200))\n  (+ first-variable second-variable))"
 "longer let* with multiple bindings")
;; ============================================================
;; Cond Form Formatting
;; ============================================================
;; Short cond stays on one line
(assert-equal (format-sexp '(cond ((= x 1) "one") (#t "other")) 0)
 "(cond ((= x 1) \"one\") (#t \"other\"))" "short cond on one line")
;; ============================================================
;; If Form Formatting
;; ============================================================
;; Short if stays on one line
(assert-equal (format-sexp '(if test then else) 0) "(if test then else)"
 "short if on one line")
;; ============================================================
;; Dotted Pair Formatting
;; ============================================================
(assert-equal (sexp-to-string (cons 'a 'b)) "(a . b)" "dotted pair")
(assert-equal (sexp-to-string '(a b . c)) "(a b . c)" "improper list")

;; ============================================================
;; Round-trip Formatting (idempotence)
;; ============================================================
;; Format twice should produce same result
(let ((expr '(defun test (a b) (+ a b))))
  (let ((once (format-sexp expr 0))
        (twice (format-sexp expr 0)))
    (assert-equal once twice "format is idempotent")))

;; ============================================================
;; Success message
;; ============================================================
(princ "All formatter tests passed!\n")

