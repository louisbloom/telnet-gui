;; Optional Parameters Test Suite
;; Tests &optional and &rest parameter support in lambda expressions

(load "tests/test-helpers.lisp")

;; ===========================================
;; Basic Optional Parameters
;; ===========================================

;; Single optional parameter (defaults to nil)
(assert-equal ((lambda (a &optional b) (list a b)) 1)
  (list 1 nil)
  "single optional param - not provided")

(assert-equal ((lambda (a &optional b) (list a b)) 1 2)
  (list 1 2)
  "single optional param - provided")

;; Multiple optional parameters
(assert-equal ((lambda (a &optional b c d) (list a b c d)) 1)
  (list 1 nil nil nil)
  "multiple optional params - none provided")

(assert-equal ((lambda (a &optional b c d) (list a b c d)) 1 2)
  (list 1 2 nil nil)
  "multiple optional params - one provided")

(assert-equal ((lambda (a &optional b c d) (list a b c d)) 1 2 3)
  (list 1 2 3 nil)
  "multiple optional params - two provided")

(assert-equal ((lambda (a &optional b c d) (list a b c d)) 1 2 3 4)
  (list 1 2 3 4)
  "multiple optional params - all provided")

;; ===========================================
;; Only Optional Parameters (No Required)
;; ===========================================

(assert-equal ((lambda (&optional a b) (list a b)))
  (list nil nil)
  "only optional params - none provided")

(assert-equal ((lambda (&optional a b) (list a b)) 1)
  (list 1 nil)
  "only optional params - one provided")

(assert-equal ((lambda (&optional a b) (list a b)) 1 2)
  (list 1 2)
  "only optional params - all provided")

;; ===========================================
;; Default Values via (or param default)
;; ===========================================

;; Helper function using (or param default) pattern
(define greet
  (lambda (name &optional greeting)
    (let ((g (or greeting "Hello")))
      (concat g ", " name "!"))))

(assert-equal (greet "Alice")
  "Hello, Alice!"
  "optional param with default - not provided")

(assert-equal (greet "Bob" "Hi")
  "Hi, Bob!"
  "optional param with default - provided")

;; Numeric default
(define multiply-or-default
  (lambda (x &optional factor)
    (* x (or factor 2))))

(assert-equal (multiply-or-default 5)
  10
  "numeric optional param with default - not provided")

(assert-equal (multiply-or-default 5 3)
  15
  "numeric optional param with default - provided")

;; ===========================================
;; Rest Parameters
;; ===========================================

;; Basic rest parameter
(assert-equal ((lambda (a &rest more) (list a more)) 1)
  (list 1 nil)
  "rest param - no extra args")

(assert-equal ((lambda (a &rest more) (list a more)) 1 2 3)
  (list 1 (list 2 3))
  "rest param - multiple extra args")

;; Rest parameter collects everything
(assert-equal ((lambda (&rest all) all) 1 2 3 4 5)
  (list 1 2 3 4 5)
  "rest param only - collects all args")

;; ===========================================
;; Combined Optional and Rest Parameters
;; ===========================================

(assert-equal ((lambda (a &optional b &rest more) (list a b more)) 1)
  (list 1 nil nil)
  "optional and rest - only required provided")

(assert-equal ((lambda (a &optional b &rest more) (list a b more)) 1 2)
  (list 1 2 nil)
  "optional and rest - required and optional provided")

(assert-equal ((lambda (a &optional b &rest more) (list a b more)) 1 2 3 4)
  (list 1 2 (list 3 4))
  "optional and rest - all provided")

;; Multiple optional with rest
(assert-equal ((lambda (a &optional b c &rest more) (list a b c more)) 1)
  (list 1 nil nil nil)
  "multiple optional and rest - only required")

(assert-equal ((lambda (a &optional b c &rest more) (list a b c more)) 1 2 3 4 5)
  (list 1 2 3 (list 4 5))
  "multiple optional and rest - all provided")

;; ===========================================
;; Named Functions with Optional/Rest
;; ===========================================

;; Sum with rest parameter
(define sum
  (lambda (a &rest numbers)
    (let ((total a))
      (do ((nums numbers (cdr nums)))
        ((null? nums) total)
        (set! total (+ total (car nums)))))))

(assert-equal (sum 1)
  1
  "named function with rest - single arg")

(assert-equal (sum 1 2 3 4 5)
  15
  "named function with rest - multiple args")

;; Function with optional and rest
(define format-list
  (lambda (prefix &optional separator &rest items)
    (let ((sep (or separator ", ")))
      (concat prefix (concat sep (list->string items sep))))))

;; Helper to convert list to string with separator
(define list->string
  (lambda (lst sep)
    (if (null? lst)
      ""
      (if (null? (cdr lst))
        (string (car lst))
        (concat (string (car lst)) (concat sep (list->string (cdr lst) sep)))))))

;; Helper to convert value to string
(define string
  (lambda (x)
    (if (integer? x)
      (number->string x)
      x)))

(assert-equal (format-list "Items: " "; " "a" "b" "c")
  "Items: ; a; b; c"
  "named function with optional and rest")

;; ===========================================
;; Arity Checking
;; ===========================================

;; Too few arguments - missing required
(assert-error ((lambda (a &optional b) a))
  "too few args - required param missing")

;; Too many arguments - no rest parameter
(assert-error ((lambda (a &optional b) a) 1 2 3)
  "too many args - no rest param")

;; Correct with rest parameter - no max limit
(assert-equal ((lambda (a &rest more) (list a (length more))) 1 2 3 4 5)
  (list 1 4)
  "no max args with rest param")

;; ===========================================
;; Syntax Error Tests
;; ===========================================

;; These tests verify that invalid parameter lists are rejected at parse time

;; Multiple &optional markers
(assert-error (lambda (&optional &optional x) x)
  "syntax error - multiple &optional")

;; Multiple &rest markers
(assert-error (lambda (&rest &rest x) x)
  "syntax error - multiple &rest")

;; &rest must be followed by parameter
(assert-error (lambda (&rest) x)
  "syntax error - &rest without param")

;; &rest must be last
(assert-error (lambda (&rest x y) x)
  "syntax error - &rest not last")

;; Non-symbol parameter
(assert-error (lambda (&optional 123) x)
  "syntax error - non-symbol optional param")

;; &optional cannot appear after &rest
(assert-error (lambda (&rest x &optional y) x)
  "syntax error - &optional after &rest")

;; ===========================================
;; Edge Cases
;; ===========================================

;; Empty parameter list (valid)
(assert-equal ((lambda () 42))
  42
  "empty parameter list")

;; Only &rest parameter
(assert-equal ((lambda (&rest all) (length all)) 1 2 3)
  3
  "only &rest param")

;; Zero required, one optional
(assert-equal ((lambda (&optional x) (or x 99)))
  99
  "zero required with optional default")

(assert-equal ((lambda (&optional x) (or x 99)) 55)
  55
  "zero required with optional provided")
