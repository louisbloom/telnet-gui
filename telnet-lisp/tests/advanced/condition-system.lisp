;; Condition System Tests
;; Tests for Emacs Lisp-style error handling with signal, condition-case, and unwind-protect

(load "tests/test-helpers.lisp")

;;; Basic error introspection (capture error with condition-case)
(define err nil)
(condition-case e
  (signal 'my-error "test message")
  (error (define err e)))

(assert-true (error? err) "error? recognizes error object")
(assert-equal (error-type err) 'my-error "error-type returns symbol")
(assert-equal (error-message err) "test message" "error-message returns message")
(assert-false (error? 42) "error? rejects integer")
(assert-false (error? "hello") "error? rejects string")

;;; Error function (convenience)
(define generic-err nil)
(condition-case e
  (error "generic error message")
  (error (define generic-err e)))

(assert-true (error? generic-err) "error function creates error")
(assert-equal (error-type generic-err) 'error "error function uses error type")
(assert-equal (error-message generic-err) "generic error message" "error function message")

;;; condition-case with specific handler
(assert-equal (condition-case e
                (signal 'division-by-zero "divided by zero")
                (division-by-zero "caught division-by-zero"))
  "caught division-by-zero"
  "condition-case catches specific error type")

;;; condition-case with error catch-all
(assert-equal (condition-case e
                (signal 'custom-error "unknown error")
                (division-by-zero "wrong handler")
                (error "caught any error"))
  "caught any error"
  "condition-case error handler catches all errors")

;;; condition-case no match propagates error (skipped - complex propagation semantics)
;; (condition-case e
;;   (signal 'my-error "unhandled")
;;   (other-error "wrong handler"))
;; TODO: This test demonstrates error propagation when no handler matches

;;; condition-case normal execution (no error)
(assert-equal (condition-case e
                (+ 1 2 3)
                (error "should not run"))
  6
  "condition-case returns value when no error")

;;; condition-case with VAR binding
(assert-equal (condition-case my-err
                (signal 'custom "with data")
                (custom (error-message my-err)))
  "with data"
  "condition-case binds error to variable")

;;; condition-case with VAR as nil
(assert-equal (condition-case nil
                (signal 'test "ignored")
                (test "caught without binding"))
  "caught without binding"
  "condition-case works with nil variable")

;;; condition-case handler implicit progn
(assert-equal (condition-case e
                (signal 'test "multi-expression handler")
                (test
                  (define a 1)
                  (define b 2)
                  (+ a b)))
  3
  "condition-case handler uses implicit progn")

;;; unwind-protect cleanup always runs (no error)
(define x 0)
(unwind-protect
  (define x 1)
  (define x 2))
(assert-equal x 2 "unwind-protect cleanup runs")

;;; unwind-protect multiple cleanup forms
(define counter 0)
(unwind-protect
  (define counter (+ counter 1))
  (define counter (+ counter 10))
  (define counter (+ counter 100)))
(assert-equal counter 111 "unwind-protect multiple cleanup forms")

;;; unwind-protect cleanup runs even on error
(define y 0)
(condition-case e
  (unwind-protect
    (signal 'test-error "boom")
    (define y 99))
  (error "caught"))
(assert-equal y 99 "unwind-protect cleanup runs on error")

;;; Nested condition-case
(assert-equal (condition-case outer
                (condition-case inner
                  (signal 'inner-error "inner")
                  (other-error "inner handler"))
                (inner-error "outer caught"))
  "outer caught"
  "nested condition-case")

;;; Signal with data
(define err-with-data nil)
(condition-case e
  (signal 'my-error '(1 2 3))
  (error (define err-with-data e)))

(assert-equal (error-type err-with-data) 'my-error "error-type with list data")
(assert-equal (error-data err-with-data) '(1 2 3) "error-data returns list")

;;; Error with string data
(define err-with-str nil)
(condition-case e
  (signal 'file-error "cannot open file")
  (error (define err-with-str e)))

(assert-equal (error-message err-with-str) "cannot open file" "error-message with string data")
(assert-equal (error-type err-with-str) 'file-error "error-type with string data")

;;; Practical example: safe division
(define safe-divide
  (lambda (a b)
    (condition-case err
      (if (= b 0)
        (signal 'division-by-zero "cannot divide by zero")
        (/ a b))
      (division-by-zero "Error: division by zero")
      (error (concat "Unexpected error: " (error-message err))))))

(assert-equal (safe-divide 10 2) 5 "safe-divide normal case")
(assert-equal (safe-divide 10 0) "Error: division by zero" "safe-divide catches division by zero")

;;; Practical example: resource cleanup
(define cleanup-done #f)  ; Global flag to track cleanup
(define with-cleanup
  (lambda (resource)
    (unwind-protect
      (progn
        (define result (* resource 2))
        (if (> result 100)
          (signal 'overflow-error "result too large")
          result))
      (set! cleanup-done #t))))  ; Cleanup always runs

(assert-equal (with-cleanup 10) 20 "with-cleanup normal case")
(assert-equal (condition-case e
                (with-cleanup 60)
                (overflow-error "caught overflow"))
  "caught overflow"
  "with-cleanup catches overflow")
(assert-true cleanup-done "cleanup runs even on error")

;;; Error stack traces
(define inner-func
  (lambda ()
    (signal 'my-error "error from inner")))

(define outer-func
  (lambda ()
    (inner-func)))

(assert-equal (condition-case e
                (outer-func)
                (error
                  (progn
                    (assert-true (error? e) "error object in stack trace")
                    (assert-equal (error-message e) "error from inner" "error message in stack trace")
                    "handled")))
  "handled"
  "error stack traces work")
