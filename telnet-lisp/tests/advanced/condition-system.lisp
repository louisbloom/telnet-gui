;; Condition System Tests
;; Tests for Emacs Lisp-style error handling with signal, condition-case, and unwind-protect

;;; Basic error introspection (capture error with condition-case)
(define err nil)
(condition-case e
  (signal 'my-error "test message")
  (error (define err e)))

(error? err)  ; => #t
(error-type err)  ; => my-error
(error-message err)  ; => "my-error: test message"
(error? 42)  ; => #f
(error? "hello")  ; => #f

;;; Error function (convenience)
(define generic-err nil)
(condition-case e
  (error "generic error message")
  (error (define generic-err e)))

(error? generic-err)  ; => #t
(error-type generic-err)  ; => error
(error-message generic-err)  ; => "generic error message"

;;; condition-case with specific handler
(condition-case e
  (signal 'division-by-zero "divided by zero")
  (division-by-zero "caught division-by-zero"))
                                        ; => "caught division-by-zero"

;;; condition-case with error catch-all
(condition-case e
  (signal 'custom-error "unknown error")
  (division-by-zero "wrong handler")
  (error "caught any error"))
                                        ; => "caught any error"

;;; condition-case no match propagates error
(condition-case e
  (signal 'my-error "unhandled")
  (other-error "wrong handler"))
                                        ; ignore (returns error object)

;;; condition-case normal execution (no error)
(condition-case e
  (+ 1 2 3)
  (error "should not run"))
                                        ; => 6

;;; condition-case with VAR binding
(condition-case my-err
  (signal 'custom "with data")
  (custom (error-message my-err)))
                                        ; => "custom: with data"

;;; condition-case with VAR as nil
(condition-case nil
  (signal 'test "ignored")
  (test "caught without binding"))
                                        ; => "caught without binding"

;;; condition-case handler implicit progn
(condition-case e
  (signal 'test "multi-expression handler")
  (test
    (define a 1)
    (define b 2)
    (+ a b)))
                                        ; => 3

;;; unwind-protect cleanup always runs (no error)
(define x 0)
(unwind-protect
  (define x 1)
  (define x 2))
x  ; => 2

;;; unwind-protect multiple cleanup forms
(define counter 0)
(unwind-protect
  (define counter (+ counter 1))
  (define counter (+ counter 10))
  (define counter (+ counter 100)))
counter  ; => 111

;;; unwind-protect cleanup runs even on error
(define y 0)
(condition-case e
  (unwind-protect
    (signal 'test-error "boom")
    (define y 99))
  (error "caught"))
y  ; => 99

;;; Nested condition-case
(condition-case outer
  (condition-case inner
    (signal 'inner-error "inner")
    (other-error "inner handler"))
  (inner-error "outer caught"))
                                        ; => "outer caught"

;;; Signal with data
(define err-with-data nil)
(condition-case e
  (signal 'my-error '(1 2 3))
  (error (define err-with-data e)))

(error-type err-with-data)  ; => my-error
(error-data err-with-data)  ; => (1 2 3)

;;; Error with string data
(define err-with-str nil)
(condition-case e
  (signal 'file-error "cannot open file")
  (error (define err-with-str e)))

(error-message err-with-str)  ; => "cannot open file"
(error-type err-with-str)  ; => file-error

;;; Practical example: safe division
(define (safe-divide a b)
  (condition-case err
    (if (= b 0)
      (signal 'division-by-zero "cannot divide by zero")
      (/ a b))
    (division-by-zero "Error: division by zero")
    (error (concat "Unexpected error: " (error-message err)))))

(safe-divide 10 2)  ; => 5
(safe-divide 10 0)  ; => "Error: division by zero"

;;; Practical example: resource cleanup
(define (with-cleanup resource)
  (unwind-protect
    (progn
      (define result (* resource 2))
      (if (> result 100)
        (signal 'overflow-error "result too large")
        result))
    (define cleanup-done #t)))  ; Cleanup always runs

(with-cleanup 10)  ; => 20
(condition-case e
  (with-cleanup 60)
  (overflow-error "caught overflow"))
                                        ; => "caught overflow"
cleanup-done  ; => #t

;;; Error stack traces
(define (inner-func)
  (signal 'my-error "error from inner"))

(define (outer-func)
  (inner-func))

(condition-case e
  (outer-func)
  (error
    (progn
      (error? e)  ; => #t
      (error-message e)  ; => "my-error: error from inner"
      "handled")))
                                        ; => "handled"
