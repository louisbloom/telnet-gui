;; Error test examples
;; Demonstrate improved error messages with stack traces

;; ===========================================
;; Test undefined symbol errors
;; ===========================================

(define test-undefined
  (lambda ()
    undefined-variable))

;; This should show a stack trace
; (test-undefined)

;; ===========================================
;; Test nested function call errors
;; ===========================================

(define outer-func
  (lambda ()
    (middle-func)))

(define middle-func
  (lambda ()
    (inner-func)))

(define inner-func
  (lambda ()
    undefined-symbol))

;; This should show the call chain in the error
; (outer-func)

;; ===========================================
;; Test builtin function errors
;; ===========================================

(define test-builtin-error
  (lambda (x)
    (quotient x 0)))

;; This should show the error occurred in quotient builtin
; (test-builtin-error 10)

;; ===========================================
;; Test errors in special forms
;; ===========================================

(define test-let-error
  (lambda ()
    (let ((x undefined))
      x)))

;; This should show where the let failed
; (test-let-error)

;; ===========================================
;; Test type errors
;; ===========================================

(define test-type-error
  (lambda (x)
    (vector-ref x 0)))

;; This should show where the type error occurred
; (test-type-error 5)

;; ===========================================
;; Test deep recursion
;; ===========================================

(define recursive-func
  (lambda (n)
    (if (= n 0)
        undefined-var
        (recursive-func (- n 1)))))

;; This should show a long call chain
; (recursive-func 5)

;; ===========================================
;; Test argument count errors
;; ===========================================

(define test-arity
  (lambda (x)
    (+ x)))

;; This should show the arity error in +
; (test-arity 5)

;; ===========================================
;; Test lambdas with errors
;; ===========================================

(define make-error-lambda
  (lambda ()
    (lambda () undefined-here)))

(define call-error-lambda
  (lambda (f)
    (f)))

;; This should show nested lambda calls
; (call-error-lambda (make-error-lambda))

;; ===========================================
;; Test hash table errors
;; ===========================================

(define test-hash-error
  (lambda (ht)
    (hash-ref ht undefined-key)))

;; This should show the error chain
; (define ht (make-hash-table))
; (test-hash-error ht)

;; ===========================================
;; Test string operations with errors
;; ===========================================

(define test-string-error
  (lambda (str)
    (substring str 0 100)))

;; This should show where the substring error occurred
; (test-string-error "hello")

;; ===========================================
;; Test vector errors
;; ===========================================

(define test-vector-error
  (lambda (vec)
    (vector-ref vec 100)))

;; This should show the error context
; (define vec (make-vector 5))
; (test-vector-error vec)

;; ===========================================
;; Test conditional errors
;; ===========================================

(define test-cond-error
  (lambda (x)
    (cond
      ((< x 0) (negative-func))
      (else "positive"))))

;; This should show where the cond clause failed
; (test-cond-error -5)

;; ===========================================
;; Test let* errors
;; ===========================================

(define test-let-star-error
  (lambda ()
    (let* ((x 5)
           (y undefined-var))
      (+ x y))))

;; This should show let* context
; (test-let-star-error)

;; ===========================================
;; All tests completed
;; ===========================================
"Error test examples defined. Uncomment lines to test."

