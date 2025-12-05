;; Test: Tail recursion - tail call unwrapping in lambda bodies
;; Regression test for bug where tail calls in lambda bodies weren't being unwrapped
;; This caused functions to return #<tail-call> objects instead of actual values

;; Helper function that returns a value from another function in tail position
(define helper
  (lambda (x)
    (if (> x 0)
      (string-append "positive-" (number->string x))
      "non-positive")))

;; Function that calls helper in tail position
(define wrapper
  (lambda (x)
    (if (> x 10)
      (helper x)
      "small")))

;; Test: Simple tail call should return actual value, not tail-call object
(wrapper 15)                    ; => "positive-15"
(wrapper 5)                     ; => "small"

;; Test: Nested tail calls
(define level1
  (lambda (x)
    (string-append "L1-" x)))

(define level2
  (lambda (x)
    (level1 (string-append "L2-" x))))

(define level3
  (lambda (x)
    (level2 (string-append "L3-" x))))

(level3 "test")                 ; => "L1-L2-L3-test"

;; Test: Conditional tail calls in both branches
(define conditional-tail
  (lambda (x pred)
    (if pred
      (helper x)
      (level1 (number->string x)))))

(conditional-tail 20 #t)        ; => "positive-20"
(conditional-tail 20 #f)        ; => "L1-20"

;; Test: Tail call in let expression
(define let-tail
  (lambda (x)
    (let ((y (* x 2)))
      (helper y))))

(let-tail 7)                    ; => "positive-14"

;; Test: Complex nested structure with multiple tail calls
(define trim-punctuation
  (lambda (word)
    (if (not (and (string? word) (> (string-length word) 0)))
      ""
      (let* ((no-trailing (regex-replace "[.,!?;:]+" "" word))
              (cleaned (regex-replace "^[.,!?;:]+" "" no-trailing)))
        cleaned))))

(define clean-word
  (lambda (word)
    (if (and (string? word) (> (string-length word) 0))
      (trim-punctuation word)
      "")))

;; This was the original bug case - clean-word would return #<tail-call> instead of string
(clean-word "hello,")           ; => "hello"
(clean-word "world!")           ; => "world"
(clean-word "")                 ; => ""

;; Test: Multiple arguments with tail call
(define multi-arg-tail
  (lambda (a b c)
    (string-append a b c)))

(define multi-wrapper
  (lambda (x)
    (multi-arg-tail "A-" x "-Z")))

(multi-wrapper "test")          ; => "A-test-Z"
