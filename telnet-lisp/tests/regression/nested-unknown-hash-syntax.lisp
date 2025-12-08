;; Regression test for parser hang on unknown #\ syntax
;; Previously, encountering unknown syntax like #\space would cause:
;;   1. Silent REPL exit (no error propagation)
;;   2. Infinite loops in nested contexts (input pointer not advancing)
;; Fixed in commit dc19a66 with:
;;   1. Error propagation in read_list() (lines 210-212 of reader.c)
;;   2. Input advancement for unknown # syntax (lines 381-382 of reader.c)
;; This test verifies the parser fails cleanly at all nesting levels

(load "tests/test-helpers.lisp")

;; Helper function to test that parsing code with unknown #\ syntax fails cleanly
;; We must use a temp file approach because parse errors occur during reading,
;; before evaluation, so they can't be caught directly by assert-error
(defun test-parse-error (code-str description)
  (let ((temp-file (concat (expand-path "~/") ".telnet-lisp-parse-test.tmp")))
    ;; Write test code to temp file
    (let ((f (open temp-file "w")))
      (write-line f code-str)
      (close f))
    ;; Try to load it - should fail with parse error
    (assert-error (load temp-file) description)))

;; Top level should fail cleanly
(test-parse-error "#\\space" "Top level unknown hash syntax")

;; Nested in let body should fail cleanly (not hang)
(test-parse-error "(let ((x #\\space)) x)" "Let body with unknown syntax")

;; Nested in let binding value should fail cleanly
(test-parse-error "(let ((x 1) (y #\\space)) y)" "Let binding with unknown syntax")

;; Nested in do loop test should fail cleanly (not hang)
(test-parse-error "(do ((i 10 (- i 1))) ((let ((ch #\\space)) (= i 0))) (display i))"
  "Do loop with unknown syntax")

;; Deep nesting (5 levels) should fail cleanly - the original bug case
(test-parse-error "(defun test-func () (do ((i 10 (- i 1))) ((let ((ch #\\space)) (= i 0))) (display i)))"
  "Deep nesting with unknown syntax")

;; Nested in function call arguments
(test-parse-error "(+ 1 #\\space 3)" "Function arguments with unknown syntax")

;; Nested in if condition
(test-parse-error "(if #\\space 1 2)" "If condition with unknown syntax")

;; Nested in cond clause
(test-parse-error "(cond (#\\space 1) (else 2))" "Cond clause with unknown syntax")

;; Nested in vector
(test-parse-error "[1 2 #\\space 4]" "Vector with unknown syntax")

;; Nested in quoted form (errors during read, before quote is processed)
(test-parse-error "'(foo #\\space bar)" "Quoted form with unknown syntax")
