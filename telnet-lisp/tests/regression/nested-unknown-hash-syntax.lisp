					; Test that unknown #\ syntax fails cleanly at all nesting levels
					; Previously, this would hang infinitely in nested contexts

					; Top level should fail cleanly
#\space
					; ERROR: Unknown # syntax

					; Nested in let should fail cleanly (not hang)
(let ((x #\space)) x)
					; ERROR: Unknown # syntax

					; Nested in let binding value should fail cleanly
(let ((x 1) (y #\space)) y)
					; ERROR: Unknown # syntax

					; Nested in do loop test should fail cleanly (not hang)
(do ((i 10 (- i 1)))
  ((let ((ch #\space)) (= i 0)))
  (display i))
					; ERROR: Unknown # syntax

					; Deep nesting (5 levels) should fail cleanly - the original bug case
(defun test-func ()
  (do ((i 10 (- i 1)))
    ((let ((ch #\space)) (= i 0)))
    (display i)))
					; ERROR: Unknown # syntax

					; Nested in function call arguments
(+ 1 #\space 3)
					; ERROR: Unknown # syntax

					; Nested in if condition
(if #\space 1 2)
					; ERROR: Unknown # syntax

					; Nested in cond clause
(cond (#\space 1) (else 2))
					; ERROR: Unknown # syntax

					; Nested in vector
[1 2 #\space 4]
					; ERROR: Unknown # syntax

					; Nested in quoted form (should not error - quote protects it)
					; Actually, this will error during read, before quote is processed
'(foo #\space bar)
					; ERROR: Unknown # syntax
