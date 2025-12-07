					; Regression test for list-ref type checking
					; Bug: list-ref would segfault when passed a non-list (e.g., number, string)
					; Fixed: Added type checking before accessing cons fields

					; Test 1: list-ref with integer should error, not segfault
(condition-case err
  (list-ref 1 0)
  (error #t))  ; => #t

					; Test 2: list-ref with string should error, not segfault
(condition-case err
  (list-ref "string" 0)
  (error #t))  ; => #t

					; Test 3: list-ref with number from regex-match should error, not segfault
					; (This was the original bug scenario in TinTin++ highlight code)
(define match-result 1)  ; regex-match returns 1 on match
(condition-case err
  (list-ref match-result 0)
  (error #t))  ; => #t

					; Test 4: list-ref with improper list should error gracefully
(condition-case err
  (list-ref '(1 2 . 3) 2)  ; trying to access beyond proper list part
  (error #t))  ; => #t

					; Test 5: list-ref with vector should error, not segfault
(condition-case err
  (list-ref (make-vector 5 0) 0)
  (error #t))  ; => #t

					; Test 6: Normal list-ref operations should still work
(list-ref '(a b c d) 0)  ; => a
(list-ref '(a b c d) 2)  ; => c
(list-ref '(a b c d) 3)  ; => d

					; Test 7: list-ref with nil should error with out of bounds
(condition-case err
  (list-ref '() 0)
  (error #t))  ; => #t

					; Test 8: list-ref with index out of bounds
(condition-case err
  (list-ref '(a b c) 10)
  (error #t))  ; => #t
