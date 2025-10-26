;; Regression tests for multi-line expression parsing from pipe input

;; Test 1: Simple multi-line expression
(+
 1
 2
 3
)

;; Test 2: Nested multi-line expressions
(+
 1
 (+
  2
  3
 )
)

;; Test 3: Multi-line hash-set!
(define ht
 (make-hash-table)
)

(hash-set!
 ht
 "name"
 "Alice"
)

(hash-ref
 ht
 "name"
)

;; Test 4: Multi-line vector operations
(define v
 (make-vector 5)
)

(vector-push!
 v
 42
)

(vector-push!
 v
 100
)

(vector-ref
 v
 0
)

;; Test 5: Complex nested multi-line
(define result
 (if
  (> 10 5)
  (+ 1 2 3)
  (- 5 2)
 )
)

result

;; Test 6: Multi-line with strings containing parens
(define s
 "This is a (test) string"
)

s

;; All multi-line tests completed
"Multi-line expression parsing works!"
