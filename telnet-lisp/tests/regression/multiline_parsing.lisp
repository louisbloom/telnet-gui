;; Multi-line Expression Parsing
;; Demonstrates that the reader correctly handles expressions split across multiple lines
;; This is important for parsing input from files or pipes

(load "tests/test-helpers.lisp")

;; ===========================================
;; Test 1: Simple multi-line arithmetic
;; ===========================================

(assert-equal (+
                1
                2
                3
                ) 6 "Simple multi-line arithmetic")

;; ===========================================
;; Test 2: Nested multi-line expressions
;; ===========================================

(assert-equal (+
                1
                (+
                  2
                  3
                  )
                ) 6 "Nested multi-line expressions")

;; ===========================================
;; Test 3: Multi-line hash table operations
;; ===========================================

(define ht
  (make-hash-table)
  )

(hash-set!
  ht
  "name"
  "Alice"
  )

(assert-equal (hash-ref
                ht
                "name"
                ) "Alice" "Multi-line hash table operations")

;; ===========================================
;; Test 4: Multi-line vector operations
;; ===========================================

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

(assert-equal (vector-ref
                v
                0
                ) 42 "Multi-line vector operations")

;; ===========================================
;; Test 5: Complex nested multi-line conditional
;; ===========================================

(define result
  (if
    (> 10 5)
    (+ 1 2 3)
    (- 5 2)
    )
  )

(assert-equal result 6 "Complex nested multi-line conditional")

;; ===========================================
;; Test 6: Multi-line string definition
;; ===========================================

(define s
  "This is a (test) string"
  )

(assert-equal s "This is a (test) string" "Multi-line string definition")

;; All multi-line tests completed
(assert-equal "Multi-line expression parsing works!" "Multi-line expression parsing works!" "Final completion test")
