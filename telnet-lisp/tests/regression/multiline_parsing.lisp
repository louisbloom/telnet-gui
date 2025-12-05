;; Multi-line Expression Parsing
;; Demonstrates that the reader correctly handles expressions split across multiple lines
;; This is important for parsing input from files or pipes

;; ===========================================
;; Test 1: Simple multi-line arithmetic
;; ===========================================

(+
  1
  2
  3
  )                                        ; => 6

;; ===========================================
;; Test 2: Nested multi-line expressions
;; ===========================================

(+
  1
  (+
    2
    3
    )
  )                                        ; => 6

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

(hash-ref
  ht
  "name"
  )                                        ; => "Alice"

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

(vector-ref
  v
  0
  )                                        ; => 42

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

result                                    ; => 6

;; ===========================================
;; Test 6: Multi-line string definition
;; ===========================================

(define s
  "This is a (test) string"
  )

s                                         ; => "This is a (test) string"

;; All multi-line tests completed
"Multi-line expression parsing works!"     ; => "Multi-line expression parsing works!"
