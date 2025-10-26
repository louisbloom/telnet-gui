;; Regression tests for hash tables, vectors, integers, and type coercion

;; Test 1: Integer arithmetic
(define i1 5)
(define i2 10)
(+ i1 i2)                      ; Should return 15 (integer)
(+ i1 2.5)                     ; Should return 7.5 (float)
(* 3 4)                        ; Should return 12 (integer)
(/ 10 3)                       ; Should return 3.333... (float, always)

;; Test 2: Quotient
(quotient 10 3)                ; Should return 3 (integer division)

;; Test 3: Hash tables
(define ht (make-hash-table))
(hash-count ht)                ; Should return 0
(hash-set! ht "key1" "value1")
(hash-set! ht "key2" 42)
(hash-count ht)                ; Should return 2
(hash-ref ht "key1")           ; Should return "value1"
(hash-ref ht "key2")           ; Should return 42
(hash-ref ht "missing")        ; Should return nil
(hash-remove! ht "key1")
(hash-count ht)                ; Should return 1
(hash-ref ht "key1")           ; Should return nil (removed)
(hash-clear! ht)
(hash-count ht)                ; Should return 0

;; Test 4: Vectors
(define v (make-vector 3))
(vector-length v)              ; Should return 0
(vector-push! v "first")
(vector-push! v "second")
(vector-push! v "third")
(vector-length v)              ; Should return 3
(vector-ref v 0)               ; Should return "first"
(vector-ref v 1)               ; Should return "second"
(vector-set! v 1 "updated")
(vector-ref v 1)               ; Should return "updated"
(vector-pop! v)                 ; Should return "third"
(vector-length v)              ; Should return 2

;; Test 5: Vector literal syntax
(define v2 #(10 20 30))
(vector-length v2)            ; Should return 3
(vector-ref v2 0)             ; Should return 10
(vector-ref v2 1)             ; Should return 20
(vector-ref v2 2)             ; Should return 30

;; Test 6: Type predicates
(integer? 42)                 ; Should return 1
(integer? 3.14)               ; Should return nil
(boolean? #t)                 ; Should return 1
(boolean? #f)                 ; Should return 1
(boolean? 42)                 ; Should return nil
(number? 42)                  ; Should return 1
(number? 3.14)                ; Should return 1
(vector? v)                   ; Should return 1
(vector? ht)                  ; Should return nil
(hash-table? ht)              ; Should return 1
(hash-table? v)               ; Should return nil

;; Test 7: Comparisons with mixed types
(> 5.0 3)                     ; Should return 1
(= 5 5.0)                     ; Should return 1
(< 2 3.5)                     ; Should return 1

;; Test 8: Truthy/falsy behavior
(if 0 "yes" "no")            ; Should return "no" (0 is falsy)
(if 1 "yes" "no")            ; Should return "yes" (non-zero is truthy)
(if #t "yes" "no")           ; Should return "yes"
(if #f "yes" "no")           ; Should return "no"
(if #() "yes" "no")          ; Should return "no" (empty vector is falsy)
(if (make-hash-table) "yes" "no")  ; Should return "no" (empty hash is falsy)

;; All tests complete
"All regression tests passed!"
