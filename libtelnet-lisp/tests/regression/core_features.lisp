;; Core Features Regression Tests
;; Comprehensive tests for hash tables, vectors, integers, and type coercion

;; ===========================================
;; Integer Arithmetic and Type Coercion
;; ===========================================

(define i1 5)
(define i2 10)
(+ i1 i2)                                  ; => 15
(+ i1 2.5)                                 ; => 7.5
(* 3 4)                                    ; => 12
(/ 10 3)                                   ; => 3.3333333333333335

;; Integer division (quotient)
(quotient 10 3)                            ; => 3

;; ===========================================
;; Hash Table Operations
;; ===========================================

(define ht (make-hash-table))
(hash-count ht)                            ; => 0
(hash-set! ht "key1" "value1")
(hash-set! ht "key2" 42)
(hash-count ht)                            ; => 2
(hash-ref ht "key1")                       ; => "value1"
(hash-ref ht "key2")                       ; => 42
(hash-ref ht "missing")                    ; => nil
(hash-remove! ht "key1")
(hash-count ht)                            ; => 1
(hash-ref ht "key1")                       ; => nil
(hash-clear! ht)
(hash-count ht)                            ; => 0

;; ===========================================
;; Vector Operations
;; ===========================================

(define v (make-vector 3))
(vector-length v)                          ; => 0
(vector-push! v "first")
(vector-push! v "second")
(vector-push! v "third")
(vector-length v)                        ; => 3
(vector-ref v 0)                         ; => "first"
(vector-ref v 1)                         ; => "second"
(vector-set! v 1 "updated")
(vector-ref v 1)                         ; => "updated"
(vector-pop! v)                          ; => "third"
(vector-length v)                        ; => 2

;; Vector literal syntax
(define v2 #(10 20 30))
(vector-length v2)                        ; => 3
(vector-ref v2 0)                         ; => 10
(vector-ref v2 1)                         ; => 20
(vector-ref v2 2)                         ; => 30

;; ===========================================
;; Type Predicates
;; ===========================================

(integer? 42)                             ; => 1
(integer? 3.14)                            ; => nil
(boolean? #t)                              ; => 1
(boolean? #f)                              ; => 1
(boolean? 42)                              ; => nil
(number? 42)                               ; => 1
(number? 3.14)                             ; => 1
(vector? v)                                 ; => 1
(vector? ht)                               ; => nil
(hash-table? ht)                           ; => 1
(hash-table? v)                            ; => nil

;; ===========================================
;; Type Coercion in Comparisons
;; ===========================================

(> 5.0 3)                                  ; => 1
(= 5 5.0)                                  ; => 1
(< 2 3.5)                                  ; => 1

;; ===========================================
;; Truthy/Falsy Behavior
;; ===========================================

(if 0 "yes" "no")                        ; => "no"
(if 1 "yes" "no")                          ; => "yes"
(if #t "yes" "no")                         ; => "yes"
(if #f "yes" "no")                         ; => "no"
(if #() "yes" "no")                        ; => "no"
(if (make-hash-table) "yes" "no")         ; => "no"

;; All core feature tests complete
"All regression tests passed!"           ; => "All regression tests passed!"
