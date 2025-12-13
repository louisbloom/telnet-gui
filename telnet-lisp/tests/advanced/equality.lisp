;;; Equality Predicates Tests

(load "tests/test-helpers.lisp")

;; eq? - Identity equality with interned symbols
(assert-true (eq? 'foo 'foo) "eq? with same symbol")
(assert-nil (eq? 'foo 'bar) "eq? with different symbols")
(define s1 'foo)
(define s2 'foo)
(assert-true (eq? s1 s2) "eq? with interned symbols")
(define s3 s1)
(assert-true (eq? s1 s3) "eq? with same reference")
(define x '(1 2 3))
(define y x)
(assert-true (eq? x y) "eq? with same list reference")
(assert-nil (eq? '(1 2) '(1 2)) "eq? with different list instances")
(assert-nil (eq? "abc" "abc") "eq? with different string instances")

;; equal? - Structural equality with primitives
(assert-true (equal? 1 1) "equal? with same integers")
(assert-nil (equal? 1 2) "equal? with different integers")
(assert-nil (equal? 1 1.0) "equal? integer vs float")
(assert-true (equal? "abc" "abc") "equal? with same strings")
(assert-nil (equal? "abc" "def") "equal? with different strings")
;; Symbols are compared by name, not identity
(assert-true (equal? 'foo 'foo) "equal? with same symbols")
(assert-nil (equal? 'foo 'bar) "equal? with different symbols")

;; equal? - Lists
(assert-true (equal? '(1 2 3) '(1 2 3)) "equal? with same lists")
(assert-nil (equal? '(1 2 3) '(1 2 4)) "equal? with different lists")
(assert-true (equal? '(1 (2 3)) '(1 (2 3))) "equal? with nested lists same")
(assert-nil (equal? '(1 (2 3)) '(1 (2 4))) "equal? with nested lists different")

;; equal? - Vectors
(define v1 (make-vector 3))
(vector-set! v1 0 1)
(vector-set! v1 1 2)
(vector-set! v1 2 3)
(define v2 (make-vector 3))
(vector-set! v2 0 1)
(vector-set! v2 1 2)
(vector-set! v2 2 3)
(assert-true (equal? v1 v2) "equal? with same vectors")
(vector-set! v2 2 4)
(assert-nil (equal? v1 v2) "equal? with different vectors")
(define v3 (make-vector 0))
(define v4 (make-vector 0))
(assert-true (equal? v3 v4) "equal? with empty vectors")

;; equal? - Hash tables
(define h1 (make-hash-table))
(define h2 (make-hash-table))
(hash-set! h1 "a" 1)
(hash-set! h1 "b" 2)
(hash-set! h2 "a" 1)
(hash-set! h2 "b" 2)
(assert-true (equal? h1 h2) "equal? with same hash tables")

(define h3 (make-hash-table))
(hash-set! h3 "a" 1)
(hash-set! h3 "b" 3)
(assert-nil (equal? h1 h3) "equal? with different hash values")

(define h4 (make-hash-table))
(hash-set! h4 "a" 1)
(assert-nil (equal? h1 h4) "equal? with different hash size")

;; string=? - String equality predicate
(assert-true (string=? "foo" "foo") "string=? with same strings")
(assert-nil (string=? "foo" "bar") "string=? with different strings")
(assert-true (string=? "" "") "string=? with empty strings")

;; Empty collections
(assert-true (equal? '() '()) "equal? with empty lists")
(define empty1 (make-vector 0))
(define empty2 (make-vector 0))
(assert-true (equal? empty1 empty2) "equal? with empty vectors")
(assert-true (equal? "" "") "equal? with empty strings")

;; Mixed types
(assert-nil (equal? 1 "1") "equal? integer vs string")
(assert-nil (equal? 'symbol "symbol") "equal? symbol vs string")

;; eq? vs equal? comparison
(define list1 '(1 2 3))
(define list2 '(1 2 3))
(assert-nil (eq? list1 list2) "eq? fails on different list instances")
(assert-true (equal? list1 list2) "equal? succeeds on structurally same lists")

;; Boolean internment tests - #t should be interned (same object)
(assert-true (eq? #t #t) "eq? with #t returns true (interned)")
(define t1 #t)
(define t2 #t)
(assert-true (eq? t1 t2) "eq? with interned #t variables")
(assert-true (eq? t1 #t) "eq? with variable and literal #t")
(assert-true (eq? t2 #t) "eq? with another variable and literal #t")
;; Test that #t from different sources is the same object
(assert-true (eq? (if #t #t #f) #t) "eq? with #t from if expression")
(assert-true (eq? (and #t #t) #t) "eq? with #t from and expression")
