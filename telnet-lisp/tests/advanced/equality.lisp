;;; Equality Predicates Tests

;; eq? - Identity equality with interned symbols
(eq? 'foo 'foo)                    ; => 1
(eq? 'foo 'bar)                    ; => nil
(define s1 'foo)                   ; ignore
(define s2 'foo)                   ; ignore
(eq? s1 s2)                        ; => 1
(define s3 s1)                     ; ignore
(eq? s1 s3)                        ; => 1
(define x '(1 2 3))                ; ignore
(define y x)                       ; ignore
(eq? x y)                          ; => 1
(eq? '(1 2) '(1 2))                ; => nil
(eq? "abc" "abc")                  ; => nil

;; equal? - Structural equality with primitives
(equal? 1 1)                       ; => 1
(equal? 1 2)                       ; => nil
(equal? 1 1.0)                     ; => nil
(equal? "abc" "abc")               ; => 1
(equal? "abc" "def")               ; => nil
;; Symbols are compared by name, not identity
(equal? 'foo 'foo)                 ; => 1
(equal? 'foo 'bar)                 ; => nil

;; equal? - Lists
(equal? '(1 2 3) '(1 2 3))         ; => 1
(equal? '(1 2 3) '(1 2 4))         ; => nil
(equal? '(1 (2 3)) '(1 (2 3)))     ; => 1
(equal? '(1 (2 3)) '(1 (2 4)))     ; => nil

;; equal? - Vectors
(define v1 (make-vector 3))        ; ignore
(vector-set! v1 0 1)               ; ignore
(vector-set! v1 1 2)               ; ignore
(vector-set! v1 2 3)               ; ignore
(define v2 (make-vector 3))        ; ignore
(vector-set! v2 0 1)               ; ignore
(vector-set! v2 1 2)               ; ignore
(vector-set! v2 2 3)               ; ignore
(equal? v1 v2)                     ; => 1
(vector-set! v2 2 4)               ; ignore
(equal? v1 v2)                     ; => nil
(define v3 (make-vector 0))        ; ignore
(define v4 (make-vector 0))        ; ignore
(equal? v3 v4)                     ; => 1

;; equal? - Hash tables
(define h1 (make-hash-table))      ; ignore
(define h2 (make-hash-table))      ; ignore
(hash-set! h1 "a" 1)               ; ignore
(hash-set! h1 "b" 2)               ; ignore
(hash-set! h2 "a" 1)               ; ignore
(hash-set! h2 "b" 2)               ; ignore
(equal? h1 h2)                     ; => 1

(define h3 (make-hash-table))      ; ignore
(hash-set! h3 "a" 1)               ; ignore
(hash-set! h3 "b" 3)               ; ignore
(equal? h1 h3)                     ; => nil

(define h4 (make-hash-table))      ; ignore
(hash-set! h4 "a" 1)               ; ignore
(equal? h1 h4)                     ; => nil

;; string=? - String equality predicate
(string=? "foo" "foo")             ; => 1
(string=? "foo" "bar")             ; => nil
(string=? "" "")                   ; => 1

;; Empty collections
(equal? '() '())                   ; => 1
(define empty1 (make-vector 0))    ; ignore
(define empty2 (make-vector 0))    ; ignore
(equal? empty1 empty2)             ; => 1
(equal? "" "")                     ; => 1

;; Mixed types
(equal? 1 "1")                     ; => nil
(equal? 'symbol "symbol")          ; => nil

;; eq? vs equal? comparison
(define list1 '(1 2 3))            ; ignore
(define list2 '(1 2 3))            ; ignore
(eq? list1 list2)                  ; => nil
(equal? list1 list2)               ; => 1
