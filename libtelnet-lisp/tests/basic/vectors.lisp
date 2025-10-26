;; Vector operations examples

;; Create vectors using literal syntax
(define v1 #(10 20 30))
v1                              ; => #(10 20 30)
(vector-ref v1 0)               ; => 10
(vector-ref v1 1)               ; => 20
(vector-ref v1 2)               ; => 30

;; Create empty vector
(define empty1 #())
empty1                          ; => #() (falsy)
(vector-length empty1)          ; => 0

;; Create and manipulate vectors programmatically
(define v (make-vector 5))
(vector-length v)               ; => 0 (empty vector)

;; Add elements
(vector-push! v 10)
(vector-push! v 20)
(vector-push! v 30)
(vector-length v)               ; => 3

v                               ; => #(10 20 30)

;; Access elements
(vector-ref v 0)                ; => 10
(vector-ref v 1)                ; => 20
(vector-ref v 2)                ; => 30

;; Modify elements
(vector-set! v 0 100)
v                               ; => #(100 20 30)

;; Pop elements
(vector-pop! v)                 ; => 30
(vector-length v)              ; => 2

;; Vectors are truthy when non-empty, falsy when empty
(define empty (make-vector 0))
empty                           ; => #() (falsy)
(vector-length empty)           ; => 0

(if v "not empty" "empty")     ; => "not empty"
(if empty "not empty" "empty") ; => "empty"
(if empty1 "not empty" "empty") ; => "empty"
(if v1 "not empty" "empty")    ; => "not empty"
