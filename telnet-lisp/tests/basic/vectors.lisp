;; Vector operations examples

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; Create vectors using literal syntax
(define v1 #(10 20 30))
(assert-equal v1 #(10 20 30) "vector literal creates vector")
(assert-equal (vector-ref v1 0) 10 "vector-ref at index 0")
(assert-equal (vector-ref v1 1) 20 "vector-ref at index 1")
(assert-equal (vector-ref v1 2) 30 "vector-ref at index 2")

;; Create empty vector
(define empty1 #())
(assert-equal empty1 #() "empty vector literal")
(assert-equal (length empty1) 0 "empty vector has length 0")

;; Create and manipulate vectors programmatically
(define v (make-vector 5))
(assert-equal (length v) 0 "make-vector creates empty vector")

;; Add elements
(vector-push! v 10)
(vector-push! v 20)
(vector-push! v 30)
(assert-equal (length v) 3 "vector has 3 elements after pushes")

(assert-equal v #(10 20 30) "vector contains pushed elements")

;; Create vector with initial value (regression test for make-vector bug)
(define v-init (make-vector 5 42))
(assert-equal (length v-init) 5 "make-vector with initial value creates vector of correct length")
(assert-equal (vector-ref v-init 0) 42 "first element has initial value")
(assert-equal (vector-ref v-init 1) 42 "second element has initial value")
(assert-equal (vector-ref v-init 4) 42 "last element has initial value")
(assert-equal v-init #(42 42 42 42 42) "all elements initialized correctly")

;; Create vector with initial value 0 (should not be nil)
(define v-zero (make-vector 3 0))
(assert-equal (vector-ref v-zero 0) 0 "zero is a valid initial value")
(assert-equal (vector-ref v-zero 1) 0 "all elements set to zero")
(assert-equal v-zero #(0 0 0) "vector of zeros created correctly")

;; Access elements
(assert-equal (vector-ref v 0) 10 "access first element")
(assert-equal (vector-ref v 1) 20 "access second element")
(assert-equal (vector-ref v 2) 30 "access third element")

;; Modify elements
(vector-set! v 0 100)
(assert-equal v #(100 20 30) "vector-set! modifies element")

;; Pop elements
(assert-equal (vector-pop! v) 30 "vector-pop! returns popped element")
(assert-equal (length v) 2 "vector length decreases after pop")

;; Vectors are always truthy (even when empty - only nil is falsy in this Lisp)
(define empty (make-vector 0))
(assert-equal empty #() "empty vector equals #()")
(assert-equal (length empty) 0 "empty vector has length 0")

(assert-equal (if v "not empty" "empty") "not empty" "non-empty vector is truthy")
(assert-equal (if empty "not empty" "empty") "not empty" "empty vector is truthy (only nil is falsy)")
(assert-equal (if empty1 "not empty" "empty") "not empty" "empty vector literal is truthy")
(assert-equal (if v1 "not empty" "empty") "not empty" "non-empty vector literal is truthy")
