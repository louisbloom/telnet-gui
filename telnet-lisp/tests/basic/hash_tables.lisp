;; Hash table operations examples

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; Create a hash table
(define ht (make-hash-table))

;; Check if hash-table? predicate works
(assert-true (hash-table? ht) "make-hash-table creates a hash table")
(assert-equal (hash-count ht) 0 "new hash table is empty")

;; Store values
(hash-set! ht "name" "Alice")
(hash-set! ht "age" 30)
(hash-set! ht "city" "New York")
(assert-equal (hash-count ht) 3 "hash table contains 3 entries")

;; Retrieve values
(assert-equal (hash-ref ht "name") "Alice" "hash-ref retrieves string value")
(assert-equal (hash-ref ht "age") 30 "hash-ref retrieves integer value")
(assert-equal (hash-ref ht "city") "New York" "hash-ref retrieves city value")
(assert-nil (hash-ref ht "missing") "hash-ref returns nil for missing key")

;; Update existing values
(hash-set! ht "age" 31)
(assert-equal (hash-ref ht "age") 31 "hash-set! updates existing value")

;; Remove entries
(hash-remove! ht "city")
(assert-nil (hash-ref ht "city") "hash-remove! removes entry")
(assert-equal (hash-count ht) 2 "hash-count decreases after removal")

;; Clear all entries
(hash-clear! ht)
(assert-equal (hash-count ht) 0 "hash-clear! empties hash table")

;; Hash tables are always truthy (even when empty - only nil is falsy in this Lisp)
(assert-true ht "empty hash table is truthy")
(assert-true (if ht #t #f) "empty hash table evaluates to true in conditional")

(hash-set! ht "key" "value")
(assert-true ht "non-empty hash table is truthy")
(assert-equal (hash-count ht) 1 "hash table has one entry after adding")

;; Use with symbols as keys
(hash-set! ht 'name "Bob")
(assert-equal (hash-ref ht 'name) "Bob" "hash-set! and hash-ref work with symbol keys")
