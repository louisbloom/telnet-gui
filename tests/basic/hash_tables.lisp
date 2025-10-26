;; Hash table operations examples

;; Create a hash table
(define ht (make-hash-table))
ht                              ; => #<hash-table>
(hash-count ht)                ; => 0 (empty)

;; Store values
(hash-set! ht "name" "Alice")
(hash-set! ht "age" 30)
(hash-set! ht "city" "New York")
(hash-count ht)                ; => 3

;; Retrieve values
(hash-ref ht "name")           ; => "Alice"
(hash-ref ht "age")            ; => 30
(hash-ref ht "city")           ; => "New York"
(hash-ref ht "missing")        ; => nil (not found)

;; Update existing values
(hash-set! ht "age" 31)
(hash-ref ht "age")            ; => 31

;; Remove entries
(hash-remove! ht "city")
(hash-ref ht "city")           ; => nil (removed)
(hash-count ht)                ; => 2

;; Clear all entries
(hash-clear! ht)
(hash-count ht)                ; => 0 (cleared)

;; Hash tables are truthy when non-empty, falsy when empty
ht                              ; => #<hash-table> (falsy because empty)

(hash-set! ht "key" "value")
ht                              ; => #<hash-table> (truthy because non-empty)

;; Use with symbols as keys
(hash-set! ht 'name "Bob")
(hash-ref ht 'name)            ; => "Bob"
