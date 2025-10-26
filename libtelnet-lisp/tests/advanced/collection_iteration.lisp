;; Collection Iteration Examples
;; Comprehensive demonstration of iterating over hash tables, vectors, and lists using do loops

;; ===========================================
;; Hash Table Iteration
;; ===========================================

(define ht (make-hash-table))
(hash-set! ht "name" "Alice")
(hash-set! ht "age" 30)
(hash-set! ht "city" "NYC")
(hash-count ht)                        ; => 3

;; Get all keys
(define keys (hash-keys ht))
keys                                    ; => ("name" "age" "city")

;; Get all values
(define values (hash-values ht))
values                                  ; => ("Alice" 30 "NYC")

;; Get key-value pairs as list of cons cells
(define entries (hash-entries ht))
entries                                 ; => (("name" . "Alice") ("age" . 30) ("city" . "NYC"))

;; Iterate over keys using do loop
(do ((remaining keys (cdr remaining)))  ; => "done"
    ((null? remaining) "done")
  (car remaining))

;; Iterate over entries and process key-value pairs
(do ((remaining entries (cdr remaining)))  ; => "done"
    ((null? remaining) "done")
  (let ((entry (car remaining)))
    (let ((key (car entry))
          (val (cdr entry)))
      (concat key ": " val))))

;; ===========================================
;; Vector Iteration
;; ===========================================

(define v #(10 20 30 40 50))

;; Iterate with index and access elements
(do ((i 0 (+ i 1)))                    ; => "done"
    ((>= i (vector-length v)) "done")
  (vector-ref v i))

;; Sum all elements using accumulator
(define sum 0)
(do ((i 0 (+ i 1))
     (acc 0))
    ((>= i (vector-length v)) acc)      ; => 150
  (set! acc (+ acc (vector-ref v i))))

;; ===========================================
;; List Iteration
;; ===========================================

(define lst '(1 2 3 4 5))

;; Basic list traversal with do loop
(do ((remaining lst (cdr remaining)))   ; => "done"
    ((null? remaining) "done")
  (car remaining))

;; Get list length
(list-length lst)                       ; => 5

;; Access by index
(list-ref lst 0)                        ; => 1
(list-ref lst 2)                        ; => 3
(list-ref lst 4)                        ; => 5

;; Iterate with index counter
(do ((remaining lst (cdr remaining))
     (i 0 (+ i 1)))
    ((null? remaining) "done")          ; => "done"
  (car remaining))

;; ===========================================
;; Practical Examples: Filtering and Transformation
;; ===========================================

;; Collect all positive numbers from a list
(define numbers '(10 -5 3 -2 8 0 -1))
(define positive '())
(do ((remaining numbers (cdr remaining)))
    ((null? remaining) positive)        ; => (8 3 10)
  (let ((n (car remaining)))
    (if (> n 0)
        (set! positive (cons n positive)))))

;; Count hash table entries with specific value
(define user_ht (make-hash-table))
(hash-set! user_ht "user1" "admin")
(hash-set! user_ht "user2" "user")
(hash-set! user_ht "user3" "admin")
(hash-count user_ht)                   ; => 3

(define admins 0)
(define entries (hash-entries user_ht))
(do ((remaining entries (cdr remaining)))
    ((null? remaining) admins)         ; => 2
  (let ((entry (car remaining)))
    (let ((key (car entry))
          (val (cdr entry)))
      (if (string= val "admin")
          (set! admins (+ admins 1))))))

;; ===========================================
;; Empty Collections
;; ===========================================

(define empty_ht (make-hash-table))
(hash-keys empty_ht)                   ; => ()
(hash-values empty_ht)                 ; => ()
(hash-entries empty_ht)                 ; => ()
(hash-count empty_ht)                  ; => 0

(define empty_list '())
(list-length empty_list)               ; => 0
(null? empty_list)                      ; => 1

(define empty_vec #())
(vector-length empty_vec)               ; => 0
