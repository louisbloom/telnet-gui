;; Collection iteration examples
;; Demonstrate how to iterate over lists, vectors, and hash tables using the do loop

;; ===========================================
;; Hash Table Iteration
;; ===========================================

(define ht (make-hash-table))
(hash-set! ht "name" "Alice")
(hash-set! ht "age" 30)
(hash-set! ht "city" "NYC")

;; Get all keys
(define keys (hash-keys ht))
;; keys = ("name" "age" "city")

;; Get all values  
(define values (hash-values ht))
;; values = ("Alice" 30 "NYC")

;; Get key-value pairs
(define entries (hash-entries ht))
;; entries = (("name" . "Alice") ("age" . 30) ("city" . "NYC"))

;; Iterate over keys
(do ((remaining keys (cdr remaining)))
    ((null? remaining) "done")
  (car remaining))

;; Iterate over entries
(do ((remaining entries (cdr remaining)))
    ((null? remaining) "done")
  (let ((entry (car remaining)))
    (let ((key (car entry))
          (val (cdr entry)))
      (concat key ": " val))))

;; ===========================================
;; Vector Iteration
;; ===========================================

(define v #(10 20 30 40 50))

;; Iterate with index
(do ((i 0 (+ i 1)))
    ((>= i (vector-length v)) "done")
  (vector-ref v i))

;; Sum all elements
(define sum 0)
(do ((i 0 (+ i 1))
     (acc 0))
    ((>= i (vector-length v)) acc)
  (set! acc (+ acc (vector-ref v i))))

;; ===========================================
;; List Iteration
;; ===========================================

(define lst '(1 2 3 4 5))

;; Basic list traversal
(do ((remaining lst (cdr remaining)))
    ((null? remaining) "done")
  (car remaining))

;; Get list length
(list-length lst)
;; => 5

;; Access by index
(list-ref lst 0)    ; => 1
(list-ref lst 2)    ; => 3
(list-ref lst 4)    ; => 5

;; Iterate with index (using do)
(do ((remaining lst (cdr remaining))
     (i 0 (+ i 1)))
    ((null? remaining) "done")
  (car remaining))

;; ===========================================
;; Practical Examples
;; ===========================================

;; Collect all positive numbers from a list
(define numbers '(10 -5 3 -2 8 0 -1))
(define positive '())
(do ((remaining numbers (cdr remaining)))
    ((null? remaining) positive)
  (let ((n (car remaining)))
    (if (> n 0)
        (set! positive (cons n positive)))))

;; Count hash table entries with specific value
(define user_ht (make-hash-table))
(hash-set! user_ht "user1" "admin")
(hash-set! user_ht "user2" "user")
(hash-set! user_ht "user3" "admin")

(define admins 0)
(define entries (hash-entries user_ht))
(do ((remaining entries (cdr remaining)))
    ((null? remaining) admins)
  (let ((entry (car remaining)))
    (let ((key (car entry))
          (val (cdr entry)))
      (if (string= val "admin")
          (set! admins (+ admins 1))))))

;; ===========================================
;; Empty Collections
;; ===========================================

(define empty_ht (make-hash-table))
(hash-keys empty_ht)      ; => ()
(hash-values empty_ht)    ; => ()
(hash-entries empty_ht)   ; => ()

(define empty_list '())
(list-length empty_list)  ; => 0

(define empty_vec #())
(vector-length empty_vec) ; => 0

