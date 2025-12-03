;; List Reverse Tests

;; Basic List Reversal
(reverse '())                       ; => nil
(reverse nil)                       ; => nil
(reverse '(1))                      ; => (1)
(reverse '(1 2))                    ; => (2 1)
(reverse '(1 2 3))                  ; => (3 2 1)
(reverse '(1 2 3 4 5))              ; => (5 4 3 2 1)

;; Different Element Types
(reverse '("a" "b" "c"))            ; => ("c" "b" "a")
(reverse '(#t #f #t))               ; => (#t #f #t)
(reverse '(1 "two" 3))              ; => (3 "two" 1)

;; Nested Lists
(reverse '((1 2) (3 4)))            ; => ((3 4) (1 2))
(reverse '(1 (2 3) 4))              ; => (4 (2 3) 1)

;; Use with Other Functions
(define lst '(1 2 3 4 5))           ; ignore
(list-length (reverse lst))         ; => 5
(car (reverse lst))                 ; => 5
(list-ref (reverse lst) 0)          ; => 5
(list-ref (reverse lst) 4)          ; => 1

;; Double reverse returns original
(reverse (reverse '(1 2 3)))        ; => (1 2 3)

;; Integration with cons and car/cdr
(car (reverse '(1 2 3)))            ; => 3
(cdr (reverse '(1 2 3)))            ; => (2 1)
