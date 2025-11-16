;;; Test alist operations

;; Create a simple alist
(define simple-alist '(("a" . 1) ("b" . 2) ("c" . 3)))  ; ignore

;; Test assoc - find by value equality
(assoc "a" simple-alist)        ; => ("a" . 1)
(assoc "b" simple-alist)        ; => ("b" . 2)
(assoc "c" simple-alist)        ; => ("c" . 3)
(assoc "d" simple-alist)        ; => nil

;; Test assoc with integer keys
(define num-alist '((1 . "one") (2 . "two") (3 . "three")))  ; ignore
(assoc 1 num-alist)             ; => (1 . "one")
(assoc 2 num-alist)             ; => (2 . "two")
(assoc 4 num-alist)             ; => nil

;; Test alist-get - returns value only
(alist-get "a" simple-alist)    ; => 1
(alist-get "b" simple-alist)    ; => 2
(alist-get "c" simple-alist)    ; => 3
(alist-get "d" simple-alist)    ; => nil

;; Test alist-get with default value
(alist-get "a" simple-alist 99)  ; => 1
(alist-get "z" simple-alist 99)  ; => 99

;; Test with nested pairs
(define nested-alist '(("x" . 10) ("y" . 20) ("z" . 30)))  ; ignore
(car (assoc "x" nested-alist))  ; => "x"
(cdr (assoc "x" nested-alist))  ; => 10

;; Test assq - pointer equality (for symbols/same objects)
(define sym1 "test")            ; ignore
(define sym2 "test")            ; ignore
(define sym-alist (list (cons sym1 100) (cons sym2 200)))  ; ignore

;; assoc uses value equality, so finds first match
(alist-get sym1 sym-alist)      ; => 100

;; Test mapping functions

;; Test map with builtin function
(map (lambda (x) (* x 2)) '(1 2 3 4 5))  ; => (2 4 6 8 10)

;; Test mapcar (should be same as map)
(mapcar (lambda (x) (* x 2)) '(1 2 3 4 5))  ; => (2 4 6 8 10)

;; Test map with addition
(map (lambda (x) (+ x 10)) '(1 2 3))  ; => (11 12 13)

;; Test map with string operations
(define words '("hello" "world" "lisp"))  ; ignore
(map (lambda (s) (string-upcase s)) words)  ; => ("HELLO" "WORLD" "LISP")

;; Test map on empty list
(map (lambda (x) (* x 2)) '())  ; => nil

;; Test map with more complex function
(map (lambda (x) (if (> x 5) "big" "small")) '(3 7 2 9 4))  ; => ("small" "big" "small" "big" "small")

;; Test combining alists and map
(define people '(("Alice" . 25) ("Bob" . 30) ("Carol" . 35)))  ; ignore
(map (lambda (pair) (car pair)) people)  ; => ("Alice" "Bob" "Carol")
(map (lambda (pair) (cdr pair)) people)  ; => (25 30 35)

;; Test map to double all values in an alist (returns list of pairs)
(map (lambda (pair) (cons (car pair) (* (cdr pair) 2))) '((1 . 10) (2 . 20) (3 . 30)))  ; => ((1 . 20) (2 . 40) (3 . 60))
