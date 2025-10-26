; Mini Lisp Interpreter Demo
; This file demonstrates all the features of the interpreter

; ===== ARITHMETIC =====
(define a 10)
(define b 20)
(+ a b)                    ; 30
(- 100 25)                 ; 75
(* 6 7)                    ; 42
(/ 100 4)                  ; 25

; ===== FUNCTIONS =====
(define square (lambda (x) (* x x)))
(square 8)                 ; 64

(define factorial
  (lambda (n)
    (if (<= n 1)
      1
      (* n (factorial (- n 1))))))
(factorial 6)              ; 720

; ===== CONDITIONALS =====
(if (> 10 5) "yes" "no")   ; "yes"
(if (< 3 2) "yes" "no")    ; "no"

; ===== STRING OPERATIONS =====
(concat "Hello" " " "World")           ; "Hello World"
(split "red,green,blue" ",")           ; ("red" "green" "blue")
(string-contains "programming" "gram") ; 1
(string-match "test123" "test*")       ; 1

; ===== COMPARISONS =====
(> 10 5)                   ; 1
(< 10 5)                   ; nil
(= 42 42)                  ; 1
(>= 10 10)                 ; 1
(<= 5 10)                  ; 1

(string= "hello" "hello")  ; 1
(string< "abc" "xyz")      ; 1

; ===== BOOLEAN OPERATIONS =====
(and 1 2 3)                ; 3
(or nil 42)                ; 42
(not nil)                  ; 1
(not 5)                    ; nil

; ===== LIST OPERATIONS =====
(define mylist (list 1 2 3 4 5))
(car mylist)               ; 1
(cdr mylist)               ; (2 3 4 5)
(cons 0 mylist)            ; (0 1 2 3 4 5)

; ===== QUOTES =====
'(1 2 3)                   ; (1 2 3)
(car '(a b c))             ; a

; ===== LET BINDINGS =====
(let ((x 5) (y 10))
  (+ (* x x) (* y y)))     ; 125

; ===== COMPLEX EXAMPLE =====
(define abs (lambda (n) (if (< n 0) (- n) n)))
(abs -42)                  ; 42

(define max
  (lambda (a b)
    (if (> a b) a b)))
(max 15 23)                ; 23
