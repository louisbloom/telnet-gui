; Comprehensive test of all Mini LISP features
; This file tests every implemented feature

; === ARITHMETIC ===
(+ 1 2 3 4 5)              ; 15
(- 100 25)                 ; 75
(* 6 7)                    ; 42
(/ 100 4)                  ; 25
(- 5)                      ; -5 (unary negation)

; === VARIABLES ===
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))   ; ~314.159

; === FUNCTIONS ===
(define square (lambda (x) (* x x)))
(square 7)                 ; 49

(define add3 (lambda (a b c) (+ a b c)))
(add3 10 20 30)            ; 60

; === RECURSION ===
(define fib
  (lambda (n)
    (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))
(fib 10)                   ; 55

; === CONDITIONALS ===
(if (> 10 5) "greater" "lesser")  ; "greater"
(if (< 3 2) "yes" "no")           ; "no"
(if nil "truthy" "falsy")         ; "falsy"
(if "" "truthy" "falsy")          ; "falsy"
(if 0 "truthy" "falsy")           ; "truthy"

; === NUMBER COMPARISONS ===
(> 10 5)                   ; 1
(< 5 10)                   ; 1
(= 42 42)                  ; 1
(>= 10 10)                 ; 1
(<= 5 10)                  ; 1

; === STRING OPERATIONS ===
(concat "Hello" " " "World" "!")   ; "Hello World!"
(split "red,green,blue" ",")       ; ("red" "green" "blue")
(split "a-b-c" "-")                ; ("a" "b" "c")

; === STRING COMPARISONS ===
(string= "test" "test")    ; 1
(string< "abc" "xyz")      ; 1
(string> "xyz" "abc")      ; 1
(string<= "abc" "abc")     ; 1
(string>= "xyz" "abc")     ; 1

; === STRING PREDICATES ===
(string-contains "hello world" "world")  ; 1
(string-contains "hello" "xyz")          ; nil
(string-match "test123" "test*")         ; 1
(string-match "hello" "h?llo")           ; 1

; === BOOLEAN OPERATIONS ===
(and 1 2 3)                ; 3
(and 1 nil 3)              ; nil
(or nil nil 42)            ; 42
(or nil nil)               ; nil
(not nil)                  ; 1
(not 5)                    ; nil

; === LIST OPERATIONS ===
(list 1 2 3 4 5)           ; (1 2 3 4 5)
(car (list 10 20 30))      ; 10
(cdr (list 10 20 30))      ; (20 30)
(cons 0 (list 1 2 3))      ; (0 1 2 3)

; === PREDICATES ===
(null nil)                 ; 1
(null 5)                   ; nil
(atom 42)                  ; 1
(atom (list 1 2))          ; nil

; === QUOTES ===
'(1 2 3)                   ; (1 2 3)
(quote (a b c))            ; (a b c)
(car '(first second))      ; first

; === LET BINDINGS ===
(let ((x 10) (y 20))
  (+ x y))                 ; 30

(let ((a 5))
  (let ((b 10))
    (* a b)))              ; 50

; === COMPLEX EXAMPLES ===
(define abs (lambda (n) (if (< n 0) (- n) n)))
(abs -42)                  ; 42
(abs 42)                   ; 42

(define max (lambda (a b) (if (> a b) a b)))
(max 15 23)                ; 23

(define min (lambda (a b) (if (< a b) a b)))
(min 15 23)                ; 15

; === NESTED FUNCTIONS ===
(define make-adder
  (lambda (n)
    (lambda (x) (+ x n))))

(define add5 (make-adder 5))
(add5 10)                  ; 15

; === STRING PROCESSING ===
(define join
  (lambda (sep words)
    (if (null (cdr words))
      (car words)
      (concat (car words) sep (join sep (cdr words))))))

; Test with simple case
(concat "a" "," "b")       ; "a,b"
