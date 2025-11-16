;; Named Functions Examples
;; Demonstrate how function names appear in stack traces and REPL output

;; ===========================================
;; Basic Named Functions
;; ===========================================

; Define a simple named function
(define add (lambda (x y) (+ x y)))
add  ; Should print #<lambda:add>

(add 3 5)  ; => 8

; Define factorial
(define factorial (lambda (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1))))))

factorial  ; Should print #<lambda:factorial>
(factorial 5)  ; => 120

; ===========================================
;; Anonymous Lambdas (no name)
;; ===========================================

; Anonymous lambda still works
((lambda (x) (* x x)) 5)  ; => 25

; Store anonymous lambda in variable
(define anon (lambda (x) (+ x 1)))
anon  ; Should print #<lambda:anon> (gets named from define)

; ===========================================
;; Error Testing with Named Functions
;; ===========================================

; Function that calls undefined variable
(define bad-func (lambda () undefined-var))

; Uncomment to test - should show "bad-func" in stack trace
; (bad-func)

; ===========================================
;; Nested Named Function Calls
;; ===========================================

(define outer (lambda () (middle)))
(define middle (lambda () (inner)))
(define inner (lambda () (deepest)))
(define deepest (lambda () unknown-symbol))

; Uncomment to test - should show full call chain:
; outer -> middle -> inner -> deepest
; (outer)

; ===========================================
;; Named Functions with Parameters
;; ===========================================

(define greet (lambda (name)
  (define greeting (lambda () (+ "Hello, " name)))
  (greeting)))

(greet "World")  ; => "Hello, World"

; ===========================================
;; Reassignment
;; ===========================================

(define func1 (lambda () "first"))
func1  ; => #<lambda:func1>
(func1)  ; => "first"

; Redefine with new lambda
(define func1 (lambda () "second"))
func1  ; => #<lambda:func1>
(func1)  ; => "second"

; ===========================================
;; Let bindings (lambdas stay anonymous)
;; ===========================================

(let ((f (lambda (x) (* x 2))))
  (f 10))  ; => 20
; The lambda in let stays anonymous

; ===========================================
;; Higher-order functions
;; ===========================================

(define make-adder (lambda (x)
  (lambda (y) (+ x y))))

(define add5 (make-adder 5))
add5  ; => #<lambda:add5>
(add5 10)  ; => 15

; The inner lambda from make-adder is anonymous
; But when we define it as add5, it gets that name

; ===========================================
;; Recursive named functions
;; ===========================================

(define fib (lambda (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

(fib 10)  ; => 55

; ===========================================
;; Summary
;; ===========================================

"Named functions test complete. Functions now show their names in:"
"1. REPL output: #<lambda:name>"
"2. Stack traces: at name"
"3. Error messages: Error in name"

