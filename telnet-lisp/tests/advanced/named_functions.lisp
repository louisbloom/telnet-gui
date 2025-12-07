;; Named Functions Examples
;; Demonstrate how function names appear in stack traces and REPL output

(load "tests/test-helpers.lisp")

;; ===========================================
;; Basic Named Functions
;; ===========================================

;; Define a simple named function
(define add (lambda (x y) (+ x y)))
;; add should print #<lambda:add>

(assert-equal (add 3 5) 8 "named function add")

;; Define factorial
(define factorial (lambda (n)
                    (if (<= n 1)
                      1
                      (* n (factorial (- n 1))))))

;; factorial should print #<lambda:factorial>
(assert-equal (factorial 5) 120 "named factorial function")

;; ===========================================
;; Anonymous Lambdas (no name)
;; ===========================================

;; Anonymous lambda still works
(assert-equal ((lambda (x) (* x x)) 5) 25 "anonymous lambda")

;; Store anonymous lambda in variable
(define anon (lambda (x) (+ x 1)))
;; anon should print #<lambda:anon> (gets named from define)

;; ===========================================
;; Error Testing with Named Functions
;; ===========================================

;; Function that calls undefined variable
(define bad-func (lambda () undefined-var))

;; Should show "bad-func" in stack trace
(assert-error (bad-func) "named function with undefined variable")

;; ===========================================
;; Nested Named Function Calls
;; ===========================================

(define outer (lambda () (middle)))
(define middle (lambda () (inner)))
(define inner (lambda () (deepest)))
(define deepest (lambda () unknown-symbol))

;; Should show full call chain: outer -> middle -> inner -> deepest
(assert-error (outer) "nested named function calls with error")

;; ===========================================
;; Named Functions with Parameters
;; ===========================================

(define greet (lambda (name)
                (define greeting (lambda () (concat "Hello, " name)))
                (greeting)))

(assert-equal (greet "World") "Hello, World" "named function with closure")

;; ===========================================
;; Reassignment
;; ===========================================

(define func1 (lambda () "first"))
;; func1 => #<lambda:func1>
(assert-equal (func1) "first" "initial function definition")

;; Redefine with new lambda
(define func1 (lambda () "second"))
;; func1 => #<lambda:func1>
(assert-equal (func1) "second" "function redefinition")

;; ===========================================
;; Let bindings (lambdas stay anonymous)
;; ===========================================

(assert-equal (let ((f (lambda (x) (* x 2))))
                (f 10))
  20
  "lambda in let binding")
;; The lambda in let stays anonymous

;; ===========================================
;; Higher-order functions
;; ===========================================

(define make-adder (lambda (x)
                     (lambda (y) (+ x y))))

(define add5 (make-adder 5))
;; add5 => #<lambda:add5>
(assert-equal (add5 10) 15 "higher-order function")

;; The inner lambda from make-adder is anonymous
;; But when we define it as add5, it gets that name

;; ===========================================
;; Recursive named functions
;; ===========================================

(define fib (lambda (n)
              (if (< n 2)
                n
                (+ (fib (- n 1)) (fib (- n 2))))))

(assert-equal (fib 10) 55 "recursive named function")

