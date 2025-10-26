;; Integer and mixed arithmetic examples

;; Integer arithmetic
(+ 1 2 3)                       ; => 6 (integer)
(- 10 3)                        ; => 7 (integer)
(* 2 3 4)                       ; => 24 (integer)

;; Mixed integer/float
(+ 1 2.5)                       ; => 3.5 (float)
(* 3 4.0)                       ; => 12.0 (float)
(+ 1.5 2)                       ; => 3.5 (float)

;; Division
(/ 10 2)                        ; => 5.0 (always float)
(/ 10 3)                        ; => 3.33333... (float)
(quotient 10 3)                 ; => 3 (integer, truncated)

;; Type coercion in operations
(define a 5)                    ; integer
(define b 2.5)                  ; float
(+ a b)                         ; => 7.5 (float - promoted)
(* a 10)                        ; => 50 (integer - both integers)

;; Comparisons work with both types
(> 5 3)                         ; => 1
(> 5.0 3)                       ; => 1
(< 2 3.5)                       ; => 1
(= 5 5.0)                       ; => 1 (equal values)
(= 5.0 5)                      ; => 1 (equal values)

;; Truthy/falsy behavior
(define x 0)
x                               ; => 0 (falsy)
(define y 5)
y                               ; => 5 (truthy)
(if 0 "yes" "no")              ; => "no" (0 is falsy)
(if 1 "yes" "no")              ; => "yes" (non-zero is truthy)
