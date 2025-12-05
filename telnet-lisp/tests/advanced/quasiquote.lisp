                                        ; Test quasiquote (backquote) functionality

                                        ; Test 1: Simple quasiquote without unquote (same as quote)
`(1 2 3)                                ; => (1 2 3)

                                        ; Test 2: Unquote a single value
(define x 42)                           ; ignore
`(1 ,x 3)                               ; => (1 42 3)

                                        ; Test 3: Multiple unquotes
`(,x ,(+ 1 2) ,(* 2 3))                 ; => (42 3 6)

                                        ; Test 4: Nested lists with unquote
`(a (b ,x c) d)                         ; => (a (b 42 c) d)

                                        ; Test 5: Unquote-splicing with a list
(define lst '(a b c))                   ; ignore
`(1 ,@lst 4)                            ; => (1 a b c 4)

                                        ; Test 6: Multiple unquote-splicing
`(,@lst ,@lst)                          ; => (a b c a b c)

                                        ; Test 7: Mixing unquote and unquote-splicing
`(start ,x middle ,@lst end)            ; => (start 42 middle a b c end)

                                        ; Test 8: Empty list unquote-splicing
(define empty '())                      ; ignore
`(1 ,@empty 2)                          ; => (1 2)

                                        ; Test 9: Using quasiquote in a macro
(defmacro my-when (condition body)
  `(if ,condition ,body nil))
(my-when #t 99)                         ; => 99
(my-when #f 99)                         ; => nil

                                        ; Test 10: Building code with quasiquote
(define make-adder (lambda (n)
                     `(lambda (x) (+ x ,n))))
(make-adder 5)                          ; => (lambda (x) (+ x 5))
