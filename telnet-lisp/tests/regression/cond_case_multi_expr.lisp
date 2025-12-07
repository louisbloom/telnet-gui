                                        ; Regression test for cond/case with multiple expressions in clause body
                                        ; Bug: eval_cond and eval_case only evaluated first expression, ignoring rest
                                        ; Fixed: Changed to use eval_progn to evaluate all expressions (implicit progn)

                                        ; Test 1: cond with multiple expressions (implicit progn)
(define x 0)
(define y 0)
(cond
  ((= 1 1)
    (set! x 10)
    (set! y 20)))
x  ; => 10
y  ; => 20

                                        ; Test 2: cond else clause with multiple expressions
(define a 0)
(define b 0)
(cond
  ((= 1 2) "never")
  (else
    (set! a 30)
    (set! b 40)))
a  ; => 30
b  ; => 40

                                        ; Test 3: case with multiple expressions
(define m 0)
(define n 0)
(case 5
  ((1 2 3) "small")
  ((4 5 6)
    (set! m 50)
    (set! n 60))
  (else "large"))
m  ; => 50
n  ; => 60

                                        ; Test 4: case else clause with multiple expressions
(define p 0)
(define q 0)
(case 99
  ((1 2 3) "small")
  (else
    (set! p 70)
    (set! q 80)))
p  ; => 70
q  ; => 80

                                        ; Test 5: cond in do loop (original bug scenario)
(define result "")
(define counter 0)
(do ()
  ((>= counter 3) result)
  (cond
    (#t
      (set! result (concat result "X"))
      (set! counter (+ counter 1)))))
result   ; => "XXX"
counter  ; => 3

                                        ; Test 6: cond with nested let and multiple set! (exact bug scenario)
(let ((a 0)
       (b "")
       (limit 3))
  (do ()
    ((>= a limit) b)
    (let ((ch "Y"))
      (cond
        (#t
          (set! b (concat b ch))
          (set! a (+ a 1)))))))
                                        ; => "YYY"

                                        ; Test 7: case returns value of last expression
(case 1
  ((1)
    (define temp1 100)
    (define temp2 200)
    (+ temp1 temp2)))
                                        ; => 300

                                        ; Test 8: cond returns value of last expression
(cond
  (#t
    (define temp3 111)
    (define temp4 222)
    (+ temp3 temp4)))
                                        ; => 333
