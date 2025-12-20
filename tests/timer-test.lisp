;; Timer system tests
;; Tests run-at-time, cancel-timer, cancel-function-timers, list-timers, run-timers
(load "test-helpers.lisp")

;; ============================================================================
;; Test run-at-time and list-timers
;; ============================================================================

(define test-timer (run-at-time 10 nil (lambda () nil)))
(assert-true (list? test-timer) "run-at-time returns a list")
(assert-equal (list-length test-timer) 5 "timer list has 5 elements")
(assert-equal (list-length (list-timers)) 1 "list-timers shows 1 timer")

;; ============================================================================
;; Test cancel-timer
;; ============================================================================

(assert-true (cancel-timer test-timer) "cancel-timer returns true for valid timer")
(assert-equal (list-length (list-timers)) 0 "timer removed after cancel")
(assert-false (cancel-timer test-timer) "cancel-timer returns false for already cancelled")

;; ============================================================================
;; Test cancel-function-timers
;; ============================================================================

(define my-fn (lambda () nil))
(run-at-time 10 nil my-fn)
(run-at-time 20 nil my-fn)
(run-at-time 30 nil (lambda () nil))  ; different function
(assert-equal (list-length (list-timers)) 3 "3 timers created")
(assert-equal (cancel-function-timers my-fn) 2 "cancel-function-timers returns count")
(assert-equal (list-length (list-timers)) 1 "1 timer remains")

;; Cleanup
(set! *timer-list* '())

;; ============================================================================
;; Test run-timers fires one-shot timers
;; ============================================================================

(define *callback-count* 0)

(run-at-time 0 nil (lambda () (set! *callback-count* (+ *callback-count* 1))))
(assert-equal (list-length (list-timers)) 1 "timer created")

(run-timers)
(assert-equal *callback-count* 1 "callback was called")
(assert-equal (list-length (list-timers)) 0 "one-shot timer removed after firing")

;; ============================================================================
;; Test run-timers with repeating timers
;; ============================================================================

(set! *callback-count* 0)
(define repeat-timer (run-at-time 0 1 (lambda () (set! *callback-count* (+ *callback-count* 1)))))

(run-timers)
(assert-equal *callback-count* 1 "repeating timer fired once")
(assert-equal (list-length (list-timers)) 1 "repeating timer still in list")

;; Cancel using original reference (ID-based matching)
(assert-true (cancel-timer repeat-timer) "can cancel rescheduled timer by original reference")
(assert-equal (list-length (list-timers)) 0 "repeating timer cancelled")

;; ============================================================================
;; Test run-timers with callback arguments
;; ============================================================================

(define *callback-args* nil)
(run-at-time 0 nil (lambda (a b) (set! *callback-args* (list a b))) "hello" 42)

(run-timers)
(assert-equal *callback-args* '("hello" 42) "callback received arguments")

;; ============================================================================
;; Test timer doesn't fire before its time
;; ============================================================================

(set! *callback-count* 0)
;; Create timer far in the future (won't fire)
(define future-timer (run-at-time 9999999 nil (lambda () (set! *callback-count* 1))))

(run-timers)
(assert-equal *callback-count* 0 "future timer did not fire")
(assert-equal (list-length (list-timers)) 1 "future timer still in list")

(cancel-timer future-timer)

;; ============================================================================
;; Test multiple timers fire in same run-timers call
;; ============================================================================

(set! *callback-count* 0)
(run-at-time 0 nil (lambda () (set! *callback-count* (+ *callback-count* 1))))
(run-at-time 0 nil (lambda () (set! *callback-count* (+ *callback-count* 10))))
(run-at-time 0 nil (lambda () (set! *callback-count* (+ *callback-count* 100))))

(run-timers)
(assert-equal *callback-count* 111 "all timers fired in same run-timers call")

;; Cleanup
(set! *timer-list* '())

(princ "All timer tests passed!\n")
