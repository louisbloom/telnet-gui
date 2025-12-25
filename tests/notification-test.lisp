;; Notification system tests
;; Tests notify, notification-active?, notification-clear, and queue behavior
(load "test-helpers.lisp")

;; ============================================================================
;; Mock notification-set for testing (C builtin not available in lisp-repl)
;; ============================================================================

(defvar *mock-notification-text* nil
  "Mock storage for notification text (for testing)")

;; Mock notification-set if not already defined (C builtin)
(if (not (bound? 'notification-set))
    (defun notification-set (text)
      "Mock notification-set for testing"
      (set! *mock-notification-text* text)))

;; ============================================================================
;; Setup - Clear any existing state
;; ============================================================================

(notification-clear)
(assert-false (notification-active?) "no notification active initially")
(assert-equal *notification-queue* '() "queue empty initially")

;; ============================================================================
;; Test notification-set builtin (low-level C interface)
;; ============================================================================

;; notification-set with string
(notification-set "test message")
;; We can't easily verify the C-side state, but at least it doesn't error

;; notification-set with nil clears
(notification-set nil)
;; Again, no error means success for builtin

;; ============================================================================
;; Test notify creates timer and sets active state
;; ============================================================================

(notify "Hello!" 1000)
(assert-true (notification-active?) "notification is active after notify")
(assert-true *notification-timer* "timer was created")
(assert-equal *notification-queue* '() "queue empty when first notification shown immediately")

;; ============================================================================
;; Test queuing behavior when notification already active
;; ============================================================================

;; Add more notifications while one is active
(notify "Second" 2000)
(notify "Third" 3000)

(assert-equal (length *notification-queue*) 2 "two notifications queued")
(assert-equal (car (car *notification-queue*)) "Second" "first queued message is 'Second'")
(assert-equal (cdr (car *notification-queue*)) 2000 "first queued timeout is 2000")
(assert-equal (car (list-ref *notification-queue* 1)) "Third" "second queued message is 'Third'")
(assert-equal (cdr (list-ref *notification-queue* 1)) 3000 "second queued timeout is 3000")

;; ============================================================================
;; Test notification-clear clears everything
;; ============================================================================

(notification-clear)
(assert-false (notification-active?) "not active after clear")
(assert-false *notification-timer* "timer cleared")
(assert-equal *notification-queue* '() "queue cleared")

;; ============================================================================
;; Test notification-dismiss advances queue
;; ============================================================================

;; Set up: show one, queue two
(notify "First" 1000)
(notify "Second" 2000)
(notify "Third" 3000)

(assert-equal (length *notification-queue*) 2 "two in queue before dismiss")

;; Manually call dismiss (simulates timer firing)
(notification-dismiss)

;; Now "Second" should be showing, "Third" in queue
(assert-true (notification-active?) "still active after dismiss (next notification)")
(assert-equal (length *notification-queue*) 1 "one in queue after dismiss")
(assert-equal (car (car *notification-queue*)) "Third" "Third is now queued")

;; Dismiss again
(notification-dismiss)
(assert-true (notification-active?) "still active (Third showing)")
(assert-equal (length *notification-queue*) 0 "queue empty")

;; Final dismiss
(notification-dismiss)
(assert-false (notification-active?) "not active after all dismissed")
(assert-equal *notification-queue* '() "queue still empty")

;; ============================================================================
;; Test default timeout is used when not specified
;; ============================================================================

(notification-clear)
(define original-timeout *notification-timeout*)
(set! *notification-timeout* 9999)

(notify "Default timeout")
;; Check queue has our message with default timeout
;; But since it shows immediately, we need to queue another to see the timeout
(notify "Second with default")

(assert-equal (cdr (car *notification-queue*)) 9999 "default timeout used")

;; Restore
(set! *notification-timeout* original-timeout)
(notification-clear)

;; ============================================================================
;; Test ANSI escape sequences in notifications (doesn't error)
;; ============================================================================

(notify "\033[32m✓ Success\033[0m" 1000)
(assert-true (notification-active?) "ANSI notification is active")
(notification-clear)

;; ============================================================================
;; Test emoji in notifications (doesn't error)
;; ============================================================================

(notify "🔔 Alert!" 1000)
(assert-true (notification-active?) "emoji notification is active")
(notification-clear)

;; ============================================================================
;; Cleanup
;; ============================================================================

(notification-clear)
(set! *timer-list* '())

(princ "All notification tests passed!\n")
