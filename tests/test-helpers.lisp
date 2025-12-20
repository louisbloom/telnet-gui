;; Test Assertion Macros for TinTin++ Test Suite
;; These macros provide assert-equal, assert-true, and assert-false functionality
;; for use in test files.

;; Assert that actual equals expected (handles numbers and structural equality)
;; Usage: (assert-equal actual expected "description")
;; Returns: nil on success, aborts test with error on failure
(defmacro assert-equal (actual expected message)
  `(let ((actual-val ,actual)
          (expected-val ,expected))
     ;; Use = for numbers, equal? for everything else
     (let ((values-equal (if (and (number? actual-val) (number? expected-val))
                           (= actual-val expected-val)
                           (equal? actual-val expected-val))))
       (if values-equal
         nil  ; Success: silent
         (error (format nil "Assertion failed: ~A~%  Expected: ~S~%  Actual:   ~S"
                  ,message expected-val actual-val))))))

;; Assert that condition evaluates to truthy value (anything except nil/#f)
;; Usage: (assert-true condition "description")
;; Returns: nil on success, aborts test with error on failure
(defmacro assert-true (condition message)
  `(let ((result ,condition))
     (if result
       nil  ; Success: silent
       (error (format nil "Assertion failed: ~A (expected truthy, got: ~S)"
                ,message result)))))

;; Assert that condition evaluates to falsy value (nil or #f)
;; Usage: (assert-false condition "description")
;; Returns: nil on success, aborts test with error on failure
(defmacro assert-false (condition message)
  `(let ((result ,condition))
     (if result
       (error (format nil "Assertion failed: ~A (expected falsy, got: ~S)"
                ,message result))
       nil)))  ; Success: silent

;; ============================================================================
;; Mock hook system (for tests that load code using add-hook)
;; ============================================================================

(define *hooks* '())
(defvar *user-input-handled* nil)
(defvar *user-input-result* nil)

(defun add-hook (hook-name fn)
  "Mock add-hook: record but don't execute."
  (set! *hooks* (cons (cons hook-name fn) *hooks*))
  nil)

(defun remove-hook (hook-name fn)
  "Mock remove-hook: do nothing."
  nil)

(defun run-hook (hook-name &rest args)
  "Mock run-hook: do nothing."
  nil)
