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
;; Load init.lisp for hook system and other core functionality
;; ============================================================================
;; Tests run via lisp-repl need init.lisp loaded for the hook system.
;; Path is relative to build/tests/ directory where tests run.
(load "../lisp/init.lisp")
