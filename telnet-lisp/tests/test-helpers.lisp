;; Test Assertion Macros for telnet-lisp Test Suite
;; Provides assertion macros for test files: assert-equal, assert-true, assert-false,
;; assert-error, and assert-nil. All macros abort tests via (error ...) on failure.

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

;; Assert that expression throws an error
;; Usage: (assert-error expr "description")
;; Returns: nil on success (if error thrown), aborts test if expr succeeds
(defmacro assert-error (expr message)
  `(condition-case err
     (progn
       ,expr
       (error (format nil "Assertion failed: ~A (expected error but succeeded)" ,message)))
     (error nil)))  ; Success: error was thrown

;; Assert that expression evaluates to nil explicitly
;; Usage: (assert-nil expr "description")
;; Returns: nil on success, aborts test with error on failure
(defmacro assert-nil (expr message)
  `(let ((result ,expr))
     (if (null? result)
       nil  ; Success
       (error (format nil "Assertion failed: ~A (expected nil, got: ~S)" ,message result)))))
