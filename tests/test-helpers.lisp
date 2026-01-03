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

;; Assert that expression evaluates to nil explicitly
;; Usage: (assert-nil expr "description")
;; Returns: nil on success, aborts test with error on failure
(defmacro assert-nil (expr message)
  `(let ((result ,expr))
     (if (null? result)
       nil  ; Success
       (error (format nil "Assertion failed: ~A (expected nil, got: ~S)" ,message result)))))

;; ============================================================================
;; Load init.lisp for hook system and other core functionality
;; ============================================================================
;; Tests run via lisp-repl need init.lisp loaded for the hook system.
;; Path is relative to build/tests/ directory where tests run.
(load "../lisp/init.lisp")

;; ============================================================================
;; Mock C Builtins (only available in GUI, not in lisp-repl tests)
;; ============================================================================
;; These mocks capture calls for verification in tests.
;; Only define if not already defined (allows tests to provide their own mocks).

;; Capture lists for mock calls
(if (not (bound? '*mock-terminal-echoes*))
  (define *mock-terminal-echoes* '()))
(if (not (bound? '*mock-telnet-sends*))
  (define *mock-telnet-sends* '()))
(if (not (bound? '*mock-notifications*))
  (define *mock-notifications* '()))

;; Mock terminal-echo: captures echoed text (newest first via cons)
(if (not (bound? 'terminal-echo))
  (defun terminal-echo (text)
    "Mock terminal-echo: record the echoed text."
    (set! *mock-terminal-echoes* (cons text *mock-terminal-echoes*))
    nil))

;; Mock telnet-send: captures sent text (newest first via cons)
(if (not (bound? 'telnet-send))
  (defun telnet-send (text)
    "Mock telnet-send: record the sent text."
    (set! *mock-telnet-sends* (cons text *mock-telnet-sends*))
    nil))

;; Mock notification-set: captures notification text (newest first via cons)
(if (not (bound? 'notification-set))
  (defun notification-set (text)
    "Mock notification-set: record the notification."
    (set! *mock-notifications* (cons text *mock-notifications*))
    nil))

;; Mock terminal-info: returns basic terminal dimensions
(if (not (bound? 'terminal-info))
  (defun terminal-info ()
    "Mock terminal-info: return default dimensions."
    '((cols . 80) (rows . 24))))

;; Mock strip-ansi: removes ANSI escape sequences from text
(if (not (bound? 'strip-ansi))
  (defun strip-ansi (text)
    "Mock strip-ansi: remove ANSI escape sequences."
    (if (not (string? text))
      ""
      (regex-replace-all "\033\\[[0-9;]*[A-Za-z]" text ""))))

;; Mock divider-mode-set: updates *divider-modes* directly
(if (not (bound? 'divider-mode-set))
  (defun divider-mode-set (sym display &rest args)
    "Mock divider-mode-set: update *divider-modes* directly."
    (let ((priority (if (null? args) 50 (car args))))
      ;; Remove existing entry for this symbol
      (set! *divider-modes*
        (filter (lambda (e)
                  (not (eq? (car (cdr e)) sym)))
          *divider-modes*))
      ;; Add new entry: (priority . (symbol . display))
      (let ((new-entry (cons priority (cons sym display))))
        (set! *divider-modes* (cons new-entry *divider-modes*))))
    nil))

;; Mock divider-mode-remove: updates *divider-modes* directly
(if (not (bound? 'divider-mode-remove))
  (defun divider-mode-remove (sym)
    "Mock divider-mode-remove: update *divider-modes* directly."
    (set! *divider-modes*
      (filter (lambda (e)
                (not (eq? (car (cdr e)) sym)))
        *divider-modes*))
    nil))

;; Clear all mock capture lists
(defun mock-clear-all ()
  "Clear all mock capture lists for fresh test state."
  (set! *mock-terminal-echoes* '())
  (set! *mock-telnet-sends* '())
  (set! *mock-notifications* '())
  (set! *divider-modes* '())
  nil)

;; Helper: check if any string in list contains substring
(defun mock-any-contains? (lst substr)
  "Return true if any string in lst contains substr."
  (if (null? lst)
    #f
    (if (and (string? (car lst))
          (regex-match? (regex-quote substr) (car lst)))
      #t
      (mock-any-contains? (cdr lst) substr))))

;; Helper: check if a divider mode is set
(defun mock-divider-mode-has? (sym)
  "Check if symbol is in *divider-modes* alist."
  (let ((found #f))
    (do ((modes *divider-modes* (cdr modes)))
      ((or found (null? modes)) found)
      (let ((entry (car modes)))
        (if (eq? (car (cdr entry)) sym)
          (set! found #t))))))

;; ============================================================================
;; Mock TinTin++ Variables (for testing scripts that use $variables)
;; ============================================================================
;; Create *tintin-variables* if not already defined (normally in tintin.lisp)
(if (not (bound? '*tintin-variables*))
  (define *tintin-variables* (make-hash-table)))
