;; Completion function tests
;; Tests the filter-commands function and completion-hook

;; Load test helper macros
(load "test-helpers.lisp")

;; All available commands
(define all-commands '("help" "hello" "helicopter" "list" "lisp" "listen"
                        "test" "telnet" "terminal" "temp" "show" "shell" "shutdown"))  ; ignore

;; Helper function to filter commands that match the prefix
(define filter-commands
  (lambda (prefix commands)
    (if (null? commands)
      ()
      (if (string-prefix? prefix (car commands))
        (cons (car commands) (filter-commands prefix (cdr commands)))
        (filter-commands prefix (cdr commands))))))  ; ignore

;; Simple completion hook that filters all commands
(define completion-hook
  (lambda (text)
    (filter-commands text all-commands)))  ; ignore

;; ============================================================================
;; Test 1: Filter commands with "hel" prefix
;; ============================================================================

(assert-equal (filter-commands "hel" all-commands) '("help" "hello" "helicopter")
  "Should filter commands with 'hel' prefix")

;; ============================================================================
;; Test 2: Filter commands with "te" prefix
;; ============================================================================

(assert-equal (filter-commands "te" all-commands) '("test" "telnet" "terminal" "temp")
  "Should filter commands with 'te' prefix")

;; ============================================================================
;; Test 3: Filter commands with "lis" prefix
;; ============================================================================

(assert-equal (filter-commands "lis" all-commands) '("list" "lisp" "listen")
  "Should filter commands with 'lis' prefix")

;; ============================================================================
;; Test 4: Filter commands with "sh" prefix
;; ============================================================================

(assert-equal (filter-commands "sh" all-commands) '("show" "shell" "shutdown")
  "Should filter commands with 'sh' prefix")

;; ============================================================================
;; Test 5: No matches for unknown prefix
;; ============================================================================

(assert-equal (filter-commands "xyz" all-commands) '()
  "Should return empty list for unknown prefix")

;; ============================================================================
;; Test 6: Empty prefix returns all commands
;; ============================================================================

(assert-equal (filter-commands "" all-commands)
  '("help" "hello" "helicopter" "list" "lisp" "listen" "test" "telnet" "terminal" "temp" "show" "shell" "shutdown")
  "Empty prefix should return all commands")

;; ============================================================================
;; Test 7: Completion hook works correctly
;; ============================================================================

(assert-equal (completion-hook "hel") '("help" "hello" "helicopter")
  "Completion hook should filter commands with 'hel' prefix")
(assert-equal (completion-hook "te") '("test" "telnet" "terminal" "temp")
  "Completion hook should filter commands with 'te' prefix")
