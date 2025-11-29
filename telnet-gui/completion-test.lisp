;; Completion function tests
;; Tests the filter-commands function and completion-hook

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

(filter-commands "hel" all-commands)  ; => ("help" "hello" "helicopter")

;; ============================================================================
;; Test 2: Filter commands with "te" prefix
;; ============================================================================

(filter-commands "te" all-commands)  ; => ("test" "telnet" "terminal" "temp")

;; ============================================================================
;; Test 3: Filter commands with "lis" prefix
;; ============================================================================

(filter-commands "lis" all-commands)  ; => ("list" "lisp" "listen")

;; ============================================================================
;; Test 4: Filter commands with "sh" prefix
;; ============================================================================

(filter-commands "sh" all-commands)  ; => ("show" "shell" "shutdown")

;; ============================================================================
;; Test 5: No matches for unknown prefix
;; ============================================================================

(filter-commands "xyz" all-commands)  ; => ()

;; ============================================================================
;; Test 6: Empty prefix returns all commands
;; ============================================================================

(filter-commands "" all-commands)  ; => ("help" "hello" "helicopter" "list" "lisp" "listen" "test" "telnet" "terminal" "temp" "show" "shell" "shutdown")

;; ============================================================================
;; Test 7: Completion hook works correctly
;; ============================================================================

(completion-hook "hel")  ; => ("help" "hello" "helicopter")
(completion-hook "te")   ; => ("test" "telnet" "terminal" "temp")
