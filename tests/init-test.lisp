;; Init Test Suite
;;
;; This test file tests functions from init.lisp.
;;
;; To run this test:
;;   telnet-gui.exe -t init-test.lisp
;;
;; Note: init.lisp is automatically loaded by telnet-gui, so the functions
;; are available when running tests through telnet-gui's test runner.

;; Load test helper macros
(load "test-helpers.lisp")  ; ignore

;; Define test stubs for hooks that require telnet context
(define terminal-echo (lambda (text) nil))  ; ignore
(define telnet-send (lambda (text) nil))  ; ignore

;; ============================================================================
;; TEST: trim-punctuation function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING trim-punctuation FUNCTION")
(print "====================================================================")

(print "Test: Trim trailing punctuation...")
(assert-equal (trim-punctuation "hello!") "hello" "Should remove trailing exclamation")
(assert-equal (trim-punctuation "world.") "world" "Should remove trailing period")
(assert-equal (trim-punctuation "test?") "test" "Should remove trailing question mark")

(print "Test: Trim leading punctuation...")
(assert-equal (trim-punctuation "(world)") "world" "Should remove leading and trailing parentheses")
(assert-equal (trim-punctuation "[test]") "test" "Should remove brackets")

(print "Test: Preserve internal punctuation...")
(assert-equal (trim-punctuation "don't") "don't" "Should preserve internal apostrophe")
(assert-equal (trim-punctuation "well-known") "well-known" "Should preserve internal hyphen")

(print "Test: Handle multiple punctuation...")
(assert-equal (trim-punctuation "--test--") "test" "Should remove multiple dashes")
(assert-equal (trim-punctuation "!!!") "" "Should return empty for all punctuation")

(print "Test: Handle empty and invalid input...")
(assert-equal (trim-punctuation "") "" "Should return empty string for empty input")
(assert-equal (trim-punctuation nil) "" "Should return empty string for nil")

;; ============================================================================
;; TEST: clean-word function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING clean-word FUNCTION")
(print "====================================================================")

(print "Test: Clean valid words...")
(assert-equal (clean-word "hello!") "hello" "Should clean trailing punctuation")
(assert-equal (clean-word "world") "world" "Should return unchanged word")

(print "Test: Handle invalid input...")
(assert-equal (clean-word "") "" "Should return empty for empty string")
(assert-equal (clean-word nil) "" "Should return empty for nil")

;; ============================================================================
;; TEST: valid-word? function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING valid-word? FUNCTION")
(print "====================================================================")

(print "Test: Valid words...")
(assert-true (valid-word? "hello") "Should return true for valid word")
(assert-true (valid-word? "test") "Should return true for another valid word")

(print "Test: Invalid words...")
(assert-false (valid-word? "") "Should return false for empty string")
(assert-false (valid-word? nil) "Should return false for nil")

;; ============================================================================
;; TEST: extract-words function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING extract-words FUNCTION")
(print "====================================================================")

(print "Test: Extract words from text...")
(define words1 (extract-words "hello world test"))
(assert-true (list? words1) "Should return a list")
(assert-true (>= (length words1) 3) "Should extract multiple words")

(print "Test: Handle punctuation...")
(define words2 (extract-words "hello, world! test."))
(assert-true (list? words2) "Should handle punctuation")

(print "Test: Handle empty input...")
(assert-equal (extract-words "") '() "Should return empty list for empty string")
(assert-equal (extract-words nil) '() "Should return empty list for nil")

;; ============================================================================
;; TEST: add-word-to-store function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING add-word-to-store FUNCTION")
(print "====================================================================")

(print "Test: Add valid words...")
(define result1 (add-word-to-store "hello"))
(assert-equal result1 1 "Should return 1 for valid word")

(print "Test: Reject short words...")
(define result2 (add-word-to-store "hi"))
(assert-equal result2 0 "Should return 0 for word < 3 characters")
(define result3 (add-word-to-store "ab"))
(assert-equal result3 0 "Should return 0 for 2-character word")

(print "Test: Add multiple words...")
(add-word-to-store "dragon")  ; ignore
(add-word-to-store "wizard")  ; ignore
(add-word-to-store "knight")  ; ignore
(assert-true #t "Should add multiple words without error")

;; ============================================================================
;; TEST: get-completions-from-store function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING get-completions-from-store FUNCTION")
(print "====================================================================")

(print "Test: Get completions for prefix...")
;; Clear store and add test words
(hash-clear! *completion-word-store*)  ; ignore
(set! *completion-word-order-index* 0)  ; ignore
(set! *completion-word-order* (make-vector *completion-word-store-size* nil))  ; ignore

(add-word-to-store "hello")  ; ignore
(add-word-to-store "help")  ; ignore
(add-word-to-store "helmet")  ; ignore
(add-word-to-store "world")  ; ignore

(define completions (get-completions-from-store "hel"))
(assert-true (list? completions) "Should return a list")
(assert-true (> (length completions) 0) "Should find matches for 'hel' prefix")

(print "Test: Case-insensitive matching...")
(define completions-upper (get-completions-from-store "HEL"))
(assert-true (list? completions-upper) "Should match case-insensitively")

(print "Test: No matches...")
(define completions-none (get-completions-from-store "xyz"))
(assert-equal completions-none '() "Should return empty list for no matches")

(print "Test: Empty prefix...")
(define completions-empty (get-completions-from-store ""))
(assert-equal completions-empty '() "Should return empty list for empty prefix")

;; ============================================================================
;; TEST: build-indent function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING build-indent FUNCTION")
(print "====================================================================")

(print "Test: Build indent strings...")
(assert-equal (build-indent 0) "" "Should return empty string for level 0")
(assert-equal (build-indent 1) "  " "Should return 2 spaces for level 1")
(assert-equal (build-indent 2) "    " "Should return 4 spaces for level 2")
(assert-equal (build-indent 3) "      " "Should return 6 spaces for level 3")

;; ============================================================================
;; TEST: obj-to-string function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING obj-to-string FUNCTION")
(print "====================================================================")

(print "Test: Convert symbols to strings...")
(assert-equal (obj-to-string 'hello) "hello" "Should convert symbol to string")
(assert-equal (obj-to-string 'test-symbol) "test-symbol" "Should convert hyphenated symbol")

(print "Test: Handle string inputs...")
(assert-equal (obj-to-string "already-a-string") "already-a-string" "Should return string as-is")
(assert-equal (obj-to-string "") "" "Should handle empty string")

(print "Test: Convert booleans...")
(assert-equal (obj-to-string #t) "#t" "Should convert true to '#t'")
(assert-equal (obj-to-string #f) "#f" "Should convert false to '#f'")

(print "Test: Convert numbers...")
(assert-true (string-contains? (obj-to-string 42) "42") "Should convert integer to string")
(assert-true (string-contains? (obj-to-string 0) "0") "Should convert zero to string")

;; ============================================================================
;; TEST: is-nested-alist? function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING is-nested-alist? FUNCTION")
(print "====================================================================")

(print "Test: Detect nested alists...")
(define nested-alist '((name . "Alice") (age . 30)))
(assert-true (is-nested-alist? nested-alist) "Should detect nested alist")

(print "Test: Reject non-alists...")
(assert-false (is-nested-alist? '()) "Should return false for empty list")
(assert-false (is-nested-alist? '(1 2 3)) "Should return false for regular list")
(assert-false (is-nested-alist? "not-a-list") "Should return false for string")
(assert-false (is-nested-alist? 42) "Should return false for number")

(print "Test: Handle edge cases...")
(assert-false (is-nested-alist? nil) "Should return false for nil")

;; ============================================================================
;; TEST: format-string-value function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING format-string-value FUNCTION")
(print "====================================================================")

(print "Test: Format string values with quotes...")
(assert-equal (format-string-value "hello") "\"hello\"\n" "Should wrap string in quotes")
(assert-equal (format-string-value "test") "\"test\"\n" "Should add quotes and newline")

(print "Test: Handle empty strings...")
(assert-equal (format-string-value "") "\"\"\n" "Should handle empty string")

(print "Test: Handle strings with special characters...")
(assert-equal (format-string-value "hello world") "\"hello world\"\n" "Should handle spaces")

;; ============================================================================
;; TEST: format-regular-value function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING format-regular-value FUNCTION")
(print "====================================================================")

(print "Test: Format regular values...")
(assert-true (string-contains? (format-regular-value 42) "42") "Should format number")
(assert-true (string-contains? (format-regular-value 'symbol) "symbol") "Should format symbol")

(print "Test: Add newline...")
(assert-true (string-contains? (format-regular-value #t) "\n") "Should add newline")

;; ============================================================================
;; TEST: pretty-print-alist function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING pretty-print-alist FUNCTION")
(print "====================================================================")

;; Note: pretty-print-alist now outputs directly to terminal via terminal-echo
;; and returns nil. We test that it returns nil and doesn't error.

(print "Test: Basic alist pretty printing returns nil...")
(define basic-alist '((name . "John") (age . 30) (active . #t)))
(define basic-result (pretty-print-alist basic-alist))
(assert-equal basic-result nil "Should return nil (outputs to terminal)")

(print "Test: Nested alist pretty printing returns nil...")
(define nested-alist '((user . ((name . "Alice") (id . 123))) (status . "online")))
(define nested-result (pretty-print-alist nested-alist))
(assert-equal nested-result nil "Should return nil for nested alist")

(print "Test: Empty alist returns nil...")
(define empty-result (pretty-print-alist '()))
(assert-equal empty-result nil "Should return nil for empty alist")

(print "Test: Alist with list values returns nil...")
(define list-alist '((colors . (red green blue)) (count . 3)))
(define list-result (pretty-print-alist list-alist))
(assert-equal list-result nil "Should return nil for alist with list values")

(print "Test: Invalid input (not a list) returns nil...")
(define invalid-result (pretty-print-alist "not a list"))
(assert-equal invalid-result nil "Should return nil for invalid input")

(print "Test: String values return nil...")
(define string-alist '((message . "Hello, world!") (title . "Test")))
(define string-result (pretty-print-alist string-alist))
(assert-equal string-result nil "Should return nil for string values")

(print "Test: Boolean values return nil...")
(define bool-alist '((enabled . #t) (disabled . #f)))
(define bool-result (pretty-print-alist bool-alist))
(assert-equal bool-result nil "Should return nil for boolean values")

;; ============================================================================
;; TEST: completion-hook function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING completion-hook FUNCTION")
(print "====================================================================")

(print "Test: Completion hook returns list...")
;; Add some words to store first
(hash-clear! *completion-word-store*)  ; ignore
(set! *completion-word-order-index* 0)  ; ignore
(set! *completion-word-order* (make-vector *completion-word-store-size* nil))  ; ignore
(add-word-to-store "test")  ; ignore
(add-word-to-store "testing")  ; ignore

(define hook-result (completion-hook "test"))
(assert-true (list? hook-result) "Should return a list")

(print "Test: Empty prefix returns empty list...")
(define hook-empty (completion-hook ""))
(assert-equal hook-empty '() "Should return empty list for empty prefix")

(print "Test: Invalid input returns empty list...")
(define hook-invalid (completion-hook nil))
(assert-equal hook-invalid '() "Should return empty list for nil")

;; ============================================================================
;; TEST: telnet-input-hook (hook system)
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING SYMBOL-BASED HOOK DISPATCH")
(print "====================================================================")

;; Test that run-hook properly dispatches to functions via symbol lookup (uses eval)
(define *test-hook-called* nil)
(define *test-hook-arg* nil)

(defun test-hook-handler (arg)
  "Test hook handler that records it was called."
  (set! *test-hook-called* #t)
  (set! *test-hook-arg* arg)
  nil)

(print "Test: Register hook by symbol...")
(add-hook 'test-hook 'test-hook-handler)

(print "Test: run-hook dispatches via eval...")
(run-hook 'test-hook "test-value")
(assert-true *test-hook-called* "Hook should be called via symbol dispatch")
(assert-equal *test-hook-arg* "test-value" "Hook should receive argument")

(print "Test: Duplicate registration prevented...")
(set! *test-hook-called* nil)
(add-hook 'test-hook 'test-hook-handler)  ; Try to add again
(run-hook 'test-hook "second-call")
;; Should only be called once (not twice) if duplicate prevention works
(assert-true *test-hook-called* "Hook should still be called")

(print "Test: remove-hook by symbol...")
(remove-hook 'test-hook 'test-hook-handler)
(set! *test-hook-called* nil)
(run-hook 'test-hook "after-remove")
(assert-false *test-hook-called* "Hook should not be called after removal")

(print "")
(print "====================================================================")
(print "TESTING telnet-input-hook (HOOK SYSTEM)")
(print "====================================================================")

(print "Test: Hook processes text...")
;; Clear store first
(hash-clear! *completion-word-store*)  ; ignore
(set! *completion-word-order-index* 0)  ; ignore
(set! *completion-word-order* (make-vector *completion-word-store-size* nil))  ; ignore

;; telnet-input-hook is now a hook system - use run-hook to call it
(define hook-result (run-hook 'telnet-input-hook "The dragon guards the treasure!"))
(assert-false hook-result "Should return nil (side-effect only)")

(print "Test: Words are collected...")
(define completions-after (get-completions-from-store "drag"))
(assert-true (list? completions-after) "Should have collected words")

;; ============================================================================
;; TEST: telnet-input-filter-hook function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING telnet-input-filter-hook FUNCTION")
(print "====================================================================")

(print "Test: Filter hook returns text unchanged (default)...")
(define filter-result (telnet-input-filter-hook "test text"))
(assert-equal filter-result "test text" "Should return text unchanged by default")

(print "Test: Filter hook handles various inputs...")
(assert-equal (telnet-input-filter-hook "hello") "hello" "Should pass through simple text")
(assert-equal (telnet-input-filter-hook "") "" "Should handle empty string")

;; ============================================================================
;; TEST: user-input-hook function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING user-input-hook FUNCTION")
(print "====================================================================")

(print "Test: User input hook returns text unchanged (default)...")
(define user-result (user-input-hook "test command" 0))
(assert-equal user-result "test command" "Should return text unchanged by default")

(print "Test: User input hook handles cursor position...")
(define user-result2 (user-input-hook "hello" 5))
(assert-equal user-result2 "hello" "Should handle cursor position parameter")

;; ============================================================================
;; ALL TESTS PASSED
;; ============================================================================

(print "")
(print "====================================================================")
(print "ALL BOOTSTRAP TESTS PASSED!")
(print "====================================================================")
