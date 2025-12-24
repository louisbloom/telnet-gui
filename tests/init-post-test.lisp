;; Init-Post Test Suite
;;
;; This test file tests functions from init-post.lisp.
;;
;; To run this test:
;;   lisp-repl.exe ../tests/init-post-test.lisp
;;
;; Note: init-post.lisp is automatically loaded by telnet-gui after SDL/GUI
;; initialization, so the functions are available when running tests through
;; telnet-gui's test runner.

;; ============================================================================
;; Minimal test assertion macros (so this test can run under lisp-repl.exe)
;; ============================================================================

(defmacro assert-equal (actual expected message)
  `(let ((actual-val ,actual)
         (expected-val ,expected))
     (let ((values-equal (if (and (number? actual-val) (number? expected-val))
                           (= actual-val expected-val)
                           (equal? actual-val expected-val))))
       (if values-equal
         nil
         (error (format nil "Assertion failed: ~A~%  Expected: ~S~%  Actual:   ~S"
                        ,message expected-val actual-val))))))

(defmacro assert-true (condition message)
  `(let ((result ,condition))
     (if result
       nil
       (error (format nil "Assertion failed: ~A (expected truthy, got: ~S)"
                      ,message result)))))

(defmacro assert-false (condition message)
  `(let ((result ,condition))
     (if result
       (error (format nil "Assertion failed: ~A (expected falsy, got: ~S)"
                      ,message result))
       nil)))

;; ============================================================================
;; Minimal stubs for init-post.lisp dependencies
;; ============================================================================

;; init-post registers a hook; for unit testing we can make hooks no-ops.
(define add-hook (lambda (hook handler) nil))  ; ignore

;; Define test stubs for functions that require GUI context
(define terminal-echo (lambda (text) nil))  ; ignore
(define terminal-scroll-locked? (lambda () #f))  ; ignore

;; Stub animation functions (may not be available if rlottie not compiled in)
(when (not (bound? 'animation-load))
  (define animation-load (lambda (path) nil))  ; ignore
  (define animation-loaded? (lambda (anim) #f))  ; ignore
  (define animation-playing? (lambda (anim) #f))  ; ignore
  (define animation-play (lambda (anim) nil))  ; ignore
  (define animation-set-loop (lambda (anim loop) nil))  ; ignore
  (define animation-set-dim-mode (lambda (anim alpha) nil)))  ; ignore

;; Load init-post.lisp to test its functions
;; Note: This will execute the version announcement code, but that's okay for testing
(load "../lisp/init-post.lisp")  ; ignore

;; ============================================================================
;; TEST: visual-length function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING visual-length FUNCTION")
(print "====================================================================")

(print "Test: Calculate length without ANSI codes...")
(assert-equal (visual-length "hello") 5 "Should return 5 for 'hello'")
(assert-equal (visual-length "test") 4 "Should return 4 for 'test'")

(print "Test: Strip ANSI escape codes...")
(assert-equal (visual-length "\033[1mhello\033[0m") 5 "Should ignore ANSI codes")
(assert-equal (visual-length "\033[38;2;255;0;0mred\033[0m") 3 "Should ignore RGB ANSI codes")
(assert-equal (visual-length "\033[1;38;2;255;215;0mbold\033[0m") 4 "Should ignore complex ANSI codes")

(print "Test: Handle empty string...")
(assert-equal (visual-length "") 0 "Should return 0 for empty string")

(print "Test: Handle invalid input...")
(assert-equal (visual-length nil) 0 "Should return 0 for nil")
(assert-equal (visual-length 42) 0 "Should return 0 for non-string")

(print "Test: Handle strings with only ANSI codes...")
(assert-equal (visual-length "\033[1m\033[0m") 0 "Should return 0 for only ANSI codes")

;; ============================================================================
;; TEST: pad-string function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING pad-string FUNCTION")
(print "====================================================================")

(print "Test: Pad string to specified width...")
(assert-equal (visual-length (pad-string "hello" 10)) 10 "Should pad to width 10")
(assert-equal (visual-length (pad-string "test" 8)) 8 "Should pad to width 8")

(print "Test: Don't pad if already at or over width...")
(assert-equal (pad-string "hello" 3) "hello" "Should not truncate, return as-is")
(assert-equal (pad-string "test" 4) "test" "Should return unchanged if exact width")

(print "Test: Handle ANSI codes in padding calculation...")
(define padded-ansi (pad-string "\033[1mhello\033[0m" 10))
(assert-equal (visual-length padded-ansi) 10 "Should pad based on visual length, not string length")

(print "Test: Handle empty string...")
(assert-equal (visual-length (pad-string "" 5)) 5 "Should pad empty string to width")
(assert-equal (pad-string "" 0) "" "Should return empty for width 0")

(print "Test: Handle invalid input...")
(assert-equal (pad-string nil 10) "" "Should return empty string for nil")
(assert-equal (pad-string "test" -5) "test" "Should return unchanged for negative width")

;; ============================================================================
;; TEST: repeat-string function
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING repeat-string FUNCTION")
(print "====================================================================")

(print "Test: Repeat string N times...")
(assert-equal (repeat-string "a" 5) "aaaaa" "Should repeat single character")
(assert-equal (repeat-string "─" 3) "───" "Should repeat box-drawing character")
(assert-equal (repeat-string "test" 2) "testtest" "Should repeat multi-character string")

(print "Test: Handle zero and negative counts...")
(assert-equal (repeat-string "a" 0) "" "Should return empty for count 0")
(assert-equal (repeat-string "test" -1) "" "Should return empty for negative count")

(print "Test: Handle empty string...")
(assert-equal (length (repeat-string "" 5)) 0 "Should return empty string")
(assert-equal (repeat-string "" 0) "" "Should return empty for empty string and count 0")

(print "Test: Handle single repetition...")
(assert-equal (repeat-string "hello" 1) "hello" "Should return string once for count 1")

;; ============================================================================
;; TEST: scroll-lock notification functions
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING SCROLL-LOCK NOTIFICATION FUNCTIONS")
(print "====================================================================")

(print "Test: *scroll-lock-notification-enabled* variable exists...")
(assert-true (bound? '*scroll-lock-notification-enabled*) "Variable should be defined")
(assert-true *scroll-lock-notification-enabled* "Should default to enabled (#t)")

(print "Test: *scroll-lock-notification-animation* variable exists...")
(assert-true (bound? '*scroll-lock-notification-animation*) "Variable should be defined")

(print "Test: maybe-play-scroll-lock-notification function exists...")
(assert-true (bound? 'maybe-play-scroll-lock-notification) "Function should be defined")

(print "Test: scroll-lock-notification-hook function exists...")
(assert-true (bound? 'scroll-lock-notification-hook) "Function should be defined")

(print "Test: maybe-play-scroll-lock-notification respects enabled flag...")
;; Disable notifications
(set! *scroll-lock-notification-enabled* #f)
;; Function should return nil when disabled (no error)
(define result-disabled (maybe-play-scroll-lock-notification))
(assert-equal result-disabled nil "Should return nil when disabled")
;; Re-enable
(set! *scroll-lock-notification-enabled* #t)

(print "Test: scroll-lock-notification-hook accepts text parameter...")
;; Hook should accept text and call maybe-play-scroll-lock-notification
(define hook-result (scroll-lock-notification-hook "test text"))
(assert-equal hook-result nil "Hook should return nil")

(print "Test: Hook is registered with telnet-input-hook...")
;; Verify hook was registered (we can't easily test the hook system directly,
;; but we can verify the function exists and works)
(assert-true (bound? 'scroll-lock-notification-hook) "Hook function should exist")

;; ============================================================================
;; TEST: Version announcement code (indirect testing)
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING VERSION ANNOUNCEMENT (INDIRECT)")
(print "====================================================================")

(print "Test: Helper functions work with version strings...")
;; Test that visual-length works with typical version strings
(assert-true (> (visual-length "1.2.3") 0) "Should handle version strings")
(assert-true (> (visual-length "v1.0.0") 0) "Should handle version strings with prefix")

(print "Test: pad-string works with version formatting...")
(define version-str "1.2.3")
(define padded-version (pad-string version-str 10))
(assert-equal (visual-length padded-version) 10 "Should pad version strings correctly")

(print "Test: repeat-string works for border characters...")
(define border (repeat-string "─" 20))
(assert-equal (length border) 20 "Should create border of correct length")

;; ============================================================================
;; TEST: Edge cases and error handling
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING EDGE CASES AND ERROR HANDLING")
(print "====================================================================")

(print "Test: visual-length with complex ANSI sequences...")
(assert-equal (visual-length "\033[1;38;2;255;215;0m✦\033[0m") 1 "Should handle Unicode with ANSI")
(assert-equal (visual-length "\033[38;2;100;149;237mtest\033[0m") 4 "Should handle RGB color codes")

(print "Test: pad-string with very long strings...")
(define long-str "this is a very long string that exceeds the padding width")
(assert-equal (pad-string long-str 10) long-str "Should return unchanged if already longer")

(print "Test: repeat-string with special characters...")
(assert-equal (repeat-string "│" 3) "│││" "Should handle box-drawing characters")
(assert-equal (repeat-string "┌" 1) "┌" "Should handle single box character")

;; ============================================================================
;; TEST: build-version-box alignment
;; ============================================================================

(print "")
(print "====================================================================")
(print "TESTING build-version-box ALIGNMENT")
(print "====================================================================")

;; Helper to check if all non-empty lines in a box have equal visual width
(defun box-aligned? (box-str)
  "Returns #t if all non-empty lines have equal visual width."
  (let ((lines (split box-str "\n"))
        (expected-width nil)
        (aligned #t))
    ;; Find width of first non-empty line
    (do ((ls lines (cdr ls)))
      ((or (null? ls) expected-width))
      (when (> (visual-length (car ls)) 0)
        (set! expected-width (visual-length (car ls)))))
    ;; Check all non-empty lines match
    (do ((ls lines (cdr ls)))
      ((null? ls) aligned)
      (when (> (visual-length (car ls)) 0)
        (when (not (= (visual-length (car ls)) expected-width))
          (set! aligned #f))))))

(print "Test: All lines have equal visual width...")
(assert-true (box-aligned? (build-version-box "1.0.0" "0.3.17" "2.30.0" "2.22.0" "8.2" "10.45" "Windows" "x86_64" "GNU 14.2.0"))
  "Box with varying version lengths should be aligned")

(print "Test: Box handles short version strings...")
(assert-true (box-aligned? (build-version-box "1.0" "0.1" "2.0" "2.0" "8" "10" "Linux" "arm64" "Clang"))
  "Box with short version strings should be aligned")

(print "Test: Box handles long version strings...")
(assert-true (box-aligned? (build-version-box "1.0.0-beta.1" "0.3.17-dev" "2.30.0.1" "2.22.0" "8.2.6" "10.45.2" "MINGW64_NT-10.0" "x86_64" "GNU 14.2.0"))
  "Box with long version strings should be aligned")

;; ============================================================================
;; ALL TESTS PASSED
;; ============================================================================

(print "")
(print "====================================================================")
(print "ALL INIT-POST TESTS PASSED!")
(print "====================================================================")
