;; Path Expansion Tests

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; ============================================================================
;; home-directory Tests
;; ============================================================================

;; home-directory should return a string or nil
(define home (home-directory))
(assert-true (or (string? home) (null? home)) "home-directory returns string or nil")

;; If home exists, it should be non-empty
(assert-true (if (string? home) (> (length home) 0) #t) "home directory is non-empty if it exists")

;; ============================================================================
;; expand-path Tests - Basic Functionality
;; ============================================================================

;; expand-path requires an argument
(assert-error (expand-path) "expand-path requires an argument")

;; expand-path requires a string
(assert-error (expand-path 42) "expand-path requires a string")

;; Non-~/ paths return unchanged
(assert-true (string=? (expand-path "/etc/config") "/etc/config") "absolute path unchanged")
(assert-true (string=? (expand-path "relative/path") "relative/path") "relative path unchanged")
(assert-true (string=? (expand-path "./local") "./local") "dot-relative path unchanged")
(assert-true (string=? (expand-path "test.txt") "test.txt") "simple filename unchanged")

;; ============================================================================
;; expand-path Tests - Home Expansion (conditional on home-directory)
;; ============================================================================

;; If we have a home directory, test expansion
(assert-true
  (if (string? (home-directory))
    (progn
      ;; ~/path should expand to home/path
      (define expanded (expand-path "~/test.txt"))
      (and (string? expanded)
        (string-prefix? (home-directory) expanded)
        (string-contains? expanded "test.txt")))
    #t)
  "~/path expands to home directory")

;; ~ alone should expand to home directory
(assert-true
  (if (string? (home-directory))
    (string=? (expand-path "~") (home-directory))
    #t)
  "~ expands to home directory")

;; ~/subdir/file should work
(assert-true
  (if (string? (home-directory))
    (progn
      (define expanded (expand-path "~/docs/notes.txt"))
      (and (string? expanded)
        (string-prefix? (home-directory) expanded)
        (string-contains? expanded "docs")
        (string-contains? expanded "notes.txt")))
    #t)
  "~/subdir/file expands correctly")

;; ============================================================================
;; Integration Tests - File I/O
;; ============================================================================

;; Should be usable with file I/O functions (if home exists)
(assert-true
  (if (string? (home-directory))
    (progn
      ;; Create test file in home directory
      (define test-path (expand-path "~/test-expand-path.tmp"))
      (define f (open test-path "w"))
      (write-line f "test content")
      (close f)

      ;; Read it back
      (define f2 (open test-path "r"))
      (define content (read-line f2))
      (close f2)

      ;; Verify
      (string=? content "test content"))
    #t)
  "expanded path works with file I/O")

;; ============================================================================
;; Edge Cases
;; ============================================================================

;; ~/ with empty rest should add separator
(assert-true
  (if (string? (home-directory))
    (let ((result (expand-path "~/")))
      (string-prefix? (home-directory) result))
    #t)
  "~/ expands correctly with separator")

;; Backslash works too (Windows compatibility)
(assert-true
  (if (string? (home-directory))
    (let ((result (expand-path "~\\test.txt")))
      (and (string? result)
        (string-prefix? (home-directory) result)))
    #t)
  "~\\path works on Windows")
