;; Path Expansion Tests

;; ============================================================================
;; home-directory Tests
;; ============================================================================

;; home-directory should return a string or nil
(define home (home-directory))
(or (string? home) (null? home))                ; => 1

;; If home exists, it should be non-empty
(if (string? home)
  (> (string-length home) 0)
  #t)                                            ; => 1

;; ============================================================================
;; expand-path Tests - Basic Functionality
;; ============================================================================

;; expand-path requires an argument
(condition-case err
  (expand-path)
  (error #t))                                    ; => 1

;; expand-path requires a string
(condition-case err
  (expand-path 42)
  (error #t))                                    ; => 1

;; Non-~/ paths return unchanged
(string=? (expand-path "/etc/config") "/etc/config")     ; => 1
(string=? (expand-path "relative/path") "relative/path") ; => 1
(string=? (expand-path "./local") "./local")             ; => 1
(string=? (expand-path "test.txt") "test.txt")           ; => 1

;; ============================================================================
;; expand-path Tests - Home Expansion (conditional on home-directory)
;; ============================================================================

;; If we have a home directory, test expansion
(if (string? (home-directory))
  (progn
    ;; ~/path should expand to home/path
    (define expanded (expand-path "~/test.txt"))
    (and (string? expanded)
      (string-prefix? (home-directory) expanded)
      (string-contains? expanded "test.txt"))
    )
  #t)                                            ; => 1

;; ~ alone should expand to home directory
(if (string? (home-directory))
  (string=? (expand-path "~") (home-directory))
  #t)                                            ; => 1

;; ~/subdir/file should work
(if (string? (home-directory))
  (progn
    (define expanded (expand-path "~/docs/notes.txt"))
    (and (string? expanded)
      (string-prefix? (home-directory) expanded)
      (string-contains? expanded "docs")
      (string-contains? expanded "notes.txt"))
    )
  #t)                                            ; => 1

;; ============================================================================
;; Integration Tests - File I/O
;; ============================================================================

;; Should be usable with file I/O functions (if home exists)
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
  #t)                                            ; => 1

;; ============================================================================
;; Edge Cases
;; ============================================================================

;; ~/ with empty rest should add separator
(if (string? (home-directory))
  (let ((result (expand-path "~/")))
    (string-prefix? (home-directory) result))
  #t)                                            ; => 1

;; Backslash works too (Windows compatibility)
(if (string? (home-directory))
  (let ((result (expand-path "~\\test.txt")))
    (and (string? result)
      (string-prefix? (home-directory) result)))
  #t)                                            ; => 1
