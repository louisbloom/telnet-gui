;; Bootstrap Lisp file for telnet-gui
;; This file is always loaded on startup before any user-provided Lisp file
;; All variables defined here can be overridden in your custom Lisp configuration file

;; ============================================================================
;; COMPLETION PATTERN CONFIGURATION
;; ============================================================================
;; *completion-pattern*: PCRE2 regex pattern that matches the text to complete
;;
;; The pattern is applied to the current line, and the matched text is passed
;; to the completion-hook function. The pattern should match the word or
;; command prefix that the user wants to complete.
;;
;; Examples:
;;   "\\S+$"          - Match any non-whitespace characters at end of line
;;                      (default: completes any word at cursor position)
;;   "[a-zA-Z_][a-zA-Z0-9_]*$" - Match identifiers (letters, digits, underscore)
;;                      (good for command completion)
;;   "[^\\s]+$"       - Match any non-space characters at end of line
;;                      (similar to default but excludes tabs)
;;   "\\w+$"          - Match word characters (letters, digits, underscore)
;;                      (simpler than identifier pattern)
;;
;; Pattern matching details:
;;   - The pattern is anchored to the end of the line ($)
;;   - Only the matched portion is passed to completion-hook
;;   - Use capturing groups if you need to extract specific parts
;;   - PCRE2 syntax: \\S = non-whitespace, \\w = word char, \\s = whitespace
(define *completion-pattern* "\\S+$")

;; ============================================================================
;; COMPLETION HOOK CONFIGURATION
;; ============================================================================
;; completion-hook: Function called when user requests tab completion
;;
;; Signature: (lambda (text) -> list)
;;   - text: The matched text from *completion-pattern*
;;   - Returns: List of completion candidates (strings)
;;
;; The completion system will:
;;   1. Match *completion-pattern* against the current line
;;   2. Call completion-hook with the matched text
;;   3. Display the returned completions in a popup menu
;;   4. User can select a completion with arrow keys and Enter
;;
;; Examples:
;;
;; Simple command completion:
;;   (define completion-hook
;;     (lambda (text)
;;       (cond
;;         ((string-prefix? "hel" text) '("help" "hello" "helix"))
;;         ((string-prefix? "lis" text) '("list" "lisp" "listen"))
;;         (#t '()))))
;;
;; File path completion (simplified):
;;   (define completion-hook
;;     (lambda (text)
;;       (if (string-prefix? "/" text)
;;           '("/home/user/" "/usr/bin/" "/etc/")
;;           '("file1.txt" "file2.lisp" "file3.c"))))
;;
;; Debug completion (prints what it receives):
;;   (define completion-hook
;;     (lambda (text)
;;       (progn
;;         (princ "Completing: ")
;;         (princ text)
;;         (princ "\n")
;;         '("option1" "option2" "option3"))))
;;
;; No completions (default behavior):
;;   (define completion-hook (lambda (text) ()))
(define completion-hook (lambda (text) ()))

;; ============================================================================
;; TELNET INPUT HOOK CONFIGURATION
;; ============================================================================
;; telnet-input-hook: Function called when text is received from telnet server
;;
;; Signature: (lambda (text) -> any)
;;   - text: Plain text from server (ANSI codes already stripped)
;;   - Returns: Ignored (hook is side-effect only)
;;
;; This hook receives all text output from the telnet server AFTER ANSI
;; escape codes have been removed. It's called for every chunk of text
;; received, allowing you to monitor, log, or process server output.
;;
;; Important notes:
;;   - This hook is SIDE-EFFECT ONLY - it cannot modify the data flow
;;   - Text is already stripped of ANSI codes before reaching this hook
;;   - Hook is called synchronously during telnet processing
;;   - For filtering/modifying input, use telnet-input-filter (future feature)
;;
;; Examples:
;;
;; Simple logging to console:
;;   (define telnet-input-hook
;;     (lambda (text)
;;       (progn
;;         (princ "[TELNET] ")
;;         (princ text))))
;;
;; Log to file (requires file I/O functions):
;;   (define telnet-input-hook
;;     (lambda (text)
;;       (write-to-file "telnet.log" text)))
;;
;; Multi-function processing:
;;   (define telnet-input-hook
;;     (lambda (text)
;;       (progn
;;         (log-to-file text)        ; Log everything
;;         (check-for-alerts text)   ; Check for important messages
;;         (update-status-bar text)  ; Update UI status
;;         ())))                     ; Return nil
;;
;; Conditional processing:
;;   (define telnet-input-hook
;;     (lambda (text)
;;       (if (string-contains? "ERROR" text)
;;           (progn
;;             (princ "*** ERROR DETECTED ***\n")
;;             (princ text)
;;             (princ "\n"))
;;           ())))
;;
;; No processing (default - silent):
;;   (define telnet-input-hook (lambda (text) ()))
(define telnet-input-hook (lambda (text) ()))

;; ============================================================================
;; MOUSE WHEEL SCROLLING CONFIGURATION
;; ============================================================================
;; *scroll-lines-per-click*: Number of terminal lines to scroll per mouse wheel click
;;
;; This controls how much the terminal scrolls when you use the mouse wheel.
;; Higher values scroll more per click, lower values provide finer control.
;;
;; Examples:
;;   1  - Very fine control, one line at a time (good for precise reading)
;;   3  - Default, balanced scrolling (good for most users)
;;   5  - Faster scrolling, good for long documents
;;   10 - Very fast scrolling, good for quickly jumping through output
;;
;; Note: This only affects discrete wheel clicks. For smooth scrolling
;;       (trackpads), see *smooth-scrolling-enabled* below.
(define *scroll-lines-per-click* 3)

;; ============================================================================
;; SMOOTH SCROLLING CONFIGURATION
;; ============================================================================
;; *smooth-scrolling-enabled*: Enable smooth scrolling for high-resolution trackpads
;;
;; When enabled (#t), the terminal uses smooth pixel-based scrolling that
;; responds to trackpad gestures with sub-line precision. This provides a
;; natural, fluid scrolling experience similar to modern text editors.
;;
;; When disabled (#f), the terminal uses discrete line-based scrolling that
;; jumps by whole lines. This is more traditional and may be preferred for
;; some use cases.
;;
;; Examples:
;;   #t - Enable smooth scrolling (default, recommended for trackpads)
;;   #f - Disable smooth scrolling (use discrete line jumps)
;;
;; Use cases:
;;   - Enable (#t) if you have a trackpad or high-resolution mouse
;;   - Enable (#t) for a modern, fluid user experience
;;   - Disable (#f) if you prefer traditional terminal scrolling
;;   - Disable (#f) if smooth scrolling feels laggy on your system
(define *smooth-scrolling-enabled* #t)

;; ============================================================================
;; SCROLLBACK BUFFER CONFIGURATION
;; ============================================================================
;; *max-scrollback-lines*: Maximum number of scrollback lines to keep in memory
;;
;; This controls how many lines of terminal output history are retained for
;; scrolling back through past output. When the buffer is full, oldest lines
;; are automatically evicted to make room for new output (FIFO circular buffer).
;;
;; Values:
;;   0     - Unbounded (default, limited to 10000 lines for safety)
;;   1000  - Small buffer, uses less memory
;;   5000  - Medium buffer, good balance
;;   10000 - Large buffer, maximum scrollback
;;
;; Note: Set to 0 for unbounded scrollback (limited by SCROLLBACK_MAX_LINES = 10000
;;       in code for safety). Setting a smaller value can reduce memory usage for
;;       very long sessions with lots of output.
;;
;; Memory usage: Each line uses ~100-500 bytes depending on content
;;   1000 lines  ≈ 100KB - 500KB
;;   5000 lines  ≈ 500KB - 2.5MB
;;   10000 lines ≈ 1MB - 5MB
(define *max-scrollback-lines* 0)

;; ============================================================================
;; AUTO-SCROLL TO BOTTOM CONFIGURATION
;; ============================================================================
;; *scroll-to-bottom-on-user-input*: Auto-scroll to bottom when user sends input
;;
;; When enabled (#t), the terminal automatically scrolls to the bottom (current
;; output) whenever you send a command or text to the server. This ensures you
;; always see the response to your commands.
;;
;; Values:
;;   #t - Auto-scroll on user input (default, recommended)
;;   #f - Don't auto-scroll, stay at current scroll position
;;
;; Use cases:
;;   - Enable (#t) for normal interactive use (default)
;;   - Disable (#f) if you want to read scrollback while typing commands
(define *scroll-to-bottom-on-user-input* #t)

;; *scroll-to-bottom-on-telnet-input*: Auto-scroll to bottom when server sends data
;;
;; When enabled (#t), the terminal automatically scrolls to the bottom whenever
;; new data arrives from the server. This keeps you following the live output.
;; When disabled (#f), you can scroll up to read history while new data continues
;; to arrive in the background.
;;
;; Values:
;;   #t - Auto-scroll on server output (follows live output)
;;   #f - Don't auto-scroll, stay at current scroll position (default)
;;
;; Use cases:
;;   - Enable (#t) to always follow live output
;;   - Disable (#f) to read scrollback while server continues to send data
;;   - Disable (#f) for high-traffic servers where you want to review past output
(define *scroll-to-bottom-on-telnet-input* #f)
