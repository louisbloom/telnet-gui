;; Bootstrap Lisp file for telnet-gui
;; This file is always loaded on startup before any user-provided Lisp file

;; Default completion pattern: match non-whitespace at end of line
(define *completion-pattern* "\\S+$")

;; Default completion hook: returns empty list (no completions by default)
;; Users can override this by setting completion-hook in their Lisp file
(define completion-hook (lambda (text) (progn (princ text) (princ "\n") ())))

;; Mouse wheel scrolling configuration
;; Number of lines to scroll per wheel click (default: 3)
(define *scroll-lines-per-click* 3)

;; Enable smooth scrolling for high-resolution trackpads (default: #t)
(define *smooth-scrolling-enabled* #t)
