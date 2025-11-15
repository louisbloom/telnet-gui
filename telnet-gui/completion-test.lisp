;; Test configuration for tab-cycling completion
;; Load with: ./build/telnet-gui/telnet-gui.exe -l completion-test.lisp <hostname> <port>

;; All available commands
(define all-commands '("help" "hello" "helicopter" "list" "lisp" "listen"
                       "test" "telnet" "terminal" "temp" "show" "shell" "shutdown"))

;; Helper function to filter commands that match the prefix
(define filter-commands
  (lambda (prefix commands)
    (if (null? commands)
        ()
        (if (string-prefix? prefix (car commands))
            (cons (car commands) (filter-commands prefix (cdr commands)))
            (filter-commands prefix (cdr commands))))))

;; Simple completion hook that filters all commands
(define completion-hook
  (lambda (text)
    (filter-commands text all-commands)))

;; Print message on startup
(princ "Tab-cycling completion test loaded!\n")
(princ "Try typing: hel<TAB> or te<TAB> or lis<TAB>\n")
(princ "- TAB: cycle through completions\n")
(princ "- ESC or Ctrl+G: cancel and revert\n")
(princ "- Any other key: accept current completion\n")
