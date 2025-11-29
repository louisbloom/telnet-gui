;; Test suite for TinTin++ layer (tintin.lisp)
;;
;; This test file works both standalone (with lisp-repl) and in telnet-gui.
;; It mocks terminal-echo for standalone execution.
;;
;; Usage:
;;   Standalone: ../lisp-repl.exe tintin-test.lisp
;;   In telnet-gui: /test tintin-test.lisp
;;   Headless: telnet-gui.exe -t tintin-test.lisp

;; Mock terminal-echo for standalone execution (no-op if already defined)
(if (not (symbol? 'terminal-echo))
    (define terminal-echo (lambda (text) nil)))  ; ignore

;; Load the tintin layer
;; Note: tintin.lisp is copied to build directory by CMake for tests
(load "tintin.lisp")  ; ignore

;; Test 1: Command separator
(print "=== Test 1: Command Separator ===")  ; => "=== Test 1: Command Separator ==="
(print (tintin-split-commands "cmd1;cmd2"))  ; => ("cmd1" "cmd2")
(print (tintin-split-commands "cmd1;;cmd2"))  ; => ("cmd1" "" "cmd2")

;; Test 2: Speedwalk
(print "=== Test 2: Speedwalk ===")  ; => "=== Test 2: Speedwalk ==="
(print (tintin-expand-speedwalk "2s5w"))  ; => "sswwwww"
(print (tintin-expand-speedwalk "nesw"))  ; => "nesw"

;; Test 3: Alias creation
(print "=== Test 3: Alias Creation ===")  ; => "=== Test 3: Alias Creation ==="
(tintin-process-command "#alias {k} {kill %1}")  ; ignore
(print (hash-ref *tintin-aliases* "k"))  ; => ("kill %1" 5)

;; Test 4: Alias expansion
(print "=== Test 4: Alias Expansion ===")  ; => "=== Test 4: Alias Expansion ==="
(print (tintin-process-command "k orc"))  ; => "kill orc"

;; Test 5: Variable substitution
(print "=== Test 5: Variable Substitution ===")  ; => "=== Test 5: Variable Substitution ==="
(tintin-process-command "#alias {test} {echo %0}")  ; ignore
(print (tintin-process-command "test a b c"))  ; => "echo test a b c"

;; Test 6: Pattern matching
(print "=== Test 6: Pattern Matching ===")  ; => "=== Test 6: Pattern Matching ==="
(tintin-process-command "#alias {k %1 with %2} {draw %2;attack %1}")  ; ignore
(print (tintin-process-command "k orc with sword"))  ; => "draw sword;attack orc"

;; Test 7: Full input processing
(print "=== Test 7: Full Input Processing ===")  ; => "=== Test 7: Full Input Processing ==="
(tintin-process-command "#alias {n} {north}")  ; ignore
(tintin-process-command "#alias {s} {south}")  ; ignore
(print (tintin-process-input "n;s;k orc"))  ; => "north\r\nsouth\r\nkill orc"

;; Test 8: Summary
(print "=== Test 8: Summary ===")  ; => "=== Test 8: Summary ==="
(print (concat "Alias count: " (number->string (hash-count *tintin-aliases*))))  ; => "Alias count: 5"
(print (concat "Speedwalk enabled: " (if *tintin-speedwalk-enabled* "#t" "#f")))  ; => "Speedwalk enabled: #t"

(print "=== All Tests Completed ===")  ; => "=== All Tests Completed ==="
