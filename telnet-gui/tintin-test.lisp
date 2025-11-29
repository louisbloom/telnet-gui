;; TinTin++ Test Suite - Building incrementally

(if (not (symbol? 'terminal-echo))
    (define terminal-echo (lambda (text) nil)))

(load "tintin.lisp")

;; Test 1: Command separator
(print "=== Test 1: Command Separator ===")
(print (tintin-split-commands "cmd1;cmd2"))
(print (tintin-split-commands "cmd1;;cmd2"))

;; Test 2: Speedwalk
(print "=== Test 2: Speedwalk ===")
(print (tintin-expand-speedwalk "2s5w"))
(print (tintin-expand-speedwalk "nesw"))
(print (tintin-expand-speedwalk "3n"))
(print (tintin-expand-speedwalk "2ne3s"))
(print (tintin-expand-speedwalk "nw10e"))

;; Test 3: Alias creation
(print "=== Test 3: Alias Creation ===")
(tintin-process-command "#alias {k} {kill %1}")
(print (hash-ref *tintin-aliases* "k"))

;; Test 4: Simple alias expansion
(print "=== Test 4: Alias Expansion ===")
(print (tintin-process-command "k orc"))

;; Test 5: %0 substitution (all arguments)
(print "=== Test 5: %0 Substitution ===")
(tintin-process-command "#alias {say} {tell bob %0}")
(print (tintin-process-command "say hello there"))

;; Test 6: Pattern matching in alias names
(print "=== Test 6: Pattern Matching ===")
(tintin-process-command "#alias {attack %1 with %2} {kill %1;wield %2}")
(print (tintin-process-command "attack orc with sword"))

;; Test 7: Full input processing (split + expand)
(print "=== Test 7: Full Processing ===")
(print (tintin-process-input "k orc;say hello world"))

;; Test 8: Variable creation and retrieval
(print "=== Test 8: Variables ===")
(tintin-process-command "#variable {target} {orc}")
(tintin-process-command "#variable {weapon} {sword}")
(print (hash-ref *tintin-variables* "target"))
(print (hash-ref *tintin-variables* "weapon"))

;; Test 9: Variable expansion in commands
(print "=== Test 9: Variable Expansion ===")
(print (tintin-process-command "k $target"))
(print (tintin-process-command "attack $target with $weapon"))

(print "=== Tests Complete ===")
