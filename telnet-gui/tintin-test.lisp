;; TinTin++ Test Suite

;; Setup - define terminal-echo and telnet-send stubs that capture calls
(define *telnet-send-log* '())              ; ignore
(define *terminal-echo-log* '())            ; ignore

(define terminal-echo
    (lambda (text)
      (set! *terminal-echo-log* (cons text *terminal-echo-log*))
      nil))  ; ignore

(define telnet-send
    (lambda (text)
      (set! *telnet-send-log* (cons text *telnet-send-log*))
      nil))  ; ignore

;; Load TinTin++ implementation
(load "tintin.lisp")  ; ignore

;; ============================================================================
;; Test 1: Command Separator
;; ============================================================================

;; Split on semicolon
(tintin-split-commands "cmd1;cmd2")        ; => ("cmd1" "cmd2")

;; Empty command between separators
(tintin-split-commands "cmd1;;cmd2")       ; => ("cmd1" "" "cmd2")

;; ============================================================================
;; Test 2: Speedwalk
;; ============================================================================

;; Multiple of same direction
(tintin-expand-speedwalk "2s5w")           ; => "s;s;w;w;w;w;w"

;; Two-character directions (ne=northeast, sw=southwest)
(tintin-expand-speedwalk "nesw")           ; => "ne;sw"

;; Count with direction
(tintin-expand-speedwalk "3n")             ; => "n;n;n"

;; Multiple counted directions with 2-char direction
(tintin-expand-speedwalk "2ne3s")          ; => "ne;ne;s;s;s"

;; 2-char direction followed by count+direction
(tintin-expand-speedwalk "nw10e")          ; => "nw;e;e;e;e;e;e;e;e;e;e"

;; Invalid speedwalk patterns (should return original unchanged)
(tintin-expand-speedwalk "det")            ; => "det"
(tintin-expand-speedwalk "3x")             ; => "3x"
(tintin-expand-speedwalk "abc")            ; => "abc"
(tintin-expand-speedwalk "n2t")            ; => "n2t"

;; ============================================================================
;; Test 3: Alias Creation
;; ============================================================================

;; Create simple alias with %1 substitution
(tintin-process-command "#alias {k} {kill %1}")  ; ignore
(hash-ref *tintin-aliases* "k")                  ; => ("kill %1" 5)

;; ============================================================================
;; Test 4: Simple Alias Expansion
;; ============================================================================

;; Use the alias defined above
(tintin-process-command "k orc")           ; => "kill orc"

;; ============================================================================
;; Test 5: %0 Substitution (All Arguments)
;; ============================================================================

;; Create alias that captures all arguments
(tintin-process-command "#alias {say} {tell bob %0}")  ; ignore
(tintin-process-command "say hello there")             ; => "tell bob hello there"

;; ============================================================================
;; Test 6: Pattern Matching in Alias Names
;; ============================================================================

;; Create alias with multiple pattern variables
(tintin-process-command "#alias {attack %1 with %2} {kill %1;wield %2}")  ; ignore
(tintin-process-command "attack orc with sword")  ; => "kill orc;wield sword"

;; ============================================================================
;; Test 7: Full Input Processing (Split + Expand)
;; ============================================================================

;; Process multiple commands with alias expansion
(tintin-process-input "k orc;say hello world")  ; => "kill orc;tell bob hello world"

;; ============================================================================
;; Test 8: Variable Creation and Retrieval
;; ============================================================================

;; Create variables
(tintin-process-command "#variable {target} {orc}")    ; ignore
(tintin-process-command "#variable {weapon} {sword}")  ; ignore

;; Check they were stored
(hash-ref *tintin-variables* "target")     ; => "orc"
(hash-ref *tintin-variables* "weapon")     ; => "sword"

;; ============================================================================
;; Test 9: Variable Expansion in Commands
;; ============================================================================

;; Use variable in simple alias
(tintin-process-command "k $target")                      ; => "kill orc"

;; Use variables in pattern matching alias
(tintin-process-command "attack $target with $weapon")    ; => "kill orc;wield sword"

;; ============================================================================
;; Test 10: User-Input-Hook Integration
;; ============================================================================

;; Hook should take 2 arguments and return string or nil
(define test-result (tintin-user-input-hook "test" 0))   ; ignore
(or (string? test-result) (null? test-result))           ; => #t

;; When enabled, hook returns nil (hook handles echo/send)
;; This is the proper contract: nil means hook handled everything
(null? (tintin-user-input-hook "hello" 0))               ; => #t
(null? (tintin-user-input-hook "k orc" 0))               ; => #t
(null? (tintin-user-input-hook "3n2e" 0))                ; => #t

;; Cursor position should be ignored (same result regardless)
;; Both calls should return nil (hook handles everything)
(and (null? (tintin-user-input-hook "test" 0))
     (null? (tintin-user-input-hook "test" 5)))        ; => #t

;; ============================================================================
;; Test 11: Toggle Functions
;; ============================================================================

;; Initial state should be enabled
*tintin-enabled*                                      ; => #t

;; Disable should turn off processing (returns original text)
(tintin-disable!)                                     ; ignore
*tintin-enabled*                                      ; => #f
(tintin-user-input-hook "k orc" 0)                   ; => "k orc"
(tintin-user-input-hook "3n" 0)                      ; => "3n"
(tintin-user-input-hook "hello" 0)                   ; => "hello"

;; Enable should turn on processing (returns nil)
(tintin-enable!)                                      ; ignore
*tintin-enabled*                                      ; => #t
(null? (tintin-user-input-hook "k orc" 0))           ; => #t
(null? (tintin-user-input-hook "3n" 0))              ; => #t

;; Toggle should switch state
(tintin-toggle!)                                      ; ignore
*tintin-enabled*                                      ; => #f
(tintin-user-input-hook "test" 0)                    ; => "test"
(tintin-toggle!)                                      ; ignore
*tintin-enabled*                                      ; => #t
(null? (tintin-user-input-hook "test" 0))            ; => #t

;; ============================================================================
;; Test 12: Edge Cases Through Hook
;; ============================================================================

;; Empty string (when enabled, returns nil; when disabled, returns original)
(tintin-enable!)                                      ; ignore
(null? (tintin-user-input-hook "" 0))                ; => #t

;; Whitespace only (when enabled, returns nil)
(null? (tintin-user-input-hook "   " 0))             ; => #t

;; Hook should work when disabled (pass-through)
(tintin-disable!)                                     ; ignore
(tintin-user-input-hook "anything" 0)                ; => "anything"
(tintin-user-input-hook "" 0)                        ; => ""
(tintin-enable!)                                      ; ignore

;; ============================================================================
;; Test 13: Multiple Commands Sent Separately
;; ============================================================================

;; Clear logs
(set! *telnet-send-log* '())                         ; ignore
(set! *terminal-echo-log* '())                       ; ignore

;; Test: "s;s" should send two separate telnet commands with CRLF
;; Note: logs are in reverse order (most recent first)
(tintin-user-input-hook "s;s" 0)                     ; ignore
(list-length *telnet-send-log*)                      ; => 2
(list-ref *telnet-send-log* 1)                       ; => "s\r\n"
(list-ref *telnet-send-log* 0)                       ; => "s\r\n"

;; Clear logs
(set! *telnet-send-log* '())                         ; ignore
(set! *terminal-echo-log* '())                       ; ignore

;; Test: "3n" should send three "n" commands with CRLF
(tintin-user-input-hook "3n" 0)                      ; ignore
(list-length *telnet-send-log*)                      ; => 3
(list-ref *telnet-send-log* 2)                       ; => "n\r\n"
(list-ref *telnet-send-log* 1)                       ; => "n\r\n"
(list-ref *telnet-send-log* 0)                       ; => "n\r\n"

;; Clear logs
(set! *telnet-send-log* '())                         ; ignore
(set! *terminal-echo-log* '())                       ; ignore

;; Test: Alias expansion then multiple sends with CRLF
(tintin-process-command "#alias {go} {n;s;e;w}")     ; ignore
(tintin-user-input-hook "go" 0)                      ; ignore
(list-length *telnet-send-log*)                      ; => 4
(list-ref *telnet-send-log* 3)                       ; => "n\r\n"
(list-ref *telnet-send-log* 2)                       ; => "s\r\n"
(list-ref *telnet-send-log* 1)                       ; => "e\r\n"
(list-ref *telnet-send-log* 0)                       ; => "w\r\n"

;; Clear logs
(set! *telnet-send-log* '())                         ; ignore
(set! *terminal-echo-log* '())                       ; ignore

;; Test: Each command echoed to terminal with CRLF
(tintin-user-input-hook "n;s" 0)                     ; ignore
(list-length *terminal-echo-log*)                    ; => 2
(list-ref *terminal-echo-log* 1)                     ; => "n\r\n"
(list-ref *terminal-echo-log* 0)                     ; => "s\r\n"

;; Clear logs
(set! *telnet-send-log* '())                         ; ignore
(set! *terminal-echo-log* '())                       ; ignore

;; Test: Empty commands are skipped
(tintin-user-input-hook "n;;s" 0)                    ; ignore
(list-length *telnet-send-log*)                      ; => 2
(list-ref *telnet-send-log* 1)                       ; => "n\r\n"
(list-ref *telnet-send-log* 0)                       ; => "s\r\n"
;; ============================================================================
;; Test 14: Partial Command Matching
;; ============================================================================

;; Partial match for #alias
(tintin-process-command "#al {k} {kill %1}")     ; ignore
(hash-ref *tintin-aliases* "k")                  ; => ("kill %1" 5)

;; Partial match for #variable
(tintin-process-command "#var {hp} {100}")       ; ignore
(hash-ref *tintin-variables* "hp")               ; => "100"

;; Very short prefix - should match first command (alias comes before variable alphabetically)
(tintin-process-command "#a {short} {s}")        ; ignore
(hash-ref *tintin-aliases* "short")              ; => ("s" 5)

;; Another partial for variable
(tintin-process-command "#v {mp} {50}")          ; ignore
(hash-ref *tintin-variables* "mp")               ; => "50"

;; ============================================================================
;; Test 15: Command Error Handling
;; ============================================================================

;; Clear echo log
(set! *terminal-echo-log* '())                   ; ignore

;; Unknown command shows error (not sent to telnet)
(tintin-process-command "#foo bar")              ; => ""
(list-length *terminal-echo-log*)                ; => 2
(string-prefix? "#foo bar" (list-ref *terminal-echo-log* 1))  ; => #t
(string-prefix? "Unknown TinTin++" (list-ref *terminal-echo-log* 0))  ; => #t

;; Clear echo log
(set! *terminal-echo-log* '())                   ; ignore

;; Invalid format (just # with space)
(tintin-process-command "# ")                    ; => ""
(list-length *terminal-echo-log*)                ; => 2
(string-prefix? "# " (list-ref *terminal-echo-log* 1))  ; => #t
(string-prefix? "Invalid TinTin++" (list-ref *terminal-echo-log* 0))  ; => #t

;; Clear echo log
(set! *terminal-echo-log* '())                   ; ignore

;; Malformed known command shows syntax error
(tintin-process-command "#alias missing")        ; => ""
(list-length *terminal-echo-log*)                ; => 2
(string-prefix? "#alias missing" (list-ref *terminal-echo-log* 1))  ; => #t
(string-prefix? "Syntax error" (list-ref *terminal-echo-log* 0))  ; => #t

;; ============================================================================
;; Test 16: Case Insensitive Matching
;; ============================================================================

;; Upper case command
(tintin-process-command "#ALIAS {u} {up}")       ; ignore
(hash-ref *tintin-aliases* "u")                  ; => ("up" 5)

;; Mixed case
(tintin-process-command "#VaRiAbLe {test} {val}")  ; ignore
(hash-ref *tintin-variables* "test")             ; => "val"

;; Upper case partial
(tintin-process-command "#AL {d} {down}")        ; ignore
(hash-ref *tintin-aliases* "d")                  ; => ("down" 5)

;; Mixed case partial
(tintin-process-command "#VaR {foo} {bar}")      ; ignore
(hash-ref *tintin-variables* "foo")              ; => "bar"

;; ============================================================================
;; Test 17: Brace-Aware Command Splitting (Regression - Bug Fix)
;; ============================================================================

;; Bug: Semicolons inside braces were incorrectly splitting commands
;; This broke aliases like #alias {ef} {gb $food; eat $food}

;; Test that semicolons inside braces are NOT split
(tintin-split-commands "#alias {ef} {gb $food; eat $food}")
					; => ("#alias {ef} {gb $food; eat $food}")

;; Test nested braces with semicolons
(tintin-split-commands "{a;b};{c;d}")            ; => ("{a;b}" "{c;d}")

;; Test multiple levels of nesting
(tintin-split-commands "cmd1;{a{b;c}d};cmd2")    ; => ("cmd1" "{a{b;c}d}" "cmd2")

;; Test alias creation with semicolon in command
(tintin-process-command "#alias {ef} {get food; eat food}")  ; ignore
(hash-ref *tintin-aliases* "ef")                 ; => ("get food; eat food" 5)

;; Test alias execution expands to multiple commands
(tintin-process-command "ef")                    ; => "get food;eat food"

;; ============================================================================
;; Test 18: Variable Expansion in Alias Results (Regression - Bug Fix)
;; ============================================================================

;; Bug: $variables in alias templates were not expanded
;; For example: #alias {lb} {look $bag} would send "look $bag" instead of "look sack"

;; Create variable and alias that uses it
(tintin-process-command "#variable {bag} {sack}")      ; ignore
(tintin-process-command "#alias {lb} {look $bag}")    ; ignore

;; Test that variable is expanded in alias result
(tintin-process-command "lb")                          ; => "look sack"

;; Test with multiple variables
(tintin-process-command "#variable {container} {chest}")  ; ignore
(tintin-process-command "#variable {item} {gold}")        ; ignore
(tintin-process-command "#alias {gi} {get $item from $container}")  ; ignore
(tintin-process-command "gi")                             ; => "get gold from chest"

;; Test variable expansion with pattern matching aliases
(tintin-process-command "#alias {loot %1} {get %1 from $container}")  ; ignore
(tintin-process-command "loot sword")                     ; => "get sword from chest"

;; Test variable expansion with semicolons in alias
(tintin-process-command "#alias {quick} {get $item; examine $item}")  ; ignore
(tintin-process-command "quick")                          ; => "get gold;examine gold"

;; ============================================================================
;; Test 19: Echo Original Commands (Regression - Bug Fix)
;; ============================================================================

;; Bug: Aliased commands were not echoing the original input
;; For example: typing "bag sack" (aliased to "#var {bag} {%0}") didn't echo "bag sack"

;; Clear logs
(set! *telnet-send-log* '())                              ; ignore
(set! *terminal-echo-log* '())                            ; ignore

;; Create an alias
(tintin-process-command "#alias {testcmd} {north}")       ; ignore

;; Test that original command is echoed before processing
(tintin-user-input-hook "testcmd" 0)                      ; ignore
;; Should echo "testcmd\r\n" (original input)
(> (list-length *terminal-echo-log*) 0)                   ; => #t
(string-prefix? "testcmd" (list-ref *terminal-echo-log* 0))  ; => #t

;; Clear logs
(set! *telnet-send-log* '())                              ; ignore
(set! *terminal-echo-log* '())                            ; ignore

;; Test # commands are still echoed (existing behavior)
(tintin-user-input-hook "#alias {x} {test}" 0)            ; ignore
(> (list-length *terminal-echo-log*) 0)                   ; => #t
(string-prefix? "#alias" (list-ref *terminal-echo-log* 1))  ; => #t

;; ============================================================================
;; Test 20: Combined Regression Test (All Three Bugs)
;; ============================================================================

;; Test all three fixes work together:
;; 1. Brace-aware splitting
;; 2. Variable expansion in aliases
;; 3. Original command echo

;; Clear logs
(set! *telnet-send-log* '())                              ; ignore
(set! *terminal-echo-log* '())                            ; ignore

;; Create variable
(tintin-process-command "#variable {food} {bread}")       ; ignore

;; Create alias with semicolon AND variable (tests fixes 1 & 2)
(tintin-process-command "#alias {ef} {get $food; eat $food}")  ; ignore

;; Execute alias (tests all three fixes)
(tintin-user-input-hook "ef" 0)                           ; ignore

;; Verify variable was expanded and commands were sent
(list-length *telnet-send-log*)                           ; => 2
(list-ref *telnet-send-log* 1)                            ; => "get bread\r\n"
(list-ref *telnet-send-log* 0)                            ; => "eat bread\r\n"

;; Verify original command was echoed
(> (list-length *terminal-echo-log*) 0)                   ; => #t
(string-prefix? "ef" (list-ref *terminal-echo-log* 0))    ; => #t
