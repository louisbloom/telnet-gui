;; TinTin++ Test Suite

;; Setup - define terminal-echo if not available (for headless testing)
(define terminal-echo (lambda (text) nil))  ; ignore

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
