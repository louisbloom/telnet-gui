;; Practice mode tests
;; Tests the practice.lisp script (/practice command)
(load "test-helpers.lisp")

;; ============================================================================
;; Additional mock init.lisp functions (hooks in test-helpers.lisp)
;; ============================================================================

;; Mock timer system
(define *timer-list* '())
(define *timer-next-id* 1)

(defun run-at-time (time repeat fn &rest args)
  "Mock run-at-time: create a fake timer."
  (let ((timer (list *timer-next-id* 0 (if repeat repeat 0) fn args)))
    (set! *timer-next-id* (+ *timer-next-id* 1))
    (set! *timer-list* (cons timer *timer-list*))
    timer))

(defun cancel-timer (timer)
  "Mock cancel-timer: remove from list."
  (let ((id (car timer)))
    (set! *timer-list* (filter (lambda (t) (not (= (car t) id))) *timer-list*))
    #t))

;; Helper for filter (may not exist in minimal env)
(defun filter (pred lst)
  (if (null? lst)
    '()
    (if (pred (car lst))
      (cons (car lst) (filter pred (cdr lst)))
      (filter pred (cdr lst)))))

;; Helper for member (may not exist in minimal env)
(defun member (item lst)
  (if (null? lst)
    nil
    (if (equal? item (car lst))
      lst
      (member item (cdr lst)))))

;; Helper: check if any string in list contains substring
(defun any-contains? (lst substr)
  "Check if any string in list contains the substring."
  (if (null? lst)
    nil
    (if (string-contains? (car lst) substr)
      #t
      (any-contains? (cdr lst) substr))))

;; Mock regex-extract - simplified for mana pattern matching
;; Pattern: (\d+)%m - extract mana percentage from prompt
(defun regex-extract (pattern text)
  "Mock regex-extract: handles the mana pattern specially.
   Returns list of capture groups."
  ;; Simple mock: look for digits followed by %m
  (let ((pos (string-index text "%m")))
    (if (not pos)
      nil
      ;; Extract digits before %m
      (let ((num-str (extract-digits-before text pos)))
        (if num-str
          (list num-str)  ; List of capture groups
          nil)))))

;; Mock regex-match? - simplified for hunger/thirst pattern
;; Pattern: "Your (hunger|thirst) \\w+ you"
(defun regex-match? (pattern text)
  "Mock regex-match?: handles hunger/thirst pattern.
   Returns #t if pattern matches, nil otherwise."
  ;; Check for hunger/thirst damage patterns
  (if (string-contains? text "Your hunger")
    (if (string-contains? text " you")
      #t
      nil)
    (if (string-contains? text "Your thirst")
      (if (string-contains? text " you")
        #t
        nil)
      nil)))

;; Helper: extract consecutive digits before position
(defun extract-digits-before (text pos)
  (if (<= pos 0)
    nil
    (let ((start (find-digit-start text (- pos 1))))
      (if start
        (substring text start pos)
        nil))))

(defun find-digit-start (text pos)
  (if (< pos 0)
    nil
    (let ((ch (substring text pos (+ pos 1))))
      (if (digit-char? ch)
        (let ((prev (find-digit-start text (- pos 1))))
          (if prev prev pos))
        (if (= pos (length text))
          nil
          (+ pos 1))))))

(defun digit-char? (ch)
  (or (string=? ch "0") (string=? ch "1") (string=? ch "2")
      (string=? ch "3") (string=? ch "4") (string=? ch "5")
      (string=? ch "6") (string=? ch "7") (string=? ch "8")
      (string=? ch "9")))

;; ============================================================================
;; Mock GUI functions - capture calls instead of performing real actions
;; ============================================================================

(define *mock-telnet-sends* '())
(define *mock-terminal-echoes* '())

(defun telnet-send (text)
  "Mock telnet-send: record the sent text."
  (set! *mock-telnet-sends* (append *mock-telnet-sends* (list text)))
  nil)

(defun terminal-echo (text)
  "Mock terminal-echo: record the echoed text."
  (set! *mock-terminal-echoes* (append *mock-terminal-echoes* (list text)))
  nil)

;; Mock divider-mode-set and divider-mode-remove
;; These match the C builtin behavior in the real app - they update *divider-modes*
;; The variable *divider-modes* is defined in init.lisp (or we define it here)
(define *divider-modes* '())

(defun divider-mode-set (sym display &rest args)
  "Mock divider-mode-set: update *divider-modes* directly."
  (let ((priority (if (and args (not (null? args))) (car args) 50)))
    ;; Remove existing entry for this symbol
    (set! *divider-modes*
      (filter (lambda (entry)
                (not (eq? (car (cdr entry)) sym)))
              *divider-modes*))
    ;; Add new entry: (priority . (symbol . display))
    (let ((new-entry (cons priority (cons sym display))))
      (set! *divider-modes* (cons new-entry *divider-modes*))))
  nil)

(defun divider-mode-remove (sym)
  "Mock divider-mode-remove: update *divider-modes* directly."
  (set! *divider-modes*
    (filter (lambda (entry)
              (not (eq? (car (cdr entry)) sym)))
            *divider-modes*))
  nil)

;; Helper to check if a mode is set in *divider-modes*
(defun divider-mode-has? (sym)
  "Check if symbol is in *divider-modes* alist."
  (let ((found nil))
    (do ((modes *divider-modes* (cdr modes)))
      ((or found (null? modes)) found)
      (let ((entry (car modes)))
        (if (and entry (pair? entry))
          (let ((sym-display (cdr entry)))
            (if (and sym-display (pair? sym-display))
              (if (eq? (car sym-display) sym)
                (set! found #t)))))))))

;; Mock user-input-hook (will be saved by practice.lisp)
(defun user-input-hook (text cursor-pos)
  "Mock original user-input-hook: just return text."
  text)

;; Reset all mocks
(defun reset-mocks ()
  (set! *mock-telnet-sends* '())
  (set! *mock-terminal-echoes* '())
  ;; Clear divider modes directly (it's a global variable)
  (set! *divider-modes* '()))

;; ============================================================================
;; Load practice.lisp
;; ============================================================================

(load "../lisp/contrib/practice.lisp")

;; ============================================================================
;; Test practice-command? helper function
;; ============================================================================

(reset-mocks)

;; Test full command
(assert-equal (practice-command? "/practice c lightb") "c lightb"
  "practice-command? parses /practice with args")

;; Test partial prefixes
(assert-equal (practice-command? "/p c lightb") "c lightb"
  "practice-command? parses /p with args")
(assert-equal (practice-command? "/pr test") "test"
  "practice-command? parses /pr with args")
(assert-equal (practice-command? "/prac foo bar") "foo bar"
  "practice-command? parses /prac with args")

;; Test bare command (no args)
(assert-equal (practice-command? "/p") ""
  "practice-command? returns empty string for bare /p")
(assert-equal (practice-command? "/practice") ""
  "practice-command? returns empty string for bare /practice")

;; Test non-matching commands
(assert-false (practice-command? "look")
  "practice-command? returns nil for non-slash command")
(assert-false (practice-command? "/other command")
  "practice-command? returns nil for different slash command")
(assert-false (practice-command? "/px test")
  "practice-command? returns nil for non-prefix like /px")

;; ============================================================================
;; Test practice-matches-any-pattern? helper
;; ============================================================================

(assert-true (practice-matches-any-pattern? "You failed." '("You failed." "You lost"))
  "matches first pattern in list")
(assert-true (practice-matches-any-pattern? "You lost your concentration." '("You failed." "You lost"))
  "matches second pattern (partial)")
(assert-false (practice-matches-any-pattern? "Success!" '("You failed." "You lost"))
  "returns nil when no pattern matches")
(assert-false (practice-matches-any-pattern? "anything" '())
  "returns nil for empty pattern list")

;; ============================================================================
;; Test practice-extract-mana helper
;; ============================================================================

(assert-equal (practice-extract-mana "<100%hp 100%m 100%mv>") 100
  "extracts 100% mana from prompt")
(assert-equal (practice-extract-mana "<100%hp 28%m 100%mv>") 28
  "extracts 28% mana from prompt")
(assert-equal (practice-extract-mana "civilized <100%hp 5%m 100%mv 2667tnl>") 5
  "extracts 5% mana from full prompt")
(assert-false (practice-extract-mana "no mana here")
  "returns nil when no mana pattern found")

;; ============================================================================
;; Test practice-start
;; ============================================================================

(reset-mocks)
;; Reset state
(set! *practice-mode* nil)
(set! *practice-command* nil)
(set! *practice-sleep-mode* nil)
(set! *practice-sleep-timer* nil)

(practice-start "c lightb")

(assert-true *practice-mode* "practice-start sets *practice-mode* to true")
(assert-equal *practice-command* "c lightb" "practice-start stores the command")
(assert-false *practice-sleep-mode* "practice-start clears sleep mode")
(assert-true (member "c lightb" *mock-telnet-sends*)
  "practice-start sends the command via telnet")
(assert-true (divider-mode-has? 'practice)
  "practice-start sets divider mode to P")

;; ============================================================================
;; Test practice-start when already practicing
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "existing")

(practice-start "new command")

(assert-equal *practice-command* "existing"
  "practice-start does not change command when already practicing")
(assert-false (member "new command" *mock-telnet-sends*)
  "practice-start does not send new command when already practicing")

;; ============================================================================
;; Test practice-stop
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(set! *practice-sleep-timer* nil)

(practice-stop)

(assert-false *practice-mode* "practice-stop clears *practice-mode*")
(assert-false *practice-command* "practice-stop clears *practice-command*")
(assert-false (divider-mode-has? 'practice)
  "practice-stop removes divider mode")

;; ============================================================================
;; Test practice-enter-sleep
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(set! *practice-sleep-timer* nil)

(practice-enter-sleep)

(assert-true *practice-sleep-mode* "practice-enter-sleep sets sleep mode")
(assert-true *practice-sleep-timer* "practice-enter-sleep creates timer")
(assert-true (member "sleep" *mock-telnet-sends*)
  "practice-enter-sleep sends sleep command")
(assert-true (divider-mode-has? 'practice-sleep)
  "practice-enter-sleep sets divider mode to Z")

;; Cancel the timer we created
(if *practice-sleep-timer* (cancel-timer *practice-sleep-timer*))

;; ============================================================================
;; Test practice-exit-sleep
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* #t)
(set! *practice-sleep-timer* (run-at-time 9999 9999 (lambda () nil)))

(practice-exit-sleep)

(assert-false *practice-sleep-mode* "practice-exit-sleep clears sleep mode")
(assert-false *practice-sleep-timer* "practice-exit-sleep clears timer reference")
(assert-true (member "stand" *mock-telnet-sends*)
  "practice-exit-sleep sends stand command")
(assert-true (member "c lightb" *mock-telnet-sends*)
  "practice-exit-sleep resumes with practice command")
(assert-false (divider-mode-has? 'practice-sleep)
  "practice-exit-sleep removes sleep indicator")

;; ============================================================================
;; Test practice-telnet-hook - failure pattern detection
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(set! *practice-sleep-timer* nil)

(practice-telnet-hook "You failed.\r\n")

(assert-true (member "c lightb" *mock-telnet-sends*)
  "telnet hook retries command on 'You failed.'")

(reset-mocks)
(practice-telnet-hook "You lost your concentration.\r\n")

(assert-true (member "c lightb" *mock-telnet-sends*)
  "telnet hook retries command on 'You lost your concentration.'")

;; ============================================================================
;; Test practice-telnet-hook - mana exhaustion detection
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(set! *practice-sleep-timer* nil)

(practice-telnet-hook "You don't have enough mana.\r\n")

(assert-true *practice-sleep-mode* "telnet hook enters sleep mode on mana exhaustion")
(assert-true (member "sleep" *mock-telnet-sends*)
  "telnet hook sends sleep on mana exhaustion")

;; Cleanup timer
(if *practice-sleep-timer* (cancel-timer *practice-sleep-timer*))

;; ============================================================================
;; Test practice-telnet-hook - mana restoration detection
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* #t)
(set! *practice-sleep-timer* (run-at-time 9999 9999 (lambda () nil)))

(practice-telnet-hook "civilized <100%hp 100%m 100%mv>\r\n")

(assert-false *practice-sleep-mode* "telnet hook exits sleep mode on 100% mana")
(assert-true (member "stand" *mock-telnet-sends*)
  "telnet hook sends stand on mana restoration")
(assert-true (member "c lightb" *mock-telnet-sends*)
  "telnet hook resumes practice on mana restoration")

;; ============================================================================
;; Test practice-telnet-hook - partial mana (stays in sleep)
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* #t)
(set! *practice-sleep-timer* (run-at-time 9999 9999 (lambda () nil)))

(practice-telnet-hook "civilized <100%hp 50%m 100%mv>\r\n")

(assert-true *practice-sleep-mode* "telnet hook stays in sleep mode on partial mana")
(assert-false (member "stand" *mock-telnet-sends*)
  "telnet hook does not send stand on partial mana")

;; Cleanup timer
(if *practice-sleep-timer* (cancel-timer *practice-sleep-timer*))

;; ============================================================================
;; Test practice-telnet-hook - inactive when not in practice mode
;; ============================================================================

(reset-mocks)
(set! *practice-mode* nil)

(practice-telnet-hook "You failed.\r\n")

(assert-equal *mock-telnet-sends* '()
  "telnet hook does nothing when practice mode is inactive")

;; ============================================================================
;; Test practice-user-input-hook integration (via add-hook system)
;; ============================================================================

(reset-mocks)
(set! *practice-mode* nil)
(set! *practice-command* nil)
(set! *user-input-handled* nil)
(set! *user-input-result* nil)

;; Test /p start - call practice-user-input-hook directly (simulating hook dispatch)
(practice-user-input-hook "/p c lightb" 0)
(assert-true *user-input-handled* "practice-user-input-hook sets *user-input-handled*")
(assert-true *practice-mode* "practice-user-input-hook /p starts practice mode")
(assert-equal *practice-command* "c lightb" "practice-user-input-hook /p stores command")

;; Test /p stop
(reset-mocks)
(set! *user-input-handled* nil)
(practice-user-input-hook "/p stop" 0)
(assert-true *user-input-handled* "practice-user-input-hook sets *user-input-handled* on stop")
(assert-false *practice-mode* "practice-user-input-hook /p stop stops practice mode")

;; Test non-matching command (should not handle)
(reset-mocks)
(set! *user-input-handled* nil)
(set! *practice-mode* nil)
(practice-user-input-hook "look" 0)
(assert-false *user-input-handled* "practice-user-input-hook does not handle non-practice commands")

;; ============================================================================
;; Test practice-user-input-hook - status display (/p with no args)
;; ============================================================================

;; Test /p when not practicing
(reset-mocks)
(set! *practice-mode* nil)
(set! *user-input-handled* nil)
(practice-user-input-hook "/p" 0)
(assert-true *user-input-handled* "practice-user-input-hook handles /p status")
;; Check if any echo contains the message (terminal-echo adds ANSI codes)
(assert-true (any-contains? *mock-terminal-echoes* "Not practicing")
  "practice-user-input-hook shows 'Not practicing' message")

;; Test /p when practicing
(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(set! *user-input-handled* nil)
(practice-user-input-hook "/p" 0)
(assert-true *user-input-handled* "practice-user-input-hook handles /p status when practicing")
(assert-true (any-contains? *mock-terminal-echoes* "Currently practicing: c lightb")
  "practice-user-input-hook shows current command when practicing")

;; Test /p when practicing and sleeping
(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* #t)
(set! *user-input-handled* nil)
(practice-user-input-hook "/p" 0)
(assert-true *user-input-handled* "practice-user-input-hook handles /p status when sleeping")
(assert-true (any-contains? *mock-terminal-echoes* "Currently practicing: c lightb (sleeping)")
  "practice-user-input-hook shows sleeping status")

;; ============================================================================
;; Test practice-telnet-hook - hunger/thirst damage detection
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(set! *practice-sleep-timer* nil)

;; Test hunger damage detection
(practice-telnet-hook "Your hunger grazes you.\r\n")
(assert-false *practice-mode* "telnet hook stops practice on hunger damage")
(assert-true (member "quit" *mock-telnet-sends*)
  "telnet hook sends quit on hunger damage")
(assert-true (any-contains? *mock-terminal-echoes* "Hunger/thirst damage detected")
  "telnet hook shows quit message on hunger damage")

;; Test thirst damage detection
(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(practice-telnet-hook "Your thirst grazes you.\r\n")
(assert-false *practice-mode* "telnet hook stops practice on thirst damage")
(assert-true (member "quit" *mock-telnet-sends*)
  "telnet hook sends quit on thirst damage")

;; Test hunger damage with different verb
(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(practice-telnet-hook "Your hunger hits you.\r\n")
(assert-false *practice-mode* "telnet hook stops practice on hunger damage with different verb")
(assert-true (member "quit" *mock-telnet-sends*)
  "telnet hook sends quit on hunger damage with different verb")

;; ============================================================================
;; Test practice-telnet-hook - mana threshold detection
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(set! *practice-sleep-timer* nil)

;; Test mana below threshold (15%)
(practice-telnet-hook "civilized <100%hp 15%m 100%mv>\r\n")
(assert-true *practice-sleep-mode* "telnet hook enters sleep mode when mana < 20%")
(assert-true (member "sleep" *mock-telnet-sends*)
  "telnet hook sends sleep when mana < 20%")

;; Cleanup timer
(if *practice-sleep-timer* (cancel-timer *practice-sleep-timer*))

;; Test mana at threshold boundary (19%)
(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(practice-telnet-hook "civilized <100%hp 19%m 100%mv>\r\n")
(assert-true *practice-sleep-mode* "telnet hook enters sleep mode when mana = 19%")

;; Cleanup timer
(if *practice-sleep-timer* (cancel-timer *practice-sleep-timer*))

;; Test mana at threshold boundary (20% - should not sleep)
(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(practice-telnet-hook "civilized <100%hp 20%m 100%mv>\r\n")
(assert-false *practice-sleep-mode* "telnet hook does not enter sleep mode when mana = 20%")

;; ============================================================================
;; Test practice-telnet-hook - "You are already" retry pattern
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(set! *practice-sleep-timer* nil)

(practice-telnet-hook "You are already practicing that spell.\r\n")
(assert-true (member "c lightb" *mock-telnet-sends*)
  "telnet hook retries command on 'You are already' pattern")

;; ============================================================================
;; Test practice-add-retry-pattern
;; ============================================================================

(reset-mocks)
(set! *practice-retry-patterns* '("You failed." "You lost your concentration." "You are already"))

;; Test adding new pattern
(practice-add-retry-pattern "Your spell fizzles.")
(assert-true (member "Your spell fizzles." *practice-retry-patterns*)
  "practice-add-retry-pattern adds new pattern to list")
(assert-true (any-contains? *mock-terminal-echoes* "Added retry pattern: Your spell fizzles.")
  "practice-add-retry-pattern shows success message")

;; Test adding duplicate pattern
(reset-mocks)
(practice-add-retry-pattern "You failed.")
(assert-false (any-contains? *mock-terminal-echoes* "Added retry pattern: You failed.")
  "practice-add-retry-pattern does not add duplicate")
(assert-true (any-contains? *mock-terminal-echoes* "Pattern already exists: You failed.")
  "practice-add-retry-pattern shows exists message for duplicate")

;; ============================================================================
;; Test practice-send-empty (timer callback)
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-sleep-mode* #t)

(practice-send-empty)
(assert-true (member "" *mock-telnet-sends*)
  "practice-send-empty sends empty string when in sleep mode")

;; Test when not in sleep mode
(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-sleep-mode* nil)

(practice-send-empty)
(assert-false (member "" *mock-telnet-sends*)
  "practice-send-empty does not send when not in sleep mode")

;; Test when not in practice mode
(reset-mocks)
(set! *practice-mode* nil)
(set! *practice-sleep-mode* #t)

(practice-send-empty)
(assert-false (member "" *mock-telnet-sends*)
  "practice-send-empty does not send when not in practice mode")

;; ============================================================================
;; Test practice-stop - edge cases
;; ============================================================================

;; Test stop when not practicing
(reset-mocks)
(set! *practice-mode* nil)
(practice-stop)
(assert-true (any-contains? *mock-terminal-echoes* "Not currently practicing")
  "practice-stop shows message when not practicing")

;; Test stop with active timer
(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* #t)
(set! *practice-sleep-timer* (run-at-time 9999 9999 (lambda () nil)))

(practice-stop)
(assert-false *practice-sleep-timer* "practice-stop cancels active timer")
(assert-false *practice-mode* "practice-stop clears practice mode")

;; ============================================================================
;; Test practice-enter-sleep - idempotency
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* #t)
(set! *practice-sleep-timer* (run-at-time 9999 9999 (lambda () nil)))

;; Count timers before
(define timer-count-before (length *timer-list*))
(practice-enter-sleep)
(define timer-count-after (length *timer-list*))

;; Should not create duplicate timer
(assert-equal timer-count-before timer-count-after
  "practice-enter-sleep does not create duplicate timer when already sleeping")

;; Cleanup
(if *practice-sleep-timer* (cancel-timer *practice-sleep-timer*))

;; ============================================================================
;; Test practice-exit-sleep - idempotency
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(set! *practice-sleep-timer* nil)

(practice-exit-sleep)
(assert-false (member "stand" *mock-telnet-sends*)
  "practice-exit-sleep does nothing when not in sleep mode")

;; ============================================================================
;; Test practice-quit-on-hunger-thirst directly
;; ============================================================================

(reset-mocks)
(set! *practice-mode* #t)
(set! *practice-command* "c lightb")
(set! *practice-sleep-mode* nil)
(set! *practice-sleep-timer* nil)

(practice-quit-on-hunger-thirst)
(assert-false *practice-mode* "practice-quit-on-hunger-thirst stops practice mode")
(assert-true (member "quit" *mock-telnet-sends*)
  "practice-quit-on-hunger-thirst sends quit command")
(assert-true (any-contains? *mock-terminal-echoes* "Hunger/thirst damage detected")
  "practice-quit-on-hunger-thirst shows quit message")

;; Cleanup any remaining timers
(set! *timer-list* '())

;; ============================================================================
;; All tests passed
;; ============================================================================

(princ "All practice command tests passed!\n")
