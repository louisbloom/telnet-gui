;; slash.lisp - Slash command handlers for telnet-gui
;;
;; This file handles slash commands (e.g., /practice, /p).
;; Slash commands are processed via the user-input-hook system before
;; TinTin++ processing, allowing clean separation of concerns.
;;
;; Usage:
;;   :source slash.lisp        ; Load the script
;;   /p c lightb               ; Start practicing (or /practice, /pr, etc.)
;;   /p stop                   ; Stop practice mode
;;   /p                        ; Show current status
;;
;; Practice Mode Features:
;; - Retries command on failure patterns ("You failed.", etc.)
;; - Enters sleep mode on "You don't have enough mana."
;; - Wakes and resumes when prompt shows 100% mana
;; - Visual indicators on divider line: P when practicing, P+Z when sleeping
;;
;; Configuration (via eval mode, Shift+Tab):
;;   *practice-retry-patterns*   - List of patterns that retry the command
;;   *practice-sleep-pattern*    - Pattern that enters sleep mode
;;   *practice-mana-pattern*     - Regex to extract mana % from prompt
;;   *practice-sleep-interval*   - Seconds between prompt refreshes while sleeping
;;
;; Example: Add a retry pattern
;;   (set! *practice-retry-patterns* (cons "Your spell fizzles." *practice-retry-patterns*))

;; ============================================================================
;; CONFIGURATION
;; ============================================================================

(defvar *practice-mana-pattern* "(\\d+)%m"
  "Regex pattern to extract mana percentage from prompt.")

(defvar *practice-mana-threshold* 20
  "Mana percentage below which to enter sleep mode.")

(defvar *practice-sleep-interval* 10
  "Seconds between empty sends during sleep mode to refresh prompt.")

(defvar *practice-sleep-pattern* "You don't have enough mana."
  "Pattern that triggers immediate sleep mode (spell too costly).")

(defvar *practice-retry-patterns*
  '("You failed." "You lost your concentration." "You are already")
  "List of patterns that trigger a retry of the practice command.")

;; ============================================================================
;; STATE VARIABLES
;; ============================================================================

(defvar *practice-mode* nil
  "Whether practice mode is active.")

(defvar *practice-command* nil
  "The command being practiced.")

(defvar *practice-sleep-mode* nil
  "Whether we're in sleep sub-mode (waiting for mana).")

(defvar *practice-sleep-timer* nil
  "Timer object for periodic empty sends during sleep.")

;; ============================================================================
;; HELPER FUNCTIONS
;; ============================================================================

(defun practice-echo (msg)
  "Echo a practice status message to the terminal."
  (terminal-echo (concat "\r\n\033[36m[Practice]\033[0m " msg "\r\n")))

(defun practice-send (cmd)
  "Send a command to telnet with echo."
  ;; Echo the command in cyan (same color as [Practice] tag)
  (terminal-echo (concat "\033[36m" cmd "\033[0m\r\n"))
  ;; Send to server
  (telnet-send cmd))

(defun practice-extract-mana (text)
  "Extract mana percentage from prompt text. Returns number or nil."
  (let ((groups (regex-extract *practice-mana-pattern* text)))
    (if (and groups (not (null? groups)))
      (string->number (car groups))
      nil)))

(defun practice-matches-any-pattern? (text patterns)
  "Check if text contains any of the patterns in the list."
  (if (null? patterns)
    nil
    (if (string-contains? text (car patterns))
      #t
      (practice-matches-any-pattern? text (cdr patterns)))))

;; ============================================================================
;; CORE FUNCTIONS
;; ============================================================================

(defun practice-start (command)
  "Start practice mode with the given command."
  (if *practice-mode*
    (practice-echo (concat "Already practicing: " *practice-command*))
    (progn
      (set! *practice-mode* #t)
      (set! *practice-command* command)
      (set! *practice-sleep-mode* nil)
      (set! *practice-sleep-timer* nil)
      (divider-mode-set 'practice "🤹" 20)
      (practice-send command))))

(defun practice-stop ()
  "Stop practice mode."
  (if (not *practice-mode*)
    (practice-echo "Not currently practicing")
    (progn
      ;; Cancel timer if active
      (if *practice-sleep-timer*
        (progn
          (cancel-timer *practice-sleep-timer*)
          (set! *practice-sleep-timer* nil)))
      ;; Clear state
      (set! *practice-mode* nil)
      (set! *practice-command* nil)
      (set! *practice-sleep-mode* nil)
      ;; Remove indicators
      (divider-mode-remove 'practice)
      (divider-mode-remove 'practice-sleep)
      (practice-echo "Stopped"))))

(defun practice-send-empty ()
  "Timer callback: send empty string to refresh prompt."
  (if (and *practice-mode* *practice-sleep-mode*)
    (telnet-send "")))

(defun practice-enter-sleep ()
  "Enter sleep sub-mode when out of mana."
  (if (not *practice-sleep-mode*)
    (progn
      (set! *practice-sleep-mode* #t)
      ;; Add sleep indicator (P remains, Z added)
      (divider-mode-set 'practice-sleep "💤" 21)
      (practice-echo "Sleeping (low mana)...")
      (practice-send "sleep")
      ;; Start timer for periodic prompt refresh
      (set! *practice-sleep-timer*
        (run-at-time *practice-sleep-interval* *practice-sleep-interval*
          practice-send-empty)))))

(defun practice-exit-sleep ()
  "Exit sleep sub-mode when mana is restored."
  (if *practice-sleep-mode*
    (progn
      ;; Cancel the timer
      (if *practice-sleep-timer*
        (progn
          (cancel-timer *practice-sleep-timer*)
          (set! *practice-sleep-timer* nil)))
      ;; Clear sleep mode
      (set! *practice-sleep-mode* nil)
      ;; Remove sleep indicator (P remains)
      (divider-mode-remove 'practice-sleep)
      (practice-echo "Waking up (mana restored)...")
      ;; Stand up and resume practicing
      (practice-send "stand")
      (practice-send *practice-command*))))

;; ============================================================================
;; TELNET INPUT HOOK
;; ============================================================================

(defun practice-telnet-hook (text)
  "Handle telnet input for practice mode.
   Triggers on specific patterns for retry/sleep, prompt only for waking."
  (if *practice-mode*
    (let ((mana (practice-extract-mana text)))
      (cond
        ;; Check for mana exhaustion message (spell too costly)
        ((and (not *practice-sleep-mode*)
           (string-contains? text *practice-sleep-pattern*))
          (practice-enter-sleep))
        ;; Check if mana dropped below threshold
        ((and (not *practice-sleep-mode*)
           mana
           (< mana *practice-mana-threshold*))
          (practice-enter-sleep))
        ;; Check for retry patterns (spell failed, lost concentration, etc.)
        ((and (not *practice-sleep-mode*)
           (practice-matches-any-pattern? text *practice-retry-patterns*))
          (practice-send *practice-command*))
        ;; In sleep mode: check prompt for mana restoration
        (*practice-sleep-mode*
          (if (and mana (>= mana 100))
            (practice-exit-sleep)))))))

;; Register the telnet hook (only once)
(unless (bound? '*practice-hooks-registered*)
  (add-hook 'telnet-input-hook practice-telnet-hook))

;; ============================================================================
;; USER INPUT HOOK
;; ============================================================================

;; Check if text starts with a partial match of "/practice"
(defun practice-command? (text)
  "Check if text is a /practice command (accepts /p, /pr, /pra, etc.)
   Returns the argument portion after the command, or nil if not a match."
  (if (not (string-prefix? "/" text))
    nil
    (let ((space-pos (string-index text " ")))
      (if (not space-pos)
        ;; No space - check if it's just "/p" or similar (bare command)
        (if (string-prefix? text "/practice")
          ""  ; Return empty string for bare command
          nil)
        ;; Has space - check if prefix matches
        (let ((cmd (substring text 0 space-pos)))
          (if (string-prefix? cmd "/practice")
            (substring text (+ space-pos 1) (string-length text))
            nil))))))

;; User input hook handler for /practice commands
;; Uses add-hook system - sets *user-input-handled* when handling input
(defun practice-user-input-hook (text cursor-pos)
  "Handle /practice commands via the hook system.
   Accepts partial prefixes: /p, /pr, /pra, /prac, /pract, /practi, /practic, /practice"
  (let ((args (practice-command? text)))
    (when args
      (cond
        ;; /p stop or /practice stop
        ((string=? args "stop")
          (practice-stop))
        ;; /p <command> or /practice <command>
        ((> (string-length args) 0)
          (practice-start args))
        ;; Just /p with no args - show status
        (#t
          (if *practice-mode*
            (practice-echo (concat "Currently practicing: " *practice-command*
                             (if *practice-sleep-mode* " (sleeping)" "")))
            (practice-echo "Not practicing. Use /p <command> to start."))))
      ;; Mark as handled
      (set! *user-input-handled* #t)
      (set! *user-input-result* nil))))

;; Register the user input hook (only once)
(unless (bound? '*practice-hooks-registered*)
  (add-hook 'user-input-hook practice-user-input-hook))

;; Mark hooks as registered
(defvar *practice-hooks-registered* #t
  "Guard to prevent duplicate hook registration on reload.")

;; ============================================================================
;; INITIALIZATION MESSAGE
;; ============================================================================

(practice-echo "Loaded. Use /p <command> to start, /p stop to end.")
