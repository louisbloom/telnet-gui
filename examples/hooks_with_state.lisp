;; telnet-gui hooks with state management using set!
;; Demonstrates how to mutate global state from within hook lambdas

;; ============================================
;; Example 1: Simple message counter
;; ============================================

(define message_count 0)

(define count_messages (lambda (data)
  ;; set! allows mutating global state from within lambda
  (set! message_count (+ message_count 1))
  data))  ; Pass data through

(register-user-input-hook count_messages)

;; After typing messages, check:
message_count  ; => number of messages typed

;; ============================================
;; Example 2: Command history
;; ============================================

(define command_history nil)

(define save_to_history (lambda (data)
  (set! command_history (cons data command_history))
  data))

(register-user-input-hook save_to_history)

;; History is saved in command_history list

;; ============================================
;; Example 3: Statistics tracking
;; ============================================

(define stats_messages 0)
(define stats_chars 0)

(define track_stats (lambda (data)
  (set! stats_messages (+ stats_messages 1))
  (set! stats_chars (+ stats_chars (strlen data)))
  data))

(register-user-input-hook track_stats)

;; Check stats:
stats_messages
stats_chars

;; ============================================
;; Example 4: Smart filtering with state
;; ============================================

(define blocked_count 0)
(define filter_enabled true)

(define smart_filter (lambda (data)
  (if (and filter_enabled (string-contains data "spam"))
      (progn
        (set! blocked_count (+ blocked_count 1))
        nil)  ; Block the message
      data)))  ; Allow it

(register-user-input-hook smart_filter)

;; Toggle filter off:
(set! filter_enabled false)

;; ============================================
;; KEY TAKEAWAY
;; ============================================
;; set! is the ONLY way to mutate global state
;; from within lambdas/hooks. It updates existing
;; variable bindings rather than creating new ones.
