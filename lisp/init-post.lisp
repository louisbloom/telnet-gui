;; init-post.lisp - Features that depend on SDL/GUI being initialized
;;
;; This file is loaded after init.lisp and after the renderer is ready.
;; Animation functions are only available when built with rlottie support.

;; ============================================================================
;; SCROLL-LOCK NOTIFICATION ANIMATION
;; ============================================================================
;; Only enabled when rlottie support is compiled in (animation-load is bound)

(defvar *scroll-lock-notification-enabled* #t
  "Enable/disable scroll-lock notification animation.

When the terminal is in scroll-lock mode (user scrolled back from live output)
and new telnet data arrives, play a subtle notification animation to alert
the user that new content is available.

## Values
- `#t` - Enable notification animation (default)
- `#f` - Disable notification animation

Note: Requires rlottie support to be compiled in.")

;; Animation object (loaded immediately since renderer is ready)
(define *scroll-lock-notification-animation* nil)

;; Check if animation support is available (rlottie compiled in)
(define *animation-support-available* (bound? 'animation-load))

(defun load-scroll-lock-notification ()
  "Load the scroll-lock notification animation.

  ## Description
  Loads the notification.json animation file and configures it for single-play
  (no loop) with a high dim overlay (0.85) for subtlety.

  ## Returns
  The animation object if loaded successfully, nil otherwise."
  (if (and *scroll-lock-notification-enabled* *animation-support-available*)
    (let ((anim (animation-load "notification.json")))
      (if (and anim
            (not (error? anim))
            (animation-loaded? anim))
        (progn
          (animation-set-loop anim nil)      ; Play once, don't loop
          (animation-set-dim-mode anim 0.85) ; Subtle overlay
          (set! *scroll-lock-notification-animation* anim)
          anim)
        nil))
    nil))

(defun maybe-play-scroll-lock-notification ()
  "Play notification animation if scroll-locked and not already playing.

  ## Description
  Called from telnet-input-hook when new data arrives. Checks if:
  1. Notifications are enabled
  2. Animation support is available
  3. Terminal is in scroll-lock mode
  4. Animation is loaded
  5. Animation is not already playing

  If all conditions are met, plays the animation once."
  (if (and *scroll-lock-notification-enabled*
        *animation-support-available*
        *scroll-lock-notification-animation*
        (terminal-scroll-locked?)
        (not (animation-playing? *scroll-lock-notification-animation*)))
    (animation-play *scroll-lock-notification-animation*)))

;; Load animation immediately if support is available
(if *animation-support-available*
  (load-scroll-lock-notification))

;; Register hook for scroll-lock notifications
(add-hook 'telnet-input-hook
  (lambda (text) (maybe-play-scroll-lock-notification)))
