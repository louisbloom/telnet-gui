;; init-post.lisp - Features that depend on SDL/GUI being initialized
;;
;; This file is loaded after init.lisp and after the renderer is ready.
;; Animation functions are only available when built with rlottie support.

;; ============================================================================
;; SCROLL-LOCK NOTIFICATION ANIMATION
;; ============================================================================

(defvar *scroll-lock-notification-enabled* #t
  "Enable/disable scroll-lock notification animation.

When the terminal is in scroll-lock mode (user scrolled back from live output)
and new telnet data arrives, play a subtle notification animation to alert
the user that new content is available.

## Values
- `#t` - Enable notification animation (default)
- `#f` - Disable notification animation

Note: Requires rlottie support to be compiled in.")

;; Animation object
(define *scroll-lock-notification-animation* nil)

;; Load animation if rlottie support is available
(when (bound? 'animation-load)
  (let ((anim (animation-load "ripple-nudge.json")))
    (when (and anim (not (error? anim)) (animation-loaded? anim))
      (animation-set-loop anim nil)      ; Play once, don't loop
      (animation-set-dim-mode anim 0.85) ; Subtle overlay
      (set! *scroll-lock-notification-animation* anim))))

(defun maybe-play-scroll-lock-notification ()
  "Play notification animation if scroll-locked and not already playing."
  (when (and *scroll-lock-notification-enabled*
          *scroll-lock-notification-animation*
          (terminal-scroll-locked?)
          (not (animation-playing? *scroll-lock-notification-animation*)))
    (animation-play *scroll-lock-notification-animation*)))

;; Register hook for scroll-lock notifications
(add-hook 'telnet-input-hook
  (lambda (text) (maybe-play-scroll-lock-notification)))
