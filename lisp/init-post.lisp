;; init-post.lisp - Features that depend on SDL/GUI being initialized
;;
;; This file is loaded after init.lisp and after the renderer is ready.
;; Animation functions are only available when built with rlottie support.

;; ============================================================================
;; HELPER FUNCTIONS FOR VERSION ANNOUNCEMENT
;; ============================================================================

;; Calculate visual display length of string, excluding ANSI escape codes
(defun visual-length (str)
  (if (not (string? str))
    0
    (let ((ansi-pattern "\\033\\[[0-9;]*m"))
      (length (regex-replace-all ansi-pattern str "")))))

;; Pad string to specified width with trailing spaces
(defun pad-string (str width)
  (if (not (string? str))
    ""
    (let ((visual-len (visual-length str)))
      (let ((padding-needed (- width visual-len)))
        (if (<= padding-needed 0)
          str
          (let ((result str))
            (do ((i 0 (+ i 1)))
              ((>= i padding-needed) result)
              (set! result (concat result " ")))))))))

;; Repeat a string N times
(defun repeat-string (str count)
  (if (<= count 0)
    ""
    (let ((result ""))
      (do ((i 0 (+ i 1)))
        ((>= i count) result)
        (set! result (concat result str))))))

;; Find maximum visual length from a list of strings
(defun max-visual-length (str-list)
  (if (null? str-list)
    0
    (let ((first (visual-length (car str-list)))
           (rest (max-visual-length (cdr str-list))))
      (if (> first rest) first rest))))

;; Format a single info line with proper padding
(defun format-info-line (name value name-width value-width box-width)
  (let* ((name-padding (- name-width (length name)))
          (right-padding (- box-width name-width value-width 4)))
    (concat "│    \033[38;2;220;220;220m" name "\033[0;38;2;100;149;237m"
      (pad-string "" name-padding)
      "\033[38;2;144;238;144m" (pad-string value value-width) "\033[38;2;100;149;237m"
      (pad-string "" right-padding) "│\n")))

;; Format multiple info lines from a list of (name . value) pairs
(defun format-info-lines (entries name-width value-width box-width)
  (if (null? entries)
    ""
    (concat (format-info-line (car (car entries)) (cdr (car entries)) name-width value-width box-width)
      (format-info-lines (cdr entries) name-width value-width box-width))))

;; Build the complete version announcement box
(defun build-version-box (version libvterm sdl2 sdl2-ttf bdw-gc pcre2 rlottie system arch toolchain)
  (let* ((title-text (concat "Telnet GUI  v" version))
          (name-width 10)
          (lib-entries (if rlottie
                         (list (cons "libvterm:" libvterm) (cons "SDL2:" sdl2) (cons "SDL2_ttf:" sdl2-ttf)
                           (cons "bdw-gc:" bdw-gc) (cons "PCRE2:" pcre2) (cons "rlottie:" rlottie))
                         (list (cons "libvterm:" libvterm) (cons "SDL2:" sdl2) (cons "SDL2_ttf:" sdl2-ttf)
                           (cons "bdw-gc:" bdw-gc) (cons "PCRE2:" pcre2))))
          (build-entries (list (cons "System:" system) (cons "Arch:" arch) (cons "Compiler:" toolchain)))
          (value-width (max-visual-length (list libvterm sdl2 sdl2-ttf bdw-gc pcre2 (or rlottie "") system arch toolchain)))
          (title-width (+ (visual-length title-text) 4))
          (content-width (+ name-width value-width 7))
          (box-width (if (> title-width content-width) title-width content-width))
          (border (repeat-string "─" box-width)))
    (concat "\033[38;2;100;149;237m"
      "┌" border "┐\n"
      "│  \033[1;38;2;255;215;0mTelnet GUI\033[0;38;2;100;149;237m  "
      "\033[38;2;144;238;144mv" version "\033[38;2;100;149;237m"
      (pad-string "" (- box-width (visual-length title-text) 2)) "│\n"
      "├" border "┤\n"
      "│  \033[38;2;200;200;200mLibraries:\033[0;38;2;100;149;237m" (pad-string "" (- box-width 12)) "│\n"
      (format-info-lines lib-entries name-width value-width box-width)
      "├" border "┤\n"
      "│  \033[38;2;200;200;200mBuild:\033[0;38;2;100;149;237m" (pad-string "" (- box-width 8)) "│\n"
      (format-info-lines build-entries name-width value-width box-width)
      "└" border "┘\n"
      "\033[0m")))

;; ============================================================================
;; VERSION ANNOUNCEMENT
;; ============================================================================

;; Helper to extract value from version alist
(defun version-get (key)
  (cdr (assoc key telnet-gui-version)))

;; Display version announcement if telnet-gui-version is available
(when (bound? 'telnet-gui-version)
  (terminal-echo (build-version-box
                   (version-get 'version) (version-get 'libvterm) (version-get 'sdl2)
                   (version-get 'sdl2-ttf) (version-get 'bdw-gc) (version-get 'pcre2)
                   (version-get 'rlottie) (version-get 'system) (version-get 'architecture)
                   (version-get 'toolchain))))

;; ============================================================================
;; EMOJI/SYMBOL RENDERING TEST
;; ============================================================================

;; Display emoji and symbol test
;; Emoji (Unicode Emoji=Yes) are 2-cell; pure symbols are 1-cell
(terminal-echo "\033[38;2;100;149;237mSymbols (1-cell):\033[0m ◆◇☆★✦✧♠♣♥♦ ")
(terminal-echo "\033[38;2;100;149;237mEmoji (2-cell):\033[0m ⚔ 🗡 ☀ ☁ ☂ ❄ ✂ ✈ ⚙\n")
(terminal-echo "\033[38;2;100;149;237mTrue Emoji:\033[0m ⚓⚡😀🎮🏰🐉🔮💎🎯🎨🎭🎪🎢🎡🎠🏆🥇🚀\n")

;; Display block elements test
(terminal-echo "\033[38;2;100;149;237mBlock Elements:\033[0m ")
(terminal-echo "█▇▆▅▄▃▂▁ ▏▎▍▌▋▊▉█ ▀▄ ▌▐ ▔▕ ░▒▓█ ▖▗▘▝ ▙▚▛▜▞▟\n")

;; Display ANSI color test
(terminal-echo "\033[38;2;100;149;237mANSI Colors:\033[0m ")
(terminal-echo "\033[38;2;180;180;180m16:\033[0m \033[31m█\033[32m█\033[33m█\033[34m█\033[35m█\033[36m█\033[37m█\033[90m█\033[91m█\033[92m█\033[93m█\033[94m█\033[95m█\033[96m█\033[97m█\033[0m ")
(terminal-echo "\033[38;2;180;180;180m256:\033[0m \033[38;5;196m█\033[38;5;208m█\033[38;5;220m█\033[38;5;46m█\033[38;5;51m█\033[38;5;21m█\033[38;5;129m█\033[0m ")
(terminal-echo "\033[38;2;180;180;180m24-bit:\033[0m \033[38;2;255;0;0m█\033[38;2;255;255;0m█\033[38;2;0;255;0m█\033[38;2;0;255;255m█\033[38;2;0;0;255m█\033[38;2;255;0;255m█\033[0m\n")

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

;; Hook wrapper for scroll-lock notifications (matches telnet-input-hook signature)
(defun scroll-lock-notification-hook (text)
  "Telnet input hook handler for scroll-lock notification animation."
  (maybe-play-scroll-lock-notification))

;; Register hook for scroll-lock notifications
(add-hook 'telnet-input-hook 'scroll-lock-notification-hook)
