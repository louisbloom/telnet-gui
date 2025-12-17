;; TinTin++ Implementation - Test-Driven Development
;; Building incrementally, one test at a time

;; ============================================================================
;; USAGE AS USER-INPUT-HOOK
;; ============================================================================
;;
;; To use TinTin++ in telnet-gui, simply load this file:
;;
;;    ./build/telnet-gui/telnet-gui.exe -l telnet-gui/tintin.lisp <host> <port>
;;
;; TinTin++ automatically activates when loaded! You can use:
;;    - Command separation: n;s;e;w
;;    - Speedwalk: 3n2e (expands to n;n;n;e;e)
;;    - Aliases: #alias {k} {kill %1}
;;    - Variables: #variable {target} {orc}
;;    - Pattern matching: #alias {attack %1 with %2} {kill %1;wield %2}
;;    - Partial matching: #al {k} {kill %1} or #var {target} {orc}
;;    - Case-insensitive: #ALIAS, #Alias, #alias all work
;;
;; All # commands support:
;;    - Partial prefix matching (e.g., #al → #alias, #v → #variable)
;;    - Case-insensitive matching (e.g., #ALIAS, #Alias, #alias)
;;    - Error messages for unknown commands (never sent to telnet server)
;;
;; Toggle TinTin++ on/off:
;;    (tintin-toggle!)   ; Toggle on/off
;;    (tintin-enable!)   ; Enable
;;    (tintin-disable!)  ; Disable
;;
;; Example configuration file (save as my-config.lisp):
;;
;;   ;; Load TinTin++ (automatically activates)
;;   (load "tintin.lisp")
;;
;;   ;; Pre-define common directional aliases
;;   (tintin-process-command "#alias {n} {north}")
;;   (tintin-process-command "#alias {s} {south}")
;;   (tintin-process-command "#alias {e} {east}")
;;   (tintin-process-command "#alias {w} {west}")
;;   (tintin-process-command "#alias {u} {up}")
;;   (tintin-process-command "#alias {d} {down}")
;;
;;   ;; Set up combat aliases
;;   (tintin-process-command "#alias {k} {kill %1}")
;;   (tintin-process-command "#alias {ka} {kill all.%1}")
;;
;;   ;; Set up variables
;;   (tintin-process-command "#variable {target} {orc}")
;;   (tintin-process-command "#variable {weapon} {sword}")
;;
;;   ;; Print confirmation
;;   (terminal-echo "TinTin++ configuration loaded!\r\n")
;;
;; Then run: ./telnet-gui.exe -l my-config.lisp <host> <port>
;;
;; ============================================================================
;; DATA STRUCTURES
;; ============================================================================

(define *tintin-aliases* (make-hash-table))
(define *tintin-variables* (make-hash-table))
(define *tintin-highlights* (make-hash-table))
(define *tintin-actions* (make-hash-table))
(define *tintin-action-executing* #f)
(define *tintin-speedwalk-enabled* #t)
(define *tintin-speedwalk-diagonals* #f)
(define *tintin-enabled* #t)
(define *tintin-max-alias-depth* 10)

;; TinTin++ command registry with metadata
;; Each entry: (handler-fn arg-count syntax-help)
;; Registry is populated after handlers are defined (see COMMAND HANDLERS section)
(define *tintin-commands* (make-hash-table))

;; ============================================================================
;; STUB DEFINITIONS (for standalone use in lisp-repl)
;; ============================================================================

;; Define terminal-echo as printing to stdout if not already defined
(if (not (condition-case err
           (progn terminal-echo #t)
           (error #f)))
  (define terminal-echo (lambda (text) (print text) nil)))

;; Define telnet-send as printing to stderr if not already defined
(if (not (condition-case err
           (progn telnet-send #t)
           (error #f)))
  (define telnet-send (lambda (text) (eprint text) nil)))

;; ============================================================================
;; UTILITY FUNCTIONS
;; NOTE: string->number and reverse are now native built-in functions
;; ============================================================================


;; ============================================================================
;; HIGHLIGHT COLOR PARSING SYSTEM
;; ============================================================================

;; TinTin++ Color Name Mappings
;; Standard ANSI colors (30-37 for FG, 40-47 for BG)
(define *tintin-colors-fg*
  '(("black" . "30") ("red" . "31") ("green" . "32") ("yellow" . "33")
     ("blue" . "34") ("magenta" . "35") ("cyan" . "36") ("white" . "37")))

(define *tintin-colors-bg*
  '(("black" . "40") ("red" . "41") ("green" . "42") ("yellow" . "43")
     ("blue" . "44") ("magenta" . "45") ("cyan" . "46") ("white" . "47")))

;; Bright/light colors (90-97 for FG, 100-107 for BG)
(define *tintin-colors-bright-fg*
  '(("light black" . "90") ("light red" . "91") ("light green" . "92")
     ("light yellow" . "93") ("light blue" . "94") ("light magenta" . "95")
     ("light cyan" . "96") ("light white" . "97")))

(define *tintin-colors-bright-bg*
  '(("light black" . "100") ("light red" . "101") ("light green" . "102")
     ("light yellow" . "103") ("light blue" . "104") ("light magenta" . "105")
     ("light cyan" . "106") ("light white" . "107")))

;; Tertiary colors (from TinTin++ docs) - map to RGB equivalents
(define *tintin-tertiary-colors*
  '(("azure" . "acf") ("ebony" . "000") ("jade" . "afc") ("lime" . "cfa")
     ("orange" . "fc8") ("pink" . "fca") ("silver" . "ccc") ("tan" . "ca8")
     ("violet" . "fac") ("white" . "fff")))

;; Text attributes
(define *tintin-attributes*
  '(("reset" . "0") ("bold" . "1") ("dim" . "2") ("italic" . "3")
     ("underscore" . "4") ("underline" . "4") ("blink" . "5")
     ("reverse" . "7") ("strikethrough" . "9")))

;; Helper: Convert hex character to decimal (0-15)
(defun tintin-hex-to-dec (hex-char)
  (let ((ch (string-downcase hex-char)))
    (cond
      ((string=? ch "0") 0) ((string=? ch "1") 1) ((string=? ch "2") 2)
      ((string=? ch "3") 3) ((string=? ch "4") 4) ((string=? ch "5") 5)
      ((string=? ch "6") 6) ((string=? ch "7") 7) ((string=? ch "8") 8)
      ((string=? ch "9") 9) ((string=? ch "a") 10) ((string=? ch "b") 11)
      ((string=? ch "c") 12) ((string=? ch "d") 13) ((string=? ch "e") 14)
      ((string=? ch "f") 15)
      (#t 0))))

;; Helper: Expand 3-char RGB to full RGB values
;; Example: "abc" → (170 187 204)
(defun tintin-expand-rgb (rgb-str)
  (let ((len (string-length rgb-str)))
    (if (= len 3)
      ;; 3-char: each char represents 0-255 in 16 steps (multiply by 17)
      (list (* (tintin-hex-to-dec (substring rgb-str 0 1)) 17)
        (* (tintin-hex-to-dec (substring rgb-str 1 2)) 17)
        (* (tintin-hex-to-dec (substring rgb-str 2 3)) 17))
      ;; 6-char: parse as two-digit hex pairs
      (if (= len 6)
        (list (+ (* (tintin-hex-to-dec (substring rgb-str 0 1)) 16)
                (tintin-hex-to-dec (substring rgb-str 1 2)))
          (+ (* (tintin-hex-to-dec (substring rgb-str 2 3)) 16)
            (tintin-hex-to-dec (substring rgb-str 3 4)))
          (+ (* (tintin-hex-to-dec (substring rgb-str 4 5)) 16)
            (tintin-hex-to-dec (substring rgb-str 5 6))))
        ;; Invalid length - return black
        (list 0 0 0)))))

;; Helper: Convert RGB values to ANSI 24-bit color code
;; is-bg: #t for background (48;2), #f for foreground (38;2)
(defun tintin-rgb-to-ansi (r g b is-bg)
  (concat (if is-bg "48;2;" "38;2;")
    (number->string r) ";"
    (number->string g) ";"
    (number->string b)))

;; Parse RGB color code <rgb>, <Frgb>, or <Frrggbb>
;; Returns ANSI code string or nil
(defun tintin-parse-rgb-color (rgb-string is-bg)
  (if (and (> (string-length rgb-string) 2)
        (string=? (substring rgb-string 0 1) "<")
        (string=? (substring rgb-string (- (string-length rgb-string) 1)
                    (string-length rgb-string)) ">"))
    ;; Extract content between < and >
    (let ((content (substring rgb-string 1 (- (string-length rgb-string) 1)))
           (len (- (string-length rgb-string) 2)))
      (cond
        ;; <rgb> format (3 chars)
        ((= len 3)
          (let ((rgb (tintin-expand-rgb content)))
            (tintin-rgb-to-ansi (list-ref rgb 0) (list-ref rgb 1) (list-ref rgb 2) is-bg)))
        ;; <Frgb> format (4 chars) - ignore F, use last 3
        ((= len 4)
          (let ((rgb (tintin-expand-rgb (substring content 1 4))))
            (tintin-rgb-to-ansi (list-ref rgb 0) (list-ref rgb 1) (list-ref rgb 2) is-bg)))
        ;; <Frrggbb> format (7 chars) - ignore F, use last 6
        ((= len 7)
          (let ((rgb (tintin-expand-rgb (substring content 1 7))))
            (tintin-rgb-to-ansi (list-ref rgb 0) (list-ref rgb 1) (list-ref rgb 2) is-bg)))
        (#t nil)))
    nil))

;; Look up named color in association list
(defun tintin-lookup-color (name alist)
  (if (or (null? alist) (not (list? alist)))
    nil
    (let ((pair (assoc name alist)))
      (if pair (cdr pair) nil))))

;; Strip attribute keywords from text (bold, dim, italic, etc.)
;; Returns text with attribute keywords removed
(defun tintin-strip-attributes (text)
  (let* ((text-lower (string-downcase text))
          (result text-lower))
    ;; Remove each attribute keyword
    (do ((i 0 (+ i 1)))
      ((>= i (list-length *tintin-attributes*)) result)
      (let ((keyword (car (list-ref *tintin-attributes* i))))
        (set! result (string-replace result keyword ""))))
    ;; Trim whitespace
    (tintin-trim result)))

;; Parse named color (e.g., "red", "light blue")
;; Returns ANSI code string or nil
(defun tintin-parse-named-color (name is-bg)
  (let ((name-lower (string-downcase (tintin-trim name))))
    ;; Try tertiary colors first (convert to RGB)
    (let ((tertiary (tintin-lookup-color name-lower *tintin-tertiary-colors*)))
      (if tertiary
        (tintin-parse-rgb-color (concat "<" tertiary ">") is-bg)
        ;; Try light/bright colors
        (let ((bright (tintin-lookup-color name-lower
                        (if is-bg *tintin-colors-bright-bg* *tintin-colors-bright-fg*))))
          (if bright
            bright
            ;; Try standard colors
            (tintin-lookup-color name-lower
              (if is-bg *tintin-colors-bg* *tintin-colors-fg*))))))))

;; Parse attributes from text (bold, underscore, etc.)
;; Returns list of ANSI attribute codes
(defun tintin-parse-attributes (text)
  (let ((text-lower (string-downcase text))
         (attrs '()))
    ;; Check each attribute keyword
    (do ((i 0 (+ i 1)))
      ((>= i (list-length *tintin-attributes*)) attrs)
      (let* ((pair (list-ref *tintin-attributes* i))
              (keyword (car pair))
              (code (cdr pair)))
        (if (string-contains? text-lower keyword)
          (set! attrs (cons code attrs)))))))

;; Helper: Find first occurrence of character in string
;; Returns position or nil if not found
;; ch should be a character (e.g., #\:)
(defun tintin-string-find-char (str ch)
  (let ((len (string-length str))
         (pos 0)
         (found nil))
    (do ()
      ((or (>= pos len) found) found)
      (if (char=? (string-ref str pos) ch)
        (set! found pos)
        (set! pos (+ pos 1))))))

;; Split color spec on colon (FG:BG separator)
;; Returns (fg-part bg-part) or (fg-part nil)
(defun tintin-split-fg-bg (spec)
  (let ((colon-pos (tintin-string-find-char spec #\:)))
    (if colon-pos
      (list (substring spec 0 colon-pos)
        (substring spec (+ colon-pos 1) (string-length spec)))
      (list spec nil))))

;; Parse single color component (foreground or background)
;; Returns ANSI code string (may include attributes)
(defun tintin-parse-color-component (text is-bg)
  (if (or (not text) (string=? text ""))
    nil
    (let ((text-trimmed (tintin-trim text))
           (codes '()))
      ;; Extract attributes first
      (let ((attr-codes (tintin-parse-attributes text-trimmed)))
        (set! codes attr-codes))

      ;; Try RGB color format
      (let ((start-bracket (string-index text-trimmed "<")))
        (if start-bracket
          (let ((end-bracket (string-index text-trimmed ">")))
            (if end-bracket
              (let ((rgb-str (substring text-trimmed start-bracket (+ end-bracket 1))))
                (let ((rgb-code (tintin-parse-rgb-color rgb-str is-bg)))
                  (if rgb-code
                    (set! codes (cons rgb-code codes)))))))))

      ;; If no RGB found, try named colors
      (if (and (not (string-contains? text-trimmed "<"))
            (or (string-contains? text-trimmed "black")
              (string-contains? text-trimmed "red")
              (string-contains? text-trimmed "green")
              (string-contains? text-trimmed "yellow")
              (string-contains? text-trimmed "blue")
              (string-contains? text-trimmed "magenta")
              (string-contains? text-trimmed "cyan")
              (string-contains? text-trimmed "white")
              (string-contains? text-trimmed "azure")
              (string-contains? text-trimmed "jade")
              (string-contains? text-trimmed "violet")
              (string-contains? text-trimmed "lime")
              (string-contains? text-trimmed "pink")
              (string-contains? text-trimmed "orange")))
        (let ((color-only (tintin-strip-attributes text-trimmed)))
          (let ((named-code (tintin-parse-named-color color-only is-bg)))
            (if named-code
              (set! codes (cons named-code codes))))))

      ;; Combine codes with semicolons
      (if (eq? codes '())
        nil
        (let ((result "")
               (first #t))
          (do ((remaining (reverse codes) (cdr remaining)))
            ((null? remaining) result)
            (if first
              (set! first #f)
              (set! result (concat result ";")))
            (set! result (concat result (car remaining)))))))))

;; Build ANSI escape sequence from fg and bg codes
;; Returns complete \033[...m sequence
(defun tintin-build-ansi-code (fg-codes bg-codes)
  "Build complete ANSI SGR escape sequence from color codes.

  ## Parameters
  - `fg-codes` - Foreground ANSI codes (string or `nil`), e.g., `\"1;31\"`
  - `bg-codes` - Background ANSI codes (string or `nil`), e.g., `\"44\"`

  ## Returns
  Complete ANSI escape sequence: `\\033[codes...m`. Returns empty string `\"\"`
  if both fg-codes and bg-codes are `nil`.

  ## Description
  Combines foreground and background ANSI code strings into a complete terminal
  escape sequence. Codes are joined with semicolons and wrapped in the standard
  ANSI SGR format: ESC `[` codes `m`.

  **Format**: `\\033[fg-codes;bg-codes m`

  ## Examples
  ```lisp
  ; Foreground only
  (tintin-build-ansi-code \"31\" nil)
  ; => \"\\033[31m\"  ; Red text

  ; Background only
  (tintin-build-ansi-code nil \"44\")
  ; => \"\\033[44m\"  ; Blue background

  ; Both foreground and background
  (tintin-build-ansi-code \"31\" \"44\")
  ; => \"\\033[31;44m\"  ; Red on blue

  ; Multiple codes (bold + red foreground)
  (tintin-build-ansi-code \"1;31\" nil)
  ; => \"\\033[1;31m\"  ; Bold red text

  ; Complex combination
  (tintin-build-ansi-code \"1;3;96\" \"40\")
  ; => \"\\033[1;3;96;40m\"  ; Bold italic light cyan on black

  ; Both nil
  (tintin-build-ansi-code nil nil)
  ; => \"\"  ; Empty string
  ```

  ## Notes
  - Input codes should NOT include ESC `[` prefix or `m` suffix
  - Foreground and background can be independently `nil`
  - Codes combined with semicolon separator
  - Used by highlight system to wrap matched text
  - Complements `tintin-parse-color-spec` which generates the codes

  ## ANSI Escape Sequence Format
  - **Structure**: ESC `[` parameters `m`
  - **ESC**: Escape character (`\\033` or `\\x1b`)
  - **Parameters**: Semicolon-separated numbers
  - **Terminator**: `m` (SGR - Select Graphic Rendition)

  ## See Also
  - `tintin-parse-color-spec` - Parse color specs into codes
  - `tintin-wrap-match` - Uses this to highlight text
  - `tintin-apply-highlights` - Main highlight application"
  (let ((codes '()))
    (if fg-codes
      (set! codes (cons fg-codes codes)))
    (if bg-codes
      (set! codes (cons bg-codes codes)))
    (if (eq? codes '())
      ""
      (let ((combined "")
             (first #t))
        (do ((remaining (reverse codes) (cdr remaining)))
          ((null? remaining))
          (if first
            (set! first #f)
            (set! combined (concat combined ";")))
          (set! combined (concat combined (car remaining))))
        (concat "\033[" combined "m")))))

;; Main color spec parser
;; Parses TinTin++ color specification and returns ANSI escape code
;; Examples:
;;   "red" → "\033[31m"
;;   "<fff>" → "\033[38;2;255;255;255m"
;;   "bold red:blue" → "\033[1;31;44m"
;;   "light red" → "\033[91m"
(defun tintin-parse-color-spec (spec)
  "Parse TinTin++ color specification into ANSI escape codes.

  ## Parameters
  - `spec` - Color specification string (TinTin++ format)

  ## Returns
  List of two ANSI code strings: `(fg-codes bg-codes)`. Returns `(nil nil)`
  if spec is empty/invalid. Either element can be `nil` if not specified.

  ## Description
  Main entry point for color parsing. Converts TinTin++ color specifications
  into ANSI SGR (Select Graphic Rendition) escape codes for terminal display.
  Supports named colors, RGB colors, text attributes, and foreground:background
  combinations.

  **Color Specification Formats:**

  - **Named colors**: `\"red\"`, `\"blue\"`, `\"light cyan\"`
  - **RGB hex (3-char)**: `\"<fff>\"` = white, `\"<f00>\"` = red
  - **RGB hex (4-char)**: `\"<Ffff>\"` (F ignored, uses last 3)
  - **RGB hex (7-char)**: `\"<Fff0000>\"` (F ignored, uses last 6)
  - **Attributes**: `\"bold\"`, `\"italic\"`, `\"underscore\"`, `\"blink\"`
  - **Combined**: `\"bold red\"`, `\"italic light blue\"`
  - **Foreground:Background**: `\"red:blue\"`, `\"<fff>:<000>\"`

  **Named Colors:**
  - Standard: black, red, green, yellow, blue, magenta, cyan, white
  - Bright: light black, light red, light green, etc.
  - Tertiary: azure, ebony, jade, lime, orange, pink, silver, tan, violet

  **Attributes:**
  - bold (1), dim (2), italic (3), underscore/underline (4)
  - blink (5), reverse (7), strikethrough (9), reset (0)

  ## Examples
  ```lisp
  ; Simple named color (foreground only)
  (tintin-parse-color-spec \"red\")
  ; => (\"31\" nil)  ; ANSI code 31 = red foreground

  ; Named color with background
  (tintin-parse-color-spec \"red:blue\")
  ; => (\"31\" \"44\")  ; FG=31 (red), BG=44 (blue)

  ; RGB color (24-bit)
  (tintin-parse-color-spec \"<fff>\")
  ; => (\"38;2;255;255;255\" nil)  ; 24-bit white

  ; Attribute + color
  (tintin-parse-color-spec \"bold red\")
  ; => (\"1;31\" nil)  ; Bold + red

  ; Complex combination
  (tintin-parse-color-spec \"bold italic light cyan:black\")
  ; => (\"1;3;96\" \"40\")  ; Multiple attributes + colors

  ; Empty/invalid
  (tintin-parse-color-spec \"\")
  ; => (nil nil)
  (tintin-parse-color-spec nil)
  ; => (nil nil)
  ```

  ## Notes
  - Returns list of code strings (NOT full escape sequences)
  - Use `tintin-build-ansi-code` to build complete `\\033[...m` sequences
  - Foreground and background parsed independently
  - Attributes extracted from both foreground and background parts
  - RGB colors override named colors if both present
  - Case-insensitive color names

  ## ANSI Code Reference
  - **Foreground**: 30-37 (standard), 90-97 (bright), 38;2;R;G;B (24-bit)
  - **Background**: 40-47 (standard), 100-107 (bright), 48;2;R;G;B (24-bit)
  - **Attributes**: 0 (reset), 1 (bold), 2 (dim), 3 (italic), 4 (underline),
    5 (blink), 7 (reverse), 9 (strikethrough)

  ## See Also
  - `tintin-parse-color-component` - Parse single color component
  - `tintin-build-ansi-code` - Build complete ANSI escape sequence
  - `tintin-split-fg-bg` - Split foreground:background
  - `#highlight` command - Uses this for color parsing"
  (if (or (not spec) (string=? spec ""))
    (list nil nil)
    (let ((parts (tintin-split-fg-bg spec)))
      (let ((fg-part (list-ref parts 0))
             (bg-part (list-ref parts 1)))
        (list (tintin-parse-color-component fg-part #f)
          (if bg-part (tintin-parse-color-component bg-part #t) nil))))))

;; ============================================================================
;; HIGHLIGHT PATTERN MATCHING SYSTEM
;; ============================================================================

;; Check if character needs regex escaping
(defun tintin-regex-special-char? (ch)
  (or (char=? ch #\.)
    (char=? ch #\*)
    (char=? ch #\+)
    (char=? ch #\?)
    (char=? ch #\[)
    (char=? ch #\])
    (char=? ch #\{)
    (char=? ch #\})
    (char=? ch #\()
    (char=? ch #\))
    (char=? ch #\|)
    (char=? ch #\\)
    (char=? ch #\^)
    (char=? ch #\$)))

;; Convert TinTin++ pattern to PCRE2 regex
;; Pattern translation:
;;   %* or %1-%99 → (.*?) (non-greedy capture)
;;   ^ at start → ^ (line anchor)
;;   Other chars → escaped for regex
;; Examples:
;;   "You hit %*" → "You hit (.*?)"
;;   "^Health: %1" → "^Health: (.*?)"
;;   "Valgar" → "Valgar"
(defun tintin-pattern-to-regex (pattern)
  "Convert TinTin++ pattern to PCRE2 regular expression.

  ## Parameters
  - `pattern` - TinTin++ pattern string with wildcards

  ## Returns
  PCRE2-compatible regular expression string. Returns empty string `\"\"` if
  pattern is invalid.

  ## Description
  Translates TinTin++ wildcard syntax into standard PCRE2 regex patterns.
  Automatically escapes regex special characters and converts wildcards into
  non-greedy capture groups for pattern matching and data extraction.

  **Pattern Translation Rules:**

  - **`%*`** → `(.*?)` - Match anything (non-greedy capture)
  - **`%1-%99`** → `(.*?)` - Numbered wildcards (capture group)
  - **`^` at start** → `^` - Line anchor (start of line)
  - **Regex special chars** → Escaped (`.` → `\\.`, `*` → `\\*`, etc.)
  - **Other characters** → Literal match

  **Wildcard Semantics:**
  - `%*` and `%1`-`%99` are functionally identical (all become `(.*?)`)
  - Numbers serve as capture group identifiers for reference
  - Non-greedy matching prevents over-capture
  - Capture groups extractable via `tintin-extract-captures`

  ## Examples
  ```lisp
  ; Simple wildcard
  (tintin-pattern-to-regex \"You hit %*\")
  ; => \"You hit (.*?)\"

  ; Numbered wildcards
  (tintin-pattern-to-regex \"%1 attacks %2\")
  ; => \"(.*?) attacks (.*?)\"

  ; Line anchor
  (tintin-pattern-to-regex \"^Health: %1\")
  ; => \"^Health: (.*?)\"

  ; Literal text (no wildcards)
  (tintin-pattern-to-regex \"Valgar\")
  ; => \"Valgar\"

  ; Escaped special characters
  (tintin-pattern-to-regex \"Cost: $%1\")
  ; => \"Cost: \\\\$(.*?)\"

  ; Complex pattern
  (tintin-pattern-to-regex \"^[%1] %2 says: %*\")
  ; => \"^\\\\[(.*?)\\\\] (.*?) says: (.*?)\"

  ; Invalid input
  (tintin-pattern-to-regex nil)
  ; => \"\"
  ```

  ## Notes
  - Non-greedy capture: `(.*?)` stops at first match
  - Regex special chars: `. * + ? [ ] { } ( ) | \\\\ ^ $`
  - Line anchor `^` only special at pattern start
  - Used by highlights, actions, and pattern matching
  - PCRE2 features available (character classes, lookahead, etc.)

  ## Common Patterns
  - **Any text**: `%*` or `%1`
  - **Specific then any**: `\"Valgar %*\"`
  - **Line start**: `\"^Health: %1\"`
  - **Multiple captures**: `\"%1 gives %2 to %3\"`

  ## See Also
  - `tintin-match-highlight-pattern` - Test pattern match
  - `tintin-extract-captures` - Extract wildcard values
  - `#highlight` command - Uses patterns for highlighting
  - `#action` command - Uses patterns for triggers"
  (if (not (string? pattern))
    ""
    (let ((len (string-length pattern))
           (pos 0)
           (result ""))
      (do ()
        ((>= pos len) result)
        (let ((ch (string-ref pattern pos)))
          (cond
            ;; Handle % placeholders
            ((char=? ch #\%)
              (if (< (+ pos 1) len)
                (let ((next-ch (string-ref pattern (+ pos 1))))
                  (if (char=? next-ch #\*)
                    ;; %* → (.*?)
                    (progn
                      (set! result (concat result "(.*?)"))
                      (set! pos (+ pos 2)))
                    ;; Check if it's %1-%99
                    (if (and (char>=? next-ch #\0) (char<=? next-ch #\9))
                      (let ((digit-end (+ pos 2)))
                        ;; Consume second digit if present
                        (if (and (< digit-end len)
                              (char>=? (string-ref pattern digit-end) #\0)
                              (char<=? (string-ref pattern digit-end) #\9))
                          (set! digit-end (+ digit-end 1)))
                        ;; %N or %NN → (.*?)
                        (set! result (concat result "(.*?)"))
                        (set! pos digit-end))
                      ;; Not %* or %N - literal %
                      (progn
                        (set! result (concat result "\\%"))
                        (set! pos (+ pos 1))))))
                ;; % at end of string - literal
                (progn
                  (set! result (concat result "\\%"))
                  (set! pos (+ pos 1)))))

            ;; Handle ^ at start (line anchor)
            ((and (char=? ch #\^) (= pos 0))
              (set! result (concat result "^"))
              (set! pos (+ pos 1)))

            ;; Escape regex special characters
            ((tintin-regex-special-char? ch)
              (set! result (concat result "\\" (char->string ch)))
              (set! pos (+ pos 1)))

            ;; Regular character - no escaping needed
            (#t
              (set! result (concat result (char->string ch)))
              (set! pos (+ pos 1)))))))))

;; Test if TinTin++ pattern matches text using regex
;; Returns #t if match found, #f otherwise
(defun tintin-match-highlight-pattern (pattern text)
  "Test if TinTin++ pattern matches text using regex.

  ## Parameters
  - `pattern` - TinTin++ pattern string (with wildcards %*, %1-%99)
  - `text` - Text to test against pattern

  ## Returns
  `#t` if pattern matches text, `#f` otherwise. Returns `#f` if either
  parameter is invalid.

  ## Description
  Tests whether a TinTin++ pattern matches the given text. Converts pattern
  to PCRE2 regex via `tintin-pattern-to-regex` and performs regex matching.
  Used by highlight and action systems to determine if patterns should trigger.

  **Matching Behavior:**
  - Wildcards (`%*`, `%1`-`%99`) match any text
  - Literal text must match exactly (case-sensitive)
  - Line anchor (`^`) requires match at start of text
  - Returns boolean (does NOT extract capture values)

  ## Examples
  ```lisp
  ; Simple wildcard match
  (tintin-match-highlight-pattern \"You hit %*\" \"You hit the orc\")
  ; => #t

  ; No match (wrong text)
  (tintin-match-highlight-pattern \"You hit %*\" \"You miss the orc\")
  ; => #f

  ; Multiple wildcards
  (tintin-match-highlight-pattern \"%1 attacks %2\" \"Valgar attacks Bob\")
  ; => #t

  ; Line anchor (start of line)
  (tintin-match-highlight-pattern \"^Health: %1\" \"Health: 100\")
  ; => #t

  ; Line anchor fails (not at start)
  (tintin-match-highlight-pattern \"^Health: %1\" \"Your Health: 100\")
  ; => #f

  ; Literal text match
  (tintin-match-highlight-pattern \"Valgar\" \"Valgar enters the room\")
  ; => #t

  ; Case-sensitive
  (tintin-match-highlight-pattern \"valgar\" \"Valgar enters\")
  ; => #f  ; Case mismatch

  ; Invalid input
  (tintin-match-highlight-pattern nil \"text\")
  ; => #f
  (tintin-match-highlight-pattern \"pattern\" nil)
  ; => #f
  ```

  ## Notes
  - Uses PCRE2 regex for matching (not simple string search)
  - Case-sensitive by default (TinTin++ standard behavior)
  - Wildcards match greedily within constraints
  - For capture extraction, use `tintin-extract-captures` instead
  - Used internally by highlight and action systems

  ## Common Use Cases
  - **Highlights**: Test if line should be colored
  - **Actions**: Test if line should trigger command execution
  - **Pattern validation**: Check if pattern matches expected format

  ## See Also
  - `tintin-pattern-to-regex` - Convert pattern to regex
  - `tintin-extract-captures` - Extract wildcard values
  - `tintin-highlight-line` - Apply highlights using pattern matching
  - `tintin-trigger-actions-for-line` - Execute actions using pattern matching"
  (if (or (not (string? pattern)) (not (string? text)))
    #f
    (let ((regex-pattern (tintin-pattern-to-regex pattern)))
      (if (string=? regex-pattern "")
        #f
        ;; Use regex-match? to test if pattern matches
        (let ((match-result (regex-match? regex-pattern text)))
          (if match-result #t #f))))))

;; Sort highlight entries by priority (descending - higher priority first)
;; Input: list of (pattern . (fg bg priority)) pairs
;; Output: sorted list by priority (highest first)
(defun tintin-sort-highlights-by-priority (highlight-list)
  "Sort highlight entries by priority (descending order - higher priority first).

  ## Parameters
  - `highlight-list` - List of highlight entries: `((pattern . (fg bg priority)) ...)`

  ## Returns
  Sorted list with highest priority entries first. Returns empty list `()` if
  input is empty or invalid.

  ## Description
  Sorts highlights by priority in descending order (highest priority processed
  first). This determines the order in which highlight patterns are applied to
  incoming text. Higher priority highlights are applied before lower priority
  ones.

  **Sorting Rules:**
  - Primary: Priority (descending: 10, 9, 8, ... 5 (default), ... 1)
  - Tiebreaker: Pattern length (longer patterns first)
  - Algorithm: Insertion sort (O(n²) but stable and simple)

  **Priority Semantics:**
  - Higher number = processed first
  - Default priority: 5
  - Range: Typically 1-10, but any integer allowed

  ## Examples
  ```lisp
  (define highlights
    (list (cons \"dragon\" (list \"red\" nil 10))
          (cons \"orc\" (list \"green\" nil 5))
          (cons \"gold\" (list \"yellow\" nil 8))))

  (tintin-sort-highlights-by-priority highlights)
  ; => ((\"dragon\" . (\"red\" nil 10))   ; Priority 10 first
  ;     (\"gold\" . (\"yellow\" nil 8))   ; Priority 8 second
  ;     (\"orc\" . (\"green\" nil 5)))    ; Priority 5 (default) last

  ; Empty input
  (tintin-sort-highlights-by-priority '())
  ; => ()
  ```

  ## Notes
  - Used internally by `tintin-highlight-line` for pattern matching order
  - Higher priority = applied first (allows overriding lower priority highlights)
  - Tiebreaker ensures longer patterns match before shorter ones (specificity)
  - Stable sort: preserves relative order of equal-priority entries

  ## See Also
  - `tintin-highlight-line` - Applies highlights in priority order
  - `tintin-apply-highlights` - Main highlight application entry point
  - `tintin-sort-actions-by-priority` - Opposite order (ascending) for actions"
  (if (or (null? highlight-list) (= (list-length highlight-list) 0))
    '()
    ;; Simple insertion sort by priority
    (let ((sorted '()))
      (do ((remaining highlight-list (cdr remaining)))
        ((null? remaining) sorted)
        (let* ((entry (car remaining))
                (priority (car (cdr (cdr (cdr entry))))))
          ;; Insert entry in sorted position
          (set! sorted (tintin-insert-by-priority entry priority sorted)))))))

;; Helper: Insert entry into sorted list by priority, then by pattern length
(defun tintin-insert-by-priority (entry priority sorted-list)
  (if (null? sorted-list)
    (list entry)
    (let ((first-entry (car sorted-list))
           (first-priority (car (cdr (cdr (cdr (car sorted-list)))))))
      (if (> priority first-priority)
        ;; Higher priority - insert at head
        (cons entry sorted-list)
        (if (= priority first-priority)
          ;; Same priority - use pattern length as tiebreaker (longer first)
          (let ((entry-pattern (car entry))
                 (first-pattern (car first-entry)))
            (if (>= (string-length entry-pattern) (string-length first-pattern))
              (cons entry sorted-list)
              (cons first-entry
                (tintin-insert-by-priority entry priority (cdr sorted-list)))))
          ;; Lower priority - insert later
          (cons first-entry
            (tintin-insert-by-priority entry priority (cdr sorted-list))))))))

;; ============================================================================
;; ACTION PRIORITY SORTING
;; ============================================================================

;; Sort action entries by priority (ascending - lower priority first)
;; Input: list of (pattern . (commands-string priority)) pairs
;; Output: sorted list by priority (lowest first)
(defun tintin-sort-actions-by-priority (action-list)
  "Sort action entries by priority (ascending order - lower priority first).

  ## Parameters
  - `action-list` - List of action entries: `((pattern . (commands priority)) ...)`

  ## Returns
  Sorted list with lowest priority entries first. Returns empty list `()` if
  input is empty or invalid.

  ## Description
  Sorts actions by priority in ascending order (lowest priority processed first).
  This determines the order in which action patterns are tested against incoming
  server output. Lower priority actions execute before higher priority ones.

  **Sorting Rules:**
  - Primary: Priority (ascending: 1, 2, 3, ... 5 (default), ... 10)
  - Tiebreaker: Pattern length (longer patterns first)
  - Algorithm: Insertion sort (O(n²) but stable and simple)

  **Priority Semantics (opposite of highlights):**
  - Lower number = processed first
  - Default priority: 5
  - Range: Typically 1-10, but any integer allowed
  - Use low priority (1-2) for high-importance triggers

  ## Examples
  ```lisp
  (define actions
    (list (cons \"danger\" (list \"flee\" 1))
          (cons \"%1 attacks\" (list \"kill %1\" 5))
          (cons \"You are hungry\" (list \"eat bread\" 8))))

  (tintin-sort-actions-by-priority actions)
  ; => ((\"danger\" . (\"flee\" 1))           ; Priority 1 first (urgent)
  ;     (\"%1 attacks\" . (\"kill %1\" 5))   ; Priority 5 (default) second
  ;     (\"You are hungry\" . (\"eat bread\" 8)))  ; Priority 8 last (low urgency)

  ; Empty input
  (tintin-sort-actions-by-priority '())
  ; => ()
  ```

  ## Notes
  - Used internally by `tintin-trigger-actions-for-line` for execution order
  - **Opposite of highlights**: Lower priority = executes first (not last)
  - Tiebreaker ensures longer patterns match before shorter ones (specificity)
  - Stable sort: preserves relative order of equal-priority entries
  - Common pattern: Use priority 1-2 for emergency actions (flee, cure)

  ## See Also
  - `tintin-trigger-actions-for-line` - Executes actions in priority order
  - `tintin-sort-highlights-by-priority` - Opposite order (descending) for highlights
  - `tintin-handle-action` - Defines actions with optional priority"
  (if (or (null? action-list) (= (list-length action-list) 0))
    '()
    ;; Simple insertion sort by priority
    (let ((sorted '()))
      (do ((remaining action-list (cdr remaining)))
        ((null? remaining) sorted)
        (let* ((entry (car remaining))
                (priority (car (cdr (cdr entry)))))
          ;; Insert entry in sorted position
          (set! sorted (tintin-insert-action-by-priority entry priority sorted)))))))

;; Helper: Insert action entry into sorted list by priority, then by pattern length
(defun tintin-insert-action-by-priority (entry priority sorted-list)
  (if (null? sorted-list)
    (list entry)
    (let ((first-entry (car sorted-list))
           (first-priority (car (cdr (cdr (car sorted-list))))))
      (if (< priority first-priority)
        ;; Lower priority - insert at head (actions use ascending order)
        (cons entry sorted-list)
        (if (= priority first-priority)
          ;; Same priority - use pattern length as tiebreaker (longer first)
          (let ((entry-pattern (car entry))
                 (first-pattern (car first-entry)))
            (if (>= (string-length entry-pattern) (string-length first-pattern))
              (cons entry sorted-list)
              (cons first-entry
                (tintin-insert-action-by-priority entry priority (cdr sorted-list)))))
          ;; Higher priority - insert later
          (cons first-entry
            (tintin-insert-action-by-priority entry priority (cdr sorted-list))))))))

;; ============================================================================
;; HIGHLIGHT APPLICATION
;; ============================================================================

;; Split text into lines, preserving line endings
;; Returns list of lines with their line endings intact
(defun tintin-split-lines (text)
  (if (not (string? text))
    '()
    (let ((len (string-length text))
           (pos 0)
           (line-start 0)
           (lines '()))
      (do ()
        ((>= pos len)
          ;; Add final line if any
          (if (< line-start len)
            (reverse (cons (substring text line-start len) lines))
            (reverse lines)))
        (let ((ch (string-ref text pos)))
          (if (char=? ch #\newline)
            ;; Found line ending - add line including \n
            (progn
              (set! lines (cons (substring text line-start (+ pos 1)) lines))
              (set! pos (+ pos 1))
              (set! line-start pos))
            ;; Regular character - continue
            (set! pos (+ pos 1))))))))

;; ============================================================================
;; ANSI STATE TRACKING (for nested/overlapping highlights)
;; ============================================================================

;; Extract the most recent (closest) ANSI escape sequence before a position
;; This represents the "active formatting state" at that position
;; Returns the ANSI sequence string or "" if none found or if reset encountered
(defun tintin-find-active-ansi-before (text pos)
  "Extract the active ANSI formatting state before a text position.

  ## Parameters
  - `text` - Text containing ANSI escape sequences
  - `pos` - Character position to scan before

  ## Returns
  Most recent ANSI SGR escape sequence before `pos` (e.g., `\\\"\\\\033[1;31m\\\"`).
  Returns empty string `\\\"\\\"` if:
  - No ANSI sequence found before position
  - Reset code (`\\\\033[0m` or `\\\\033[m`) encountered
  - Invalid input (pos ≤ 0 or text not a string)

  ## Description
  Scans backwards from position to find the most recent ANSI escape sequence,
  representing the \"active formatting state\" at that position. Used by
  highlight wrapping to restore formatting after inserting colored text.

  **Scanning Algorithm:**
  1. Start at position - 1, scan backwards
  2. Look for ESC `[` sequence start (`\\\\033[`)
  3. Extract complete sequence (scan forward to `m` terminator)
  4. Check sequence type:
     - Reset (`\\\\033[0m` or `\\\\033[m`): Return `\\\"\\\"` (no active state)
     - Other: Return sequence (active formatting)
  5. Stop at first valid sequence found (closest to position)

  **Why This Matters:**
  When inserting highlights into pre-formatted text, we need to know what
  formatting is active at the insertion point so we can restore it after
  the highlight ends. Without this, highlights break existing formatting.

  ## Examples
  ```lisp
  ; Find bold formatting
  (tintin-find-active-ansi-before \"\\\\033[1mBold text here\" 15)
  ; => \"\\\\033[1m\"  (bold active at position 15)

  ; After reset code
  (tintin-find-active-ansi-before \"\\\\033[1mBold\\\\033[0m normal\" 20)
  ; => \"\"  (reset clears all formatting)

  ; No formatting
  (tintin-find-active-ansi-before \"Plain text\" 5)
  ; => \"\"  (no ANSI codes before position)

  ; Multiple codes - returns closest
  (tintin-find-active-ansi-before \"\\\\033[1m\\\\033[31mBold red\" 20)
  ; => \"\\\\033[31m\"  (red is most recent, bold overridden)

  ; Invalid position
  (tintin-find-active-ansi-before \"text\" 0)
  ; => \"\"
  ```

  ## Notes
  - Scans backwards to find MOST RECENT (closest) sequence
  - Stops at first complete sequence found (doesn't scan entire text)
  - Reset codes (`\\\\033[0m`) clear all formatting → return `\\\"\\\"`
  - Used by `tintin-wrap-match` to preserve nested highlights
  - Only recognizes SGR sequences (ESC `[` ... `m`), ignores other ANSI codes

  ## ANSI Sequence Format
  - **Structure**: ESC `[` parameters `m`
  - **ESC**: Escape character (`\\\\033` or `\\\\x1b`)
  - **Parameters**: Semicolon-separated codes (e.g., `1;31` = bold red)
  - **Terminator**: `m` (SGR - Select Graphic Rendition)

  ## See Also
  - `tintin-wrap-match` - Uses this to restore formatting after highlights
  - `tintin-check-after-match` - Checks what follows matched text
  - `tintin-build-ansi-code` - Builds ANSI sequences from color specs"
  (if (or (not (string? text)) (<= pos 0))
    ""
    (let ((scan-pos (- pos 1))
           (found-ansi ""))
      ;; Scan backwards looking for the FIRST (most recent) ANSI sequence
      (do ()
        ((or (< scan-pos 0) (not (string=? found-ansi ""))) found-ansi)
        (if (and (>= scan-pos 0)
              (string=? (substring text scan-pos (+ scan-pos 1)) "\033")
              (< (+ scan-pos 1) (string-length text))
              (string=? (substring text (+ scan-pos 1) (+ scan-pos 2)) "["))
          ;; Found ESC[ - extract the complete sequence
          (let ((seq-end (+ scan-pos 2)))
            ;; Find the 'm' terminator
            (do ()
              ((or (>= seq-end (string-length text))
                 (string=? (substring text seq-end (+ seq-end 1)) "m")))
              (set! seq-end (+ seq-end 1)))
            ;; Check if we found a complete sequence
            (if (and (< seq-end (string-length text))
                  (string=? (substring text seq-end (+ seq-end 1)) "m"))
              (let ((sequence (substring text scan-pos (+ seq-end 1))))
                ;; Check if this is a reset code (ESC[0m or ESC[m)
                (if (or (string=? sequence "\033[0m")
                      (string=? sequence "\033[m"))
                  ;; Reset code - return empty (no active formatting)
                  (set! found-ansi "reset")  ; Special marker to exit and return ""
                  ;; Non-reset code - this is the active state
                  (set! found-ansi sequence))
                ;; Don't continue scanning - we found what we need
                (set! scan-pos -1))
              (set! scan-pos (- scan-pos 1))))
          ;; Not an ANSI sequence, continue backwards
          (set! scan-pos (- scan-pos 1))))
      ;; Return empty string if we found a reset, otherwise return the sequence
      (if (string=? found-ansi "reset") "" found-ansi))))

;; Find the position where matched text starts in the line
;; Returns position or -1 if not found
(defun tintin-find-match-position (line matched-text)
  (if (or (not (string? line)) (not (string? matched-text)))
    -1
    (let ((pos (string-index line matched-text)))
      (if pos pos -1))))

;; Check what comes immediately after a position:
;; Returns: 'reset if reset code found, 'ansi if non-reset ANSI found, 'text if regular text
(defun tintin-check-after-match (text pos)
  (if (or (not (string? text)) (>= pos (string-length text)))
    'text
    (let ((len (string-length text))
           (scan-pos pos))
      ;; Check if there's an ANSI code immediately after
      (if (and (< (+ scan-pos 1) len)
            (string=? (substring text scan-pos (+ scan-pos 1)) "\033")
            (< (+ scan-pos 1) len)
            (string=? (substring text (+ scan-pos 1) (+ scan-pos 2)) "["))
        ;; Found ESC[ - check what kind
        (let ((seq-end (+ scan-pos 2)))
          ;; Find the 'm' terminator
          (do ()
            ((or (>= seq-end len)
               (string=? (substring text seq-end (+ seq-end 1)) "m")))
            (set! seq-end (+ seq-end 1)))
          ;; Check if complete sequence
          (if (and (< seq-end len)
                (string=? (substring text seq-end (+ seq-end 1)) "m"))
            (let ((sequence (substring text scan-pos (+ seq-end 1))))
              (if (or (string=? sequence "\033[0m")
                    (string=? sequence "\033[m"))
                'reset  ; Reset code follows
                'ansi)) ; Non-reset ANSI code follows
            'text)) ; Incomplete sequence, treat as text
        ;; No ANSI code immediately after
        'text))))

;; Wrap matched pattern in line with ANSI color codes
;; Returns line with highlight applied or original line if no match
;; Now with ANSI state tracking: restores previous state unless reset follows
(defun tintin-wrap-match (line pattern fg-color bg-color)
  "Wrap matched text in line with ANSI color codes (with state tracking).

  ## Parameters
  - `line` - Line of text to process (may contain existing ANSI codes)
  - `pattern` - TinTin++ pattern to match (supports %* wildcards)
  - `fg-color` - Foreground color specification or `nil`
  - `bg-color` - Background color specification or `nil`

  ## Returns
  Line with matched text wrapped in ANSI escape codes. Returns original line
  unchanged if no match found or pattern/color invalid.

  ## Description
  Core highlight wrapping function that applies color formatting to matched
  text while preserving nested ANSI codes. Uses ANSI state tracking to
  correctly restore formatting after the highlighted section, enabling
  nested/overlapping highlights without breaking existing formatting.

  **ANSI State Tracking Algorithm:**

  1. **Find match**: Convert TinTin++ pattern to regex, locate matched text
  2. **Scan before**: Extract active ANSI formatting state before match
  3. **Scan after**: Determine what follows match (text, ANSI code, or reset)
  4. **Wrap intelligently**:
     - Open: Insert highlight ANSI codes before match
     - Close: Choose appropriate closing based on context:
       - If reset follows: Use `\\033[0m` (clean break)
       - If ANSI follows or text: Restore previous state (preserve nesting)

  **Why State Tracking Matters:**

  Without tracking, nested highlights break:
  ```
  Original:  \"\\033[1mBold \\033[31mred\\033[0m text\"
  Bad wrap:  \"\\033[1mBold \\033[33m\\033[31mred\\033[0m\\033[0m text\"
                                                      ^^^^^ Kills bold!

  Good wrap: \"\\033[1mBold \\033[33m\\033[31mred\\033[0m\\033[1m text\"
                                                      ^^^^^ Restores bold
  ```

  ## Examples
  ```lisp
  ; Simple highlight
  (tintin-wrap-match \"The dragon appears\" \"dragon\" \"red\" nil)
  ; => \"The \\\\033[31mdragon\\\\033[0m appears\"

  ; With existing formatting (bold text)
  (tintin-wrap-match \"\\\\033[1mBold dragon text\\\\033[0m\" \"dragon\" \"red\" nil)
  ; => \"\\\\033[1mBold \\\\033[31mdragon\\\\033[0m\\\\033[1m text\\\\033[0m\"
  ;    Note: Bold restored after 'dragon' highlight

  ; Nested highlights
  (let ((line \"The red dragon breathes fire\"))
    (set! line (tintin-wrap-match line \"red dragon\" \"<f00>\" nil))
    (tintin-wrap-match line \"dragon\" \"bold yellow\" nil))
  ; => Correctly nested color codes preserving both highlights
  ```

  ## Notes
  - Used internally by `tintin-highlight-line` for each matching pattern
  - ANSI state tracking prevents highlight interference
  - Returns line unchanged if pattern doesn't match
  - Color specs parsed via `tintin-parse-color-component`
  - Pattern converted to regex via `tintin-pattern-to-regex`

  ## See Also
  - `tintin-highlight-line` - Applies multiple highlights to a line
  - `tintin-find-active-ansi-before` - Extracts ANSI state before position
  - `tintin-check-after-match` - Determines what follows matched text
  - `tintin-parse-color-component` - Parses color specifications"
  (if (not (string? line))
    line
    (let ((regex-pattern (tintin-pattern-to-regex pattern)))
      (if (string=? regex-pattern "")
        line
        ;; Parse color spec to get ANSI codes
        (let ((fg-ansi (if fg-color
                         (tintin-parse-color-component fg-color #f)
                         nil))
               (bg-ansi (if bg-color
                          (tintin-parse-color-component bg-color #t)
                          nil)))
          ;; Build opening ANSI sequence
          (let ((ansi-open (tintin-build-ansi-code fg-ansi bg-ansi)))
            (if (string=? ansi-open "")
              line
              ;; Find the matched text
              (let ((matched-text (regex-find regex-pattern line)))
                (if matched-text
                  ;; Find where the match occurs in the line
                  (let ((match-pos (tintin-find-match-position line matched-text)))
                    (if (< match-pos 0)
                      line
                      (let ((match-end-pos (+ match-pos (string-length matched-text))))
                        ;; Check what comes immediately after the match
                        (let ((after-type (tintin-check-after-match line match-end-pos)))
                          (let ((prev-state (tintin-find-active-ansi-before line match-pos)))
                            (let ((ansi-close
                                    (cond
                                      ;; Reset follows: use reset to close cleanly
                                      ((eq? after-type 'reset) "\033[0m")
                                      ;; Another ANSI code follows: restore previous state if any, else reset
                                      ;; This preserves outer highlights when nested
                                      ((eq? after-type 'ansi)
                                        (if (string=? prev-state "")
                                          "\033[0m"
                                          ;; Reset first to clear all attributes, then restore prev state
                                          (concat "\033[0m" prev-state)))
                                      ;; Regular text follows: restore previous state or reset
                                      (#t (if (string=? prev-state "")
                                            "\033[0m"
                                            ;; Reset first to clear all attributes, then restore prev state
                                            (concat "\033[0m" prev-state))))))
                              (string-replace line
                                matched-text
                                (concat ansi-open matched-text ansi-close))))))))
                  line)))))))))

;; Apply highlights to a single line
;; Returns highlighted line or original line if no highlights match
(defun tintin-highlight-line (line)
  "Apply all matching highlight patterns to a single line of text.

  ## Parameters
  - `line` - Line of text to highlight (may contain ANSI codes)

  ## Returns
  Line with ANSI color codes applied for all matching patterns. Returns
  original line if no patterns match or highlight table is empty.

  ## Description
  Tests all defined highlight patterns against a line and applies matching
  highlights in priority order (higher priority processed first). Multiple
  patterns can match the same line, with later highlights preserving earlier
  ones via ANSI state tracking.

  **Process:**
  1. Retrieve all highlight entries from `*tintin-highlights*` hash table
  2. Sort by priority (descending: 10, 9, ... 5 (default), ... 1)
  3. For each pattern (highest priority first):
     - Test if pattern matches current result
     - If match: Apply highlight via `tintin-wrap-match`
  4. Return transformed line with all highlights applied

  **Priority Semantics:**
  - Higher priority = processed first
  - Default priority: 5
  - Multiple matches accumulate (later highlights preserve earlier ones)

  ## Examples
  ```lisp
  ; Define highlights
  (hash-set! *tintin-highlights* \"dragon\"
    (list \"red\" nil 10))
  (hash-set! *tintin-highlights* \"fire\"
    (list \"<f80>\" nil 5))

  ; Apply to line with both matches
  (tintin-highlight-line \"The dragon breathes fire\")
  ; => \"The \\\\033[31mdragon\\\\033[0m breathes \\\\033[38;2;255;128;0mfire\\\\033[0m\"
  ;    Both patterns matched and highlighted

  ; No matches
  (tintin-highlight-line \"The orc attacks\")
  ; => \"The orc attacks\"  (unchanged)

  ; Empty highlight table
  (set! *tintin-highlights* (make-hash-table))
  (tintin-highlight-line \"any text\")
  ; => \"any text\"  (unchanged, fast path)
  ```

  ## Notes
  - Fast path: Returns immediately if highlight table empty (no overhead)
  - Pattern matching uses TinTin++ wildcard syntax (%*, %1-%99)
  - ANSI state tracking enables nested/overlapping highlights
  - Used internally by `tintin-apply-highlights` for each line
  - Priority sorting via `tintin-sort-highlights-by-priority`

  ## See Also
  - `tintin-apply-highlights` - Process entire multi-line text
  - `tintin-wrap-match` - Core wrapping logic for single pattern
  - `tintin-sort-highlights-by-priority` - Priority-based pattern ordering
  - `#highlight` command - Define highlight patterns"
  (if (or (not (string? line)) (= (hash-count *tintin-highlights*) 0))
    line
    ;; Get all highlights sorted by priority (highest first)
    (let ((highlight-entries (hash-entries *tintin-highlights*)))
      (let ((sorted (tintin-sort-highlights-by-priority highlight-entries)))
        ;; Try all patterns and apply all that match
        (let ((result line))
          (do ((i 0 (+ i 1)))
            ((>= i (list-length sorted))
              result)
            (let* ((entry (list-ref sorted i))
                    (pattern (car entry))
                    (data (cdr entry))
                    (fg-color (car data))
                    (bg-color (car (cdr data))))
              ;; Check if pattern matches the current result
              (if (tintin-match-highlight-pattern pattern result)
                ;; Apply highlight to current result (allows multiple highlights)
                (set! result (tintin-wrap-match result pattern fg-color bg-color))))))))))

;; Main entry point: Apply highlights to incoming text
;; Splits text into lines, highlights each line, returns transformed text
(defun tintin-apply-highlights (text)
  "Apply color highlighting to text based on defined highlight patterns.

  ## Parameters
  - `text` - Text to process (typically server output)

  ## Description
  Scans text for patterns defined via `#highlight` commands and wraps matching
  text with ANSI color codes. Highlights are applied line-by-line in priority
  order (higher priority highlights are processed first).

  **Process:**
  1. Split text into lines
  2. For each line, check all highlight patterns (sorted by priority)
  3. Wrap matching substrings with ANSI color codes
  4. Join lines back together with original separators

  ## Returns
  Text with ANSI color codes inserted around matching patterns.
  If no highlights are defined or text is invalid, returns original text unchanged.

  ## Examples
  ```lisp
  ; Define highlights
  (tintin-process-command \"#highlight {dragon} {bright red}\")
  (tintin-process-command \"#highlight {gold} {yellow}\")

  ; Apply highlights to server output
  (define output \"The dragon guards the gold.\")
  (tintin-apply-highlights output)
  ; => \"The \\033[1;31mdragon\\033[0m guards the \\033[33mgold\\033[0m.\"

  ; No highlights defined
  (set! *tintin-highlights* (make-hash-table))
  (tintin-apply-highlights \"some text\")
  ; => \"some text\"
  ```

  ## Notes
  - Preserves existing ANSI codes in text (handles nested highlights correctly)
  - Empty highlight table returns text unchanged (no processing overhead)
  - Patterns support wildcards: `*` (zero or more), `?` (exactly one), `[abc]` (character set)
  - Priority determines application order (higher priority = applied first)

  ## See Also
  - `#highlight` command - Define color highlights
  - `tintin-highlight-line` - Highlight a single line (internal)
  - Used automatically by `telnet-input-filter-hook` when TinTin++ is enabled"
  (if (or (not (string? text)) (= (hash-count *tintin-highlights*) 0))
    text
    ;; Split into lines
    (let ((lines (tintin-split-lines text)))
      (if (null? lines)
        text
        ;; Highlight each line
        (let ((highlighted '()))
          (do ((i 0 (+ i 1)))
            ((>= i (list-length lines))
              ;; Join highlighted lines back together
              (let ((result ""))
                (do ((j 0 (+ j 1)))
                  ((>= j (list-length highlighted)) result)
                  (set! result (concat result (list-ref highlighted j))))))
            (let ((line (list-ref lines i)))
              (set! highlighted (cons (tintin-highlight-line line) highlighted))))
          ;; Need to reverse since we cons'd in reverse order
          (set! highlighted (reverse highlighted))
          ;; Join lines
          (let ((result ""))
            (do ((k 0 (+ k 1)))
              ((>= k (list-length highlighted)) result)
              (set! result (concat result (list-ref highlighted k))))))))))

;; ============================================================================
;; TEST 1: COMMAND SEPARATOR
;; ============================================================================

;; Helper: Trim leading and trailing whitespace from string
(defun tintin-trim (str)
  "Remove leading and trailing whitespace from string.

  ## Parameters
  - `str` - String to trim

  ## Returns
  String with leading and trailing whitespace removed. Returns empty string `\"\"`
  if input is invalid or all whitespace.

  ## Description
  Removes space, tab, carriage return, and newline characters from both ends
  of the string. Uses two-pass algorithm: find first non-whitespace character
  from start, then find last non-whitespace character from end, and extract
  substring between them.

  **Whitespace characters**: space, tab (`\\t`), carriage return (`\\r`),
  newline (`\\n`)

  **Two-Pass Algorithm:**
  1. Scan forward from start to find first non-whitespace position
  2. Scan backward from end to find last non-whitespace position
  3. Extract substring from first to last (inclusive)

  ## Examples
  ```lisp
  ; Basic trimming
  (tintin-trim \"  hello  \")
  ; => \"hello\"

  ; Mixed whitespace
  (tintin-trim \"\\t\\n hello world \\r\\n\")
  ; => \"hello world\"

  ; No whitespace
  (tintin-trim \"hello\")
  ; => \"hello\"

  ; All whitespace
  (tintin-trim \"   \\t\\n  \")
  ; => \"\"

  ; Empty string
  (tintin-trim \"\")
  ; => \"\"

  ; Invalid input
  (tintin-trim nil)
  ; => \"\"
  ```

  ## Notes
  - Used extensively by command parsing functions
  - Critical for `tintin-split-commands` to clean command strings
  - Preserves internal whitespace (only trims ends)
  - Efficient: O(n) where n is string length

  ## See Also
  - `tintin-split-commands` - Uses this to clean each split command
  - `tintin-parse-arguments` - Uses this implicitly via split logic
  - `tintin-find-first-non-ws` - Helper for forward scan
  - `tintin-find-last-non-ws` - Helper for backward scan"
  (if (not (string? str))
    ""
    (let ((len (string-length str)))
      (if (= len 0)
        ""
        ;; Find first non-whitespace character
        (let ((start (tintin-find-first-non-ws str 0 len)))
          (if (>= start len)
            ""  ; All whitespace
            ;; Find last non-whitespace character
            (let ((end (tintin-find-last-non-ws str (- len 1))))
              (substring str start (+ end 1)))))))))

;; Helper: Find first non-whitespace character index
(defun tintin-find-first-non-ws (str pos len)
  (if (>= pos len)
    pos
    (let ((ch (string-ref str pos)))
      (if (or (char=? ch #\space)
            (char=? ch #\tab)
            (char=? ch #\return)
            (char=? ch #\newline))
        (tintin-find-first-non-ws str (+ pos 1) len)
        pos))))

;; Helper: Find last non-whitespace character index
(defun tintin-find-last-non-ws (str pos)
  (if (< pos 0)
    -1
    (let ((ch (string-ref str pos)))
      (if (or (char=? ch #\space)
            (char=? ch #\tab)
            (char=? ch #\return)
            (char=? ch #\newline))
        (tintin-find-last-non-ws str (- pos 1))
        pos))))

;; Recursive helper for splitting commands
(defun tintin-split-loop (str pos len depth current results)
  (if (>= pos len)
    ;; Done - add final command if any and return reversed list
    (if (not (string=? current ""))
      (reverse (cons current results))
      (reverse results))
    ;; Process current character
    (let ((ch (string-ref str pos)))
      (cond
        ((char=? ch #\{)
          (tintin-split-loop str (+ pos 1) len (+ depth 1) (concat current (char->string ch)) results))
        ((char=? ch #\})
          (tintin-split-loop str (+ pos 1) len (- depth 1) (concat current (char->string ch)) results))
        ((and (char=? ch #\;) (= depth 0))
          (tintin-split-loop str (+ pos 1) len depth "" (cons current results)))
        (#t
          (tintin-split-loop str (+ pos 1) len depth (concat current (char->string ch)) results))))))

(defun tintin-split-commands (str)
  "Split command string by semicolons, respecting brace nesting.

  ## Parameters
  - `str` - Command string potentially containing multiple commands

  ## Returns
  List of trimmed command strings. Returns empty list `()` if string is invalid.

  ## Description
  Splits input by semicolons (`;`) into individual commands while respecting
  brace nesting. Semicolons inside braces are NOT treated as separators,
  allowing complex commands with embedded semicolons to be parsed correctly.

  **Splitting Rules:**
  - Top-level semicolons (depth 0): Split point
  - Nested semicolons (depth > 0): Part of current command
  - Braces: `{` increases depth, `}` decreases depth
  - Each command is trimmed (leading/trailing whitespace removed)

  **Why Brace Nesting Matters:**
  TinTin++ commands use braces to group arguments that may contain special
  characters like semicolons. Without nesting-aware splitting, complex
  commands would be incorrectly broken apart.

  ## Examples
  ```lisp
  ; Simple split
  (tintin-split-commands \"n;s;e\")
  ; => (\"n\" \"s\" \"e\")

  ; With braces (semicolons protected)
  (tintin-split-commands \"#alias {go} {n;s;e}\")
  ; => (\"#alias {go} {n;s;e}\")  ; Single command, not split

  ; Mixed
  (tintin-split-commands \"look;#alias {k} {kill %1};n\")
  ; => (\"look\" \"#alias {k} {kill %1}\" \"n\")

  ; Nested braces
  (tintin-split-commands \"#alias {complex} {{a;b};c}\")
  ; => (\"#alias {complex} {{a;b};c}\")

  ; Whitespace trimmed
  (tintin-split-commands \"  n  ;  s  ;  e  \")
  ; => (\"n\" \"s\" \"e\")

  ; Empty commands filtered
  (tintin-split-commands \"n;;s\")
  ; => (\"n\" \"s\")  ; Empty middle command removed by trim
  ```

  ## Notes
  - Uses recursive helper `tintin-split-loop` for depth tracking
  - Each command trimmed via `tintin-trim` (whitespace removal)
  - Empty strings in result list (after trim) preserved
  - Used by `tintin-process-input` for command separation
  - Critical for TinTin++ command parsing (#alias, #action, etc.)

  ## Common Use Cases
  - Parse user input: `\"n;s;look\"` → 3 separate commands
  - Protect # commands: `\"#alias {go} {n;s}\"` → 1 command
  - Speedwalk + commands: `\"3n;look;2e\"` → separate for processing

  ## See Also
  - `tintin-split-loop` - Recursive splitting implementation
  - `tintin-trim` - Whitespace trimming for each command
  - `tintin-process-input` - Uses this for input separation"
  (if (not (string? str))
    '()
    ;; Split commands and trim whitespace from each
    (map tintin-trim (tintin-split-loop str 0 (string-length str) 0 "" '()))))

;; ============================================================================
;; TEST 2: SPEEDWALK
;; ============================================================================

;; Check if character is a digit
(defun tintin-is-digit? (ch)
  (and (string? ch)
    (= (string-length ch) 1)
    (or (string=? ch "0") (string=? ch "1") (string=? ch "2") (string=? ch "3")
      (string=? ch "4") (string=? ch "5") (string=? ch "6") (string=? ch "7")
      (string=? ch "8") (string=? ch "9"))))

;; Check if a string is a valid direction
(defun tintin-is-direction? (str)
  (or (string=? str "n") (string=? str "e") (string=? str "s") (string=? str "w")
    (string=? str "u") (string=? str "d")
    (and *tintin-speedwalk-diagonals*
      (or (string=? str "ne") (string=? str "nw")
        (string=? str "se") (string=? str "sw")))))

;; Expand speedwalk string like "3n2e" to "n;n;n;e;e"
(defun tintin-expand-speedwalk (input)
  "Expand speedwalk syntax into individual movement commands.

  ## Parameters
  - `input` - Input string potentially containing speedwalk syntax

  ## Description
  Converts compact movement notation into expanded movement commands separated
  by semicolons. Speedwalk syntax uses numeric prefixes to repeat directions:

  **Syntax:**
  - `n`, `s`, `e`, `w`, `u`, `d` - Single direction commands
  - `3n` - Repeat north 3 times (expands to `n;n;n`)
  - `2e3n` - Move east twice, then north 3 times (expands to `e;e;n;n;n`)
  - Diagonal directions (if `*tintin-speedwalk-diagonals*` is enabled):
    `ne`, `nw`, `se`, `sw`, `nu`, `nd`, etc.

  **Behavior:**
  - If speedwalk is disabled (`*tintin-speedwalk-enabled*` = `#f`): returns input unchanged
  - If input contains invalid syntax: returns input unchanged
  - If all syntax is valid: returns expanded commands joined with semicolons

  ## Returns
  - If valid speedwalk: Expanded command string (e.g., `\"n;n;n;e;e\"`)
  - If invalid or disabled: Original input string unchanged

  ## Examples
  ```lisp
  ; Basic speedwalk expansion
  (tintin-expand-speedwalk \"3n2e\")
  ; => \"n;n;n;e;e\"

  ; Single directions
  (tintin-expand-speedwalk \"n\")
  ; => \"n\"

  ; Mixed with other commands (invalid speedwalk - returns original)
  (tintin-expand-speedwalk \"3n look e\")
  ; => \"3n look e\"

  ; No numeric prefix (each direction repeated once)
  (tintin-expand-speedwalk \"nsew\")
  ; => \"n;s;e;w\"

  ; Diagonal directions (if enabled)
  (set! *tintin-speedwalk-diagonals* #t)
  (tintin-expand-speedwalk \"2ne3sw\")
  ; => \"ne;ne;sw;sw;sw\"
  ```

  ## Notes
  - Only valid for pure speedwalk input (no spaces or other text)
  - Invalid syntax returns original input (no error message)
  - Controlled by `*tintin-speedwalk-enabled*` (default: `#t`)
  - Diagonal support controlled by `*tintin-speedwalk-diagonals*` (default: `#f`)

  ## Valid Directions
  - Basic: n, s, e, w, u, d (north, south, east, west, up, down)
  - Diagonals (if enabled): ne, nw, se, sw, nu, nd, su, sd, eu, ed, wu, wd

  ## See Also
  - `tintin-process-command` - Process commands (includes speedwalk expansion)
  - `tintin-process-input` - Full input processing with command separation"
  (if (or (not (string? input)) (not *tintin-speedwalk-enabled*))
    input
    (let ((len (string-length input))
           (pos 0)
           (result '())
           (valid #t))  ; Track if entire input is valid speedwalk
      (do ()
        ((>= pos len))
        (let ((count-str "")
               (direction ""))
          ;; Collect digits for count
          (do ()
            ((or (>= pos len) (not (tintin-is-digit? (substring input pos (+ pos 1)))))
              nil)
            (set! count-str (concat count-str (substring input pos (+ pos 1))))
            (set! pos (+ pos 1)))

          ;; Get direction (1 or 2 characters)
          (if (< pos len)
            (let ((ch1 (substring input pos (+ pos 1))))
              ;; Try 2-char direction first (only if diagonals enabled)
              (if (and *tintin-speedwalk-diagonals*
                    (< (+ pos 1) len)
                    (tintin-is-direction? (concat ch1 (substring input (+ pos 1) (+ pos 2)))))
                (progn
                  (set! direction (concat ch1 (substring input (+ pos 1) (+ pos 2))))
                  (set! pos (+ pos 2)))
                ;; Try 1-char direction
                (if (tintin-is-direction? ch1)
                  (progn
                    (set! direction ch1)
                    (set! pos (+ pos 1)))
                  ;; Not a valid direction - mark as invalid
                  (progn
                    (set! valid #f)
                    (set! pos (+ pos 1)))))))

          ;; Expand direction N times (only if we found a valid direction)
          (if (not (string=? direction ""))
            (let ((count (if (string=? count-str "") 1 (string->number count-str))))
              (do ((i 0 (+ i 1)))
                ((>= i count))
                (set! result (cons direction result)))))))

      ;; Return original input if any part was invalid, otherwise return expanded
      (if (not valid)
        input
        ;; Join results with semicolons
        (let ((reversed (reverse result))
               (output ""))
          (do ((i 0 (+ i 1)))
            ((>= i (list-length reversed)) output)
            (set! output (concat output
                           (if (> i 0) ";" "")
                           (list-ref reversed i)))))))))

;; ============================================================================
;; TEST 3: ALIAS CREATION
;; ============================================================================

;; Extract braced argument (including braces)
;; Returns (braced-text . next-pos) or nil if no braced text found
;; Example: "{hello {world}}" → ("{hello {world}}" . position-after-closing-brace)
(defun tintin-extract-braced (str start-pos)
  "Extract brace-delimited text with nesting support.

  ## Parameters
  - `str` - String to extract from
  - `start-pos` - Position to start scanning (0-based index)

  ## Returns
  Cons cell `(braced-text . next-pos)` where:
  - `braced-text` - Extracted text **including outer braces** (e.g., `\"{text}\"`)
  - `next-pos` - Position immediately after closing brace

  Returns `nil` if no braced text found or extraction fails.

  ## Description
  Extracts brace-delimited arguments from TinTin++ command strings, handling
  nested braces correctly via depth tracking. Scans forward from `start-pos`
  to find opening brace, then tracks brace depth to find matching closing brace.

  **Critical for TinTin++ command parsing**: Commands use braces to delimit
  arguments that may contain special characters (spaces, semicolons, nested
  braces). This function ensures nested structures are extracted intact.

  **Depth Tracking Algorithm:**
  1. Scan forward from `start-pos` to find opening `{`
  2. Initialize depth counter to 1
  3. Scan character-by-character:
     - `{` increments depth
     - `}` decrements depth
     - Stop when depth reaches 0 (found matching closing brace)
  4. Return substring from opening `{` to closing `}` (inclusive)

  ## Examples
  ```lisp
  ; Simple braced text
  (tintin-extract-braced \"{hello}\" 0)
  ; => (\"{hello}\" . 7)

  ; Nested braces
  (tintin-extract-braced \"{outer {inner} text}\" 0)
  ; => (\"{outer {inner} text}\" . 20)

  ; Multiple levels of nesting
  (tintin-extract-braced \"{a {b {c} d} e}\" 0)
  ; => (\"{a {b {c} d} e}\" . 14)

  ; Braces not at start
  (tintin-extract-braced \"#alias k {kill %1}\" 9)
  ; => (\"{kill %1}\" . 18)

  ; No braces found
  (tintin-extract-braced \"plain text\" 0)
  ; => nil

  ; Past end of string
  (tintin-extract-braced \"text\" 10)
  ; => nil

  ; Unmatched brace
  (tintin-extract-braced \"{incomplete\" 0)
  ; => nil  (depth never reaches 0)
  ```

  ## Notes
  - **Preserves braces**: Returned text includes outer `{` and `}`
  - **Depth tracking**: Prevents early termination on nested closing braces
  - **Position tracking**: `next-pos` enables sequential extraction
  - Used by `tintin-parse-arguments` for mixed braced/unbraced args
  - Critical for commands like `#alias {go} {n;s;e}` where semicolons are protected

  ## Common Use Cases
  - Extract command arguments: `#alias {name} {commands}`
  - Parse color specs: `#highlight {pattern} {color}`
  - Handle nested structures: `#alias {complex} {{a;b};c}`

  ## Edge Cases Handled
  - Nested braces at any depth
  - Braces not at start position (scans forward to find)
  - Unmatched braces (returns nil gracefully)
  - Empty braces `{}`
  - String too short (returns nil)

  ## See Also
  - `tintin-parse-arguments` - Uses this for braced argument extraction
  - `tintin-strip-braces` - Removes outer braces from extracted text
  - `tintin-extract-token` - Extracts unbraced space-delimited tokens
  - `tintin-split-commands` - Why braces matter (protect semicolons)"
  (if (>= start-pos (string-length str))
    nil
    (let ((pos start-pos)
           (len (string-length str)))
      ;; Find opening brace
      (do ()
        ((or (>= pos len) (char=? (string-ref str pos) #\{))
          (if (>= pos len)
            nil
            ;; Extract including braces - track depth for nested braces
            (let ((depth 1)
                   (brace-start pos)  ; Start at opening brace
                   (end-pos (+ pos 1)))
              (do ()
                ((or (>= end-pos len) (= depth 0))
                  (if (= depth 0)
                    ;; Return text INCLUDING braces (from brace-start to end-pos)
                    (cons (substring str brace-start end-pos) end-pos)
                    nil))
                (let ((ch (string-ref str end-pos)))
                  (if (char=? ch #\{)
                    (set! depth (+ depth 1))
                    (if (char=? ch #\})
                      (set! depth (- depth 1))))
                  (set! end-pos (+ end-pos 1)))))))
        (set! pos (+ pos 1))))))

;; Extract space-delimited token starting at pos
;; Returns (token . next-pos) or nil if no token found
;; Example: (tintin-extract-token "#load Det" 6) => ("Det" . 9)
(defun tintin-extract-token (str start-pos)
  (if (>= start-pos (string-length str))
    nil
    (let ((len (string-length str))
           (pos start-pos))
      ;; Skip leading whitespace
      (do ()
        ((or (>= pos len) (not (char=? (string-ref str pos) #\space))))
        (set! pos (+ pos 1)))
      ;; Check if we have any characters left
      (if (>= pos len)
        nil
        ;; Find end of token (space or end of string)
        (let ((start pos)
               (end pos))
          (do ()
            ((or (>= end len) (char=? (string-ref str end) #\space)))
            (set! end (+ end 1)))
          ;; Return token and position
          (if (= start end)
            nil
            (cons (substring str start end) end)))))))

;; Extract from start-pos to end of string (for last argument in unbraced format)
;; Returns: (string . end-pos) or nil
(defun tintin-extract-to-end (str start-pos)
  (if (>= start-pos (string-length str))
    nil
    (let ((len (string-length str))
           (pos start-pos))
      ;; Skip leading whitespace
      (do ()
        ((or (>= pos len) (not (char=? (string-ref str pos) #\space))))
        (set! pos (+ pos 1)))
      ;; Check if we have any characters left
      (if (>= pos len)
        nil
        ;; Return from pos to end of string
        (cons (substring str pos len) len)))))

;; Parse N arguments from command string (mixed format: braced or unbraced)
;; Returns: list of N strings or nil if parsing fails
;; Each argument can be independently braced or unbraced
;; Braced arguments preserve braces: {text} → "{text}"
;; Example: (tintin-parse-arguments "#alias bag {kill %1}" 2) => ("bag" "{kill %1}")
;;          (tintin-parse-arguments "#load Det" 1) => ("Det")
(defun tintin-parse-arguments (input n)
  "Parse N arguments from TinTin++ command string (mixed braced/unbraced format).

  ## Parameters
  - `input` - TinTin++ command string (starting with `#`)
  - `n` - Number of arguments to extract

  ## Returns
  List of `n` argument strings (braced args include braces), or `nil` if
  parsing fails (insufficient arguments or invalid syntax).

  ## Description
  Extracts exactly `n` arguments from a TinTin++ command string, handling
  mixed braced and unbraced arguments. Each argument can independently be:
  - **Braced**: `{text with spaces}` - preserves braces in output
  - **Unbraced**: Space-delimited tokens

  **Three-Phase Algorithm:**

  **Phase 1: Skip to Arguments**
  1. Skip whitespace after `#`
  2. Skip command name (until space, `{`, or end)

  **Phase 2: Extract N Arguments**
  - For each argument (0 to n-1):
    1. Skip leading whitespace
    2. Check if braced (starts with `{`):
       - **Yes**: Use `tintin-extract-braced` (handles nesting)
       - **No**: Use `tintin-extract-token` (space-delimited)
    3. Special case: **Last argument** reads to end of string (not just to space)

  **Phase 3: Return**
  - Success: Return list of `n` arguments (in original order)
  - Failure: Return `nil` (insufficient args, invalid syntax)

  **Why Last Argument Special?**
  Allows unbraced last arguments to contain spaces without braces:
  - `#load path/to/file.lisp` - path is last arg, no braces needed
  - `#echo Hello world` - entire \"Hello world\" is last arg

  ## Examples
  ```lisp
  ; Mixed braced and unbraced
  (tintin-parse-arguments \"#alias bag {kill %1}\" 2)
  ; => (\"bag\" \"{kill %1}\")

  ; All unbraced (last arg reads to end)
  (tintin-parse-arguments \"#load path/to/file.lisp\" 1)
  ; => (\"path/to/file.lisp\")

  ; All braced (nested braces preserved)
  (tintin-parse-arguments \"#alias {go} {n;s;e}\" 2)
  ; => (\"{go}\" \"{n;s;e}\")

  ; Three arguments mixed
  (tintin-parse-arguments \"#highlight {dragon} {bold red} {10}\" 3)
  ; => (\"{dragon}\" \"{bold red}\" \"{10}\")

  ; Last arg with spaces (no braces needed)
  (tintin-parse-arguments \"#echo Hello world\" 1)
  ; => (\"Hello world\")

  ; Insufficient arguments
  (tintin-parse-arguments \"#alias k\" 2)
  ; => nil  (only 1 arg found, need 2)

  ; Empty command
  (tintin-parse-arguments \"#alias\" 2)
  ; => nil
  ```

  ## Notes
  - **Preserves braces**: Braced arguments returned with braces intact
  - **Mixed format**: Each argument independently braced or unbraced
  - **Last arg special**: Reads to end of string (enables space-containing values)
  - **Nesting support**: Braced extraction handles nested braces via depth tracking
  - **Whitespace handling**: Leading whitespace skipped before each arg
  - **Failure modes**: Returns `nil` for insufficient args or invalid syntax

  ## Common Parsing Scenarios

  **Two-argument commands** (name + value):
  - `#alias {name} {commands}` → `(\"{name}\" \"{commands}\")`
  - `#variable {var} {value}` → `(\"{var}\" \"{value}\")`
  - `#alias k kill` → `(\"k\" \"kill\")` (last arg is \"kill\")

  **Three-argument commands** (name + value + priority):
  - `#highlight {pattern} {color} {10}` → `(\"{pattern}\" \"{color}\" \"{10}\")`

  **Single-argument commands**:
  - `#load script.lisp` → `(\"script.lisp\")`
  - `#unalias {k}` → `(\"{k}\")`

  ## Position Tracking
  Internal `start-pos` variable tracks current parsing position:
  1. Starts at 1 (after `#`)
  2. Advances past command name
  3. Advances through whitespace and arguments
  4. Updated by extraction functions (`tintin-extract-braced`, `tintin-extract-token`)

  ## See Also
  - `tintin-extract-braced` - Extract braced arguments (with nesting)
  - `tintin-extract-token` - Extract unbraced space-delimited tokens
  - `tintin-extract-to-end` - Extract from position to end of string (last arg)
  - `tintin-strip-braces` - Remove outer braces from extracted arguments
  - `tintin-dispatch-command` - Uses this to parse all # commands"
  (let ((start-pos 1)       ; Start after #
         (args '())
         (success #t))
    ;; Step 1: Skip whitespace after #
    (do ()
      ((or (>= start-pos (string-length input))
         (not (char=? (string-ref input start-pos) #\space))))
      (set! start-pos (+ start-pos 1)))

    ;; Step 2: Skip past command name (until space, {, or end)
    (do ()
      ((or (>= start-pos (string-length input))
         (char=? (string-ref input start-pos) #\space)
         (char=? (string-ref input start-pos) #\{)))
      (set! start-pos (+ start-pos 1)))

    ;; Step 3: Parse N arguments using mixed format
    ;; Each argument can be braced or unbraced independently
    (do ((i 0 (+ i 1)))
      ((or (>= i n) (not success))
        (if success (reverse args) nil))
      ;; Skip whitespace before this argument
      (do ()
        ((or (>= start-pos (string-length input))
           (not (char=? (string-ref input start-pos) #\space))))
        (set! start-pos (+ start-pos 1)))

      ;; Check if we have more input
      (if (>= start-pos (string-length input))
        (set! success #f)  ; Ran out of input before getting N arguments
        ;; Check if this argument is braced or unbraced
        (let ((is-braced (char=? (string-ref input start-pos) #\{)))
          (if is-braced
            ;; Extract braced argument (preserves braces)
            (let ((arg-data (tintin-extract-braced input start-pos)))
              (if arg-data
                (progn
                  (set! args (cons (car arg-data) args))
                  (set! start-pos (cdr arg-data)))
                (set! success #f)))
            ;; Extract unbraced token
            ;; For the last argument, read to end of string instead of stopping at space
            (let ((is-last-arg (= i (- n 1))))
              (let ((token-data (if is-last-arg
                                  (tintin-extract-to-end input start-pos)
                                  (tintin-extract-token input start-pos))))
                (if token-data
                  (progn
                    (set! args (cons (car token-data) args))
                    (set! start-pos (cdr token-data)))
                  (set! success #f))))))))))


;; Match a pattern against input and extract placeholder values
;; Returns list of extracted values or nil if no match
;; Example: (tintin-match-pattern "k %1 with %2" "k orc with sword") => ("orc" "sword")
(defun tintin-match-pattern (pattern input)
  (let ((pattern-parts (split pattern " "))
         (input-parts (split input " ")))
    (if (not (= (list-length pattern-parts) (list-length input-parts)))
      nil
      (let ((matches '())
             (success #t))
        (do ((i 0 (+ i 1)))
          ((or (>= i (list-length pattern-parts)) (not success))
            (if success (reverse matches) nil))
          (let ((p-part (list-ref pattern-parts i))
                 (i-part (list-ref input-parts i)))
            (if (string-prefix? "%" p-part)
              ;; Placeholder - capture the value
              (set! matches (cons i-part matches))
              ;; Literal - must match exactly
              (if (and (string? p-part) (string? i-part) (not (string=? p-part i-part)))
                (set! success #f)))))))))

;; Check if character is valid in variable name: [a-zA-Z0-9_-]
(defun tintin-is-varname-char? (ch)
  (or (and (char>=? ch #\a) (char<=? ch #\z))
    (and (char>=? ch #\A) (char<=? ch #\Z))
    (and (char>=? ch #\0) (char<=? ch #\9))
    (char=? ch #\_)
    (char=? ch #\-)))

;; Expand $variable references in a string
(defun tintin-expand-variables (str)
  "Expand $variable references in string (legacy O(n*m) version).

  ## Parameters
  - `str` - String potentially containing $variable references

  ## Returns
  String with all $variable references replaced by their values. Returns input
  unchanged if invalid or no variables defined.

  ## Description
  **DEPRECATED**: Use `tintin-expand-variables-fast` instead for O(m) performance.

  Replaces all `$variable` references with their values from the `*tintin-variables*`
  hash table. This legacy implementation iterates through all variable names and
  performs string replacement for each, resulting in O(n*m) complexity where n is
  number of variables and m is string length.

  **Algorithm (O(n*m)):**
  1. Get list of all variable names from hash table
  2. For each variable:
     - Replace all occurrences of `$varname` with value
  3. Return modified string

  **Performance**: Slow for large variable sets or long strings. Prefer
  `tintin-expand-variables-fast` for production use.

  ## Examples
  ```lisp
  ; Define variables
  (hash-set! *tintin-variables* \"target\" \"orc\")
  (hash-set! *tintin-variables* \"weapon\" \"sword\")

  ; Expand single variable
  (tintin-expand-variables \"kill $target\")
  ; => \"kill orc\"

  ; Expand multiple variables
  (tintin-expand-variables \"attack $target with $weapon\")
  ; => \"attack orc with sword\"

  ; Undefined variable (kept as literal)
  (tintin-expand-variables \"$undefined\")
  ; => \"$undefined\"

  ; No variables
  (tintin-expand-variables \"no variables here\")
  ; => \"no variables here\"
  ```

  ## Notes
  - Variable names: `[a-zA-Z0-9_-]+`
  - Undefined variables kept as literal `$name`
  - Case-sensitive matching
  - O(n*m) complexity (n=variables, m=string length)

  ## See Also
  - `tintin-expand-variables-fast` - O(m) optimized version (preferred)
  - `#variable` command - Define variables
  - `tintin-is-varname-char?` - Valid variable name characters"
  (if (not (string? str))
    str
    (let ((result str)
           (var-names (hash-keys *tintin-variables*)))
      ;; Replace each variable
      (do ((i 0 (+ i 1)))
        ((>= i (list-length var-names)) result)
        (let* ((var-name (list-ref var-names i))
                (var-value (hash-ref *tintin-variables* var-name)))
          (set! result (string-replace result (concat "$" var-name) var-value)))))))

;; Expand $variable references in a string (optimized O(m) single-pass)
(defun tintin-expand-variables-fast (str)
  "Expand $variable references in string (optimized O(m) single-pass version).

  ## Parameters
  - `str` - String potentially containing $variable references

  ## Returns
  String with all $variable references replaced by their values. Returns input
  unchanged if invalid.

  ## Description
  Replaces all `$variable` references with their values from the `*tintin-variables*`
  hash table using a single-pass O(m) algorithm. This is the **preferred** variable
  expansion function for production use.

  **Algorithm (O(m) single-pass):**
  1. Scan string character-by-character
  2. When `$` encountered:
     - Extract variable name (characters matching `[a-zA-Z0-9_-]`)
     - Look up value in hash table
     - Replace with value (or keep literal if undefined)
  3. Append non-$ characters directly
  4. Return result string

  **Performance**: O(m) where m is string length. Hash lookups are O(1) average.
  Much faster than `tintin-expand-variables` for large variable sets.

  ## Examples
  ```lisp
  ; Define variables
  (hash-set! *tintin-variables* \"target\" \"orc\")
  (hash-set! *tintin-variables* \"weapon\" \"sword\")
  (hash-set! *tintin-variables* \"hp\" \"50\")

  ; Expand single variable
  (tintin-expand-variables-fast \"kill $target\")
  ; => \"kill orc\"

  ; Expand multiple variables
  (tintin-expand-variables-fast \"attack $target with $weapon\")
  ; => \"attack orc with sword\"

  ; Variables in action commands
  (tintin-expand-variables-fast \"say My HP is $hp!\")
  ; => \"say My HP is 50!\"

  ; Undefined variable (kept as literal)
  (tintin-expand-variables-fast \"$undefined remains\")
  ; => \"$undefined remains\"

  ; Dollar sign without valid name (kept literal)
  (tintin-expand-variables-fast \"Cost is $ 10\")
  ; => \"Cost is $ 10\"

  ; Adjacent variables
  (tintin-expand-variables-fast \"$target$weapon\")
  ; => \"orcsword\"  ; Concatenated values
  ```

  ## Notes
  - **Variable names**: `[a-zA-Z0-9_-]+` (letters, digits, underscore, hyphen)
  - **Undefined variables**: Kept as literal `$name` (not replaced)
  - **Case-sensitive**: `$Var` and `$var` are different variables
  - **Single-pass**: O(m) complexity (m = string length)
  - **Hash lookup**: O(1) average per variable reference
  - **Preferred over**: `tintin-expand-variables` (legacy O(n*m) version)

  ## Common Use Cases
  - Alias command expansion: `\"get $item;go $direction\"`
  - Action trigger commands: `\"say %1 has $hp health\"`
  - Dynamic command generation: `\"tell $friend about $topic\"`

  ## See Also
  - `tintin-expand-variables` - Legacy O(n*m) version (deprecated)
  - `#variable` command - Define variables
  - `tintin-is-varname-char?` - Valid variable name characters
  - `tintin-substitute-captures` - Replace %1-%99 in templates"
  (if (not (string? str))
    str
    (let ((len (string-length str))
           (pos 0)
           (result ""))
      (do ()
        ((>= pos len) result)
        (let ((ch (string-ref str pos)))
          (if (char=? ch #\$)
            ;; Extract variable name
            (let ((var-start (+ pos 1))
                   (var-end (+ pos 1)))
              ;; Find end of variable name
              (do ()
                ((or (>= var-end len)
                   (not (tintin-is-varname-char? (string-ref str var-end)))))
                (set! var-end (+ var-end 1)))

              (if (= var-start var-end)
                ;; No variable name after $, keep literal $
                (progn
                  (set! result (concat result "$"))
                  (set! pos (+ pos 1)))
                ;; Variable name found, try to expand
                (let* ((var-name (substring str var-start var-end))
                        (var-value (hash-ref *tintin-variables* var-name)))
                  (if var-value
                    (set! result (concat result var-value))
                    (set! result (concat result "$" var-name)))
                  (set! pos var-end))))
            ;; Regular character
            (progn
              (set! result (concat result (char->string ch)))
              (set! pos (+ pos 1)))))))))

;; ============================================================================
;; ACTION CAPTURE EXTRACTION
;; ============================================================================

;; Extract %1-%99 capture groups from pattern match
;; Uses regex-extract builtin to get capture values from matched text
;; Returns: List of captured strings or empty list if no match
;; Example: pattern="You hit %1 for %2 damage", text="You hit orc for 15 damage"
;;          → ("orc" "15")
(defun tintin-extract-captures (pattern text)
  "Extract %1-%99 capture groups from TinTin++ pattern match.

  ## Parameters
  - `pattern` - TinTin++ pattern with wildcards (%*, %1-%99)
  - `text` - Text to extract captures from

  ## Returns
  List of captured strings (in order: %1, %2, ..., %99), or empty list `()` if
  no match or invalid parameters.

  ## Description
  Extracts wildcard capture values from text that matches a TinTin++ pattern.
  Converts pattern to PCRE2 regex via `tintin-pattern-to-regex`, then uses
  `regex-extract` builtin to extract all capture groups. Used by action triggers
  to extract data for command substitution.

  **Capture Semantics:**
  - `%*`, `%1`, `%2`, ..., `%99` all become capture groups
  - Capture order matches wildcard order in pattern (left to right)
  - Non-greedy matching: `(.*?)` stops at first match
  - Empty captures possible (e.g., `%1` matches empty string)

  ## Examples
  ```lisp
  ; Extract single capture
  (tintin-extract-captures \"You hit %1\" \"You hit orc\")
  ; => (\"orc\")

  ; Extract multiple captures
  (tintin-extract-captures \"You hit %1 for %2 damage\"
                           \"You hit orc for 15 damage\")
  ; => (\"orc\" \"15\")

  ; Numbered vs %* (functionally identical)
  (tintin-extract-captures \"%1 attacks %2\" \"orc attacks wizard\")
  ; => (\"orc\" \"wizard\")
  (tintin-extract-captures \"%* attacks %*\" \"orc attacks wizard\")
  ; => (\"orc\" \"wizard\")

  ; No captures (literal pattern)
  (tintin-extract-captures \"Valgar\" \"Valgar\")
  ; => ()  ; Match but no wildcards

  ; No match
  (tintin-extract-captures \"You hit %1\" \"You miss\")
  ; => ()

  ; Empty capture
  (tintin-extract-captures \"^%1Health: %2\" \"Health: 50\")
  ; => (\"\" \"50\")  ; %1 captured empty string
  ```

  ## Notes
  - Wildcards become capture groups in order
  - Uses PCRE2 regex backend via `regex-extract`
  - Non-greedy: `(.*?)` prefers shortest match
  - Failed match returns empty list (NOT error)
  - Capture count may be less than wildcard count if pattern doesn't fully match

  ## Common Use Cases
  - Action triggers: Extract damage, target, item names
  - Data parsing: Extract HP, mana, gold from status lines
  - Pattern analysis: Verify capture values before command execution

  ## See Also
  - `tintin-substitute-captures` - Replace %N in templates with values
  - `tintin-pattern-to-regex` - Convert TinTin++ pattern to regex
  - `#action` command - Uses captures for trigger commands
  - `regex-extract` builtin - PCRE2 capture extraction"
  (if (or (not (string? pattern)) (not (string? text)))
    '()
    (let ((regex-pattern (tintin-pattern-to-regex pattern)))
      (if (string=? regex-pattern "")
        '()
        ;; Use regex-extract to get all capture groups
        (let ((captures (regex-extract regex-pattern text)))
          (if captures captures '()))))))

;; Replace %1-%99 in template with capture values
;; Iterates through captures list, replacing each placeholder
;; Returns: Template with placeholders replaced
;; Example: template="say %1 took %2!", captures=("orc" "15")
;;          → "say orc took 15!"
(defun tintin-substitute-captures (template captures)
  "Replace %1-%99 placeholders in template with capture values.

  ## Parameters
  - `template` - String containing %1-%99 placeholders
  - `captures` - List of captured strings to substitute

  ## Returns
  Template string with placeholders replaced by corresponding capture values.
  Returns input unchanged if invalid parameters. Unused placeholders remain literal.

  ## Description
  Substitutes wildcard capture values into command templates. Used by action
  triggers to generate commands based on extracted pattern data. Placeholders
  `%1` through `%99` are replaced by corresponding list elements (1-indexed).

  **Substitution Rules:**
  - `%1` replaced by first capture (index 0)
  - `%2` replaced by second capture (index 1)
  - `%N` replaced by Nth capture (index N-1)
  - Placeholders beyond capture count kept literal
  - Non-placeholder %N kept literal (e.g., `%%1` → `%1`)

  ## Examples
  ```lisp
  ; Simple substitution
  (tintin-substitute-captures \"say %1 took %2!\"
                              '(\"orc\" \"15\"))
  ; => \"say orc took 15!\"

  ; Single capture
  (tintin-substitute-captures \"kill %1\"
                              '(\"goblin\"))
  ; => \"kill goblin\"

  ; Multiple uses of same capture
  (tintin-substitute-captures \"%1 and %1 again\"
                              '(\"foo\"))
  ; => \"foo and foo again\"

  ; Unused placeholders remain literal
  (tintin-substitute-captures \"one %1 and two %2 and %3\"
                              '(\"A\" \"B\"))
  ; => \"one A and two B and %3\"  ; %3 kept literal

  ; No captures (empty list)
  (tintin-substitute-captures \"no captures: %1 %2\"
                              '())
  ; => \"no captures: %1 %2\"

  ; Non-string capture (kept as placeholder)
  (tintin-substitute-captures \"%1 and %2\"
                              '(\"foo\" 42))
  ; => \"foo and %2\"  ; 42 not a string, %2 kept
  ```

  ## Notes
  - **1-indexed**: `%1` is first capture, `%2` is second, etc.
  - **List-indexed**: Internally uses 0-based list indexing
  - **String-only**: Non-string captures NOT substituted (placeholder kept)
  - **Multiple replacement**: Same placeholder can appear multiple times
  - **Out-of-bounds safe**: Placeholders beyond capture count kept literal

  ## Common Use Cases
  - Action command generation: `\"kill %1\"` → `\"kill orc\"`
  - Data-driven responses: `\"say %1 has %2 health\"` → `\"say orc has 50 health\"`
  - Multi-step sequences: `\"get %1;examine %1;use %1\"`

  ## See Also
  - `tintin-extract-captures` - Extract %N values from pattern match
  - `#action` command - Uses templates with captures
  - `tintin-expand-variables-fast` - Substitute $variables (different syntax)"
  (if (or (not (string? template)) (not (list? captures)))
    template
    (let ((result template))
      ;; Replace each capture group placeholder (%1, %2, ..., %99)
      (do ((i 0 (+ i 1)))
        ((>= i (list-length captures)) result)
        (let ((placeholder (concat "%" (number->string (+ i 1))))
               (value (list-ref captures i)))
          (if (string? value)
            (set! result (string-replace result placeholder value))))))))

;; ============================================================================
;; SAVE/LOAD UTILITY FUNCTIONS
;; ============================================================================

;; Escape string for Lisp syntax (backslashes first, then quotes)
(defun tintin-escape-string (str)
  (let ((result str))
    (set! result (string-replace result "\\" "\\\\"))  ; Escape backslashes first
    (set! result (string-replace result "\"" "\\\""))  ; Then escape quotes
    result))


;; Save TinTin++ state to file
(defun tintin-save-state (filename)
  (let ((file (open filename "w")))
    ;; Write header
    (write-line file ";; TinTin++ State File")
    (write-line file ";; Generated by #save command")
    (write-line file "")

    ;; Write aliases
    (write-line file ";; Aliases")
    (let ((alias-entries (hash-entries *tintin-aliases*)))
      (do ((i 0 (+ i 1)))
        ((>= i (list-length alias-entries)))
        (let* ((entry (list-ref alias-entries i))
                (name (car entry))
                (value (cdr entry))
                (commands (car value))
                (priority (car (cdr value))))
          (write-line file (concat "(hash-set! *tintin-aliases* "
                             "\"" (tintin-escape-string name) "\" "
                             "(list \"" (tintin-escape-string commands) "\" "
                             (number->string priority) "))")))))
    (write-line file "")

    ;; Write variables
    (write-line file ";; Variables")
    (let ((var-entries (hash-entries *tintin-variables*)))
      (do ((i 0 (+ i 1)))
        ((>= i (list-length var-entries)))
        (let* ((entry (list-ref var-entries i))
                (name (car entry))
                (value (cdr entry)))
          (write-line file (concat "(hash-set! *tintin-variables* "
                             "\"" (tintin-escape-string name) "\" "
                             "\"" (tintin-escape-string value) "\")")))))
    (write-line file "")

    ;; Write highlights
    (write-line file ";; Highlights")
    (let ((highlight-entries (hash-entries *tintin-highlights*)))
      (do ((i 0 (+ i 1)))
        ((>= i (list-length highlight-entries)))
        (let* ((entry (list-ref highlight-entries i))
                (pattern (car entry))
                (data (cdr entry))
                (fg-color (car data))
                (bg-color (car (cdr data)))
                (priority (car (cdr (cdr data)))))
          (write-line file (concat "(hash-set! *tintin-highlights* "
                             "\"" (tintin-escape-string pattern) "\" "
                             "(list "
                             (if fg-color
                               (concat "\"" (tintin-escape-string fg-color) "\"")
                               "nil")
                             " "
                             (if bg-color
                               (concat "\"" (tintin-escape-string bg-color) "\"")
                               "nil")
                             " "
                             (number->string priority) "))")))))
    (write-line file "")

    ;; Write actions
    (write-line file ";; Actions")
    (let ((action-entries (hash-entries *tintin-actions*)))
      (do ((i 0 (+ i 1)))
        ((>= i (list-length action-entries)))
        (let* ((entry (list-ref action-entries i))
                (pattern (car entry))
                (data (cdr entry))
                (commands (car data))
                (priority (car (cdr data))))
          (write-line file (concat "(hash-set! *tintin-actions* "
                             "\"" (tintin-escape-string pattern) "\" "
                             "(list \"" (tintin-escape-string commands) "\" "
                             (number->string priority) "))")))))
    (write-line file "")

    ;; Write settings
    (write-line file ";; Settings")
    (write-line file (concat "(set! *tintin-speedwalk-enabled* "
                       (if *tintin-speedwalk-enabled* "#t" "#f") ")"))
    (write-line file (concat "(set! *tintin-speedwalk-diagonals* "
                       (if *tintin-speedwalk-diagonals* "#t" "#f") ")"))
    (write-line file (concat "(set! *tintin-enabled* "
                       (if *tintin-enabled* "#t" "#f") ")"))

    ;; Close file
    (close file)
    filename))

;; ============================================================================
;; TINTIN++ COMMAND HELPERS
;; ============================================================================

;; Check if a string is a TinTin++ command (starts with #)
(defun tintin-is-command? (str)
  (and (string? str)
    (> (string-length str) 0)
    (char=? (string-ref str 0) #\#)))

;; Extract command name from TinTin++ command string
;; Example: "#alias {k} {kill}" → "alias"
;;          "#var{x}" → "var"
;;          "# " → nil
(defun tintin-extract-command-name (str)
  (if (not (tintin-is-command? str))
    nil
    (let ((len (string-length str))
           (pos 1))  ; Start after #
      ;; Skip any whitespace after #
      (do ()
        ((or (>= pos len) (not (char=? (string-ref str pos) #\space))))
        (set! pos (+ pos 1)))
      ;; Check if we have any characters left
      (if (>= pos len)
        nil
        ;; Find end of command word (space, {, or end of string)
        (let ((start pos)
               (end pos))
          (do ()
            ((or (>= end len)
               (char=? (string-ref str end) #\space)
               (char=? (string-ref str end) #\{)))
            (set! end (+ end 1)))
          ;; Extract and lowercase the command name
          (if (= start end)
            nil
            (string-downcase (substring str start end))))))))

;; Find a TinTin++ command by partial prefix match
;; Returns the full command name or nil if no match
;; Example: "al" → "alias", "var" → "variable"
(defun tintin-find-command (prefix)
  "Find TinTin++ command by partial prefix match (case-insensitive).

  ## Parameters
  - `prefix` - Command prefix to match (e.g., \"al\", \"var\")

  ## Returns
  Full command name if match found, `nil` if no match or invalid prefix.

  ## Description
  Searches registered TinTin++ commands for case-insensitive prefix match.
  Used by command dispatcher to support command abbreviations (e.g., \"al\" → \"alias\").

  **Matching Rules:**
  - Case-insensitive: \"AL\" matches \"alias\"
  - Prefix-only: \"var\" matches \"variable\", NOT \"alias\"
  - First match wins: non-deterministic if multiple matches

  ## Examples
  ```lisp
  ; Standard abbreviations
  (tintin-find-command \"al\")
  ; => \"alias\"

  (tintin-find-command \"var\")
  ; => \"variable\"

  (tintin-find-command \"act\")
  ; => \"action\"

  ; Full command name
  (tintin-find-command \"highlight\")
  ; => \"highlight\"

  ; Case-insensitive
  (tintin-find-command \"AL\")
  ; => \"alias\"

  ; No match
  (tintin-find-command \"xyz\")
  ; => nil

  ; Invalid input
  (tintin-find-command nil)
  ; => nil
  ```

  ## Notes
  - Used internally by `tintin-dispatch-command`
  - Command list from `*tintin-commands*` hash table
  - Returns first matching command (order non-deterministic)
  - Does NOT validate command syntax (only finds name)

  ## See Also
  - `tintin-dispatch-command` - Uses this for command lookup
  - `*tintin-commands*` - Hash of registered commands"
  (if (not (string? prefix))
    nil
    (let ((prefix-lower (string-downcase prefix))
           (commands (hash-keys *tintin-commands*))
           (result nil))
      (do ((i 0 (+ i 1)))
        ((or (>= i (list-length commands)) result) result)
        (let ((cmd (list-ref commands i)))
          (if (string-prefix? prefix-lower cmd)
            (set! result cmd)))))))

;; Match first word against alias hash table
;; Returns: (alias-entry . args) or nil
(defun tintin-match-simple-alias (cmd)
  "Match first word against simple (non-pattern) alias hash table.

  ## Parameters
  - `cmd` - Command string to match

  ## Returns
  Cons cell `(alias-entry . args)` where:
  - `alias-entry` - Alias data: `(expansion-template priority)`
  - `args` - List of remaining words after first word

  Returns `nil` if no match found.

  ## Description
  Attempts to match the first word of a command against simple (non-wildcard)
  aliases. Used for fast O(1) hash lookup before falling back to O(n) pattern
  matching for wildcard aliases.

  **Matching Strategy:**
  1. Split command by spaces
  2. Extract first word as alias name
  3. Hash lookup in `*tintin-aliases*`
  4. Return entry + remaining words as args

  **Performance**: O(1) hash lookup - much faster than pattern matching.

  ## Examples
  ```lisp
  ; Assume alias defined: \"gn\" → \"get gold;north\"
  (hash-set! *tintin-aliases* \"gn\" '(\"get gold;north\" 5))

  ; Match simple alias
  (tintin-match-simple-alias \"gn\")
  ; => ((\"get gold;north\" 5) )  ; Entry + no args

  ; Match with args
  (tintin-match-simple-alias \"gn extra args\")
  ; => ((\"get gold;north\" 5) \"extra\" \"args\")  ; Entry + arg list

  ; No match (not an alias)
  (tintin-match-simple-alias \"look\")
  ; => nil

  ; Empty command
  (tintin-match-simple-alias \"\")
  ; => nil
  ```

  ## Notes
  - **Simple aliases only**: Does NOT match pattern aliases (use `tintin-match-pattern-alias`)
  - **First word matching**: Only first space-separated word tested
  - **Args preserved**: Remaining words returned for template substitution
  - **Hash-based**: O(1) lookup performance

  ## See Also
  - `tintin-match-pattern-alias` - Match wildcard pattern aliases
  - `tintin-expand-alias` - Uses this for alias expansion
  - `#alias` command - Create aliases"
  (let ((words (split cmd " ")))
    (if (or (null? words) (= (list-length words) 0))
      nil
      (let ((first-word (car words))
             (args (cdr words)))
        (let ((alias-entry (hash-ref *tintin-aliases* first-word)))
          (if alias-entry
            (cons alias-entry args)
            nil))))))

;; Linear search for pattern aliases
;; Returns: (pattern . match-values) or nil
(defun tintin-match-pattern-alias (cmd)
  "Match command against pattern (wildcard) aliases via linear search.

  ## Parameters
  - `cmd` - Command string to match against patterns

  ## Returns
  Cons cell `(pattern . match-values)` where:
  - `pattern` - Matched alias pattern string
  - `match-values` - List of captured wildcard values

  Returns `nil` if no pattern matches.

  ## Description
  Searches all registered aliases for pattern matches using TinTin++ wildcard
  syntax (`%*`, `%1`-`%99`). This is the fallback after simple alias matching
  fails, using O(n) linear search through all alias patterns.

  **Matching Strategy:**
  1. Get all alias names from hash table
  2. For each alias name:
     - Test if name is pattern (contains wildcards)
     - Convert to regex via `tintin-pattern-to-regex`
     - Test match via `tintin-match-pattern`
     - Return first match found (non-deterministic order)

  **Performance**: O(n) where n = number of aliases - slower than simple matching.

  ## Examples
  ```lisp
  ; Assume pattern alias: \"get %*\" → \"take %1\"
  (hash-set! *tintin-aliases* \"get %*\" '(\"take %1\" 5))

  ; Match pattern alias with wildcard
  (tintin-match-pattern-alias \"get sword\")
  ; => (\"get %*\" . (\"sword\"))  ; Pattern + captured values

  ; Match with multiple wildcards
  ; Alias: \"%1 %2\" → \"say %2 then %1\"
  (tintin-match-pattern-alias \"north south\")
  ; => (\"%1 %2\" . (\"north\" \"south\"))

  ; No match
  (tintin-match-pattern-alias \"look\")
  ; => nil
  ```

  ## Notes
  - **Pattern aliases only**: Wildcard patterns like `%*`, `%1`-`%99`
  - **Linear search**: O(n) performance - use simple aliases for speed
  - **First match wins**: Returns first matching pattern (order non-deterministic)
  - **Regex-based**: Uses PCRE2 for pattern matching
  - **Captures**: Returns wildcard match values for template substitution

  ## See Also
  - `tintin-match-simple-alias` - O(1) hash lookup for non-pattern aliases
  - `tintin-match-pattern` - Tests pattern match
  - `tintin-pattern-to-regex` - Converts TinTin++ to regex
  - `#alias` command - Create pattern aliases"
  (let ((alias-names (hash-keys *tintin-aliases*))
         (matched nil))
    (if (or (null? alias-names) (= (list-length alias-names) 0))
      nil
      (do ((i 0 (+ i 1)))
        ((or (>= i (list-length alias-names)) matched) matched)
        (let* ((pattern (list-ref alias-names i))
                (match-values (tintin-match-pattern pattern cmd)))
          (if match-values
            (set! matched (cons pattern match-values))))))))

;; Replace %0, %1, %2... in template with args or match-values
;; Returns: template with placeholders replaced + unused args appended
(defun tintin-substitute-template (template args match-values)
  (let* ((arg-vals (or match-values args '()))
          (result template)
          (used-args (make-vector (list-length arg-vals) #f)))

    ;; Replace %0 with all arguments
    (if (> (list-length arg-vals) 0)
      (let ((all-args "")
             (old-result result))
        (do ((i 0 (+ i 1)))
          ((>= i (list-length arg-vals)))
          (set! all-args (concat all-args
                           (if (> i 0) " " "")
                           (list-ref arg-vals i))))
        (set! result (string-replace result "%0" all-args))
        ;; Mark all args as used if %0 was replaced
        (if (not (string=? result old-result))
          (do ((i 0 (+ i 1)))
            ((>= i (list-length arg-vals)))
            (vector-set! used-args i #t)))))

    ;; Replace %1, %2, etc.
    (do ((i 0 (+ i 1)))
      ((>= i (list-length arg-vals)))
      (let ((placeholder (concat "%" (number->string (+ i 1))))
             (old-result result))
        (set! result (string-replace result placeholder (list-ref arg-vals i)))
        (if (not (string=? result old-result))
          (vector-set! used-args i #t))))

    ;; Append unused arguments (only for simple aliases with args)
    (if args
      (let ((unused-list '()))
        (do ((j 0 (+ j 1)))
          ((>= j (list-length arg-vals)))
          (if (not (vector-ref used-args j))
            (set! unused-list (cons (list-ref arg-vals j) unused-list))))
        (if (not (eq? unused-list '()))
          (let ((unused-args "")
                 (reversed (reverse unused-list)))
            (do ((k 0 (+ k 1)))
              ((>= k (list-length reversed)))
              (set! unused-args (concat unused-args
                                  (if (> k 0) " " "")
                                  (list-ref reversed k))))
            (set! result (concat result " " unused-args))))))

    result))

;; Expand speedwalk, split by semicolons, recursively process
;; Returns: fully expanded and joined commands
;; KEY: This eliminates ~70 lines of duplication
(defun tintin-expand-and-recurse (result depth)
  ;; Check depth limit (circular alias detection)
  (if (>= depth *tintin-max-alias-depth*)
    (progn
      (tintin-echo (concat "Error: Circular alias detected or depth limit ("
                     (number->string *tintin-max-alias-depth*)
                     ") exceeded\r\n"))
      result)  ; Return unexpanded to stop recursion

    ;; Expand speedwalk only (variables expand per-command for just-in-time evaluation)
    (let ((expanded (tintin-expand-speedwalk result)))
      ;; Split by semicolon
      (let ((split-commands (tintin-split-commands expanded)))
        (if (> (list-length split-commands) 1)
          ;; Multiple commands - recursively process each
          (let ((sub-results '()))
            (do ((j 0 (+ j 1)))
              ((>= j (list-length split-commands)))
              (let ((subcmd (list-ref split-commands j)))
                (if (and (string? subcmd) (not (string=? subcmd "")))
                  ;; Expand variables for THIS command only (just-in-time)
                  (let* ((cmd-with-vars (tintin-expand-variables-fast subcmd))
                          (result (tintin-process-command-internal cmd-with-vars (+ depth 1))))
                    (if (and (string? result) (not (string=? result "")))
                      (set! sub-results (cons result sub-results)))))))
            ;; Join with semicolons
            (if (eq? sub-results '())
              ""
              (let ((reversed (reverse sub-results))
                     (output ""))
                (do ((k 0 (+ k 1)))
                  ((>= k (list-length reversed)) output)
                  (set! output (concat output
                                 (if (> k 0) ";" "")
                                 (list-ref reversed k)))))))
          ;; Single command - recursively process
          (if (> (list-length split-commands) 0)
            (let ((cmd-with-vars (tintin-expand-variables-fast (list-ref split-commands 0))))
              (tintin-process-command-internal cmd-with-vars (+ depth 1)))
            ""))))))

;; ============================================================================
;; ACTION EXECUTION
;; ============================================================================

;; Execute action commands with circular execution detection
;; Sets *tintin-action-executing* flag to prevent infinite loops
;; Processes commands via tintin-process-input and sends each via telnet-send
;; Returns: nil (side effect only)
(defun tintin-execute-action (commands)
  (if (not (string? commands))
    nil
    ;; Check circular execution flag
    (if *tintin-action-executing*
      (progn
        (tintin-echo "Warning: Action triggered during action execution (skipped)\r\n")
        nil)
      (progn
        ;; Set flag to prevent recursion
        (set! *tintin-action-executing* #t)
        ;; Process and send commands
        (condition-case err
          (progn
            (let ((processed (tintin-process-input commands)))
              (if (and (string? processed) (not (string=? processed "")))
                (let ((cmd-list (tintin-split-commands processed)))
                  (do ((i 0 (+ i 1)))
                    ((>= i (list-length cmd-list)))
                    (let ((cmd (list-ref cmd-list i)))
                      (if (and (string? cmd) (not (string=? cmd "")))
                        (condition-case send-err
                          (telnet-send (concat cmd "\r\n"))
                          (error
                            (tintin-echo (concat "Action send failed: "
                                           (error-message send-err) "\r\n"))))))))))
            ;; Clear flag after execution
            (set! *tintin-action-executing* #f))
          (error
            ;; Clear flag on error
            (set! *tintin-action-executing* #f)
            (tintin-echo (concat "Action execution error: "
                           (error-message err) "\r\n"))))))))

;; Test all action patterns against line and execute matches
;; Processes ALL matching actions in priority order (low to high)
;; For each match: extract captures → substitute → expand vars → execute
(defun tintin-trigger-actions-for-line (line)
  "Execute all matching action triggers for a line of server output.

  ## Parameters
  - `line` - Line of text from server (ANSI codes already stripped)

  ## Returns
  `nil` (side effects only - commands sent via `telnet-send`)

  ## Description
  Tests all defined action patterns against incoming server output and
  executes matching actions in priority order (lower priority first).
  Actions are automated responses to server events (e.g., auto-attack when
  someone enters, auto-heal when health drops).

  **Execution Pipeline:**
  1. Retrieve all actions from `*tintin-actions*` hash table
  2. Sort by priority (ascending: 1, 2, ... 5 (default), ... 10)
  3. For each pattern (lowest priority first):
     - Test if pattern matches line
     - If match: Extract capture groups (%1-%99)
     - Substitute captures into command template
     - Expand variables ($varname)
     - Execute via `tintin-execute-action`

  **Priority Semantics (opposite of highlights):**
  - Lower number = executed first (1 before 5)
  - Default priority: 5
  - Use low priority (1-2) for urgent actions (flee, cure)
  - Multiple actions can trigger on same line

  **Capture Groups:**
  - Pattern wildcards %1-%99 extract matched text
  - Substituted into action commands
  - Example: Pattern `\"%1 attacks you\"` + Commands `\"kill %1\"`
    → Incoming: `\"Orc attacks you\"` → Executes: `\"kill Orc\"`

  ## Examples
  ```lisp
  ; Define action to auto-attack
  (hash-set! *tintin-actions* \"%1 enters\"
    (list \"kill %1\" 5))

  ; Trigger on server output
  (tintin-trigger-actions-for-line \"Orc enters\")
  ; Sends: \"kill Orc\\r\\n\" via telnet-send

  ; Multiple actions on same line
  (hash-set! *tintin-actions* \"danger\"
    (list \"flee\" 1))  ; Priority 1 (urgent)
  (hash-set! *tintin-actions* \"danger\"
    (list \"say Help!\" 8))  ; Priority 8 (lower urgency)

  (tintin-trigger-actions-for-line \"You sense danger nearby\")
  ; Executes: \"flee\" first (priority 1), then \"say Help!\" (priority 8)

  ; No matches
  (tintin-trigger-actions-for-line \"The sun rises\")
  ; => nil (no actions triggered)
  ```

  ## Notes
  - Called automatically by `telnet-input-hook` for each line
  - Fast path: Returns immediately if action table empty (no overhead)
  - Circular execution detection via `*tintin-action-executing*` flag
  - Actions sent to server, NOT echoed to terminal (silent execution)
  - ANSI codes stripped before pattern matching (clean text only)
  - All matching actions execute (not just first match)

  ## Common Use Cases
  - **Combat automation**: Auto-attack when enemies appear
  - **Healing**: Auto-heal when health drops below threshold
  - **Alert responses**: Flee on danger, shield on spell cast
  - **Logging**: Record specific events to file
  - **Stat tracking**: Parse health/mana updates into variables

  ## See Also
  - `#action` command - Define action triggers
  - `tintin-execute-action` - Action execution with recursion prevention
  - `tintin-extract-captures` - Extract %1-%99 wildcard values
  - `tintin-substitute-captures` - Substitute captures into commands
  - `tintin-sort-actions-by-priority` - Priority ordering (ascending)"
  (if (or (not (string? line)) (= (hash-count *tintin-actions*) 0))
    nil
    ;; Get all actions sorted by priority (low to high)
    (let ((action-entries (hash-entries *tintin-actions*)))
      (let ((sorted (tintin-sort-actions-by-priority action-entries)))
        ;; Try all patterns and execute all that match
        (do ((i 0 (+ i 1)))
          ((>= i (list-length sorted)))
          (let* ((entry (list-ref sorted i))
                  (pattern (car entry))
                  (data (cdr entry))
                  (commands (car data))
                  (priority (car (cdr data))))
            ;; Check if pattern matches the line
            (if (tintin-match-highlight-pattern pattern line)
              ;; Pattern matches - extract captures and execute
              (let ((captures (tintin-extract-captures pattern line)))
                ;; Substitute captures in commands
                (let ((substituted (tintin-substitute-captures commands captures)))
                  ;; Expand variables
                  (let ((expanded (tintin-expand-variables-fast substituted)))
                    ;; Execute the action
                    (tintin-execute-action expanded)))))))))))

;; Orchestrate alias matching and expansion
;; Returns: expanded command (may contain semicolons)
(defun tintin-expand-alias (cmd depth)
  (let ((expanded-cmd (tintin-expand-variables-fast cmd)))
    ;; Try simple alias match
    (let ((simple-match (tintin-match-simple-alias expanded-cmd)))
      (if simple-match
        ;; Simple alias found
        (let* ((alias-entry (car simple-match))
                (args (cdr simple-match))
                (template (car alias-entry))
                (result (tintin-substitute-template template args nil)))
          (tintin-expand-and-recurse result depth))

        ;; Try pattern alias match
        (let ((pattern-match (tintin-match-pattern-alias expanded-cmd)))
          (if pattern-match
            ;; Pattern alias found
            (let* ((pattern (car pattern-match))
                    (match-values (cdr pattern-match))
                    (alias-data (hash-ref *tintin-aliases* pattern))
                    (template (car alias-data))
                    (result (tintin-substitute-template template nil match-values)))
              (tintin-expand-and-recurse result depth))

            ;; No alias match - just expand speedwalk
            (tintin-expand-speedwalk expanded-cmd)))))))

;; Main command router with depth tracking (internal)
(defun tintin-process-command-internal (cmd depth)
  (if (or (not (string? cmd)) (string=? cmd ""))
    ""
    ;; Check if it's a # command
    (if (tintin-is-command? cmd)
      ;; TinTin++ command - dispatch (main.c handles echoing)
      (let ((cmd-name (tintin-extract-command-name cmd)))
        (if (not cmd-name)
          (progn
            (tintin-echo (concat "Invalid TinTin++ command format: " cmd "\r\n"))
            "")
          (let ((matched (tintin-find-command cmd-name)))
            (if (not matched)
              (progn
                (tintin-echo (concat "Unknown TinTin++ command: #" cmd-name "\r\n"))
                "")
              (tintin-dispatch-command matched cmd)))))
      ;; Regular command - expand aliases
      (tintin-expand-alias cmd depth))))

(defun tintin-process-command (cmd)
  "Process a single TinTin++ command or server command.

  ## Parameters
  - `cmd` - Command string to process

  ## Description
  Processes a single command through the TinTin++ system. This function:
  1. Checks if command starts with `#` (TinTin++ command)
     - If yes: Parses and executes the TinTin++ command (#alias, #variable, etc.)
     - If no: Expands aliases and sends to server
  2. Returns transformed command string (empty if handled internally)

  ## TinTin++ Commands
  Commands starting with `#` are TinTin++ configuration commands:
  - `#alias {name} {commands}` - Define command alias
  - `#variable {name} {value}` - Set variable
  - `#action {pattern} {commands}` - Define trigger on server output
  - `#highlight {pattern} {colors}` - Colorize matching text
  - `#substitute {pattern} {replacement}` - Replace server output text

  See TINTIN.md for full command reference.

  ## Returns
  - For TinTin++ commands (#...): Empty string \"\" (handled internally)
  - For regular commands: Expanded command string (after alias substitution)

  ## Examples
  ```lisp
  ; Define an alias
  (tintin-process-command \"#alias {k} {kill %1}\")
  ; => \"\"

  ; Use the alias (expands to \"kill orc\")
  (tintin-process-command \"k orc\")
  ; => \"kill orc\"

  ; Regular command (no alias match)
  (tintin-process-command \"look\")
  ; => \"look\"
  ```

  ## Notes
  - This function processes a single command (no semicolon splitting)
  - For full input processing with semicolons, use `tintin-process-input`
  - Command echoing is handled automatically
  - Unknown TinTin++ commands print error messages

  ## See Also
  - `tintin-process-input` - Process full input line (with semicolon splitting)
  - `tintin-expand-speedwalk` - Expand speedwalk syntax (3n2e → n;n;n;e;e)"
  (tintin-process-command-internal cmd 0))

;; ============================================================================
;; TEST 7: FULL INPUT PROCESSING
;; ============================================================================

;; Process a full input line (split by semicolons, process each command)
(defun tintin-process-input (input)
  "Process full input line with command separation and TinTin++ expansion.

  ## Parameters
  - `input` - Full input line from user (may contain multiple commands)

  ## Returns
  Semicolon-separated string of expanded commands ready to send to server.
  Returns empty string `\"\"` if input invalid or all commands handled internally.

  ## Description
  Main input processing pipeline for TinTin++ system. Splits input by
  semicolons, processes each command through alias expansion and TinTin++
  command handling, then joins results back with semicolons.

  **Processing Pipeline:**
  1. Split input by semicolons (respects brace nesting via `tintin-split-commands`)
  2. For each command:
     - Pass through `tintin-process-command`
     - Apply alias expansion, variable substitution, speedwalk
     - Handle # commands (returns empty string if # command)
  3. Collect non-empty results
  4. Join with semicolons and return

  **Semicolon Splitting:**
  - Respects brace nesting: `{a;b}` is NOT split
  - Example: `\"n;s;look\"` → 3 commands
  - Example: `\"#alias {go} {n;s;e}\"` → 1 command (braces protect inner semicolons)

  ## Examples
  ```lisp
  ; Multiple commands separated by semicolons
  (tintin-process-input \"n;s;e\")
  ; => \"n;s;e\"  (each command processed individually)

  ; Command with alias expansion
  (hash-set! *tintin-aliases* \"k\" (list \"kill %1\" 5))
  (tintin-process-input \"k orc;s\")
  ; => \"kill orc;s\"

  ; TinTin++ command (returns empty - handled internally)
  (tintin-process-input \"#alias {k} {kill %1}\")
  ; => \"\"

  ; Mixed TinTin++ and regular commands
  (tintin-process-input \"#variable {x} {5};look\")
  ; => \"look\"  (# command filtered out)

  ; Speedwalk expansion
  (tintin-process-input \"3n;look\")
  ; => \"n;n;n;look\"
  ```

  ## Notes
  - Entry point for user input in TinTin++ system
  - Called by `tintin-user-input-hook` for all user input
  - Handles both TinTin++ commands (#...) and regular commands
  - TinTin++ commands return empty string (side effects only)
  - Regular commands return expanded text for server transmission
  - Preserves command order (left to right)

  ## See Also
  - `tintin-process-command` - Process single command
  - `tintin-split-commands` - Split by semicolons (respects braces)
  - `tintin-user-input-hook` - Hook that calls this function
  - `tintin-expand-speedwalk` - Speedwalk expansion (3n2e → n;n;n;e;e)"
  (if (not (string? input))
    ""
    (let ((commands (tintin-split-commands input))
           (results '()))
      ;; Process each command and collect results
      (do ((i 0 (+ i 1)))
        ((>= i (list-length commands)))
        (let ((processed (tintin-process-command (list-ref commands i))))
          (if (and (string? processed) (not (string=? processed "")))
            (set! results (cons processed results)))))
      ;; Reverse and join results with semicolons
      (let ((reversed-results (reverse results))
             (output ""))
        (do ((i 0 (+ i 1)))
          ((>= i (list-length reversed-results)) output)
          (set! output (concat output
                         (if (> i 0) ";" "")
                         (list-ref reversed-results i))))))))

;; ============================================================================
;; USER-INPUT-HOOK INTEGRATION
;; ============================================================================

;; Hook function for user-input-hook integration
;; Signature: (lambda (text cursor-pos) -> string|nil)
;; - text: User input text
;; - cursor-pos: Cursor position (ignored for TinTin++ processing)
;; Returns: nil (hook handles echo/send) or text (when disabled)
;;
;; Hook Contract: Returns nil to indicate all echo/send handled by hook.
;; This hook processes TinTin++ commands and sends each one separately.
;; For example, "s;s" becomes two separate telnet sends: "s" and "s"
(defun tintin-user-input-hook (text cursor-pos)
  (if (not *tintin-enabled*)
    text
    (progn
      ;; Note: main.c already echoes the original input, so we don't echo it here
      (let ((processed (tintin-process-input text))
             (commands nil))
        ;; Split processed output by semicolons
        (set! commands (tintin-split-commands processed))
        ;; Send each command separately
        (do ((i 0 (+ i 1)))
          ((>= i (list-length commands)))
          (let ((cmd (list-ref commands i)))
            (if (and (string? cmd) (not (string=? cmd "")))
              (progn
                ;; Echo expanded command to terminal (if different from original)
                (if (and (string? cmd) (string? text) (not (string=? cmd text)))
                  (tintin-echo (concat cmd "\r\n")))
                ;; Send to telnet server with error handling
                (condition-case err
                  (progn
                    ;; Check if we can send (connected or test mode)
                    (let ((can-send
                            (condition-case err2
                              ;; Try to check connection mode
                              (or (eq? *connection-mode* 'conn)
                                ;; If *connection-mode* undefined (test mode), check if telnet-send exists
                                (and (symbol? 'telnet-send) #t))
                              ;; If *connection-mode* not defined, we're in test mode
                              (error #t))))
                      (if can-send
                        ;; Send the command
                        (telnet-send (concat cmd "\r\n"))
                        ;; Not connected
                        (tintin-echo "\r\n*** Not connected ***\r\n"))))
                  ;; Catch any send errors
                  (error
                    (tintin-echo (concat "\r\n*** Send failed: "
                                   (error-message err) " ***\r\n")))))))))
      ;; Return nil to indicate hook handled everything (proper contract)
      ())))
;; Toggle TinTin++ processing on/off
(defun tintin-toggle! ()
  "Toggle TinTin++ processing on or off.

  ## Description
  Switches TinTin++ between enabled and disabled states. When enabled, user input
  is processed for TinTin++ commands (#alias, #variable, etc.), command separation
  (semicolons), speedwalking (3n2e), and alias expansion. When disabled, input is
  sent directly to the server without processing.

  ## Returns
  `#t` if TinTin++ is now enabled, `#f` if now disabled.

  ## Examples
  ```lisp
  (tintin-toggle!)     ; Toggles state and prints confirmation
  ; => #t (if was disabled) or #f (if was enabled)
  ```

  ## See Also
  - `tintin-enable!` - Enable TinTin++ processing
  - `tintin-disable!` - Disable TinTin++ processing"
  (set! *tintin-enabled* (not *tintin-enabled*))
  (tintin-echo (concat "TinTin++ "
                 (if *tintin-enabled* "enabled" "disabled")
                 "\r\n"))
  *tintin-enabled*)

(defun tintin-enable! ()
  "Enable TinTin++ processing.

  ## Description
  Activates TinTin++ command processing. User input will be processed for:
  - TinTin++ commands: #alias, #variable, #action, #highlight, etc.
  - Command separation: Semicolons split commands (e.g., `n;s;e;w`)
  - Speedwalking: Numeric prefixes expand directions (e.g., `3n2e` → `n;n;n;e;e`)
  - Alias expansion: Defined aliases are expanded before sending to server

  ## Returns
  Always returns `#t`.

  ## Examples
  ```lisp
  (tintin-enable!)     ; Enables TinTin++ and prints confirmation
  ; => #t
  ```

  ## Notes
  TinTin++ is automatically enabled when tintin.lisp is loaded.

  ## See Also
  - `tintin-disable!` - Disable TinTin++ processing
  - `tintin-toggle!` - Toggle TinTin++ on/off"
  (set! *tintin-enabled* #t)
  (tintin-echo "TinTin++ enabled\r\n")
  #t)

(defun tintin-disable! ()
  "Disable TinTin++ processing.

  ## Description
  Deactivates TinTin++ command processing. User input will be sent directly to
  the server without any TinTin++ transformations. Semicolons, speedwalk syntax,
  and aliases will not be processed.

  ## Returns
  Always returns `#f`.

  ## Examples
  ```lisp
  (tintin-disable!)    ; Disables TinTin++ and prints confirmation
  ; => #f
  ```

  ## Use Cases
  - Temporarily bypass TinTin++ when entering literal text (e.g., writing in-game)
  - Debugging: Verify whether issues are caused by TinTin++ processing

  ## See Also
  - `tintin-enable!` - Enable TinTin++ processing
  - `tintin-toggle!` - Toggle TinTin++ on/off"
  (set! *tintin-enabled* #f)
  (tintin-echo "TinTin++ disabled\r\n")
  #f)

;; ============================================================================
;; UTILITY FUNCTIONS (REFACTORED)
;; ============================================================================

;; Echo text to terminal if available
;; Used to centralize terminal output across all command handlers
(defun tintin-echo (text)
  (if (symbol? 'terminal-echo)
    (terminal-echo text))
  nil)

;; Report syntax error for a command
;; Returns empty string to maintain handler contract
(defun tintin-syntax-error (syntax-help)
  (tintin-echo (concat "Syntax error: " syntax-help "\r\n"))
  "")

;; Strip outer braces from a string if present
;; Example: "{text}" → "text", "text" → "text", "{a{b}c}" → "a{b}c"
(defun tintin-strip-braces (str)
  (if (not (string? str))
    str
    (let ((len (string-length str)))
      (if (and (> len 1)
            (char=? (string-ref str 0) #\{)
            (char=? (string-ref str (- len 1)) #\}))
        (substring str 1 (- len 1))
        str))))

;; Sort alias entries alphabetically by name
;; Input: list of (name . (commands priority)) pairs
;; Output: sorted list alphabetically by name
(defun tintin-sort-aliases-alphabetically (alias-list)
  "Sort alias entries alphabetically by name for display.

  ## Parameters
  - `alias-list` - List of alias entries: `((name . (commands priority)) ...)`

  ## Returns
  Sorted list in alphabetical order by alias name. Returns empty list `()` if
  input is empty or invalid.

  ## Description
  Sorts aliases alphabetically by name for consistent display in table output.
  Used by `tintin-list-aliases` to present aliases in predictable order.

  **Sorting Rules:**
  - Case-sensitive string comparison via `string<?`
  - Algorithm: Insertion sort (O(n²) but stable and simple)
  - Stable sort: preserves relative order of equal names (shouldn't occur)

  ## Examples
  ```lisp
  (define aliases
    (list (cons \"north\" (list \"n\" 5))
          (cons \"attack\" (list \"kill %1\" 5))
          (cons \"east\" (list \"e\" 5))))

  (tintin-sort-aliases-alphabetically aliases)
  ; => ((\"attack\" . (\"kill %1\" 5))
  ;     (\"east\" . (\"e\" 5))
  ;     (\"north\" . (\"n\" 5)))

  ; Empty input
  (tintin-sort-aliases-alphabetically '())
  ; => ()
  ```

  ## Notes
  - Used internally by `tintin-list-aliases` for table display
  - Case-sensitive: \"North\" comes before \"north\"
  - Does NOT affect execution order (aliases matched by exact name lookup)
  - Purely for user-friendly display

  ## See Also
  - `tintin-list-aliases` - Displays sorted alias table
  - `tintin-sort-highlights-alphabetically` - Similar for highlights
  - `tintin-sort-actions-alphabetically` - Similar for actions"
  (if (or (null? alias-list) (= (list-length alias-list) 0))
    '()
    ;; Simple insertion sort by name
    (let ((sorted '()))
      (do ((remaining alias-list (cdr remaining)))
        ((null? remaining) sorted)
        (let ((entry (car remaining))
               (name (car (car remaining))))
          ;; Insert entry in alphabetically sorted position
          (set! sorted (tintin-insert-alias-alphabetically entry name sorted)))))))

;; Helper: Insert alias entry into sorted list alphabetically by name
(defun tintin-insert-alias-alphabetically (entry name sorted-list)
  (if (null? sorted-list)
    (list entry)
    (let ((first-entry (car sorted-list))
           (first-name (car (car sorted-list))))
      (if (string<? name first-name)
        ;; Insert before first entry
        (cons entry sorted-list)
        ;; Insert later in list
        (cons first-entry
          (tintin-insert-alias-alphabetically entry name (cdr sorted-list)))))))

;; Sort highlight entries alphabetically by pattern
;; Input: list of (pattern . (fg-color bg-color priority)) pairs
;; Output: sorted list alphabetically by pattern
(defun tintin-sort-highlights-alphabetically (highlight-list)
  "Sort highlight entries alphabetically by pattern for display.

  ## Parameters
  - `highlight-list` - List of highlight entries: `((pattern . (fg bg priority)) ...)`

  ## Returns
  Sorted list in alphabetical order by pattern. Returns empty list `()` if
  input is empty or invalid.

  ## Description
  Sorts highlights alphabetically by pattern for consistent display in table
  output. Used by `tintin-list-highlights` to present highlights in predictable
  order.

  **Sorting Rules:**
  - Case-sensitive string comparison via `string<?`
  - Algorithm: Insertion sort (O(n²) but stable and simple)
  - Stable sort: preserves relative order of equal patterns (shouldn't occur)

  ## Examples
  ```lisp
  (define highlights
    (list (cons \"dragon\" (list \"red\" nil 10))
          (cons \"attack\" (list \"yellow\" nil 5))
          (cons \"gold\" (list \"<ff0>\" nil 5))))

  (tintin-sort-highlights-alphabetically highlights)
  ; => ((\"attack\" . (\"yellow\" nil 5))
  ;     (\"dragon\" . (\"red\" nil 10))
  ;     (\"gold\" . (\"<ff0>\" nil 5)))

  ; Empty input
  (tintin-sort-highlights-alphabetically '())
  ; => ()
  ```

  ## Notes
  - Used internally by `tintin-list-highlights` for table display
  - Case-sensitive: \"Dragon\" comes before \"dragon\"
  - Does NOT affect execution order (use `tintin-sort-highlights-by-priority`)
  - Purely for user-friendly display

  ## See Also
  - `tintin-list-highlights` - Displays sorted highlight table
  - `tintin-sort-highlights-by-priority` - Sorts for pattern matching order
  - `tintin-sort-aliases-alphabetically` - Similar for aliases"
  (if (or (null? highlight-list) (= (list-length highlight-list) 0))
    '()
    ;; Simple insertion sort by pattern
    (let ((sorted '()))
      (do ((remaining highlight-list (cdr remaining)))
        ((null? remaining) sorted)
        (let ((entry (car remaining))
               (pattern (car (car remaining))))
          ;; Insert entry in alphabetically sorted position
          (set! sorted (tintin-insert-highlight-alphabetically entry pattern sorted)))))))

;; Helper: Insert highlight entry into sorted list alphabetically by pattern
(defun tintin-insert-highlight-alphabetically (entry pattern sorted-list)
  (if (null? sorted-list)
    (list entry)
    (let ((first-entry (car sorted-list))
           (first-pattern (car (car sorted-list))))
      (if (string<? pattern first-pattern)
        ;; Insert before first entry
        (cons entry sorted-list)
        ;; Insert later in list
        (cons first-entry
          (tintin-insert-highlight-alphabetically entry pattern (cdr sorted-list)))))))

;; Sort action entries alphabetically by pattern
;; Input: list of (pattern . (commands-string priority)) pairs
;; Output: sorted list alphabetically by pattern
(defun tintin-sort-actions-alphabetically (action-list)
  "Sort action entries alphabetically by pattern for display.

  ## Parameters
  - `action-list` - List of action entries: `((pattern . (commands priority)) ...)`

  ## Returns
  Sorted list in alphabetical order by pattern. Returns empty list `()` if
  input is empty or invalid.

  ## Description
  Sorts actions alphabetically by pattern for consistent display in table output.
  Used by `tintin-list-actions` to present actions in predictable order.

  **Sorting Rules:**
  - Case-sensitive string comparison via `string<?`
  - Algorithm: Insertion sort (O(n²) but stable and simple)
  - Stable sort: preserves relative order of equal patterns (shouldn't occur)

  ## Examples
  ```lisp
  (define actions
    (list (cons \"You are hungry\" (list \"eat bread\" 5))
          (cons \"%1 attacks\" (list \"kill %1\" 1))
          (cons \"danger\" (list \"flee\" 1))))

  (tintin-sort-actions-alphabetically actions)
  ; => ((\"%1 attacks\" . (\"kill %1\" 1))
  ;     (\"danger\" . (\"flee\" 1))
  ;     (\"You are hungry\" . (\"eat bread\" 5)))

  ; Empty input
  (tintin-sort-actions-alphabetically '())
  ; => ()
  ```

  ## Notes
  - Used internally by `tintin-list-actions` for table display
  - Case-sensitive: \"Danger\" comes before \"danger\"
  - Does NOT affect execution order (use `tintin-sort-actions-by-priority`)
  - Purely for user-friendly display

  ## See Also
  - `tintin-list-actions` - Displays sorted action table
  - `tintin-sort-actions-by-priority` - Sorts for execution order
  - `tintin-sort-highlights-alphabetically` - Similar for highlights"
  (if (or (null? action-list) (= (list-length action-list) 0))
    '()
    ;; Simple insertion sort by pattern
    (let ((sorted '()))
      (do ((remaining action-list (cdr remaining)))
        ((null? remaining) sorted)
        (let ((entry (car remaining))
               (pattern (car (car remaining))))
          ;; Insert entry in alphabetically sorted position
          (set! sorted (tintin-insert-action-alphabetically entry pattern sorted)))))))

;; Helper: Insert action entry into sorted list alphabetically by pattern
(defun tintin-insert-action-alphabetically (entry pattern sorted-list)
  (if (null? sorted-list)
    (list entry)
    (let ((first-entry (car sorted-list))
           (first-pattern (car (car sorted-list))))
      (if (string<? pattern first-pattern)
        ;; Insert before first entry
        (cons entry sorted-list)
        ;; Insert later in list
        (cons first-entry
          (tintin-insert-action-alphabetically entry pattern (cdr sorted-list)))))))

;; ============================================================================
;; TABLE FORMATTING UTILITIES
;; ============================================================================

;; Pad string to specified width with spaces
(defun tintin-pad-string (str width)
  "Pad string to specified width with trailing spaces.

  ## Parameters
  - `str` - String to pad (may contain ANSI escape sequences)
  - `width` - Target visual width (number of display characters)

  ## Returns
  String padded to `width` with trailing spaces. Returns empty string `\"\"` if
  input is invalid. If string is already >= width, returns unchanged.

  ## Description
  Pads a string to the specified visual width by appending spaces. Uses
  `tintin-visual-length` to correctly handle ANSI escape codes, ensuring
  colored text aligns properly in table columns.

  **Padding Behavior:**
  - Visual length < width: Adds spaces to reach width
  - Visual length >= width: Returns string unchanged (no truncation)
  - ANSI codes preserved: Color formatting maintained

  ## Examples
  ```lisp
  ; Plain text padding
  (tintin-pad-string \"hello\" 10)
  ; => \"hello     \"  ; 5 spaces added

  ; Already wide enough
  (tintin-pad-string \"hello\" 3)
  ; => \"hello\"  ; No padding, no truncation

  ; With ANSI codes (preserved)
  (tintin-pad-string \"\\033[31mRED\\033[0m\" 10)
  ; => \"\\033[31mRED\\033[0m       \"  ; Pads to 10 visual chars

  ; Empty string
  (tintin-pad-string \"\" 5)
  ; => \"     \"  ; 5 spaces

  ; Invalid input
  (tintin-pad-string nil 10)
  ; => \"\"
  ```

  ## Notes
  - Used by `tintin-draw-row` for table cell alignment
  - Does NOT truncate strings that exceed width
  - Preserves ANSI escape codes (visual-length aware)
  - Padding always on the right (trailing spaces)

  ## See Also
  - `tintin-visual-length` - Computes display width
  - `tintin-draw-row` - Uses this for cell formatting
  - `tintin-wrap-text` - For text that exceeds column width"
  (if (not (string? str))
    ""
    (let ((visual-len (tintin-visual-length str)))
      (let ((padding-needed (- width visual-len)))
        (if (<= padding-needed 0)
          str
          (let ((result str))
            (do ((i 0 (+ i 1)))
              ((>= i padding-needed) result)
              (set! result (concat result " ")))))))))

;; Repeat a string N times
(defun tintin-repeat-string (str count)
  "Repeat a string N times.

  ## Parameters
  - `str` - String to repeat
  - `count` - Number of times to repeat (non-negative integer)

  ## Returns
  New string containing `str` repeated `count` times. Returns empty string `\"\"`
  if count is 0 or negative.

  ## Description
  Builds a new string by concatenating `str` to itself `count` times. Used
  internally by table border drawing to create horizontal line segments.

  ## Examples
  ```lisp
  ; Basic repetition
  (tintin-repeat-string \"─\" 5)
  ; => \"─────\"

  ; Character repetition
  (tintin-repeat-string \"*\" 3)
  ; => \"***\"

  ; Multi-character string
  (tintin-repeat-string \"abc\" 3)
  ; => \"abcabcabc\"

  ; Zero count
  (tintin-repeat-string \"x\" 0)
  ; => \"\"

  ; Negative count (treated as 0)
  (tintin-repeat-string \"x\" -5)
  ; => \"\"
  ```

  ## Notes
  - Simple implementation: O(n) concatenation in loop
  - Used primarily for horizontal border lines in tables
  - Does NOT validate string parameter (assumes valid)

  ## See Also
  - `tintin-draw-border` - Uses this to draw horizontal lines"
  (let ((result ""))
    (do ((i 0 (+ i 1)))
      ((>= i count) result)
      (set! result (concat result str)))))

;; Get visual length of string, excluding ANSI escape sequences
;; This is critical for table alignment with colored text
(defun tintin-visual-length (str)
  "Calculate visual display length of string, excluding ANSI escape codes.

  ## Parameters
  - `str` - String to measure (may contain ANSI escape sequences)

  ## Returns
  Integer representing the number of visible characters (display width).
  Returns `0` if string is invalid.

  ## Description
  Computes the visual length of a string by removing all ANSI escape sequences
  before measurement. This is critical for table alignment when cells contain
  colored text, as ANSI codes don't occupy display space but do occupy bytes.

  **ANSI Pattern Removed:**
  - `\\033[...m` - Color codes, text attributes, cursor movements
  - Matches format: ESC `[` digits/semicolons `m`

  **Use Cases:**
  - Table column alignment with colored text
  - Padding calculations for formatted output
  - Word wrap boundary detection

  ## Examples
  ```lisp
  ; Plain text
  (tintin-visual-length \"hello\")
  ; => 5

  ; With ANSI color codes
  (tintin-visual-length \"\\033[1;31mRED\\033[0m\")
  ; => 3  ; \"RED\" is 3 chars, ANSI codes don't count

  ; Multiple ANSI codes
  (tintin-visual-length \"\\033[1mbold\\033[0m and \\033[32mgreen\\033[0m\")
  ; => 14  ; \"bold and green\" without ANSI codes

  ; Empty/invalid
  (tintin-visual-length \"\")
  ; => 0
  (tintin-visual-length nil)
  ; => 0
  ```

  ## Notes
  - Used extensively by table formatting functions
  - Prevents alignment issues when mixing colored and plain text
  - Only removes ANSI SGR (Select Graphic Rendition) sequences
  - Does not handle other escape sequences (cursor control, etc.)

  ## See Also
  - `tintin-pad-string` - Uses visual-length for padding
  - `tintin-wrap-text` - Uses visual-length for line breaking
  - `tintin-draw-border` - Column widths based on visual-length"
  (if (not (string? str))
    0
    (let ((ansi-pattern "\\033\\[[0-9;]*m"))
      (string-length (regex-replace-all ansi-pattern str "")))))

;; Find best position to break text near width boundary
;; Returns position to break at (searches backwards for space/hyphen)
(defun tintin-find-break-point (text width)
  (if (<= (tintin-visual-length text) width)
    (string-length text)
    (let* ((text-len (string-length text))
            (start-pos (if (< width text-len) width (- text-len 1))))
      ;; Search backwards from width (or text end) for space or hyphen
      (do ((i start-pos (- i 1)))
        ((or (< i 0)
           (and (< i text-len)  ; Bounds check
             (let ((ch (string-ref text i)))
               (or (char=? ch #\space)
                 (char=? ch #\-)
                 (char=? ch #\newline)))))
          (if (< i 0)
            (if (< width text-len) width text-len)  ; Hard break at width or text end
            (+ i 1)))))))  ; Break after space/hyphen

;; Wrap text to fit within width, returning list of lines
(defun tintin-wrap-text (text width)
  "Wrap text to fit within specified width, breaking at word boundaries.

  ## Parameters
  - `text` - Text to wrap (may contain ANSI escape sequences)
  - `width` - Maximum visual width per line (display characters)

  ## Returns
  List of wrapped lines. Returns `(\"\" )` if text is empty/invalid, or `(text)`
  if width is invalid (≤ 0).

  ## Description
  Breaks text into multiple lines to fit within the specified visual width,
  preferring to break at word boundaries (spaces, hyphens, newlines). Used by
  table formatting to wrap long cell content across multiple display lines.

  **Breaking Rules:**
  1. Prefer space, hyphen, or newline boundaries
  2. Search backwards from width limit for break point
  3. Hard break at width if no boundary found
  4. Strip trailing spaces from each line
  5. Skip leading spaces on continuation lines
  6. Always make progress (minimum 1 char per line)

  **ANSI Code Handling:**
  - Uses `tintin-visual-length` for width calculations
  - Preserves color codes in wrapped lines
  - Width measured in display characters (not bytes)

  ## Examples
  ```lisp
  ; Short text (no wrapping needed)
  (tintin-wrap-text \"hello\" 10)
  ; => (\"hello\")

  ; Text exactly at width
  (tintin-wrap-text \"hello world\" 11)
  ; => (\"hello world\")

  ; Text exceeding width (breaks at space)
  (tintin-wrap-text \"hello world\" 8)
  ; => (\"hello\" \"world\")

  ; Multiple breaks
  (tintin-wrap-text \"the quick brown fox\" 10)
  ; => (\"the quick\" \"brown fox\")

  ; No word boundaries (hard break)
  (tintin-wrap-text \"verylongword\" 5)
  ; => (\"veryl\" \"ongwo\" \"rd\")

  ; With ANSI codes (preserved)
  (tintin-wrap-text \"\\033[31mlong red text\\033[0m\" 8)
  ; => (\"\\033[31mlong red\\033[0m\" \"\\033[31mtext\\033[0m\")

  ; Empty/invalid
  (tintin-wrap-text \"\" 10)
  ; => (\"\")
  (tintin-wrap-text \"text\" 0)
  ; => (\"text\")  ; Invalid width - return as-is
  ```

  ## Notes
  - Recursive implementation: wraps first line, then recurses on remainder
  - Prevents infinite loops: always makes progress (min 1 char)
  - Preserves ANSI codes but bases width on visible characters
  - Used by `tintin-draw-row` for multi-line table cells

  ## See Also
  - `tintin-find-break-point` - Finds optimal break position
  - `tintin-visual-length` - Measures display width
  - `tintin-draw-row` - Uses this for cell wrapping"
  (if (or (not (string? text)) (= (tintin-visual-length text) 0))
    '("")
    ;; Guard against invalid width
    (if (<= width 0)
      (list text)  ; Return as-is if width is too small
      (if (<= (tintin-visual-length text) width)
        (list text)
        ;; Find break point and split
        (let* ((break-pos (tintin-find-break-point text width))
                ;; Ensure we always make progress (at least 1 char)
                (safe-break-pos (if (<= break-pos 0) 1 break-pos))
                (line1-raw (substring text 0 safe-break-pos))
                ;; Strip trailing spaces from line1 (they get added as padding later)
                (line1-len (string-length line1-raw))
                (line1-end line1-len)
                (line1 (progn
                         ;; Find last non-space character
                         (do ()
                           ((or (<= line1-end 0)
                              (not (char=? (string-ref line1-raw (- line1-end 1)) #\space))))
                           (set! line1-end (- line1-end 1)))
                         (if (= line1-end line1-len)
                           line1-raw  ; No trailing spaces
                           (substring line1-raw 0 line1-end))))  ; Strip trailing spaces
                (rest-start safe-break-pos)
                ;; Skip leading space in rest
                (rest-start-adj (if (and (< rest-start (string-length text))
                                      (char=? (string-ref text rest-start) #\space))
                                  (+ rest-start 1)
                                  rest-start)))
          (if (>= rest-start-adj (string-length text))
            (list line1)
            (let ((rest (substring text rest-start-adj (string-length text))))
              (cons line1 (tintin-wrap-text rest width)))))))))

;; Draw generic table border for any number of columns
;; widths: list of column widths
;; position: 'top, 'middle, or 'bottom
;; Returns: border string with Unicode box-drawing characters
(defun tintin-draw-border (widths position)
  "Draw Unicode box-drawing border for table with specified column widths.

  ## Parameters
  - `widths` - List of column widths (integers)
  - `position` - Border position: `'top`, `'middle`, or `'bottom`

  ## Returns
  Border string with Unicode box-drawing characters and CRLF (`\\r\\n`).
  Returns empty string `\"\"` if widths list is empty or invalid.

  ## Description
  Generates a horizontal table border using Unicode box-drawing characters.
  The border style varies based on position (top, middle separator, or bottom).

  **Unicode Characters Used:**
  - **Top**: `┌─┬─┐` (corners and T-junctions)
  - **Middle**: `├─┼─┤` (T-junctions and cross)
  - **Bottom**: `└─┴─┘` (corners and inverted T)

  **Border Structure:**
  ```
  left-char + (─ × width1 + 2) + junction + (─ × width2 + 2) + ... + right-char
  ```
  Width padding: +2 per column (1 space on each side)

  ## Examples
  ```lisp
  ; Top border for 2 columns (width 5, 10)
  (tintin-draw-border '(5 10) 'top)
  ; => \"┌───────┬────────────┐\\r\\n\"
  ;     └─5─┘   └───10───┘
  ;      +2       +2 padding

  ; Middle separator
  (tintin-draw-border '(5 10) 'middle)
  ; => \"├───────┼────────────┤\\r\\n\"

  ; Bottom border
  (tintin-draw-border '(5 10) 'bottom)
  ; => \"└───────┴────────────┘\\r\\n\"

  ; Three columns
  (tintin-draw-border '(3 4 5) 'top)
  ; => \"┌─────┬──────┬───────┐\\r\\n\"

  ; Empty/invalid
  (tintin-draw-border '() 'top)
  ; => \"\"
  ```

  ## Notes
  - Used by `tintin-print-table` for all table borders
  - Each column gets +2 width (1 space padding on each side)
  - Always includes trailing `\\r\\n` (CRLF line ending)
  - Default to middle style if position invalid
  - Supports arbitrary number of columns

  ## See Also
  - `tintin-print-table` - Main table printer using this
  - `tintin-draw-row` - Draws data rows between borders
  - `tintin-repeat-string` - Used internally for horizontal lines"
  (if (or (null? widths) (= (list-length widths) 0))
    ""
    (let* ((chars (cond ((eq? position 'top) '("┌" "┬" "┐"))
                    ((eq? position 'middle) '("├" "┼" "┤"))
                    ((eq? position 'bottom) '("└" "┴" "┘"))
                    (#t '("├" "┼" "┤"))))  ; default to middle
            (left (car chars))
            (middle (list-ref chars 1))
            (right (list-ref chars 2))
            (line left))
      ;; Build border: left + (─*width1) + middle + (─*width2) + ... + right
      (do ((i 0 (+ i 1)))
        ((>= i (list-length widths)))
        (let ((width (list-ref widths i)))
          ;; Add horizontal line segment
          (set! line (concat line "─" (tintin-repeat-string "─" width) "─"))
          ;; Add junction or right cap
          (if (< (+ i 1) (list-length widths))
            (set! line (concat line middle))
            (set! line (concat line right)))))
      (concat line "\r\n"))))

;; DEPRECATED: Old function kept for compatibility
;; Use tintin-draw-border instead
(defun tintin-draw-table-border (widths style)
  (tintin-draw-border widths style))

;; Draw table row with wrapping support
;; cells: list of cell values (strings)
;; widths: list of column widths
;; Returns: list of display lines for this logical row
(defun tintin-draw-row (cells widths)
  "Draw a table row with multi-line cell support and Unicode borders.

  ## Parameters
  - `cells` - List of cell values (strings, may contain ANSI codes)
  - `widths` - List of column widths (integers, one per cell)

  ## Returns
  List of display lines (strings with `\\r\\n` endings). Returns `(\"\" )`
  if cells or widths are empty/invalid.

  ## Description
  Renders a single logical table row as one or more display lines. Handles
  text wrapping within cells when content exceeds column width. All cells in
  a row are aligned vertically, with shorter cells padded with spaces.

  **Two-Phase Process:**

  1. **Wrap Phase**: Each cell is wrapped to its column width using
     `tintin-wrap-text`. Tracks maximum line count across all columns.

  2. **Build Phase**: Constructs display lines by combining wrapped cell
     content from all columns. Shorter cells are padded with spaces on lines
     where they have no content.

  **Output Format:**
  ```
  │ cell1-line1 │ cell2-line1 │ cell3-line1 │\\r\\n
  │ cell1-line2 │             │ cell3-line2 │\\r\\n
  ```

  ## Examples
  ```lisp
  ; Single-line cells (no wrapping)
  (tintin-draw-row '(\"Alice\" \"30\" \"Engineer\") '(10 5 15))
  ; => (\"│ Alice      │ 30    │ Engineer        │\\r\\n\")

  ; Multi-line cell (name wraps to 2 lines)
  (tintin-draw-row '(\"Very Long Name\" \"25\" \"Dev\") '(8 5 10))
  ; => (\"│ Very     │ 25    │ Dev        │\\r\\n\"
  ;     \"│ Long     │       │            │\\r\\n\"
  ;     \"│ Name     │       │            │\\r\\n\")

  ; With ANSI color codes (preserved)
  (tintin-draw-row '(\"\\\\033[31mRed\\\\033[0m\" \"Blue\") '(10 10))
  ; => (\"│ \\\\033[31mRed\\\\033[0m       │ Blue       │\\r\\n\")

  ; Empty/invalid
  (tintin-draw-row '() '(10 10))
  ; => (\"\")
  (tintin-draw-row '(\"text\") '())
  ; => (\"\")
  ```

  ## Notes
  - Uses `tintin-wrap-text` for word-wrapping within cells
  - Uses `tintin-pad-string` for ANSI-aware padding
  - Unicode borders: `│` for column separators
  - Each line ends with `\\r\\n` (CRLF)
  - Cells and widths lists must have same length (no validation)
  - Returns list of lines (not single concatenated string)

  ## See Also
  - `tintin-wrap-text` - Wraps cell content to column width
  - `tintin-pad-string` - Pads cell content with trailing spaces
  - `tintin-print-table` - Main table printer using this
  - `tintin-draw-border` - Draws horizontal borders between rows"
  (if (or (null? cells) (null? widths))
    '("")
    (let ((wrapped-cells '())
           (max-lines 0))
      ;; Step 1: Wrap each cell to its column width
      (do ((i 0 (+ i 1)))
        ((>= i (list-length cells)))
        (let ((cell (list-ref cells i))
               (width (list-ref widths i)))
          (let ((wrapped (tintin-wrap-text cell width)))
            (set! wrapped-cells (cons wrapped wrapped-cells))
            ;; Track max lines needed
            (if (> (list-length wrapped) max-lines)
              (set! max-lines (list-length wrapped))))))

      ;; Reverse to restore original order
      (set! wrapped-cells (reverse wrapped-cells))

      ;; Step 2: Build each display line
      (let ((result '()))
        (do ((line-idx 0 (+ line-idx 1)))
          ((>= line-idx max-lines) (reverse result))
          (let ((line "│ "))
            ;; Build this display line from all columns
            (do ((col-idx 0 (+ col-idx 1)))
              ((>= col-idx (list-length cells)))
              (let* ((wrapped-cell (list-ref wrapped-cells col-idx))
                      (width (list-ref widths col-idx))
                      ;; Get text for this line (or empty if this cell has fewer lines)
                      (text (if (< line-idx (list-length wrapped-cell))
                              (list-ref wrapped-cell line-idx)
                              ""))
                      (padded (tintin-pad-string text width)))
                (set! line (concat line padded))
                ;; Add separator or end cap
                (if (< (+ col-idx 1) (list-length cells))
                  (set! line (concat line " │ "))
                  (set! line (concat line " │\r\n")))))
            (set! result (cons line result))))))))

;; DEPRECATED: Old function kept for compatibility
;; Use tintin-draw-row instead
(defun tintin-draw-table-row (values widths)
  (let ((lines (tintin-draw-row values widths)))
    ;; Return first line only (no wrapping in old version)
    (if (null? lines)
      ""
      (car lines))))

;; Calculate optimal column widths that fit within max-width
;; data: list of lists (rows x columns)
;; max-width: terminal width in characters (from terminal-info)
;; min-col-width: minimum width per column (default 8)
;; Returns: list of column widths that fit within max-width
(defun tintin-calculate-optimal-widths (data max-width min-col-width)
  "Calculate optimal column widths that fit within terminal width.

  ## Parameters
  - `data` - List of lists (all rows including headers): `((row1...) (row2...)...)`
  - `max-width` - Terminal width in characters (from `terminal-info`)
  - `min-col-width` - Minimum width per column (default: 8)

  ## Returns
  List of column widths (integers) that fit within `max-width`. Returns empty
  list `()` if data is empty/invalid.

  ## Description
  Calculates optimal column widths for table display by balancing content
  width, terminal constraints, and readability. Uses ANSI-aware measurement
  via `tintin-visual-length` to handle colored text correctly.

  **Three-Phase Algorithm:**

  **Phase 1: Content Analysis**
  - Scans ALL rows (including headers) to find maximum visual width per column
  - Accounts for ANSI escape codes (doesn't count them in width)

  **Phase 2: Width Calculation**
  - Calculates total table width: content + borders + separators
  - Border width: 4 chars (`│ ` left + ` │` right)
  - Separator width: 3 chars per separator (` │ `)
  - Formula: `sum(widths) + 4 + (num_cols - 1) × 3`

  **Phase 3: Scaling (if needed)**
  - If table fits: Return content widths as-is
  - If table exceeds max-width: Two-pass proportional scaling
    - **First pass**: Scale proportionally, enforce `min-col-width`
    - **Second pass**: If still over budget, scale again with absolute min (1)

  **Width Priority:**
  1. Content width (if fits)
  2. Proportionally scaled width (respects min-col-width)
  3. Aggressively scaled width (absolute min: 1 char)

  ## Examples
  ```lisp
  ; Content fits within terminal
  (tintin-calculate-optimal-widths
    '((\"Name\" \"Age\" \"Role\")
      (\"Alice\" \"30\" \"Engineer\")
      (\"Bob\" \"25\" \"Designer\"))
    80  ; Terminal width
    8)  ; Min column width
  ; => (5 3 8)  ; Actual content widths

  ; Content too wide - proportional scaling
  (tintin-calculate-optimal-widths
    '((\"Very Long Name\" \"Age\" \"Very Long Role Title\")
      (\"Alice Smith\" \"30\" \"Senior Software Engineer\"))
    40  ; Narrow terminal
    5)  ; Min column width
  ; => (10 5 10)  ; Scaled to fit within 40 chars

  ; Empty data
  (tintin-calculate-optimal-widths '() 80 8)
  ; => ()
  ```

  ## Notes
  - Uses `tintin-visual-length` for ANSI-aware width measurement
  - Respects `min-col-width` in first scaling pass
  - Absolute minimum: 1 char per column (headers may truncate)
  - Two-pass scaling prevents over-constraining narrow terminals
  - Called by `tintin-print-table` with terminal width from `terminal-info`

  ## Border Width Calculation
  For N columns:
  - Left border: `│ ` (2 chars)
  - Right border: ` │` (2 chars)
  - Separators: ` │ ` × (N-1) (3 chars each)
  - **Total non-content**: `4 + 3×(N-1)` chars

  ## See Also
  - `tintin-print-table` - Main table printer using this
  - `tintin-visual-length` - ANSI-aware width measurement
  - `terminal-info` - Provides terminal dimensions"
  (if (or (null? data) (= (list-length data) 0))
    '()
    (let ((num-cols (list-length (car data)))
           (col-maxes (make-vector (list-length (car data)) 0)))
      ;; Step 1: Find max visual width for each column
      (do ((i 0 (+ i 1)))
        ((>= i (list-length data)))
        (let ((row (list-ref data i)))
          (do ((j 0 (+ j 1)))
            ((>= j (list-length row)))
            (let ((cell (list-ref row j)))
              (let ((cell-width (tintin-visual-length cell))
                     (current-max (vector-ref col-maxes j)))
                (if (> cell-width current-max)
                  (vector-set! col-maxes j cell-width)))))))

      ;; Step 2: Calculate total needed width
      ;; formula: sum(widths) + (num-cols + 1) + (num-cols - 1) * 3
      (let ((content-width 0))
        ;; Sum up column widths
        (do ((k 0 (+ k 1)))
          ((>= k num-cols))
          (set! content-width (+ content-width (vector-ref col-maxes k))))

        (let* ((border-width 4)  ; Left "│ " (2) + right " │" (2)
                (separator-width (* (- num-cols 1) 3))
                (total-width (+ content-width border-width separator-width)))

          ;; Step 3: Scale based on whether table fits
          (if (<= total-width max-width)
            ;; Case A: Fits naturally - scale UP to fill terminal
            (let ((available (- max-width border-width separator-width))
                   (result '()))
              (do ((k 0 (+ k 1)))
                ((>= k num-cols) (reverse result))
                (let* ((natural (vector-ref col-maxes k))
                        ;; Scale up proportionally
                        (scaled (quotient (* natural available) content-width)))
                  (set! result (cons scaled result)))))

            ;; Case B: Doesn't fit - scale DOWN with constraints
            (let ((available (- max-width border-width separator-width)))
              ;; New algorithm: separate small (< 8) and large (>= 8) columns
              (let ((widths (make-vector num-cols 0))
                     (small-cols '())
                     (large-cols '())
                     (small-total 0)
                     (large-natural-total 0))

                ;; Step 1: Categorize columns
                (do ((k 0 (+ k 1)))
                  ((>= k num-cols))
                  (let ((natural (vector-ref col-maxes k)))
                    (if (< natural min-col-width)
                      (set! small-cols (cons k small-cols))
                      (progn
                        (set! large-cols (cons k large-cols))
                        (set! large-natural-total (+ large-natural-total natural))))))

                ;; Step 2: Small columns keep natural width (no scaling)
                (let ((iter-small small-cols))
                  (do ()
                    ((null? iter-small))
                    (let* ((k (car iter-small))
                            (natural (vector-ref col-maxes k)))
                      (vector-set! widths k natural)
                      (set! small-total (+ small-total natural))
                      (set! iter-small (cdr iter-small)))))

                ;; Step 3: Allocate remaining to large columns (min=8)
                (let ((large-available (- available small-total)))
                  (if (not (null? large-cols))
                    ;; Distribute to large columns
                    (let ((iter-large large-cols))
                      (do ()
                        ((null? iter-large))
                        (let* ((k (car iter-large))
                                (natural (vector-ref col-maxes k))
                                (scaled (if (= large-natural-total 0)
                                          min-col-width  ; Avoid division by zero
                                          (quotient (* natural large-available) large-natural-total)))
                                (final (if (> scaled min-col-width) scaled min-col-width)))
                          (vector-set! widths k final)
                          (set! iter-large (cdr iter-large)))))))

                ;; Convert vector to list
                (let ((result '()))
                  (do ((k 0 (+ k 1)))
                    ((>= k num-cols) (reverse result))
                    (set! result (cons (vector-ref widths k) result))))))))))))

;; DEPRECATED: Old function kept for compatibility
;; Use tintin-calculate-optimal-widths instead
(defun tintin-calculate-column-widths (rows)
  (tintin-calculate-optimal-widths rows 80 8))

;; ============================================================================
;; GENERIC TABLE PRINTER
;; ============================================================================

;; Print formatted table from list of lists
;; data: ((header1 header2 ...) (row1-col1 row1-col2 ...) ...)
;; First list is treated as headers (rendered in bold)
;; Automatically detects terminal width and optimizes column layout
(defun tintin-print-table (data &optional row-separators)
  "Print a formatted table with Unicode box-drawing characters.

  ## Parameters
  - `data` - List of lists: `((headers...) (row1...) (row2...)...)`
  - `row-separators` - Optional boolean (defaults to `#t`). When true, draws
    horizontal lines between data rows.

  ## Description
  Automatically detects terminal width and optimizes column layout. The first
  list in `data` is treated as headers (rendered in bold). Supports text
  wrapping within cells when content exceeds column width.

  **Features:**
  - Unicode box-drawing characters (┌─┬─┐ │ ├─┼─┤ └─┴─┘)
  - Bold header formatting (`\\033[1m`)
  - Automatic column width calculation based on terminal width
  - Text wrapping for long cell content
  - Configurable row separators

  ## Examples
  ```lisp
  ; Simple table with separators (default)
  (tintin-print-table '((\"Name\" \"Age\" \"Role\")
                        (\"Alice\" \"30\" \"Engineer\")
                        (\"Bob\" \"25\" \"Designer\")))
  ; Output:
  ; ┌───────┬─────┬──────────┐
  ; │ Name  │ Age │ Role     │
  ; ├───────┼─────┼──────────┤
  ; │ Alice │ 30  │ Engineer │
  ; ├───────┼─────┼──────────┤
  ; │ Bob   │ 25  │ Designer │
  ; └───────┴─────┴──────────┘

  ; Table without row separators
  (tintin-print-table '((\"Name\" \"Score\")
                        (\"Player1\" \"100\")
                        (\"Player2\" \"200\"))
                      #f)
  ; Output:
  ; ┌─────────┬───────┐
  ; │ Name    │ Score │
  ; ├─────────┼───────┤
  ; │ Player1 │ 100   │
  ; │ Player2 │ 200   │
  ; └─────────┴───────┘
  ```

  ## Notes
  - Requires `terminal-info` function (provided by telnet-gui)
  - Minimum column width: 8 characters
  - Headers are automatically bolded
  - Long text wraps to multiple lines within cells
  - Empty or invalid data prints error message

  ## See Also
  - Used internally by `#alias`, `#variable`, `#action`, `#highlight` list commands"
  ;; Default row-separators to #t if not provided
  (let ((row-sep (or row-separators #t)))
    (if (or (null? data) (= (list-length data) 0))
      (tintin-echo "Error: Table data cannot be empty")
      (let* ((term-info (terminal-info))
              (term-cols (cdr (assoc 'cols term-info)))
              (min-col-width 8)
              (headers (car data))
              (rows (cdr data))
              (all-rows data))

        ;; Validate that we have at least headers
        (if (or (null? headers) (= (list-length headers) 0))
          (tintin-echo "Error: Table must have at least header row")
          (let ((widths (tintin-calculate-optimal-widths all-rows term-cols min-col-width)))

            ;; Draw top border
            (tintin-echo (tintin-draw-border widths 'top))

            ;; Draw header row (with bold formatting)
            (let ((bold-headers '()))
              ;; Add bold formatting to each header (truncate if too long for column)
              (do ((i 0 (+ i 1)))
                ((>= i (list-length headers)))
                (let* ((header (list-ref headers i))
                        (col-width (list-ref widths i))
                        (header-len (tintin-visual-length header))
                        ;; Truncate header if longer than column width
                        (truncated (if (> header-len col-width)
                                     (substring header 0 col-width)
                                     header)))
                  (set! bold-headers (cons (concat "\033[1m" truncated "\033[0m") bold-headers))))
              (set! bold-headers (reverse bold-headers))

              ;; Draw header lines
              (let ((header-lines (tintin-draw-row bold-headers widths)))
                (do ((i 0 (+ i 1)))
                  ((>= i (list-length header-lines)))
                  (tintin-echo (list-ref header-lines i)))))

            ;; Draw middle border
            (tintin-echo (tintin-draw-border widths 'middle))

            ;; Draw data rows (with wrapping if needed)
            (do ((row-idx 0 (+ row-idx 1)))
              ((>= row-idx (list-length rows)))
              (let* ((row (list-ref rows row-idx))
                      (row-lines (tintin-draw-row row widths)))
                (do ((line-idx 0 (+ line-idx 1)))
                  ((>= line-idx (list-length row-lines)))
                  (tintin-echo (list-ref row-lines line-idx))))

              ;; Draw separator after each row (except the last row)
              (if (and row-sep (< (+ row-idx 1) (list-length rows)))
                (tintin-echo (tintin-draw-border widths 'middle))))

            ;; Draw bottom border
            (tintin-echo (tintin-draw-border widths 'bottom))))))))

;; ============================================================================
;; LIST COMMANDS (using generic table printer)
;; ============================================================================

;; List all defined aliases
(defun tintin-list-aliases ()
  "Display formatted table of all defined aliases.

  ## Returns
  Empty string `\"\"` - Output sent via tintin-echo.

  ## Description
  Displays all aliases in a formatted Unicode table with columns for Name,
  Commands, and Priority. Aliases are sorted alphabetically by name. If no
  aliases are defined, displays a message instead of a table.

  **Display behavior:**
  - Empty store: Shows \"No aliases defined.\"
  - Non-empty: Shows count + formatted table
  - Priority 5 (default): Displays as empty string (not shown)
  - Other priorities: Displays numeric value

  ## Table Format
  ```
  Aliases (N):
  ┌──────┬──────────┬──────────┐
  │ Name │ Commands │ Priority │
  ├──────┼──────────┼──────────┤
  │ k    │ kill %1  │          │
  │ n    │ north    │          │
  └──────┴──────────┴──────────┘
  ```

  ## Examples
  ```lisp
  ; After defining aliases
  (tintin-process-command \"#alias {k} {kill %1}\")
  (tintin-process-command \"#alias {n} {north}\")
  (tintin-list-aliases)
  ; Displays table with 2 aliases

  ; Empty store
  (set! *tintin-aliases* (make-hash-table))
  (tintin-list-aliases)
  ; Displays: \"No aliases defined.\"
  ```

  ## Notes
  - Called automatically by `tintin-handle-alias` when no arguments provided
  - Uses `tintin-print-table` for formatting
  - Sorting ensures consistent display order
  - All output via `tintin-echo` (not returned)

  ## See Also
  - `tintin-handle-alias` - #alias command dispatcher
  - `tintin-print-table` - Table formatting engine
  - `tintin-sort-aliases-alphabetically` - Sorting function"
  (let ((alias-entries (hash-entries *tintin-aliases*))
         (count (hash-count *tintin-aliases*)))
    (if (= count 0)
      (progn
        (tintin-echo "No aliases defined.\r\n")
        "")
      (progn
        (tintin-echo (concat "Aliases (" (number->string count) "):\r\n"))
        ;; Sort aliases alphabetically
        (let ((sorted (tintin-sort-aliases-alphabetically alias-entries)))
          ;; Build data structure: headers + data rows
          (let ((data (list (list "Name" "Commands" "P"))))
            ;; Add data rows
            (do ((i 0 (+ i 1)))
              ((>= i (list-length sorted)))
              (let* ((entry (list-ref sorted i))
                      (name (car entry))
                      (value (cdr entry))
                      (commands (car value))
                      (priority (car (cdr value)))
                      (priority-str (number->string priority)))
                (set! data (append data (list (list name commands priority-str))))))
            ;; Print table using generic printer
            (tintin-print-table data)))
        ""))))

;; List all defined variables
(defun tintin-list-variables ()
  "Display formatted table of all defined variables.

  ## Returns
  Empty string `\"\"` - Output sent via tintin-echo.

  ## Description
  Displays all variables in a formatted Unicode table with columns for Variable
  name and Value. Variables are displayed in hash table iteration order (not
  sorted). If no variables are defined, displays a message instead of a table.

  **Display behavior:**
  - Empty store: Shows \"No variables defined.\"
  - Non-empty: Shows count + formatted table

  ## Table Format
  ```
  Variables (N):
  ┌──────────┬───────┐
  │ Variable │ Value │
  ├──────────┼───────┤
  │ target   │ orc   │
  │ weapon   │ sword │
  └──────────┴───────┘
  ```

  ## Examples
  ```lisp
  ; After defining variables
  (tintin-process-command \"#variable {target} {orc}\")
  (tintin-process-command \"#variable {weapon} {sword}\")
  (tintin-list-variables)
  ; Displays table with 2 variables

  ; Empty store
  (set! *tintin-variables* (make-hash-table))
  (tintin-list-variables)
  ; Displays: \"No variables defined.\"
  ```

  ## Notes
  - Called automatically by `tintin-handle-variable` when no arguments provided
  - Uses `tintin-print-table` for formatting
  - Variables NOT sorted (hash table iteration order)
  - All output via `tintin-echo` (not returned)

  ## See Also
  - `tintin-handle-variable` - #variable command dispatcher
  - `tintin-print-table` - Table formatting engine
  - `tintin-expand-variables-fast` - Variable expansion in commands"
  (let ((var-entries (hash-entries *tintin-variables*))
         (count (hash-count *tintin-variables*)))
    (if (= count 0)
      (progn
        (tintin-echo "No variables defined.\r\n")
        "")
      (progn
        (tintin-echo (concat "Variables (" (number->string count) "):\r\n"))
        ;; Build data structure: headers + data rows
        (let ((data (list (list "Variable" "Value"))))
          ;; Add data rows
          (do ((i 0 (+ i 1)))
            ((>= i (list-length var-entries)))
            (let* ((entry (list-ref var-entries i))
                    (name (car entry))
                    (value (cdr entry)))
              (set! data (append data (list (list name value))))))
          ;; Print table using generic printer
          (tintin-print-table data))
        ""))))

;; List all defined highlights (sorted alphabetically)
(defun tintin-list-highlights ()
  "Display formatted table of all defined highlights.

  ## Returns
  Empty string `\"\"` - Output sent via tintin-echo.

  ## Description
  Displays all highlights in a formatted Unicode table with columns for Pattern,
  Color, and Priority. Highlights are sorted alphabetically by pattern. If no
  highlights are defined, displays a message instead of a table.

  **Display behavior:**
  - Empty store: Shows \"No highlights defined.\"
  - Non-empty: Shows count + formatted table
  - Priority 5 (default): Displays as empty string (not shown)
  - Other priorities: Displays numeric value
  - Color format: \"foreground:background\" or just \"foreground\"

  ## Table Format
  ```
  Highlights (N):
  ┌─────────┬───────────────┬──────────┐
  │ Pattern │ Color         │ Priority │
  ├─────────┼───────────────┼──────────┤
  │ dragon  │ bold red      │          │
  │ %1 dies │ light cyan:44 │ 10       │
  └─────────┴───────────────┴──────────┘
  ```

  ## Examples
  ```lisp
  ; After defining highlights
  (tintin-process-command \"#highlight {dragon} {bold red}\")
  (tintin-process-command \"#highlight {%1 dies} {light cyan:44} {10}\")
  (tintin-list-highlights)
  ; Displays table with 2 highlights

  ; Empty store
  (set! *tintin-highlights* (make-hash-table))
  (tintin-list-highlights)
  ; Displays: \"No highlights defined.\"
  ```

  ## Notes
  - Called automatically by `tintin-handle-highlight` when no arguments provided
  - Uses `tintin-print-table` for formatting
  - Sorting ensures consistent display order
  - Color column shows foreground:background (or just foreground if no background)
  - All output via `tintin-echo` (not returned)

  ## See Also
  - `tintin-handle-highlight` - #highlight command dispatcher
  - `tintin-print-table` - Table formatting engine
  - `tintin-sort-highlights-alphabetically` - Sorting function
  - `tintin-apply-highlights` - Applies highlights to incoming text"
  (let ((highlight-entries (hash-entries *tintin-highlights*))
         (count (hash-count *tintin-highlights*)))
    (if (= count 0)
      (progn
        (tintin-echo "No highlights defined.\r\n")
        "")
      (progn
        (tintin-echo (concat "Highlights (" (number->string count) "):\r\n"))
        ;; Sort alphabetically before displaying
        (let ((sorted (tintin-sort-highlights-alphabetically highlight-entries)))
          ;; Build data structure: headers + data rows
          (let ((data (list (list "Pattern" "Color" "P"))))
            ;; Add data rows
            (do ((i 0 (+ i 1)))
              ((>= i (list-length sorted)))
              (let* ((entry (list-ref sorted i))
                      (pattern (car entry))
                      (entry-data (cdr entry))
                      (fg-color (car entry-data))
                      (bg-color (car (cdr entry-data)))
                      (priority (car (cdr (cdr entry-data))))
                      (color-str (concat (if fg-color fg-color "")
                                   (if (and fg-color bg-color) ":" "")
                                   (if bg-color bg-color "")))
                      (priority-str (number->string priority)))
                (set! data (append data (list (list pattern color-str priority-str))))))
            ;; Print table using generic printer
            (tintin-print-table data)))
        ""))))

;; List all defined actions (sorted alphabetically)
(defun tintin-list-actions ()
  "Display formatted table of all defined actions.

  ## Returns
  Empty string `\"\"` - Output sent via tintin-echo.

  ## Description
  Displays all actions (triggers) in a formatted Unicode table with columns for
  Pattern, Commands, and Priority. Actions are sorted alphabetically by pattern.
  If no actions are defined, displays a message instead of a table.

  **Display behavior:**
  - Empty store: Shows \"No actions defined.\"
  - Non-empty: Shows count + formatted table
  - Priority 5 (default): Displays as empty string (not shown)
  - Other priorities: Displays numeric value
  - Lower priority = executes first (1 before 10)

  ## Table Format
  ```
  Actions (N):
  ┌────────────────┬──────────────┬──────────┐
  │ Pattern        │ Commands     │ Priority │
  ├────────────────┼──────────────┼──────────┤
  │ You are hungry │ eat bread    │          │
  │ %1 attacks you │ kill %1      │ 1        │
  └────────────────┴──────────────┴──────────┘
  ```

  ## Examples
  ```lisp
  ; After defining actions
  (tintin-process-command \"#action {You are hungry} {eat bread}\")
  (tintin-process-command \"#action {%1 attacks you} {kill %1} {1}\")
  (tintin-list-actions)
  ; Displays table with 2 actions

  ; Empty store
  (set! *tintin-actions* (make-hash-table))
  (tintin-list-actions)
  ; Displays: \"No actions defined.\"
  ```

  ## Notes
  - Called automatically by `tintin-handle-action` when no arguments provided
  - Uses `tintin-print-table` for formatting
  - Sorting ensures consistent display order
  - Priority ordering: Lower values execute first (1, 2, ... 5 (default), ... 10)
  - All output via `tintin-echo` (not returned)

  ## See Also
  - `tintin-handle-action` - #action command dispatcher
  - `tintin-print-table` - Table formatting engine
  - `tintin-sort-actions-alphabetically` - Sorting function
  - `tintin-trigger-actions-for-line` - Executes matching actions"
  (let ((action-entries (hash-entries *tintin-actions*))
         (count (hash-count *tintin-actions*)))
    (if (= count 0)
      (progn
        (tintin-echo "No actions defined.\r\n")
        "")
      (progn
        (tintin-echo (concat "Actions (" (number->string count) "):\r\n"))
        ;; Sort alphabetically before displaying
        (let ((sorted (tintin-sort-actions-alphabetically action-entries)))
          ;; Build data structure: headers + data rows
          (let ((data (list (list "Pattern" "Commands" "P"))))
            ;; Add data rows
            (do ((i 0 (+ i 1)))
              ((>= i (list-length sorted)))
              (let* ((entry (list-ref sorted i))
                      (pattern (car entry))
                      (entry-data (cdr entry))
                      (commands (car entry-data))
                      (priority (car (cdr entry-data)))
                      (priority-str (number->string priority)))
                (set! data (append data (list (list pattern commands priority-str))))))
            ;; Print table using generic printer
            (tintin-print-table data)))
        ""))))

;; ============================================================================
;; COMMAND HANDLERS (REFACTORED)
;; ============================================================================

;; Handle #alias command
;; args: (), (name), or (name commands)
(defun tintin-handle-alias (args)
  "Handle #alias command (list, show, or create aliases).

  ## Parameters
  - `args` - Parsed command arguments (list)
    - Empty list `()` - List all aliases
    - `(name)` - Show specific alias
    - `(name commands)` - Create/update alias

  ## Returns
  Empty string `\"\"` - Command handled (output via tintin-echo).

  ## Description
  Internal handler for the `#alias` TinTin++ command. Dispatches based on
  argument count to list all aliases, show a specific alias, or create/update
  an alias definition.

  **Behaviors:**
  - **No arguments**: Display formatted table of all aliases
  - **One argument**: Show details of named alias (or error if not found)
  - **Two arguments**: Create or update alias with given name and commands

  **Alias Storage:**
  - Stored in `*tintin-aliases*` hash table
  - Format: `name → (commands priority)`
  - Default priority: 5

  ## Examples
  ```lisp
  ; List all aliases
  (tintin-handle-alias '())
  ; Displays table of all aliases

  ; Show specific alias
  (tintin-handle-alias '(\"k\"))
  ; => Alias 'k': k → kill %1

  ; Create alias
  (tintin-handle-alias '(\"k\" \"kill %1\"))
  ; => Alias 'k' created: k → kill %1

  ; Update existing alias
  (tintin-handle-alias '(\"k\" \"attack %1\"))
  ; => Alias 'k' created: k → attack %1
  ```

  ## Notes
  - Called by command dispatcher (user never calls directly)
  - Arguments already parsed by `tintin-parse-arguments`
  - Braces preserved in args (stripped by this function)
  - All output via `tintin-echo` (not returned)
  - Always returns empty string

  ## See Also
  - `tintin-list-aliases` - Display all aliases
  - `tintin-handle-unalias` - Remove aliases
  - `tintin-dispatch-command` - Command dispatcher"
  (cond
    ;; No arguments - list all aliases
    ((or (null? args) (= 0 (list-length args)))
      (tintin-list-aliases))
    ;; One argument - show specific alias
    ((= 1 (list-length args))
      (let ((name (tintin-strip-braces (list-ref args 0))))
        (let ((alias-data (hash-ref *tintin-aliases* name)))
          (if alias-data
            (let ((commands (car alias-data))
                   (priority (car (cdr alias-data))))
              (tintin-echo (concat "Alias '" name "': " name " → " commands
                             (if (= priority 5)
                               ""
                               (concat " (priority: " (number->string priority) ")"))
                             "\r\n"))
              "")
            (progn
              (tintin-echo (concat "Alias '" name "' not found\r\n"))
              "")))))
    ;; Two arguments - create alias
    (#t
      (let ((name (tintin-strip-braces (list-ref args 0)))
             (commands (tintin-strip-braces (list-ref args 1)))
             (priority 5))  ; Default priority
        (hash-set! *tintin-aliases* name (list commands priority))
        (tintin-echo (concat "Alias '" name "' created: " name " → " commands
                       (if (= priority 5)
                         ""
                         (concat " (priority: " (number->string priority) ")"))
                       "\r\n"))
        ""))))

;; Handle #variable command
;; args: (), (name), or (name value)
(defun tintin-handle-variable (args)
  "Handle #variable command (list, show, or create variables).

  ## Parameters
  - `args` - Parsed command arguments (list)
    - Empty list `()` - List all variables
    - `(name)` - Show specific variable
    - `(name value)` - Create/update variable

  ## Returns
  Empty string `\"\"` - Command handled (output via tintin-echo).

  ## Description
  Internal handler for the `#variable` TinTin++ command. Dispatches based on
  argument count to list all variables, show a specific variable, or create/update
  a variable definition.

  **Behaviors:**
  - **No arguments**: Display formatted table of all variables
  - **One argument**: Show details of named variable (or error if not found)
  - **Two arguments**: Create or update variable with given name and value

  **Variable Storage:**
  - Stored in `*tintin-variables*` hash table
  - Format: `name → value` (both strings)
  - Used in command expansion via `$name` syntax

  ## Examples
  ```lisp
  ; List all variables
  (tintin-handle-variable '())
  ; Displays table of all variables

  ; Show specific variable
  (tintin-handle-variable '(\"target\"))
  ; => Variable 'target': target = orc

  ; Create variable
  (tintin-handle-variable '(\"target\" \"orc\"))
  ; => Variable 'target' set to 'orc'

  ; Update existing variable
  (tintin-handle-variable '(\"target\" \"dragon\"))
  ; => Variable 'target' set to 'dragon'
  ```

  ## Notes
  - Called by command dispatcher (user never calls directly)
  - Arguments already parsed by `tintin-parse-arguments`
  - Braces preserved in args (stripped by this function)
  - All output via `tintin-echo` (not returned)
  - Variables expanded in commands via `$name`

  ## See Also
  - `tintin-list-variables` - Display all variables
  - `tintin-expand-variables-fast` - Expand $name references
  - `tintin-dispatch-command` - Command dispatcher"
  (cond
    ;; No arguments - list all variables
    ((or (null? args) (= 0 (list-length args)))
      (tintin-list-variables))
    ;; One argument - show specific variable
    ((= 1 (list-length args))
      (let ((name (tintin-strip-braces (list-ref args 0))))
        (let ((value (hash-ref *tintin-variables* name)))
          (if value
            (progn
              (tintin-echo (concat "Variable '" name "': " name " = " value "\r\n"))
              "")
            (progn
              (tintin-echo (concat "Variable '" name "' not found\r\n"))
              "")))))
    ;; Two arguments - create variable
    (#t
      (let ((name (tintin-strip-braces (list-ref args 0)))
             (value (tintin-strip-braces (list-ref args 1))))
        (hash-set! *tintin-variables* name value)
        (tintin-echo (concat "Variable '" name "' set to '" value "'\r\n"))
        ""))))

;; Handle #unalias command
;; args: (name)
(defun tintin-handle-unalias (args)
  "Handle #unalias command (remove alias).

  ## Parameters
  - `args` - Parsed command arguments: `(name)`

  ## Returns
  Empty string `\"\"` - Command handled (output via tintin-echo).

  ## Description
  Internal handler for the `#unalias` TinTin++ command. Removes a named alias
  from the alias table. Displays error if alias doesn't exist.

  ## Examples
  ```lisp
  ; Remove existing alias
  (tintin-handle-unalias '(\"k\"))
  ; => Alias 'k' removed

  ; Remove non-existent alias
  (tintin-handle-unalias '(\"xyz\"))
  ; => Alias 'xyz' not found
  ```

  ## Notes
  - Called by command dispatcher (user never calls directly)
  - Requires exactly one argument
  - Displays error if alias not found

  ## See Also
  - `tintin-handle-alias` - Create/show aliases
  - `tintin-dispatch-command` - Command dispatcher"
  (let ((name (tintin-strip-braces (list-ref args 0))))
    (if (hash-ref *tintin-aliases* name)
      (progn
        (hash-remove! *tintin-aliases* name)
        (tintin-echo (concat "Alias '" name "' removed\r\n"))
        "")
      (progn
        (tintin-echo (concat "Alias '" name "' not found\r\n"))
        ""))))

;; Handle #highlight command
;; args: (), (pattern), (pattern color), or (pattern color priority)
;; Color spec format: "fg", "fg:bg", "<rgb>", "bold red", etc.
;; Entry format: pattern → (fg-color bg-color priority)
(defun tintin-handle-highlight (args)
  "Handle #highlight command (list, show, or create highlights).

  ## Parameters
  - `args` - Parsed command arguments (list)
    - Empty list `()` - List all highlights
    - `(pattern)` - Show specific highlight
    - `(pattern color)` - Create highlight (default priority 5)
    - `(pattern color priority)` - Create highlight with priority

  ## Returns
  Empty string `\"\"` - Command handled (output via tintin-echo).

  ## Description
  Internal handler for the `#highlight` TinTin++ command. Creates color
  highlighting rules that wrap matching server output with ANSI color codes.

  **Color Spec Formats:**
  - `\"red\"` - Named foreground color
  - `\"red:blue\"` - Foreground:background
  - `\"<fff>\"` - RGB hex (3-char)
  - `\"<ff0000>\"` - RGB hex (6-char)
  - `\"bold red\"` - Attributes + color

  **Highlight Storage:**
  - Stored in `*tintin-highlights*` hash table
  - Format: `pattern → (fg-color bg-color priority)`
  - Applied by `tintin-apply-highlights` during server output

  ## Examples
  ```lisp
  ; List all highlights
  (tintin-handle-highlight '())

  ; Create highlight
  (tintin-handle-highlight '(\"dragon\" \"bold red\"))
  ; => Highlight 'dragon' created: dragon → bold red

  ; With background
  (tintin-handle-highlight '(\"gold\" \"yellow:black\"))

  ; With priority
  (tintin-handle-highlight '(\"urgent\" \"<f00>\" \"10\"))
  ; => Higher priority (processed first)
  ```

  ## Notes
  - Higher priority highlights applied first
  - Default priority: 5
  - Patterns support TinTin++ wildcards (%*, %1-%99)
  - Colors applied via `telnet-input-filter-hook`

  ## See Also
  - `tintin-apply-highlights` - Applies highlights to text
  - `tintin-handle-unhighlight` - Remove highlights
  - `tintin-parse-color-spec` - Color parsing"
  (cond
    ;; No arguments - list all highlights
    ((or (null? args) (= 0 (list-length args)))
      (tintin-list-highlights))
    ;; One argument - show specific highlight
    ((= 1 (list-length args))
      (let ((pattern (tintin-strip-braces (list-ref args 0))))
        (let ((highlight-data (hash-ref *tintin-highlights* pattern)))
          (if highlight-data
            (let ((fg-color (car highlight-data))
                   (bg-color (car (cdr highlight-data)))
                   (priority (car (cdr (cdr highlight-data)))))
              (tintin-echo (concat "Highlight '" pattern "': " pattern " → "
                             (if fg-color fg-color "")
                             (if (and fg-color bg-color) ":" "")
                             (if bg-color bg-color "")
                             (if (= priority 5)
                               ""
                               (concat " (priority: " (number->string priority) ")"))
                             "\r\n"))
              "")
            (progn
              (tintin-echo (concat "Highlight '" pattern "' not found\r\n"))
              "")))))
    ;; Two or three arguments - create highlight
    (#t
      (let* ((pattern (tintin-strip-braces (list-ref args 0)))
              (color-spec (tintin-strip-braces (list-ref args 1)))
              (priority (if (>= (list-length args) 3)
                          (string->number (tintin-strip-braces (list-ref args 2)))
                          5)))  ; Default priority
        ;; Parse color spec into FG and BG components
        (let ((parts (tintin-split-fg-bg color-spec)))
          (let ((fg-part (list-ref parts 0))
                 (bg-part (list-ref parts 1)))
            ;; Store as (fg-color bg-color priority)
            (hash-set! *tintin-highlights* pattern
              (list (if (string=? fg-part "") nil fg-part)
                bg-part
                priority))
            (tintin-echo (concat "Highlight '" pattern "' created: "
                           pattern " → " color-spec
                           (if (= priority 5)
                             ""
                             (concat " (priority: " (number->string priority) ")"))
                           "\r\n"))
            ""))))))

;; Handle #unhighlight command
;; args: (pattern)
(defun tintin-handle-unhighlight (args)
  (let ((pattern (tintin-strip-braces (list-ref args 0))))
    (if (hash-ref *tintin-highlights* pattern)
      (progn
        (hash-remove! *tintin-highlights* pattern)
        (tintin-echo (concat "Highlight '" pattern "' removed\r\n"))
        "")
      (progn
        (tintin-echo (concat "Highlight '" pattern "' not found\r\n"))
        ""))))

;; Handle #action command
;; args: (), (pattern), (pattern commands), or (pattern commands priority)
;; Entry format: pattern → (commands-string priority)
(defun tintin-handle-action (args)
  "Handle #action command (list, show, or create triggers).

  ## Parameters
  - `args` - Parsed command arguments (list)
    - Empty list `()` - List all actions
    - `(pattern)` - Show specific action
    - `(pattern commands)` - Create action (default priority 5)
    - `(pattern commands priority)` - Create action with priority

  ## Returns
  Empty string `\"\"` - Command handled (output via tintin-echo).

  ## Description
  Internal handler for the `#action` TinTin++ command (also called triggers).
  Creates rules that automatically execute commands when server output matches
  a pattern. Actions are processed line-by-line in priority order.

  **Action Behavior:**
  - Triggered by matching server output (after ANSI removal)
  - Commands executed via `telnet-send` (not echoed)
  - Lower priority processed first (1 before 5)
  - Supports capture groups (%1-%99) for pattern extraction
  - Prevents infinite loops via execution flag

  **Action Storage:**
  - Stored in `*tintin-actions*` hash table
  - Format: `pattern → (commands-string priority)`
  - Triggered by `tintin-trigger-actions-for-line`

  ## Examples
  ```lisp
  ; List all actions
  (tintin-handle-action '())

  ; Create action
  (tintin-handle-action '(\"You are hungry\" \"eat bread\"))
  ; => Auto-eats bread when hungry

  ; With captures
  (tintin-handle-action '(\"%1 enters the room\" \"say Hello %1!\"))
  ; => Says hello when anyone enters

  ; With priority
  (tintin-handle-action '(\"danger\" \"flee\" \"1\"))
  ; => Priority 1 (executes before priority 5)
  ```

  ## Notes
  - Lower priority = processed first (1 before 10)
  - Default priority: 5
  - Circular execution detection prevents loops
  - Commands processed via `tintin-process-input`
  - Triggered by `telnet-input-hook`

  ## See Also
  - `tintin-trigger-actions-for-line` - Executes matching actions
  - `tintin-handle-unaction` - Remove actions
  - `tintin-execute-action` - Action execution engine"
  (cond
    ;; No arguments - list all actions
    ((or (null? args) (= 0 (list-length args)))
      (tintin-list-actions))
    ;; One argument - show specific action
    ((= 1 (list-length args))
      (let ((pattern (tintin-strip-braces (list-ref args 0))))
        (let ((action-data (hash-ref *tintin-actions* pattern)))
          (if action-data
            (let ((commands (car action-data))
                   (priority (car (cdr action-data))))
              (tintin-echo (concat "Action '" pattern "': " pattern " → " commands
                             (if (= priority 5)
                               ""
                               (concat " (priority: " (number->string priority) ")"))
                             "\r\n"))
              "")
            (progn
              (tintin-echo (concat "Action '" pattern "' not found\r\n"))
              "")))))
    ;; Two or three arguments - create action
    (#t
      (let* ((pattern (tintin-strip-braces (list-ref args 0)))
              (commands (tintin-strip-braces (list-ref args 1)))
              (priority (if (>= (list-length args) 3)
                          (string->number (tintin-strip-braces (list-ref args 2)))
                          5)))  ; Default priority
        ;; Store as (commands-string priority)
        (hash-set! *tintin-actions* pattern (list commands priority))
        (tintin-echo (concat "Action '" pattern "' created: "
                       pattern " → " commands
                       (if (= priority 5)
                         ""
                         (concat " (priority: " (number->string priority) ")"))
                       "\r\n"))
        ""))))

;; Handle #unaction command
;; args: (pattern)
(defun tintin-handle-unaction (args)
  (let ((pattern (tintin-strip-braces (list-ref args 0))))
    (if (hash-ref *tintin-actions* pattern)
      (progn
        (hash-remove! *tintin-actions* pattern)
        (tintin-echo (concat "Action '" pattern "' removed\r\n"))
        "")
      (progn
        (tintin-echo (concat "Action '" pattern "' not found\r\n"))
        ""))))

;; Handle #save command
;; args: (filename)
(defun tintin-handle-save (args)
  "Handle #save command (save TinTin++ state to file).

  ## Parameters
  - `args` - Parsed command arguments: `(filename)`

  ## Returns
  Empty string `\"\"` - Command handled (output via tintin-echo).

  ## Description
  Internal handler for the `#save` TinTin++ command. Saves all aliases,
  variables, highlights, and actions to a Lisp file that can be loaded later.

  **Saved State:**
  - All aliases (name → commands + priority)
  - All variables (name → value)
  - All highlights (pattern → colors + priority)
  - All actions (pattern → commands + priority)
  - Settings (speedwalk, diagonals, enabled state)

  **File Format:**
  - Lisp source code (executable)
  - Uses `hash-set!` calls to restore state
  - Can be loaded with `(load \"filename\")` or `#load`
  - Supports `~/` path expansion

  ## Examples
  ```lisp
  ; Save to file
  (tintin-handle-save '(\"my-config.lisp\"))
  ; => State saved to 'my-config.lisp'

  ; Save to home directory
  (tintin-handle-save '(\"~/tintin-state.lisp\"))
  ; => State saved to '/home/user/tintin-state.lisp'
  ```

  ## Notes
  - Overwrites existing file
  - Path expansion via `expand-path`
  - File is executable Lisp code
  - Load with `#load filename`

  ## See Also
  - `tintin-handle-load` - Load saved state
  - `tintin-save-state` - Internal save implementation
  - `expand-path` - Path expansion (~/)"
  (let ((filename (tintin-strip-braces (list-ref args 0))))
    ;; Expand ~/path if present
    (set! filename (expand-path filename))
    (tintin-save-state filename)
    (tintin-echo (concat "State saved to '" filename "'\r\n"))
    ""))

;; Handle #load command
;; args: (filename)
(defun tintin-handle-load (args)
  "Handle #load command (load TinTin++ state from file).

  ## Parameters
  - `args` - Parsed command arguments: `(filename)`

  ## Returns
  Empty string `\"\"` - Command handled (output via tintin-echo).

  ## Description
  Internal handler for the `#load` TinTin++ command. Loads aliases, variables,
  highlights, and actions from a Lisp file (typically created by `#save`).

  **Load Behavior:**
  - Executes file as Lisp code via `load`
  - Merges with existing state (doesn't clear first)
  - Reports success or failure
  - Supports `~/` path expansion
  - Error handling via `condition-case`

  **Loaded State:**
  - Aliases, variables, highlights, actions
  - TinTin++ settings (speedwalk, etc.)
  - Any custom Lisp code in file

  ## Examples
  ```lisp
  ; Load from file
  (tintin-handle-load '(\"my-config.lisp\"))
  ; => State loaded from 'my-config.lisp'

  ; Load from home directory
  (tintin-handle-load '(\"~/tintin-state.lisp\"))
  ; => State loaded from '/home/user/tintin-state.lisp'

  ; File not found
  (tintin-handle-load '(\"missing.lisp\"))
  ; => Failed to load 'missing.lisp': file not found or invalid
  ```

  ## Notes
  - Merges with existing state (not reset)
  - Path expansion via `expand-path`
  - File must be valid Lisp code
  - Reports errors gracefully
  - Can load any Lisp file (not just #save output)

  ## See Also
  - `tintin-handle-save` - Save current state
  - `load` - Lisp file loader (built-in)
  - `expand-path` - Path expansion (~/)"
  (let ((filename (tintin-strip-braces (list-ref args 0))))
    ;; Expand ~/path if present
    (set! filename (expand-path filename))
    ;; Try to load the file, catching errors
    (if (condition-case err
          (progn (load filename) #t)
          (error #f))
      ;; Success case
      (progn
        (tintin-echo (concat "State loaded from '" filename "'\r\n"))
        "")
      ;; Error case
      (progn
        (tintin-echo (concat "Failed to load '" filename "': file not found or invalid\r\n"))
        ""))))

;; Register commands with metadata (now that handlers are defined)
(hash-set! *tintin-commands* "alias"
  (list tintin-handle-alias 2 "#alias or #alias {name} or #alias {name} {commands}"))
(hash-set! *tintin-commands* "unalias"
  (list tintin-handle-unalias 1 "#unalias {name}"))
(hash-set! *tintin-commands* "variable"
  (list tintin-handle-variable 2 "#variable or #variable {name} or #variable {name} {value}"))
(hash-set! *tintin-commands* "highlight"
  (list tintin-handle-highlight 2 "#highlight or #highlight {pattern} or #highlight {pattern} {color}"))
(hash-set! *tintin-commands* "unhighlight"
  (list tintin-handle-unhighlight 1 "#unhighlight {pattern}"))
(hash-set! *tintin-commands* "save"
  (list tintin-handle-save 1 "#save {filename}"))
(hash-set! *tintin-commands* "load"
  (list tintin-handle-load 1 "#load {filename}"))
(hash-set! *tintin-commands* "action"
  (list tintin-handle-action 3 "#action or #action {pattern} or #action {pattern} {commands} [priority]"))
(hash-set! *tintin-commands* "unaction"
  (list tintin-handle-unaction 1 "#unaction {pattern}"))

;; ============================================================================
;; GENERIC COMMAND DISPATCHER (REFACTORED)
;; ============================================================================

;; Check if a TinTin++ command has any arguments
;; Returns #t if arguments present, #f if just command name
(defun tintin-has-arguments? (input)
  (let ((len (string-length input))
         (pos 1))  ; Start after #
    ;; Skip whitespace after #
    (do ()
      ((or (>= pos len) (not (char=? (string-ref input pos) #\space))))
      (set! pos (+ pos 1)))
    ;; Skip command name
    (do ()
      ((or (>= pos len)
         (char=? (string-ref input pos) #\space)
         (char=? (string-ref input pos) #\{)))
      (set! pos (+ pos 1)))
    ;; Skip whitespace after command name
    (do ()
      ((or (>= pos len) (not (char=? (string-ref input pos) #\space))))
      (set! pos (+ pos 1)))
    ;; If we have more characters, there are arguments
    (< pos len)))

;; Try parsing with progressively fewer arguments (for variable-arg commands)
;; Returns parsed args list or nil if all attempts fail
(defun tintin-try-parse-arguments (input max-count)
  (if (<= max-count 0)
    nil
    (let ((args (tintin-parse-arguments input max-count)))
      (if args
        args
        ;; Try with one fewer argument
        (tintin-try-parse-arguments input (- max-count 1))))))

;; Dispatch a TinTin++ command using metadata-driven approach
;; cmd-name: matched command name (e.g., "alias")
;; input: original input string (e.g., "#alias {k} {kill %1}")
(defun tintin-dispatch-command (cmd-name input)
  (let ((cmd-data (hash-ref *tintin-commands* cmd-name)))
    (if (not cmd-data)
      ;; Should never happen (tintin-find-command validated it)
      ""
      (let ((handler (list-ref cmd-data 0))
             (arg-count (list-ref cmd-data 1))
             (syntax-help (list-ref cmd-data 2)))
        ;; Check if input has any arguments after command name
        (let ((has-args (tintin-has-arguments? input)))
          (if (not has-args)
            ;; No arguments - call handler with empty list
            (handler '())
            ;; Has arguments - try parsing with max count down to 1
            (let ((args (tintin-try-parse-arguments input arg-count)))
              (if args
                (handler args)
                (tintin-syntax-error syntax-help)))))))))

;; ============================================================================
;; AUTO-ACTIVATION
;; ============================================================================
;; ANSI Stack Post-Processing
;;
;; When highlights are applied to text that contains server ANSI codes,
;; embedded reset codes from the server can kill the highlight colors.
;; This post-processor fixes that by maintaining a stack of ANSI states.

;; Check if an ANSI sequence is a reset code (\033[0m or \033[m)
(defun tintin-is-reset-code (seq)
  (regex-match? "^\033\\[0*m$" seq))

;; Check if an ANSI sequence is an SGR code (ends with 'm')
;; SGR codes are the ones we want to track in our stack
(defun tintin-is-sgr-code (seq)
  (regex-match? "^\033\\[[0-9;]*m$" seq))

;; Post-process text to handle nested ANSI states
;; When a reset code is encountered, if there are remaining states on the stack,
;; restore the top state after the reset instead of leaving text uncolored.
(defun tintin-post-process-ansi-stack (text)
  (tintin-ansi-stack-loop text 0 (string-length text) "" '()))

;; Recursive helper for ANSI stack processing
(defun tintin-ansi-stack-loop (text pos len result stack)
  (if (>= pos len)
    result
    (let ((char (string-ref text pos)))
      (if (char=? char #\escape)  ;; ESC character
        ;; Try to parse ANSI sequence
        (let ((seq-end (tintin-find-ansi-end text pos len)))
          (if seq-end
            (let ((seq (substring text pos seq-end)))
              (if (tintin-is-reset-code seq)
                ;; Reset code - pop from stack and potentially restore
                (if (null? stack)
                  ;; Empty stack - just pass through the reset
                  (tintin-ansi-stack-loop text seq-end len (concat result seq) stack)
                  ;; Pop the top state
                  (let ((new-stack (cdr stack)))
                    (if (null? new-stack)
                      ;; Stack now empty - just output reset
                      (tintin-ansi-stack-loop text seq-end len (concat result seq) new-stack)
                      ;; Stack has remaining state - output reset then restore top
                      (tintin-ansi-stack-loop text seq-end len (concat result seq (car new-stack)) new-stack))))
                ;; Not a reset - check if SGR (should be pushed)
                (if (tintin-is-sgr-code seq)
                  (tintin-ansi-stack-loop text seq-end len (concat result seq) (cons seq stack))
                  ;; Non-SGR ANSI code - just pass through (don't push)
                  (tintin-ansi-stack-loop text seq-end len (concat result seq) stack))))
            ;; Not a valid ANSI sequence - just add the char
            (tintin-ansi-stack-loop text (+ pos 1) len (concat result (char->string char)) stack)))
        ;; Regular character - just add it
        (tintin-ansi-stack-loop text (+ pos 1) len (concat result (char->string char)) stack)))))

;; Find the end position of an ANSI sequence starting at pos
;; Returns nil if not a valid ANSI sequence, or the end position (exclusive)
(defun tintin-find-ansi-end (text pos len)
  (if (and (< (+ pos 1) len)
        (char=? (string-ref text (+ pos 1)) #\[))  ;; '[' character
    ;; CSI sequence - find the terminator
    (tintin-find-ansi-terminator text (+ pos 2) len)
    nil))

;; Recursive helper to find ANSI terminator (scans digits and semicolons)
(defun tintin-find-ansi-terminator (text i len)
  (if (>= i len)
    nil  ;; No terminator found
    (let ((c (string-ref text i)))
      (if (or (and (char>=? c #\0)
                (char<=? c #\9))
            (char=? c #\;))
        ;; Still in parameter section, keep scanning
        (tintin-find-ansi-terminator text (+ i 1) len)
        ;; Check if this is a valid terminator (letter)
        (if (or (and (char>=? c #\A)
                  (char<=? c #\Z))
              (and (char>=? c #\a)
                (char<=? c #\z)))
          (+ i 1)  ;; Return position after terminator
          nil)))))

;; ============================================================================
;; Automatically activate TinTin++ when this file is loaded

;; Activate TinTin++ user input hook
(define user-input-hook tintin-user-input-hook)

;; Hook function for telnet-input-filter-hook integration
;; Signature: (lambda (text) -> string)
;; - text: Incoming telnet data (may contain ANSI codes)
;; Returns: Transformed text with highlights applied
;;
;; This hook receives data from the telnet server before it's displayed
;; in the terminal. We apply highlight patterns to colorize matching text.
;; After applying highlights, we post-process to handle nested ANSI states
;; so that server reset codes don't kill highlight colors.
(defun tintin-telnet-input-filter (text)
  (if (and *tintin-enabled* (> (hash-count *tintin-highlights*) 0))
    (tintin-post-process-ansi-stack (tintin-apply-highlights text))
    text))

;; Install telnet-input-filter-hook
(define telnet-input-filter-hook tintin-telnet-input-filter)

;; Override telnet-input-hook to add action triggering + word collection
;; This hook is called when data arrives from the telnet server
;; It sees stripped text (no ANSI codes), better for pattern matching
(defun telnet-input-hook (text)
  ;; Step 1: Collect words for completions (preserve default behavior from bootstrap.lisp)
  (collect-words-from-text text)

  ;; Step 2: Trigger actions (if TinTin++ enabled)
  (if (and *tintin-enabled*
        (not *tintin-action-executing*)
        (> (hash-count *tintin-actions*) 0))
    (let ((lines (tintin-split-lines text)))
      (do ((i 0 (+ i 1)))
        ((>= i (list-length lines)))
        (tintin-trigger-actions-for-line (list-ref lines i))))))

;; Announce activation (terminal is ready when this file loads via -l)
(tintin-echo "TinTin++ loaded and activated\r\n")
