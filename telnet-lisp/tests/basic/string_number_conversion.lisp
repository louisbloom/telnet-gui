;; String to Number Conversion Tests

;; Basic Positive Integers
(string->number "0")                ; => 0
(string->number "1")                ; => 1
(string->number "42")               ; => 42
(string->number "12345")            ; => 12345

;; Negative Integers
(string->number "-1")               ; => -1
(string->number "-42")              ; => -42
(string->number "-12345")           ; => -12345

;; With Plus Sign
(string->number "+1")               ; => 1
(string->number "+42")              ; => 42

;; Leading/Trailing Whitespace
(string->number "  42")             ; => 42
(string->number "42  ")             ; => 42
(string->number "  42  ")           ; => 42
(string->number "  -42  ")          ; => -42

;; Edge Cases
(string->number "0")                ; => 0
(string->number "-0")               ; => 0

;; Use with arithmetic
(+ (string->number "10") (string->number "20"))  ; => 30
(* (string->number "5") (string->number "3"))    ; => 15
