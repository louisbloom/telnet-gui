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

;; =============================================================================
;; R7RS Compliance Tests - Radix Parameter
;; =============================================================================

;; Binary (base 2)
(string->number "1010" 2)        ; => 10
(string->number "11111111" 2)    ; => 255
(string->number "-1010" 2)       ; => -10

;; Octal (base 8)
(string->number "77" 8)          ; => 63
(string->number "377" 8)         ; => 255
(string->number "-77" 8)         ; => -63

;; Hexadecimal (base 16)
(string->number "ff" 16)         ; => 255
(string->number "FF" 16)         ; => 255
(string->number "10" 16)         ; => 16
(string->number "-ff" 16)        ; => -255

;; Base 36 (max)
(string->number "z" 36)          ; => 35
(string->number "10" 36)         ; => 36

;; =============================================================================
;; R7RS Compliance Tests - Radix Prefixes
;; =============================================================================

;; Binary prefix (#b)
(string->number "#b1010")        ; => 10
(string->number "#B1010")        ; => 10
(string->number "#b11111111")    ; => 255

;; Octal prefix (#o)
(string->number "#o77")          ; => 63
(string->number "#O77")          ; => 63
(string->number "#o377")         ; => 255

;; Decimal prefix (#d)
(string->number "#d123")         ; => 123
(string->number "#D456")         ; => 456

;; Hexadecimal prefix (#x)
(string->number "#xff")          ; => 255
(string->number "#XFF")          ; => 255
(string->number "#x10")          ; => 16

;; =============================================================================
;; R7RS Compliance Tests - Float Parsing
;; =============================================================================

;; Basic floats
(string->number "3.14")          ; => 3.14
(string->number "-3.14")         ; => -3.14
(string->number "0.5")           ; => 0.5
(string->number ".5")            ; => 0.5

;; Scientific notation
(string->number "1e10")          ; => 1e10
(string->number "1E10")          ; => 1e10
(string->number "3.14e2")        ; => 314
(string->number "-2.5e-3")       ; => -0.0025
(string->number "1.23e+5")       ; => 123000

;; =============================================================================
;; R7RS Compliance Tests - Return #f on Failure
;; =============================================================================

;; Invalid strings return nil (#f), not error
(string->number "xyz")           ; => nil
(string->number "12.34.56")      ; => nil
(string->number "")              ; => nil
(string->number "  ")            ; => nil
(string->number "abc" 10)        ; => nil
(string->number "xyz" 16)        ; => nil
(string->number "2" 2)           ; => nil

;; =============================================================================
;; R7RS Compliance Tests - number->string with Radix
;; =============================================================================

;; Base 10 (default)
(number->string 0)               ; => "0"
(number->string 42)              ; => "42"
(number->string -42)             ; => "-42"

;; Binary (base 2)
(number->string 10 2)            ; => "1010"
(number->string 255 2)           ; => "11111111"
(number->string -10 2)           ; => "-1010"

;; Octal (base 8)
(number->string 63 8)            ; => "77"
(number->string 255 8)           ; => "377"
(number->string -63 8)           ; => "-77"

;; Hexadecimal (base 16)
(number->string 255 16)          ; => "ff"
(number->string 16 16)           ; => "10"
(number->string -255 16)         ; => "-ff"

;; Base 36 (max)
(number->string 35 36)           ; => "z"
(number->string 36 36)           ; => "10"

;; Floats (only base 10)
(number->string 3.14)            ; => "3.14"
(number->string -3.14)           ; => "-3.14"
(number->string 1e10)            ; => "1e+10"

;; =============================================================================
;; Round-trip Tests (string->number and number->string)
;; =============================================================================

;; Integers in various bases
(number->string (string->number "1010" 2) 2)     ; => "1010"
(number->string (string->number "ff" 16) 16)     ; => "ff"
(number->string (string->number "77" 8) 8)       ; => "77"

;; Floats
(string->number (number->string 3.14))           ; => 3.14
