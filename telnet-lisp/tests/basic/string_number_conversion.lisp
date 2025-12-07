;; String to Number Conversion Tests

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; Basic Positive Integers
(assert-equal (string->number "0") 0 "parse '0'")
(assert-equal (string->number "1") 1 "parse '1'")
(assert-equal (string->number "42") 42 "parse '42'")
(assert-equal (string->number "12345") 12345 "parse '12345'")

;; Negative Integers
(assert-equal (string->number "-1") -1 "parse '-1'")
(assert-equal (string->number "-42") -42 "parse '-42'")
(assert-equal (string->number "-12345") -12345 "parse '-12345'")

;; With Plus Sign
(assert-equal (string->number "+1") 1 "parse '+1'")
(assert-equal (string->number "+42") 42 "parse '+42'")

;; Leading/Trailing Whitespace
(assert-equal (string->number "  42") 42 "parse with leading whitespace")
(assert-equal (string->number "42  ") 42 "parse with trailing whitespace")
(assert-equal (string->number "  42  ") 42 "parse with both leading and trailing whitespace")
(assert-equal (string->number "  -42  ") -42 "parse negative with whitespace")

;; Edge Cases
(assert-equal (string->number "0") 0 "parse zero")
(assert-equal (string->number "-0") 0 "parse negative zero")

;; Use with arithmetic
(assert-equal (+ (string->number "10") (string->number "20")) 30 "add parsed numbers")
(assert-equal (* (string->number "5") (string->number "3")) 15 "multiply parsed numbers")

;; =============================================================================
;; R7RS Compliance Tests - Radix Parameter
;; =============================================================================

;; Binary (base 2)
(assert-equal (string->number "1010" 2) 10 "parse binary '1010'")
(assert-equal (string->number "11111111" 2) 255 "parse binary '11111111'")
(assert-equal (string->number "-1010" 2) -10 "parse negative binary")

;; Octal (base 8)
(assert-equal (string->number "77" 8) 63 "parse octal '77'")
(assert-equal (string->number "377" 8) 255 "parse octal '377'")
(assert-equal (string->number "-77" 8) -63 "parse negative octal")

;; Hexadecimal (base 16)
(assert-equal (string->number "ff" 16) 255 "parse hex 'ff'")
(assert-equal (string->number "FF" 16) 255 "parse hex 'FF' uppercase")
(assert-equal (string->number "10" 16) 16 "parse hex '10'")
(assert-equal (string->number "-ff" 16) -255 "parse negative hex")

;; Base 36 (max)
(assert-equal (string->number "z" 36) 35 "parse base-36 'z'")
(assert-equal (string->number "10" 36) 36 "parse base-36 '10'")

;; =============================================================================
;; R7RS Compliance Tests - Radix Prefixes
;; =============================================================================

;; Binary prefix (#b)
(assert-equal (string->number "#b1010") 10 "parse with #b prefix")
(assert-equal (string->number "#B1010") 10 "parse with #B prefix (uppercase)")
(assert-equal (string->number "#b11111111") 255 "parse #b11111111")

;; Octal prefix (#o)
(assert-equal (string->number "#o77") 63 "parse with #o prefix")
(assert-equal (string->number "#O77") 63 "parse with #O prefix (uppercase)")
(assert-equal (string->number "#o377") 255 "parse #o377")

;; Decimal prefix (#d)
(assert-equal (string->number "#d123") 123 "parse with #d prefix")
(assert-equal (string->number "#D456") 456 "parse with #D prefix (uppercase)")

;; Hexadecimal prefix (#x)
(assert-equal (string->number "#xff") 255 "parse with #x prefix")
(assert-equal (string->number "#XFF") 255 "parse with #X prefix (uppercase)")
(assert-equal (string->number "#x10") 16 "parse #x10")

;; =============================================================================
;; R7RS Compliance Tests - Float Parsing
;; =============================================================================

;; Basic floats
(assert-equal (string->number "3.14") 3.14 "parse float '3.14'")
(assert-equal (string->number "-3.14") -3.14 "parse negative float")
(assert-equal (string->number "0.5") 0.5 "parse '0.5'")
(assert-equal (string->number ".5") 0.5 "parse '.5' without leading zero")

;; Scientific notation
(assert-equal (string->number "1e10") 10000000000.0 "parse '1e10'")
(assert-equal (string->number "1E10") 10000000000.0 "parse '1E10' uppercase")
(assert-equal (string->number "3.14e2") 314.0 "parse '3.14e2'")
(assert-equal (string->number "-2.5e-3") -0.0025 "parse negative scientific notation")
(assert-equal (string->number "1.23e+5") 123000.0 "parse with explicit plus exponent")

;; =============================================================================
;; R7RS Compliance Tests - Return #f on Failure
;; =============================================================================

;; Invalid strings return nil (#f), not error
(assert-nil (string->number "xyz") "invalid string returns nil")
(assert-nil (string->number "12.34.56") "multiple decimal points returns nil")
(assert-nil (string->number "") "empty string returns nil")
(assert-nil (string->number "  ") "whitespace only returns nil")
(assert-nil (string->number "abc" 10) "invalid base-10 string returns nil")
(assert-nil (string->number "xyz" 16) "invalid hex string returns nil")
(assert-nil (string->number "2" 2) "digit out of range for base returns nil")

;; =============================================================================
;; R7RS Compliance Tests - number->string with Radix
;; =============================================================================

;; Base 10 (default)
(assert-equal (number->string 0) "0" "convert 0 to string")
(assert-equal (number->string 42) "42" "convert 42 to string")
(assert-equal (number->string -42) "-42" "convert -42 to string")

;; Binary (base 2)
(assert-equal (number->string 10 2) "1010" "convert 10 to binary")
(assert-equal (number->string 255 2) "11111111" "convert 255 to binary")
(assert-equal (number->string -10 2) "-1010" "convert -10 to binary")

;; Octal (base 8)
(assert-equal (number->string 63 8) "77" "convert 63 to octal")
(assert-equal (number->string 255 8) "377" "convert 255 to octal")
(assert-equal (number->string -63 8) "-77" "convert -63 to octal")

;; Hexadecimal (base 16)
(assert-equal (number->string 255 16) "ff" "convert 255 to hex")
(assert-equal (number->string 16 16) "10" "convert 16 to hex")
(assert-equal (number->string -255 16) "-ff" "convert -255 to hex")

;; Base 36 (max)
(assert-equal (number->string 35 36) "z" "convert 35 to base-36")
(assert-equal (number->string 36 36) "10" "convert 36 to base-36")

;; Floats (only base 10)
(assert-equal (number->string 3.14) "3.14" "convert float 3.14 to string")
(assert-equal (number->string -3.14) "-3.14" "convert -3.14 to string")
(assert-equal (number->string 10000000000.0) "10000000000" "convert large number to string")

;; =============================================================================
;; Round-trip Tests (string->number and number->string)
;; =============================================================================

;; Integers in various bases
(assert-equal (number->string (string->number "1010" 2) 2) "1010" "round-trip binary")
(assert-equal (number->string (string->number "ff" 16) 16) "ff" "round-trip hex")
(assert-equal (number->string (string->number "77" 8) 8) "77" "round-trip octal")

;; Floats
(assert-equal (string->number (number->string 3.14)) 3.14 "round-trip float")
