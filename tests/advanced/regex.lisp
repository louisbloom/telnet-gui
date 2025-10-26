; Telnet Lisp - Regex Examples
; Comprehensive demonstration of regex and pattern matching features

; ===== BASIC REGEX MATCHING =====
(regex-match "\\d+" "hello123")           ; Match digits
(regex-match "^[a-z]+$" "hello")          ; Match lowercase only
(regex-match "test" "this is a test")     ; Simple substring match

; ===== FINDING MATCHES =====
(regex-find "\\d+" "abc123def")           ; Find first number
(regex-find-all "\\d+" "a1b2c3")          ; Find all numbers
(regex-find-all "\\w+" "hello world test") ; Find all words

; ===== EXTRACTING CAPTURE GROUPS =====
(regex-extract "(\\w+)@(\\w+)" "user@domain")  ; Extract email parts
(regex-extract "(\\d+)-(\\d+)-(\\d+)" "2025-10-24")  ; Extract date parts
(regex-extract "([A-Z]+)-(\\d+)" "ABC-123")  ; Extract code parts

; ===== REPLACING TEXT =====
(regex-replace "\\d+" "X" "a1b2c3")       ; Replace first number
(regex-replace-all "\\d+" "X" "a1b2c3")   ; Replace all numbers

; Replace with capture groups
(regex-replace "(\\w+)@(\\w+)" "$2@$1" "user@domain")  ; Swap email parts
(regex-replace "(\\d+)-(\\d+)" "$2/$1" "2025-10")      ; Change date format

; ===== SPLITTING STRINGS =====
(regex-split "\\s+" "hello  world  test") ; Split by whitespace
(regex-split "," "apple,banana,cherry")   ; Split by comma
(regex-split "[,;]" "a,b;c,d")           ; Split by comma or semicolon

; ===== UTILITY FUNCTIONS =====
(regex-escape "test.string*with?special")  ; Escape special chars
(regex-valid? "\\d+")                      ; Valid regex
(regex-valid? "[invalid")                  ; Invalid regex

; ===== ENHANCED WILDCARD MATCHING =====
; Character classes
(string-match "[abc]test" "atest")        ; Match any of a, b, c
(string-match "[a-z]+" "hello")           ; Match lowercase range
(string-match "[!0-9]+" "abc")            ; Match non-digits

; Wildcards
(string-match "test*" "test123")          ; * matches zero or more
(string-match "test?" "test1")            ; ? matches exactly one

; ===== PRACTICAL EXAMPLES =====

; Email validation
(define email-pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
(regex-match email-pattern "user@example.com")
(regex-match email-pattern "invalid.email")

; Phone number extraction
(regex-find-all "\\d{3}-\\d{4}" "Call 555-1234 or 555-5678")

; URL parsing
(regex-extract "(https?)://([^/]+)(/.*)?" "https://example.com/path")

; Extract all numbers from text
(regex-find-all "\\d+" "I have 3 apples and 5 oranges")

; Clean whitespace
(regex-replace-all "\\s+" " " "hello    world   test")

; Extract words starting with capital letter
(regex-find-all "[A-Z][a-z]+" "Hello World Test")

; Password validation (at least one digit, one letter, 8+ chars)
(define password-pattern "^(?=.*[A-Za-z])(?=.*\\d)[A-Za-z\\d]{8,}$")
(regex-match password-pattern "Password123")
(regex-match password-pattern "weak")

; Extract hashtags
(regex-find-all "#\\w+" "Check out #lisp and #programming!")

; Remove HTML tags
(regex-replace-all "<[^>]+>" "" "<p>Hello <b>World</b></p>")

; Extract IPv4 addresses
(regex-find-all "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}" "Server at 192.168.1.1 and 10.0.0.1")
