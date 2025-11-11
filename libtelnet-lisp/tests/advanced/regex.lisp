;; Regular Expression Operations
;; Demonstration of PCRE2 regex pattern matching and manipulation

;; ===========================================
;; Basic Regex Matching
;; ===========================================

;; Test digit matching
(regex-match "\\d+" "hello123")                ; => 1

;; Test character class matching
(regex-match "^[a-z]+$" "hello")               ; => 1

;; Simple substring matching
(regex-match "test" "this is a test")          ; => 1

;; ===========================================
;; Finding Matches
;; ===========================================

;; Find first match
(regex-find "\\d+" "abc123def")                ; => "123"

;; Find all matches
(regex-find-all "\\d+" "a1b2c3")               ; => ("1" "2" "3")

;; Find all words
(regex-find-all "\\w+" "hello world test")      ; => ("hello" "world" "test")

;; ===========================================
;; Extracting Capture Groups
;; ===========================================

;; Extract email username and domain
(regex-extract "(\\w+)@(\\w+)" "user@domain")  ; => ("user" "domain")

;; Extract date components (YYYY-MM-DD)
(regex-extract "(\\d+)-(\\d+)-(\\d+)" "2025-10-24") ; => ("2025" "10" "24")

;; Extract code parts
(regex-extract "([A-Z]+)-(\\d+)" "ABC-123")     ; => ("ABC" "123")

;; ===========================================
;; Replacing Text
;; Arguments: (regex-replace PATTERN REPLACEMENT STRING)
;; ===========================================

;; Replace all occurrences (replaces all by default)
;; Pattern: "\d+" (one or more digits)
;; Replacement: "X"
;; String: "a1b2c3"
(regex-replace "\\d+" "X" "a1b2c3")           ; => "aXbXcX"

;; Replace all occurrences explicitly
;; Pattern: "\d+" (one or more digits)
;; Replacement: "X"
;; String: "a1b2c3"
(regex-replace-all "\\d+" "X" "a1b2c3")       ; => "aXbXcX"

;; Replace with capture groups - swap email parts
;; Pattern: "(\w+)@(\w+)" (username@domain)
;; Replacement: "$2@$1" (domain@username)
;; String: "user@domain"
(regex-replace "(\\w+)@(\\w+)" "$2@$1" "user@domain") ; => "domain@user"

;; Replace with capture groups - change date format
;; Pattern: "(\d+)-(\d+)" (year-month)
;; Replacement: "$2/$1" (month/year)
;; String: "2025-10"
(regex-replace "(\\d+)-(\\d+)" "$2/$1" "2025-10")      ; => "10/2025"

;; ===========================================
;; Replacing - Partial Match Tests
;; Arguments: (regex-replace PATTERN REPLACEMENT STRING)
;; ===========================================

;; Replace only matched portion (trailing comma)
;; Pattern: ",$" (comma at end)
;; Replacement: "X"
;; String: "hello,"
(regex-replace ",$" "X" "hello,")              ; => "helloX"

;; Remove trailing punctuation (empty replacement)
;; Pattern: "a+$" (one or more 'a' at end)
;; Replacement: "" (empty string)
;; String: "baaa"
(regex-replace "a+$" "" "baaa")                ; => "b"

;; Remove leading punctuation
;; Pattern: "^a+" (one or more 'a' at start)
;; Replacement: "" (empty string)
;; String: "aaab"
(regex-replace "^a+" "" "aaab")                ; => "b"

;; ===========================================
;; Splitting Strings
;; ===========================================

;; Split by whitespace
(regex-split "\\s+" "hello  world  test")      ; => ("hello" "world" "test")

;; Split by comma
(regex-split "," "apple,banana,cherry")         ; => ("apple" "banana" "cherry")

;; Split by multiple delimiters
(regex-split "[,;]" "a,b;c,d")                 ; => ("a" "b" "c" "d")

;; ===========================================
;; Utility Functions
;; ===========================================

;; Escape special regex characters
(regex-escape "test.string*with?special")      ; => "test\\.string\\*with\\?special"

;; Check if regex pattern is valid
(regex-valid? "\\d+")                          ; => 1
(regex-valid? "[invalid")                      ; => 0

;; ===========================================
;; Enhanced Wildcard Matching
;; ===========================================

;; Character classes - match any of a, b, or c
(string-match? "[abc]test" "atest")             ; => 1

;; Character ranges - match lowercase
(string-match? "[a-z]+" "hello")                ; => 1

;; Negated classes - match non-digits
(string-match? "[!0-9]+" "abc")                 ; => 1

;; Wildcards - * matches zero or more
(string-match? "test*" "test123")                ; => 1

;; Wildcards - ? matches exactly one
(string-match? "test?" "test1")                 ; => 1

;; ===========================================
;; Practical Examples
;; ===========================================

;; Email validation
(define email-pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
(regex-match email-pattern "user@example.com") ; => 1
(regex-match email-pattern "invalid.email")    ; => 0

;; Extract phone numbers
(regex-find-all "\\d{3}-\\d{4}" "Call 555-1234 or 555-5678") ; => ("555-1234" "555-5678")

;; URL parsing
(regex-extract "(https?)://([^/]+)(/.*)?" "https://example.com/path")
;; Result: ("https" "example.com" "/path")

;; Extract all numbers from text
(regex-find-all "\\d+" "I have 3 apples and 5 oranges") ; => ("3" "5")

;; Clean multiple whitespace
(regex-replace-all "\\s+" " " "hello    world   test")    ; => "hello world test"

;; Extract capitalized words
(regex-find-all "[A-Z][a-z]+" "Hello World Test")        ; => ("Hello" "World" "Test")

;; Password validation (8+ chars, alphanumeric)
(define password-pattern "^(?=.*[A-Za-z])(?=.*\\d)[A-Za-z\\d]{8,}$")
(regex-match password-pattern "Password123")   ; => 1
(regex-match password-pattern "weak")           ; => 0

;; Extract hashtags
(regex-find-all "#\\w+" "Check out #lisp and #programming!") ; => ("#lisp" "#programming")

;; Remove HTML tags
(regex-replace-all "<[^>]+>" "" "<p>Hello <b>World</b></p>") ; => "Hello World"

;; Extract IPv4 addresses
(regex-find-all "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}" "Server at 192.168.1.1 and 10.0.0.1")
;; Result: ("192.168.1.1" "10.0.0.1")
