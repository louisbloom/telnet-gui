;; Regular Expression Operations
;; Demonstration of PCRE2 regex pattern matching and manipulation

(load "tests/test-helpers.lisp")

;; ===========================================
;; Basic Regex Matching
;; ===========================================

;; Test digit matching
(assert-true (regex-match "\\d+" "hello123") "regex-match digits")

;; Test character class matching
(assert-true (regex-match "^[a-z]+$" "hello") "regex-match character class")

;; Simple substring matching
(assert-true (regex-match "test" "this is a test") "regex-match substring")

;; ===========================================
;; Finding Matches
;; ===========================================

;; Find first match
(assert-equal (regex-find "\\d+" "abc123def") "123" "regex-find first match")

;; Find all matches
(assert-equal (regex-find-all "\\d+" "a1b2c3") '("1" "2" "3") "regex-find-all digits")

;; Find all words
(assert-equal (regex-find-all "\\w+" "hello world test") '("hello" "world" "test") "regex-find-all words")

;; ===========================================
;; Extracting Capture Groups
;; ===========================================

;; Extract email username and domain
(assert-equal (regex-extract "(\\w+)@(\\w+)" "user@domain") '("user" "domain") "regex-extract email parts")

;; Extract date components (YYYY-MM-DD)
(assert-equal (regex-extract "(\\d+)-(\\d+)-(\\d+)" "2025-10-24") '("2025" "10" "24") "regex-extract date parts")

;; Extract code parts
(assert-equal (regex-extract "([A-Z]+)-(\\d+)" "ABC-123") '("ABC" "123") "regex-extract code parts")

;; ===========================================
;; Replacing Text
;; ===========================================

;; Replace all occurrences (replaces all by default)
(assert-equal (regex-replace "\\d+" "a1b2c3" "X") "aXbXcX" "regex-replace all digits")

;; Replace all occurrences explicitly
(assert-equal (regex-replace-all "\\d+" "a1b2c3" "X") "aXbXcX" "regex-replace-all digits")

;; Replace with capture groups - swap email parts
(assert-equal (regex-replace "(\\w+)@(\\w+)" "user@domain" "$2@$1") "domain@user" "regex-replace swap email parts")

;; Replace with capture groups - change date format
(assert-equal (regex-replace "(\\d+)-(\\d+)" "2025-10" "$2/$1") "10/2025" "regex-replace date format")

;; ===========================================
;; Replacing - Partial Match Tests
;; ===========================================

;; Replace only matched portion (trailing comma)
(assert-equal (regex-replace ",$" "hello," "X") "helloX" "regex-replace trailing comma")

;; Remove trailing punctuation (empty replacement)
(assert-equal (regex-replace "a+$" "baaa" "") "b" "regex-replace remove trailing")

;; Remove leading punctuation
(assert-equal (regex-replace "^a+" "aaab" "") "b" "regex-replace remove leading")

;; ===========================================
;; Splitting Strings
;; ===========================================

;; Split by whitespace
(assert-equal (regex-split "\\s+" "hello  world  test") '("hello" "world" "test") "regex-split by whitespace")

;; Split by comma
(assert-equal (regex-split "," "apple,banana,cherry") '("apple" "banana" "cherry") "regex-split by comma")

;; Split by multiple delimiters
(assert-equal (regex-split "[,;]" "a,b;c,d") '("a" "b" "c" "d") "regex-split by multiple delimiters")

;; ===========================================
;; Utility Functions
;; ===========================================

;; Escape special regex characters
(assert-equal (regex-escape "test.string*with?special") "test\\.string\\*with\\?special" "regex-escape special characters")

;; Check if regex pattern is valid
(assert-true (regex-valid? "\\d+") "regex-valid? valid pattern")
(assert-false (regex-valid? "[invalid") "regex-valid? invalid pattern")

;; ===========================================
;; Enhanced Wildcard Matching (using regex-match for pattern matching)
;; ===========================================

;; Character classes - match any of a, b, or c
(assert-true (regex-match "[abc]test" "atest") "regex-match character class")

;; Character ranges - match lowercase
(assert-true (regex-match "[a-z]+" "hello") "regex-match character range")

;; Negated classes - match non-digits
(assert-true (regex-match "[^0-9]+" "abc") "regex-match negated class")

;; Wildcards - .* matches zero or more
(assert-true (regex-match "test.*" "test123") "regex-match asterisk wildcard")

;; Wildcards - . matches exactly one
(assert-true (regex-match "test." "test1") "regex-match question wildcard")

;; ===========================================
;; Practical Examples
;; ===========================================

;; Email validation
(define email-pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
(assert-true (regex-match email-pattern "user@example.com") "email validation valid")
(assert-false (regex-match email-pattern "invalid.email") "email validation invalid")

;; Extract phone numbers
(assert-equal (regex-find-all "\\d{3}-\\d{4}" "Call 555-1234 or 555-5678") '("555-1234" "555-5678") "extract phone numbers")

;; URL parsing
(assert-equal (regex-extract "(https?)://([^/]+)(/.*)?" "https://example.com/path") '("https" "example.com" "/path") "URL parsing")

;; Extract all numbers from text
(assert-equal (regex-find-all "\\d+" "I have 3 apples and 5 oranges") '("3" "5") "extract numbers")

;; Clean multiple whitespace
(assert-equal (regex-replace-all "\\s+" "hello    world   test" " ") "hello world test" "clean multiple whitespace")

;; Extract capitalized words
(assert-equal (regex-find-all "[A-Z][a-z]+" "Hello World Test") '("Hello" "World" "Test") "extract capitalized words")

;; Password validation (8+ chars, alphanumeric)
(define password-pattern "^(?=.*[A-Za-z])(?=.*\\d)[A-Za-z\\d]{8,}$")
(assert-true (regex-match password-pattern "Password123") "password validation strong")
(assert-false (regex-match password-pattern "weak") "password validation weak")

;; Extract hashtags
(assert-equal (regex-find-all "#\\w+" "Check out #lisp and #programming!") '("#lisp" "#programming") "extract hashtags")

;; Remove HTML tags
(assert-equal (regex-replace-all "<[^>]+>" "<p>Hello <b>World</b></p>" "") "Hello World" "remove HTML tags")

;; Extract IPv4 addresses
(assert-equal (regex-find-all "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}" "Server at 192.168.1.1 and 10.0.0.1") '("192.168.1.1" "10.0.0.1") "extract IPv4 addresses")
