;; Test string-index builtin function

;; Basic tests
(string-index "hello world" "world")  ; => 6
(string-index "hello world" "hello")  ; => 0
(string-index "hello world" "o")      ; => 4
(string-index "hello world" " ")      ; => 5

;; Not found
(string-index "hello world" "xyz")    ; => nil
(string-index "hello world" "WORLD")  ; => nil

;; Empty strings
(string-index "" "x")                 ; => nil
(string-index "hello" "")             ; => 0

;; UTF-8 support
(string-index "Hello, 世界!" "世")     ; => 7
(string-index "Hello, 世界!" "!")      ; => 10
(string-index "🌍🌎🌏" "🌎")           ; => 1

;; Multi-character needles
(string-index "abcdef" "cd")          ; => 2
(string-index "abcdef" "cde")         ; => 2
(string-index "abcdef" "def")         ; => 3

;; Case sensitivity
(string-index "Hello" "hello")        ; => nil
(string-index "hello" "Hello")        ; => nil
