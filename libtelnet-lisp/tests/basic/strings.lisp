; Advanced string operations

; Split test
(split "apple-banana-cherry" "-")      ; => ("apple" "banana" "cherry")

; String contains
(string-contains "hello world" "world") ; => 1

; String match with wildcards
(string-match "hello" "h*o")           ; => 1

; String comparisons
(string< "abc" "def")                  ; => 1
