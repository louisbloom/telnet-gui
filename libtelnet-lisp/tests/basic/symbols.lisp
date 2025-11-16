;;; Test symbol operations

;; Test symbol? predicate
(symbol? 'foo)              ; => 1
(symbol? 'bar)              ; => 1
(symbol? '+)                ; => 1
(symbol? 'my-symbol)        ; => 1

;; Test symbol? with non-symbols
(symbol? 42)                ; => nil
(symbol? "string")          ; => nil
(symbol? #t)                ; => nil
(symbol? '(1 2 3))          ; => nil
(symbol? (cons 1 2))        ; => nil

;; Test symbol->string
(symbol->string 'foo)       ; => "foo"
(symbol->string 'bar)       ; => "bar"
(symbol->string '+)         ; => "+"
(symbol->string 'my-symbol) ; => "my-symbol"
(symbol->string 'hello-world) ; => "hello-world"

;; Test symbol->string with variables
(define x 'test-symbol)     ; ignore
(symbol? x)                 ; => 1
(symbol->string x)          ; => "test-symbol"

;; Test converting symbol to string and using it
(define sym 'example)       ; ignore
(define str (symbol->string sym))  ; ignore
(string? str)               ; => 1
(concat str "-suffix")      ; => "example-suffix"

;; Test symbol? with quoted expressions
(symbol? (quote foo))       ; => 1
(symbol? 'x)                ; => 1

;; Test with various symbol names
(symbol? 'CamelCase)        ; => 1
(symbol? 'kebab-case)       ; => 1
(symbol? 'snake_case)       ; => 1
(symbol? '*)                ; => 1
(symbol? '/)                ; => 1
(symbol? '?)                ; => 1
(symbol? '!)                ; => 1

;; Convert various symbols to strings
(symbol->string '*)         ; => "*"
(symbol->string '/)         ; => "/"
(symbol->string 'null?)     ; => "null?"
(symbol->string 'set!)      ; => "set!"

;; Test error handling for symbol->string with non-symbol
;; (symbol->string 42)      ; This would error: "symbol->string requires a symbol argument"
;; (symbol->string "foo")   ; This would error: "symbol->string requires a symbol argument"
