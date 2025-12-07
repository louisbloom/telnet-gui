;;; Test symbol operations

;; Load test helper macros
(load "tests/test-helpers.lisp")

;; Test symbol? predicate
(assert-true (symbol? 'foo) "symbol? recognizes 'foo")
(assert-true (symbol? 'bar) "symbol? recognizes 'bar")
(assert-true (symbol? '+) "symbol? recognizes '+")
(assert-true (symbol? 'my-symbol) "symbol? recognizes 'my-symbol")

;; Test symbol? with non-symbols
(assert-nil (symbol? 42) "symbol? rejects integer")
(assert-nil (symbol? "string") "symbol? rejects string")
(assert-nil (symbol? #t) "symbol? rejects boolean")
(assert-nil (symbol? '(1 2 3)) "symbol? rejects list")
(assert-nil (symbol? (cons 1 2)) "symbol? rejects cons cell")

;; Test symbol->string
(assert-equal (symbol->string 'foo) "foo" "symbol->string converts 'foo")
(assert-equal (symbol->string 'bar) "bar" "symbol->string converts 'bar")
(assert-equal (symbol->string '+) "+" "symbol->string converts '+")
(assert-equal (symbol->string 'my-symbol) "my-symbol" "symbol->string converts 'my-symbol")
(assert-equal (symbol->string 'hello-world) "hello-world" "symbol->string converts 'hello-world")

;; Test symbol->string with variables
(define x 'test-symbol)
(assert-true (symbol? x) "variable holds symbol")
(assert-equal (symbol->string x) "test-symbol" "symbol->string on variable")

;; Test converting symbol to string and using it
(define sym 'example)
(define str (symbol->string sym))
(assert-true (string? str) "symbol->string result is a string")
(assert-equal (concat str "-suffix") "example-suffix" "concat with converted symbol")

;; Test symbol? with quoted expressions
(assert-true (symbol? (quote foo)) "symbol? with quote form")
(assert-true (symbol? 'x) "symbol? with quoted 'x")

;; Test with various symbol names
(assert-true (symbol? 'CamelCase) "symbol? recognizes CamelCase")
(assert-true (symbol? 'kebab-case) "symbol? recognizes kebab-case")
(assert-true (symbol? 'snake_case) "symbol? recognizes snake_case")
(assert-true (symbol? '*) "symbol? recognizes '*")
(assert-true (symbol? '/) "symbol? recognizes '/")
(assert-true (symbol? '?) "symbol? recognizes '?")
(assert-true (symbol? '!) "symbol? recognizes '!")

;; Convert various symbols to strings
(assert-equal (symbol->string '*) "*" "symbol->string converts '*")
(assert-equal (symbol->string '/) "/" "symbol->string converts '/")
(assert-equal (symbol->string 'null?) "null?" "symbol->string converts 'null?")
(assert-equal (symbol->string 'set!) "set!" "symbol->string converts 'set!")

;; Test error handling for symbol->string with non-symbol
;; (symbol->string 42)      ; This would error: "symbol->string requires a symbol argument"
;; (symbol->string "foo")   ; This would error: "symbol->string requires a symbol argument"
