# Telnet Lisp Language Reference

A reference for the Telnet Lisp language, covering data types, special forms, built-in functions, and usage examples.

## Table of Contents

- [Data Types](#data-types)
- [Special Forms](#special-forms)
- [Built-in Functions](#built-in-functions)
- [Type Predicates](#type-predicates)
- [Naming Conventions](#naming-conventions)
- [Truthy/Falsy Values](#truthyfalsy-values)
- [Pattern Matching](#pattern-matching)
- [Error Handling](#error-handling)
- [Tail Recursion Optimization](#tail-recursion-optimization)
- [Quick Examples](#quick-examples)

## Data Types

- **Numbers**: Double precision floating-point numbers
- **Integers**: 64-bit signed integers
- **Booleans**: True (#t) and false (#f)
- **Strings**: C-style null-terminated strings with full UTF-8 support
- **Lists**: Cons cells for linked lists
- **Vectors**: Dynamic arrays (grow/shrink)
- **Hash Tables**: Key-value mappings
- **Symbols**: Variable names and function symbols
- **Lambda Functions**: User-defined closures with lexical scoping

## Special Forms

- `quote` - Quote expressions (also supported via `'expr` syntax)
- `quasiquote` - Quote with selective evaluation (also supported via `` `expr`` syntax)
- `unquote` - Evaluate within quasiquote (also supported via `,expr` syntax)
- `unquote-splicing` - Evaluate and splice list within quasiquote (also supported via `,@expr` syntax)
- `if` - Conditional evaluation
- `define` - Define variables and functions
- `set!` - Mutate existing variables (works inside lambdas/hooks)
- `lambda` - Create anonymous functions with parameters (body has implicit progn: evaluates all expressions, returns last with tail recursion optimization)
- `defmacro` - Define macros for code transformation at evaluation time
- `let` - Local variable bindings (parallel evaluation, body has implicit progn)
- `let*` - Local variable bindings (sequential evaluation, can reference previous bindings, body has implicit progn)
- `progn` - Evaluate multiple expressions sequentially and return last value
- `do` - Iteration loop with variable updates and exit condition
- `cond` - Multi-way conditional with test clauses
- `case` - Pattern matching with value-based dispatch

### Macros

Macros are special functions that receive unevaluated arguments and return code to be evaluated. They enable powerful code transformations at evaluation time.

#### Quasiquote (Backquote)

Quasiquote (`` ` ``) is like quote (`'`) but allows selective evaluation using unquote (`,`) and unquote-splicing (`,@`). This is essential for writing macros that construct code with computed values.

**Syntax:**
- `` `expr`` - Quasiquote (same as `(quasiquote expr)`)
- `,expr` - Unquote: evaluate expr and insert result (same as `(unquote expr)`)
- `,@expr` - Unquote-splicing: evaluate expr (must be a list) and splice elements (same as `(unquote-splicing expr)`)

**Examples:**
```lisp
; Simple quasiquote (same as quote)
`(1 2 3)  ; => (1 2 3)

; Unquote to insert computed values
(define x 42)
`(1 ,x 3)  ; => (1 42 3)
`(result is ,(+ 10 20))  ; => (result is 30)

; Nested lists
`(a (b ,x c) d)  ; => (a (b 42 c) d)

; Unquote-splicing to splice lists
(define lst '(a b c))
`(1 ,@lst 4)  ; => (1 a b c 4)
`(start ,x middle ,@lst end)  ; => (start 42 middle a b c end)

; Multiple splicing
`(,@lst ,@lst)  ; => (a b c a b c)

; Empty list splicing
`(1 ,@'() 2)  ; => (1 2)
```

**Common pattern in macros:**
```lisp
; Without quasiquote (cumbersome)
(defmacro when (condition body)
  (list 'if condition body nil))

; With quasiquote (clean)
(defmacro when (condition body)
  `(if ,condition ,body nil))
```

#### defmacro

Define a macro that transforms code before evaluation.

**Syntax:**
```lisp
(defmacro name (params...) body...)
(defmacro name (param1 param2 . rest-params) body...)
```

**Features:**
- Macros receive arguments unevaluated
- The macro body is evaluated to produce an expansion
- The expansion is then evaluated in the caller's context
- Supports rest parameters using dotted parameter list syntax

**Examples:**
```lisp
; Simple macro that returns a quoted expression
(defmacro simple () '(+ 1 2))
(simple)  ; => 3

; Macro with parameters
(defmacro double (x) (list '+ x x))
(double 5)          ; => 10
(double (+ 2 3))    ; => 10

; Conditional macro (like 'when')
(defmacro when (condition body)
  (list 'if condition body nil))
(when #t 42)   ; => 42
(when #f 42)   ; => nil

; Macro with rest parameters
(defmacro my-progn (first . rest)
  (cons 'progn (cons first rest)))
```

#### defun

`defun` is a built-in macro that provides a convenient syntax for defining named functions. It's implemented using quasiquote:

**Syntax:**
```lisp
(defun name (params...) body...)
```

**Definition:**
```lisp
(defmacro defun (name params . body)
  `(define ,name (lambda ,params ,@body)))
```

**Expands to:**
```lisp
(define name (lambda (params...) body...))
```

**Examples:**
```lisp
; Simple function
(defun add-one (x) (+ x 1))
(add-one 5)  ; => 6

; Multiple parameters
(defun add-three (a b c)
  (+ a b c))
(add-three 1 2 3)  ; => 6

; Multiple body expressions
(defun test-multi (x)
  (+ x 1)
  (+ x 2)
  (+ x 3))
(test-multi 10)  ; => 13

; Recursive function
(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
(factorial 5)  ; => 120
```

## Built-in Functions

### Arithmetic Functions

- `+` - Add numbers (supports multiple arguments, integer + integer = integer, mixed = float)
- `-` - Subtract numbers (supports unary negation, integer - integer = integer, mixed = float)
- `*` - Multiply numbers (supports multiple arguments, integer \* integer = integer, mixed = float)
- `/` - Divide numbers (always returns float, supports unary reciprocal)

### Integer Operations

- `quotient` - Integer division (truncates to integer, e.g., quotient 10 3 → 3)
- `remainder` - Integer remainder/modulo operation (e.g., remainder 17 5 → 2)
- `even?` - Check if number is even (returns #t for even, #f for odd)
- `odd?` - Check if number is odd (returns #t for odd, #f for even)

### Number Comparisons

- `>` - Greater than
- `<` - Less than
- `=` - Equal to
- `>=` - Greater than or equal
- `<=` - Less than or equal

### String Functions

- `concat` - Concatenate multiple strings
- `split` - Split string by pattern (supports wildcards: `*`, `?`, `[]`)
- `string-length` - Get character count (UTF-8 aware, not byte count)
- `substring` - Extract substring by character indices (UTF-8 aware)
- `string-ref` - Get character at index (UTF-8 aware, returns single character string)
- `string-replace` - Replace all occurrences of substring in string
- `string-upcase` - Convert string to uppercase (UTF-8 aware, ASCII only)
- `string-downcase` - Convert string to lowercase (UTF-8 aware, ASCII only)

### String Comparisons

- `string=` - String equality
- `string<` - String less than (lexicographic)
- `string>` - String greater than (lexicographic)
- `string<=` - String less than or equal (lexicographic)
- `string>=` - String greater than or equal (lexicographic)

### String Predicates

- `string-contains?` - Check if string contains substring
- `string-match?` - Match string against wildcard pattern
- `string-prefix?` - Check if one string is a prefix of another

### Boolean Functions

- `and` - Logical AND (returns last truthy value or nil)
- `or` - Logical OR (returns first truthy value or nil)
- `not` - Logical negation

### List Functions

- `car` - Get first element of list
- `cdr` - Get rest of list
- `cons` - Construct new list cell
- `list` - Create list from arguments
- `list-length` - Get length of list
- `list-ref` - Get element at index (0-based)
- `null?` - Check if list is empty
- `atom?` - Check if value is an atom (not a list)

### Association List (Alist) Functions

Association lists are lists of key-value pairs represented as cons cells: `((key1 . value1) (key2 . value2) ...)`.

- `assoc` - Find pair by key using value equality (equal), returns the full pair `(key . value)` or nil
- `assq` - Find pair by key using pointer equality (eq), returns the full pair `(key . value)` or nil
- `assv` - Find pair by key using equivalence equality (eqv), returns the full pair `(key . value)` or nil
- `alist-get` - Get value for key, returns the value (cdr of pair) or default value if provided

**Examples:**
```lisp
(define config '(("host" . "localhost") ("port" . 8080) ("debug" . #t)))

; Find pair by key
(assoc "host" config)           ; => ("host" . "localhost")
(assoc "missing" config)        ; => nil

; Get value directly
(alist-get "port" config)       ; => 8080
(alist-get "timeout" config 30) ; => 30 (default value)

; Extract value from assoc result
(cdr (assoc "debug" config))    ; => #t
```

### Mapping Functions

Functions for transforming lists by applying a function to each element.

- `map` - Apply function to each element of list, returns new list of results
- `mapcar` - Same as map (Common Lisp compatibility)

**Examples:**
```lisp
; Double each number
(map (lambda (x) (* x 2)) '(1 2 3 4 5))  ; => (2 4 6 8 10)

; Get first element of each pair
(define people '(("Alice" . 25) ("Bob" . 30) ("Carol" . 35)))
(map (lambda (pair) (car pair)) people)  ; => ("Alice" "Bob" "Carol")

; Transform alist values
(map (lambda (pair) (cons (car pair) (* (cdr pair) 2)))
     '((1 . 10) (2 . 20)))               ; => ((1 . 20) (2 . 40))

; Map with builtin functions
(map car '((1 . 2) (3 . 4) (5 . 6)))    ; => (1 3 5)

; String operations
(map string-upcase '("hello" "world"))   ; => ("HELLO" "WORLD")
```

### Vector Functions

- `make-vector` - Create a vector of specified size, optionally with initial value for all elements
- `vector-ref` - Get element at index
- `vector-set!` - Set element at index (mutating)
- `vector-length` - Get vector size
- `vector-push!` - Append to end (mutating)
- `vector-pop!` - Remove from end (mutating)
- `vector?` - Check if value is a vector

### Hash Table Functions

- `make-hash-table` - Create a new hash table
- `hash-ref` - Get value for key (returns nil if not found)
- `hash-set!` - Set key-value pair (mutating)
- `hash-remove!` - Remove key-value pair (mutating)
- `hash-clear!` - Remove all entries (mutating)
- `hash-count` - Get number of entries
- `hash-keys` - Get list of all keys
- `hash-values` - Get list of all values
- `hash-entries` - Get list of `(key . value)` pairs

### Type Predicates

- `null?` - Check if nil
- `atom?` - Check if atom (not a list)
- `integer?` - Check if integer
- `boolean?` - Check if boolean
- `number?` - Check if number or integer
- `string?` - Check if string
- `vector?` - Check if vector
- `hash-table?` - Check if hash table
- `symbol?` - Check if symbol

### Symbol Operations

- `symbol?` - Check if value is a symbol
- `symbol->string` - Convert symbol to string

**Examples:**
```lisp
(symbol? 'foo)              ; => 1
(symbol? 42)                ; => nil
(symbol? "string")          ; => nil

(symbol->string 'hello)     ; => "hello"
(symbol->string '+)         ; => "+"
(symbol->string 'my-var)    ; => "my-var"

; Using with variables
(define x 'test)
(symbol? x)                 ; => 1
(symbol->string x)          ; => "test"
```

### Regex Functions (PCRE2)

- `regex-match` - Test if string matches pattern (arguments: pattern, string)
- `regex-find` - Find first regex match (arguments: pattern, string)
- `regex-find-all` - Find all regex matches (arguments: pattern, string)
- `regex-extract` - Extract capture groups (arguments: pattern, string)
- `regex-replace` - Replace all matches (arguments: pattern, replacement, string)
- `regex-replace-all` - Replace all matches (arguments: pattern, replacement, string)
- `regex-split` - Split string by regex pattern (arguments: pattern, string)
- `regex-escape` - Escape special regex characters (arguments: string)
- `regex-valid?` - Validate regex pattern syntax (arguments: pattern)

### File I/O Functions

- `open` - Open a file (filename, mode) - returns file stream
- `close` - Close a file stream
- `read-line` - Read a line from file stream (returns string or nil at EOF), supports Unix (`\n`), Windows (`\r\n`), and Mac (`\r`) line endings
- `write-line` - Write a line to file stream
- `read-sexp` - Read S-expressions from file (filename or file stream) - returns single expression or list of expressions
- `read-json` - Read JSON from file (filename or file stream) - returns Lisp data structures (objects → hash tables, arrays → vectors, etc.)

### Printing Functions (Common Lisp Style)

- `princ` - Print object in human-readable form (strings without quotes), returns the object
- `prin1` - Print object in readable representation (strings with quotes), returns the object
- `print` - Print object like `prin1` but adds newline before and after, returns the object
- `format` - Formatted output with format directives (destination, format-string, args...)
- `terpri` - Print newline (terminate print), returns nil

**Format directives:**
- `~A` or `~a` - Aesthetic (princ-style, no quotes on strings)
- `~S` or `~s` - S-expression (prin1-style, with quotes on strings)
- `~%` - Newline
- `~~` - Literal tilde (~)

**Format destination:**
- `nil` - Return formatted string
- `#t` - Print to stdout and return nil

**Examples:**
```lisp
; Return as string
(format nil "Hello, ~A!" "World")        ; => "Hello, World!"
(format nil "~A + ~A = ~A" 2 3 5)        ; => "2 + 3 = 5"
(format nil "String: ~S" "test")         ; => "String: \"test\""
(format nil "Line 1~%Line 2")            ; => "Line 1\nLine 2"
(format nil "Tilde: ~~")                 ; => "Tilde: ~"

; Print to stdout
(format #t "Hello, ~A!~%" "World")       ; Prints: Hello, World!
                                          ; => nil

; Newline
(terpri)                                  ; Prints newline
                                          ; => nil
```

## Naming Conventions

Telnet Lisp follows modern Lisp naming conventions:

- **Predicates** use `?` suffix: `null?`, `vector?`, `integer?`, `boolean?`, `string?`
- **Mutating functions** use `!` suffix: `set!`, `vector-set!`, `vector-push!`, `hash-set!`
- **Non-mutating functions** have no special suffix: `car`, `cdr`, `vector-ref`, `hash-ref`

## Truthy/Falsy Values

Following JavaScript-like semantics:

- **Falsy**: `nil`, `0` (integer), empty string `""`, boolean `#f`, empty vectors, empty hash tables
- **Truthy**: Everything else, including `0.0` (float), non-empty strings, `#t`, non-empty vectors

**Note**: Integer `0` is falsy, but float `0.0` is truthy.

## Pattern Matching

The `split` and `string-match?` functions support enhanced wildcard patterns:

- `*` - Matches zero or more characters
- `?` - Matches exactly one character
- `[abc]` - Matches any character in set
- `[a-z]` - Matches character ranges
- `[!abc]` - Matches anything NOT in set

Examples:

```lisp
(string-match? "hello" "h*o")         ; => 1
(string-match? "hello" "h?llo")       ; => 1
(string-match? "hello" "h??lo")       ; => 1
(string-match? "hello" "h[aeiou]llo") ; => 1
(string-match? "hello" "h[a-z]llo")   ; => 1
```

## Error Handling

Telnet Lisp implements automatic error propagation with call stack traces. When an error occurs, it captures the full call stack showing the sequence of function calls that led to the error.

### How Errors Work

- Errors are created when invalid operations are attempted (division by zero, type mismatches, undefined symbols, etc.)
- Errors automatically propagate up the call stack
- Each error includes a descriptive message and the call stack
- The call stack shows both built-in functions and user-defined functions

### Error Examples

**Simple division by zero:**
```lisp
(/ 1 0)
; => ERROR: Division by zero
;    Call stack:
;      at "/"
```

**Error through lambda:**
```lisp
(define divide-by-zero (lambda (x) (/ x 0)))
(divide-by-zero 10)
; => ERROR: Division by zero
;    Call stack:
;      at "/"
;      at "divide-by-zero"
```

**Deep call stack:**
```lisp
(define inner (lambda (x) (/ x 0)))
(define middle (lambda (x) (inner x)))
(define outer (lambda (x) (middle x)))
(outer 10)
; => ERROR: Division by zero
;    Call stack:
;      at "/"
;      at "inner"
;      at "middle"
;      at "outer"
```

**Type error:**
```lisp
(define test (lambda (x) (+ x "not a number")))
(test 5)
; => ERROR: + requires numbers
;    Call stack:
;      at "+"
;      at "test"
```

### Common Error Types

- **Division by zero**: `(/ x 0)` or `(quotient x 0)` or `(remainder x 0)`
- **Type errors**: Wrong type passed to function (e.g., `(+ 1 "string")`)
- **Undefined symbols**: Using a variable that hasn't been defined
- **Argument count mismatch**: Wrong number of arguments to lambda
- **Invalid operations**: Invalid hash key, vector index out of bounds, etc.

### Error Propagation

Errors propagate automatically through:
- Function calls (both built-in and user-defined)
- Special forms (`if`, `let`, `do`, `cond`, `case`, etc.)
- Nested expressions

The call stack trace shows the full path from the error source to the top level, making debugging easier.

**Note**: Telnet Lisp currently does not have try-catch or exception handling constructs. Errors propagate to the top level and terminate evaluation. This may be enhanced in future versions.

## Tail Recursion Optimization

Telnet Lisp implements **tail call optimization** (TCO) using a trampoline-based approach. This allows recursive functions to execute without growing the call stack, enabling efficient recursive algorithms that would otherwise cause stack overflow.

### What is Tail Position?

An expression is in **tail position** if it is the last operation performed before returning from a function. The result of a tail-positioned expression becomes the return value of the enclosing function without any further computation.

**Tail position locations:**
- Last expression in a lambda body
- Last expression in `progn`, `let`, `let*`
- Both branches of `if` (then and else)
- Return expressions in `cond` and `case` clauses
- Body of `do` loop (result expression)

**NOT in tail position:**
- Arguments to function calls: `(+ 1 (factorial 5))` - factorial call is NOT in tail position
- Non-final expressions in sequence
- Operands of arithmetic or comparison operators

### How It Works

When a function call appears in tail position, instead of executing it immediately and growing the stack, the interpreter:
1. Creates a tail call object with the function and its evaluated arguments
2. Returns this object to the trampoline loop
3. The trampoline unwraps and executes the call iteratively

This converts deep recursion into iteration at the interpreter level.

### Examples

**Tail-recursive factorial:**
```lisp
;; Tail-recursive with accumulator
(define factorial-tail
  (lambda (n acc)
    (if (<= n 1)
        acc
        (factorial-tail (- n 1) (* n acc)))))

(define factorial
  (lambda (n)
    (factorial-tail n 1)))

(factorial 10)      ; => 3628800
(factorial 1000)    ; Works without stack overflow!
```

**Tail-recursive fibonacci:**
```lisp
(define fib-iter
  (lambda (n a b)
    (if (= n 0)
        a
        (fib-iter (- n 1) b (+ a b)))))

(define fibonacci
  (lambda (n)
    (fib-iter n 0 1)))

(fibonacci 20)      ; => 6765
(fibonacci 100)     ; => 354224848179262000000
```

**Mutually recursive functions:**
```lisp
(define even?
  (lambda (n)
    (if (= n 0)
        #t
        (odd? (- n 1)))))

(define odd?
  (lambda (n)
    (if (= n 0)
        #f
        (even? (- n 1)))))

(even? 1000)        ; => #t (works without stack overflow)
```

**Helper function in tail position:**
```lisp
(define format-result
  (lambda (x)
    (string-append "Result: " (number->string x))))

(define compute-and-format
  (lambda (x y)
    (if (> x y)
        (format-result (* x y))    ; Tail call
        (format-result (+ x y))))) ; Tail call

(compute-and-format 10 5)  ; => "Result: 50"
```

### Non-Tail Recursion (Still Works)

Non-tail recursive functions still work but use stack space:

```lisp
;; NOT tail-recursive (multiplication happens after recursive call)
(define factorial-normal
  (lambda (n)
    (if (<= n 1)
        1
        (* n (factorial-normal (- n 1))))))  ; NOT in tail position

(factorial-normal 10)   ; => 3628800 (works but uses stack)
```

### Benefits

- **No stack overflow**: Recursive algorithms can run indefinitely
- **Constant memory**: Stack size remains constant regardless of recursion depth
- **Performance**: Recursion is as efficient as iteration
- **Functional style**: Write clean recursive code without worrying about stack limits

### When to Use

Use tail recursion when:
- Implementing recursive algorithms (factorial, fibonacci, tree traversal)
- Processing lists recursively
- State machines with recursive state transitions
- Any algorithm that would naturally use iteration

Convert to tail recursion by:
- Adding accumulator parameters
- Passing partial results down instead of building up on return
- Ensuring recursive call is the last operation

## Quick Examples

### Basic Arithmetic

```lisp
(+ 1 2 3 4)        ; => 10 (integer)
(+ 1 2.5)          ; => 3.5 (float - mixed types)
(+ 1.5 2.5)        ; => 4.0 (float)
(- 10 3)           ; => 7 (integer)
(- 10 3.5)         ; => 6.5 (float - mixed types)
(* 2 3 4)          ; => 24 (integer)
(* 3 4.0)          ; => 12.0 (float - mixed types)
(/ 10 2)           ; => 5.0 (always float)
(quotient 10 3)    ; => 3 (integer division)
(remainder 17 5)   ; => 2
(even? 6)          ; => #t
(odd? 7)           ; => #t
```

### Variables

```lisp
(define x 42)
(define y 10)
(+ x y)            ; => 52
```

### Functions

```lisp
; Define a function
(define square (lambda (n) (* n n)))
(square 5)         ; => 25

; Define a function with multiple operations
(define abs (lambda (n)
  (if (< n 0)
    (- n)
    n)))
(abs -5)           ; => 5
```

### Conditionals

```lisp
(if (> 5 3) "yes" "no")              ; => "yes"
(if (< 5 3) "yes" "no")              ; => "no"
(if nil "truthy" "falsy")            ; => "falsy"
(if 0 "truthy" "falsy")              ; => "falsy" (integer 0 is falsy!)
```

### String Operations

```lisp
; Concatenation
(concat "Hello" " " "World")         ; => "Hello World"

; Split by pattern
(split "a,b,c" ",")                  ; => ("a" "b" "c")
(split "foo*bar*baz" "*")            ; => ("foo" "bar" "baz")

; String comparisons
(string= "hello" "hello")            ; => 1
(string< "abc" "def")                ; => 1
(string-contains? "hello world" "world") ; => 1
(string-match? "hello" "h*o")        ; => 1
(string-prefix? "hel" "hello")       ; => 1
(string-prefix? "lis" "lisp")        ; => 1

; UTF-8 string operations
(string-length "Hello, 世界! 🌍")     ; => 15 (character count)
(substring "Hello, 世界!" 7 9)        ; => "世界"
(string-ref "Hello, 世界!" 7)        ; => "世"

; String transformations
(string-replace "world" "universe" "hello world")  ; => "hello universe"
(string-replace "l" "L" "hello")                   ; => "heLLo"
(string-upcase "hello world")                     ; => "HELLO WORLD"
(string-downcase "HELLO WORLD")                    ; => "hello world"
```

### Regex Operations

**Note:** Regex replace functions use the argument order: `(regex-replace PATTERN REPLACEMENT STRING)`

```lisp
; Basic regex matching
(regex-match "\\d+" "hello123")      ; => 1
(regex-match "^hello$" "hello")      ; => 1

; Finding matches
(regex-find "\\d+" "abc123def")      ; => "123"
(regex-find-all "[a-z]+" "hello world")  ; => ("hello" "world")

; Extracting capture groups
(regex-extract "(\\d+)-(\\d+)-(\\d+)" "2025-10-24")  ; => ("2025" "10" "24")

; Replacing text - replaces ALL occurrences (both functions behave the same)
; Arguments: (regex-replace PATTERN REPLACEMENT STRING)
(regex-replace "\\d+" "X" "a1b2c3")  ; => "aXbXcX" (replaces all)
(regex-replace-all "\\s+" "_" "hello  world")  ; => "hello_world"

; Replace only matched portion, not entire string
(regex-replace ",$" "X" "hello,")    ; => "helloX" (replaces comma only)
(regex-replace "a+$" "" "baaa")      ; => "b" (removes trailing a's)

; Splitting by regex
(regex-split "," "apple,banana,cherry")  ; => ("apple" "banana" "cherry")

; Escaping special characters
(regex-escape "test.string*")        ; => "test\\.string\\*"

; Validating patterns
(regex-valid? "\\d+")               ; => 1
(regex-valid? "[invalid")           ; => nil
```

### Boolean Operations

```lisp
(and 1 2 3)                          ; => 3 (returns last truthy value)
(and 1 nil 3)                        ; => nil
(or nil nil 5)                       ; => 5
(or nil nil)                         ; => nil
(not nil)                            ; => 1
(not 5)                              ; => nil
```

### Number Comparisons

```lisp
(> 5 3)                              ; => 1
(< 5 3)                              ; => nil
(= 5 5)                              ; => 1
(>= 5 5)                             ; => 1
(<= 3 5)                             ; => 1
```

### Lists

```lisp
(list 1 2 3)                         ; => (1 2 3)
(car (list 1 2 3))                   ; => 1
(cdr (list 1 2 3))                   ; => (2 3)
(cons 1 (list 2 3))                  ; => (1 2 3)
```

### Quotes

```lisp
(quote (1 2 3))                      ; => (1 2 3)
'(1 2 3)                             ; => (1 2 3) (shorthand)
'x                                   ; => x (symbol, not evaluated)
```

### Let Bindings

```lisp
(let ((x 10) (y 20))
  (+ x y))                           ; => 30
```

### Let\* Bindings (Sequential)

```lisp
; let* evaluates bindings sequentially
(let* ((x 5) (y (+ x 3))) y)         ; => 8

; Works with multiple dependent bindings
(let* ((a 10)
       (b (* a 2))
       (c (+ b 5)))
  c)                                  ; => 25

; Difference from let (which evaluates in parallel)
(let ((x 5) (y (+ x 3))) y)          ; ERROR: Undefined symbol: x
(let* ((x 5) (y (+ x 3))) y)         ; => 8 (works!)

; let* body behaves like progn - evaluates all expressions and returns last
(let* ((x 10))
  (+ x 5)
  (* x 2)
  (- x 3))                            ; => 7 (returns last expression)
```

### Progn (Sequential Evaluation)

```lisp
; progn evaluates expressions sequentially and returns the last value
(progn 1 2 3)                        ; => 3

; Used for executing multiple operations
(progn
  (define x 10)
  (define y 20)
  (+ x y))                           ; => 30

; With conditional logic
(progn
  (define a 5)
  (define b (* a 2))
  b)                                 ; => 10

; Empty progn returns NIL
(progn)                              ; => NIL
```

### Do Loop (Iteration)

```lisp
; Simple counter from 0 to 9
(do ((i 0 (+ i 1)))
    ((>= i 10) i))                   ; => 10

; Factorial using do loop
(define factorial-do
  (lambda (n)
    (do ((i n (- i 1))
         (acc 1 (* acc i)))
        ((<= i 0) acc))))

(factorial-do 5)                     ; => 120

; Count down with side effects
(do ((i 10 (- i 1)))
    ((<= i 0) "blastoff")
  i)                                  ; prints 10, 9, 8, ..., 1

; Multiple variables updating in parallel
(do ((i 1 (+ i 1))
     (sum 0))
    ((> i 10) sum)
  (set! sum (+ sum i)))              ; => 55 (sum 1 to 10)
```

### Cond and Case (Multi-way Conditionals)

```lisp
; Grade calculator using cond
(define grade
  (lambda (score)
    (cond
      ((>= score 90) "A")
      ((>= score 80) "B")
      ((>= score 70) "C")
      ((>= score 60) "D")
      (else "F"))))

(grade 95)  ; => "A"
(grade 75)  ; => "C"

; Day of week using case
(define day-name
  (lambda (n)
    (case n
      ((1) "Monday")
      ((2) "Tuesday")
      ((6 7) "Weekend")
      (else "Invalid"))))

(day-name 1)   ; => "Monday"
(day-name 6)   ; => "Weekend"
(day-name 99)  ; => "Invalid"
```

### Global State Management

```lisp
; Variables persist across the REPL session and can be updated
(define counter 0)                   ; Define initial value
counter                              ; => 0
(define counter 1)                   ; Update global state
counter                              ; => 1
(define counter (+ counter 5))       ; Update using previous value
counter                              ; => 6

; Use progn for multi-step state updates
(define balance 100)
(progn
  (define balance (+ balance 50))
  (define balance (- balance 30))
  balance)                           ; => 120

; Use set! to mutate from inside lambdas/hooks
(define counter 0)
(define increment (lambda ()
  (set! counter (+ counter 1))
  counter))
(increment)                         ; => 1
(increment)                         ; => 2
counter                             ; => 2 (globally updated!)

; Variables can be reassigned with different types
(define x 42)
x                                   ; => 42
(define x "hello")
x                                   ; => "hello"
```

### Set! for Mutating State

```lisp
; set! updates existing variables (variable must exist)
(define msg_count 0)
(set! msg_count 10)
msg_count                            ; => 10

; Works inside lambdas for stateful functions
(define counter 0)
(define increment (lambda ()
  (set! counter (+ counter 1))
  counter))
(increment)                         ; => 1
(increment)                         ; => 2
counter                             ; => 2

; Example: Stateful counter function
(define message_count 0)
(define count_messages (lambda ()
  (set! message_count (+ message_count 1))
  message_count))                    ; Return current count

(count_messages)                     ; => 1
(count_messages)                     ; => 2
message_count                        ; => 2
```

### Printing Functions (Common Lisp Style)

```lisp
; princ - prints without quotes (human-readable)
(princ "Hello, World!")              ; Prints: Hello, World! (no quotes)
(princ 42)                           ; Prints: 42
(princ '(1 2 3))                     ; Prints: (1 2 3)

; prin1 - prints with quotes (readable representation)
(prin1 "Hello, World!")              ; Prints: "Hello, World!" (with quotes)
(prin1 42)                           ; Prints: 42
(prin1 '(1 2 3))                     ; Prints: (1 2 3)

; print - like prin1 but adds newline before and after
(print "Hello, World!")              ; Prints: \n"Hello, World!"\n
(print 42)                           ; Prints: \n42\n
(print '(1 2 3))                     ; Prints: \n(1 2 3)\n

; All functions return the object being printed
(define result (princ "test"))       ; Prints: test, result => "test"
```

### File I/O Operations

```lisp
; Open a file for writing
(define logfile (open "output.log" "w"))

; Write lines
(write-line logfile "Log entry 1")
(write-line logfile "Log entry 2")

; Close the file
(close logfile)

; Read it back
(define file (open "output.log" "r"))
(read-line file)                     ; => "Log entry 1"
(read-line file)                     ; => "Log entry 2"
(read-line file)                     ; => nil (end of file)
(close file)

; Different modes
(define f (open "data.txt" "a"))    ; Append mode
(write-line f "new line")
(close f)

; Read S-expressions from file
(read-sexp "config.lisp")           ; => (list of expressions or single expression)

; Read JSON from file
(read-json "data.json")              ; => hash table (for objects) or other Lisp value
```

### Vector Operations

```lisp
; Create vector with initial value
(define v (make-vector 5 42))        ; Create vector of 5 elements, all set to 42
v                                    ; => #(42 42 42 42 42)
(vector-ref v 0)                     ; => 42
(vector-ref v 4)                     ; => 42

; Create empty vector and add elements
(define vec (make-vector 3))         ; Empty vector
(vector-push! vec 10)
(vector-push! vec 20)
(vector-push! vec 30)
vec                                  ; => #(10 20 30)

; Access and modify
(vector-ref vec 1)                   ; => 20
(vector-set! vec 1 99)
vec                                  ; => #(10 99 30)
(vector-length vec)                  ; => 3

; Pop elements
(vector-pop! vec)                    ; => 30
vec                                  ; => #(10 99)
```

### Complex Example

```lisp
; Factorial function
(define factorial
  (lambda (n)
    (if (<= n 1)
      1
      (* n (factorial (- n 1))))))

(factorial 5)                        ; => 120

; String processing
(define greet
  (lambda (name)
    (concat "Hello, " name "!")))

(greet "World")                      ; => "Hello, World!"
```

## See Also

- [README.md](README.md) - Project overview, building, and C API reference
- [tests/](tests/) - Test files with more examples
- [PACKAGING.md](PACKAGING.md) - Guide for packaging the library
