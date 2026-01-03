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
- **Strings**: Unicode strings with full UTF-8 support
- **Characters**: Unicode characters (codepoints) with `#\a` reader syntax
- **Lists**: Cons cells for linked lists
- **Vectors**: Dynamic arrays with `#(elem ...)` literal syntax (grow/shrink)
- **Hash Tables**: Key-value mappings
- **Symbols**: Interned names with optional docstrings (used for variables, functions, keywords)
- **Lambda Functions**: User-defined closures with lexical scoping

## Special Forms

- `quote` - Quote expressions (also supported via `'expr` syntax)
- `quasiquote` - Quote with selective evaluation (also supported via `` `expr`` syntax)
- `unquote` - Evaluate within quasiquote (also supported via `,expr` syntax)
- `unquote-splicing` - Evaluate and splice list within quasiquote (also supported via `,@expr` syntax)
- `if` - Conditional evaluation
- `define` - Define variables and functions
- `set!` - Mutate existing variables (works inside lambdas/hooks)
- `lambda` - Create anonymous functions with required, optional (&optional), and rest (&rest) parameters (body has implicit progn: evaluates all expressions, returns last with tail recursion optimization)
- `defmacro` - Define macros for code transformation at evaluation time
- `let` - Local variable bindings (parallel evaluation, body has implicit progn)
- `let*` - Local variable bindings (sequential evaluation, can reference previous bindings, body has implicit progn)
- `progn` - Evaluate multiple expressions sequentially and return last value
- `do` - Iteration loop with variable updates and exit condition
- `cond` - Multi-way conditional with test clauses
- `case` - Pattern matching with value-based dispatch
- `and` - Logical AND with short-circuit evaluation (returns last truthy value or first falsy value)
- `or` - Logical OR with short-circuit evaluation (returns first truthy value or last falsy value)
- `condition-case` - Catch and handle errors by type (Emacs Lisp-style exception handling)
- `unwind-protect` - Guarantee cleanup code execution (like try-finally)

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

#### when and unless

`when` and `unless` are conditional macros that execute their body only when a condition is true (or false).

**Syntax:**

```lisp
(when condition body...)
(unless condition body...)
```

**Examples:**

```lisp
; when - execute body when condition is true
(when (> 5 3)
  (+ 1 2)
  (* 3 4))  ; => 12 (returns last expression)

(when nil
  (+ 1 2))  ; => nil

; unless - execute body when condition is false
(unless (< 5 3)
  "condition was false")  ; => "condition was false"

(unless #t
  "won't run")  ; => nil
```

**Implementation:**

```lisp
(defmacro when (condition . body)
  "Execute BODY when CONDITION is true."
  `(if ,condition (progn ,@body) nil))

(defmacro unless (condition . body)
  "Execute BODY when CONDITION is false."
  `(if ,condition nil (progn ,@body)))
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

#### lambda

Create anonymous functions with optional and rest parameters. Lambda functions support lexical scoping and capture their environment (closures).

**Syntax:**

```lisp
(lambda (params...) body...)
(lambda (required... &optional optional... &rest rest-param) body...)
```

**Parameter Types:**

- **Required parameters**: Must be provided, bound in order
- **Optional parameters** (after `&optional`): Default to `nil` if not provided
- **Rest parameter** (after `&rest`): Collects remaining arguments as a list

**Features:**

- Body has implicit `progn`: evaluates all expressions, returns last value
- Tail recursion optimization for last expression
- Lexical scoping: captures environment variables
- Named via `define` for better stack traces

**Examples:**

```lisp
; Basic lambda
((lambda (x) (* x 2)) 5)  ; => 10

; Multiple parameters
((lambda (a b) (+ a b)) 3 4)  ; => 7

; Multiple body expressions (implicit progn)
((lambda (x)
   (+ x 1)
   (+ x 2)
   (* x 3)) 5)  ; => 15 (only last expression returned)

; Optional parameters (default to nil)
((lambda (a &optional b) (list a b)) 1)     ; => (1 nil)
((lambda (a &optional b) (list a b)) 1 2)   ; => (1 2)

; Multiple optional parameters
((lambda (a &optional b c d) (list a b c d)) 1 2)  ; => (1 2 nil nil)

; Only optional parameters (no required)
((lambda (&optional a b) (list a b)))      ; => (nil nil)
((lambda (&optional a b) (list a b)) 10)   ; => (10 nil)

; Default values via (or param default)
(define greet
  (lambda (name &optional greeting)
    (let ((g (or greeting "Hello")))
      (concat g ", " name "!"))))
(greet "Alice")      ; => "Hello, Alice!"
(greet "Bob" "Hi")   ; => "Hi, Bob!"

; Rest parameter (collects remaining args)
((lambda (a &rest more) (list a more)) 1 2 3)  ; => (1 (2 3))
((lambda (&rest all) all) 1 2 3 4)             ; => (1 2 3 4)

; Combined optional and rest
((lambda (a &optional b &rest more) (list a b more)) 1)        ; => (1 nil ())
((lambda (a &optional b &rest more) (list a b more)) 1 2 3 4)  ; => (1 2 (3 4))

; Variadic function with rest parameter
(define sum
  (lambda (initial &rest numbers)
    (let ((total initial))
      (do ((nums numbers (cdr nums)))
        ((null? nums) total)
        (set! total (+ total (car nums)))))))
(sum 1)           ; => 1
(sum 1 2 3 4 5)   ; => 15

; Closures (capture environment)
(define make-adder
  (lambda (x)
    (lambda (y) (+ x y))))
(define add5 (make-adder 5))
(add5 10)  ; => 15

; Named functions (via define)
(define factorial
  (lambda (n)
    (if (<= n 1) 1 (* n (factorial (- n 1))))))
(factorial 5)  ; => 120
```

**Parameter Order Rules:**

1. Required parameters come first
2. `&optional` marker introduces optional parameters (all default to `nil`)
3. `&rest` marker must be last, followed by exactly one parameter name
4. `&optional` cannot appear after `&rest`

**Arity Checking:**

- Too few arguments (missing required): Error
- Too many arguments (without `&rest`): Error
- With `&rest`: No maximum argument limit

### Docstrings

Documentation strings (docstrings) follow Emacs Lisp conventions. Functions and macros get docstrings from the first string in their body. Variables use `defvar`, `defconst`, or `set-documentation!`. Docstrings use CommonMark (Markdown) format.

**Syntax for Functions/Macros:**

```lisp
(lambda (params...)
  "Docstring here (optional)"
  body...)

(defmacro name (params...)
  "Docstring here (optional)"
  body...)
```

**Syntax for Variables:**

```lisp
(defvar name value "Docstring here (optional)")
(defconst name value "Docstring here (optional)")
(set-documentation! 'name "Docstring here")
```

**Requirements:**

- Docstring must be a string literal
- For functions/macros: must be first expression after parameters, followed by at least one more expression
- Single-expression functions cannot have docstrings (prevents ambiguity)
- Docstrings use CommonMark (Markdown) format

**Introspection Functions:**

- `(documentation symbol)` - Get docstring for a symbol. First checks the symbol's own docstring (set via `set-documentation!` or copied from lambda/macro on define), then falls back to the value's docstring if bound to a lambda/macro/builtin.
- `(doc symbol)` - Shorthand for `documentation`
- `(bound? symbol)` - Check if symbol is bound in the environment
- `(set-documentation! symbol docstring)` - Set docstring directly on the interned symbol. Since symbols are interned (globally shared), this sets the docstring globally.
- `(doc-set! symbol docstring)` - Shorthand for `set-documentation!`

Returns `nil` if no docstring exists.

**Note:** Docstrings are stored on symbols (like Emacs Lisp), not on bindings. This means docstrings are global per symbol name.

#### Variable Definition Macros

**defvar** - Define a variable (only sets value if unbound):

```lisp
(defvar name)                    ; Define with nil value
(defvar name value)              ; Define with value
(defvar name value "Docstring")  ; Define with value and docstring
```

`defvar` only sets the value if the variable is not already bound. The docstring is always set if provided.

**defconst** - Define a constant (always sets value):

```lisp
(defconst name value)              ; Define constant
(defconst name value "Docstring")  ; Define with docstring
```

`defconst` always sets the value (unlike `defvar`). Note: constants are a convention, not enforced (Emacs Lisp style - hackable).

**defalias** - Create a function alias:

```lisp
(defalias alias target)              ; Create alias
(defalias alias target "Docstring")  ; Create alias with docstring
```

**Examples:**

```lisp
; Variable with docstring
(defvar my-toggle nil "Non-nil means the feature is enabled.")
(documentation 'my-toggle)  ; => "Non-nil means the feature is enabled."

; defvar doesn't rebind if already bound
(defvar my-toggle #t "New docstring")
my-toggle  ; => nil (unchanged)

; Constant with docstring
(defconst pi 3.14159 "The ratio of circumference to diameter.")
(documentation 'pi)  ; => "The ratio of circumference to diameter."

; defconst always rebinds
(defconst pi 3.14 "Truncated pi")
pi  ; => 3.14 (updated)

; Function alias
(defalias my-add + "Alias for the + function.")
(my-add 1 2 3)  ; => 6
(doc 'my-add)   ; => "Alias for the + function."

; Check if symbol is bound
(bound? 'car)                    ; => #t
(bound? 'nonexistent-symbol)     ; => #f

; Set documentation on a symbol (works whether bound or not)
(define my-var 42)
(set-documentation! 'my-var "The answer to everything.")
(documentation 'my-var)  ; => "The answer to everything."

; Can also set docstring on unbound symbols
(set-documentation! 'future-var "Will be defined later.")
(documentation 'future-var)  ; => "Will be defined later."
```

#### Function Docstrings

````lisp
; Function with docstring
(define calculate-area
  (lambda (width height)
    "Calculate the area of a rectangle.

    ## Parameters
    - `width` - Width of the rectangle
    - `height` - Height of the rectangle

    ## Returns
    The area as a number."
    (* width height)))

(documentation 'calculate-area)
; => "Calculate the area of a rectangle.\n\n    ## Parameters..."

; Macro with docstring
(defmacro when (condition . body)
  "Execute BODY when CONDITION is true."
  `(if ,condition (progn ,@body) nil))

(documentation 'when)
; => "Execute BODY when CONDITION is true."

; Single-expression lambda (NO docstring - string is return value)
(define return-msg (lambda () "Hello"))
(documentation 'return-msg)  ; => nil
(return-msg)                 ; => "Hello"

; Multi-line CommonMark docstring
(define process-data
  (lambda (data)
    "Process data using various transformations.

    ## Description
    This function applies a series of transformations:
    1. Validate input
    2. Transform data
    3. Return result

    ## Example
    ```lisp
    (process-data '(1 2 3))  ; => '(2 4 6)
    ```

    **Note**: Input must be a list."
    (map (lambda (x) (* x 2)) data)))

; Closures preserve docstrings
(define make-multiplier
  (lambda (factor)
    "Create a function that multiplies by FACTOR."
    (lambda (x)
      "Multiply X by the captured factor."
      (* x factor))))
````

**Edge Cases:**

```lisp
; Lambda without docstring
(define double (lambda (x) (* x 2)))
(documentation 'double)  ; => nil

; String-only body is NOT a docstring
(define msg (lambda () "Just a message"))
(documentation 'msg)  ; => nil (only one expression)
(msg)  ; => "Just a message" (returns the string)
```

#### Standard Library Aliases

The standard library provides convenient aliases:

- `doc` - Alias for `documentation`
- `doc-set!` - Alias for `set-documentation!`
- `string-append` - Alias for `concat`

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
- `=` - Equal to (numbers only)
- `>=` - Greater than or equal
- `<=` - Less than or equal

### Equality Predicates

#### `(eq? obj1 obj2)`

Identity equality - returns true if `obj1` and `obj2` are the same object in memory.

- Fast pointer comparison
- Reliable for symbols (symbols with the same name are always the same object)
- NOT reliable for numbers or strings (may be different objects with same value)

```lisp
(eq? 'foo 'foo)           ; => 1 (symbols are interned)
(eq? '(1 2) '(1 2))       ; => nil (different list objects)
(define x '(1 2))
(define y x)
(eq? x y)                 ; => 1 (same object)
```

#### `(equal? obj1 obj2)`

Deep structural equality - returns true if `obj1` and `obj2` have the same structure and values.

- Recursive comparison of lists, vectors, hash tables
- Use for "are these values the same?" checks
- Default choice for most equality tests

```lisp
(equal? '(1 2 3) '(1 2 3))           ; => 1 (same values)
(equal? "abc" "abc")                 ; => 1 (same string)
(equal? '(1 (2 3)) '(1 (2 3)))       ; => 1 (nested lists)
(equal? 1 1.0)                       ; => nil (different types)

; Vectors
(define v1 (make-vector 2))
(vector-set! v1 0 1)
(vector-set! v1 1 2)
(define v2 (make-vector 2))
(vector-set! v2 0 1)
(vector-set! v2 1 2)
(equal? v1 v2)                       ; => 1
```

#### `(string=? str1 str2)`

String equality - returns true if `str1` and `str2` have identical character sequences.

```lisp
(string=? "foo" "foo")               ; => 1
(string=? "foo" "bar")               ; => nil
```

**Practical Usage Guide:**

- **`eq?`** - For symbols, checking if two variables point to same object
- **`=`** - For numeric comparisons (supports integers and floats)
- **`string=?`** - For string equality
- **`equal?`** - Default choice for "are these values the same?" checks (lists, vectors, hash tables, any data structures)

### String Functions

- `concat` - Concatenate multiple strings
- `split` - Split string by pattern (supports wildcards: `*`, `?`, `[]`)
- `join` - Join a list of strings with a separator
- `length` - Get length of sequence (list, string, or vector). For strings, returns grapheme cluster count (human-visible characters).
- `substring` - Extract substring by character indices (UTF-8 aware)
- `string-ref` - Get character at index (UTF-8 aware, returns character type)
- `string-append` - Alias for `concat`
- `string-split` - Alias for `split`
- `string-join` - Alias for `join`
- `string-length` - Alias for `length` (also works on lists and vectors)
- `string-replace` - Replace all occurrences of substring in string
- `string-upcase` - Convert string to uppercase (UTF-8 aware, ASCII only)
- `string-downcase` - Convert string to lowercase (UTF-8 aware, ASCII only)
- `string-trim` - Remove leading and trailing whitespace
- `string->number` - Convert string to number (integer or float) with optional radix (R7RS compliant)
- `number->string` - Convert number to string with optional radix (R7RS compliant)

### String Comparisons

String comparisons follow Scheme R7RS naming with `?` suffix:

- `string<?` - String less than (lexicographic)
- `string>?` - String greater than (lexicographic)
- `string<=?` - String less than or equal (lexicographic)
- `string>=?` - String greater than or equal (lexicographic)

**Examples:**

```lisp
(string<? "abc" "def")     ; => 1 (true)
(string<? "def" "abc")     ; => nil (false)
(string<=? "abc" "abc")    ; => 1 (true)
(string>=? "xyz" "abc")    ; => 1 (true)
```

**R7RS Compliance:** All string comparison predicates use the `?` suffix following Scheme R7RS standard.

### String Predicates

- `string-contains?` - Check if string contains substring
- `string-index` - Find first occurrence of substring, returns character index or nil
- `string-match?` - Match string against wildcard pattern
- `string-prefix?` - Check if one string is a prefix of another

### Character Functions

Characters are Unicode codepoints with `#\a` reader syntax:

**Character Literals:**

- `#\a`, `#\Z`, `#\!` - Single characters
- `#\space`, `#\newline`, `#\tab`, `#\return` - Named characters
- `#\escape`, `#\null`, `#\backspace`, `#\delete` - Control characters
- `#\x1b`, `#\xNN` - Hex notation
- `#\u4e16`, `#\uNNNN` - Unicode notation

**Type Predicate:**

- `char?` - Returns #t if argument is a character

**Conversion:**

- `char-code` - Get codepoint: `(char-code #\a)` → `97`
- `code-char` - Get character: `(code-char 97)` → `#\a`
- `char->string` - Convert to string: `(char->string #\a)` → `"a"`
- `string->char` - Convert from string: `(string->char "a")` → `#\a` (error if length != 1)

**Comparison:**

- `char=?` - `(char=? #\a #\a)` → `#t`
- `char<?` - `(char<? #\a #\b)` → `#t`
- `char>?` - `(char>? #\b #\a)` → `#t`
- `char<=?` - `(char<=? #\a #\a)` → `#t`
- `char>=?` - `(char>=? #\a #\a)` → `#t`

**Case Conversion:**

- `char-upcase` - `(char-upcase #\a)` → `#\A`
- `char-downcase` - `(char-downcase #\A)` → `#\a`

**Classification:**

- `char-alphabetic?` - `(char-alphabetic? #\a)` → `#t`
- `char-numeric?` - `(char-numeric? #\5)` → `#t`
- `char-whitespace?` - `(char-whitespace? #\space)` → `#t`

**Examples:**

```lisp
(string-ref "hello" 0)         ; => #\h (character, not string)
(char=? (string-ref "ab" 0) #\a)  ; => #t
(char->string #\newline)       ; => "\n"
(char-code #\世)               ; => 19990
```

### Numeric Conversion Functions

#### `(string->number string)` / `(string->number string radix)`

Convert string to number (integer or float) with optional radix parameter.

**Without radix** (base 10, or inferred from prefix):

- Supports integers: `"42"`, `"-123"`, `"+99"`
- Supports floats: `"3.14"`, `"1e10"`, `"-2.5e-3"`, `"1.23e+5"`
- Supports radix prefixes:
  - `"#b1010"` (binary, base 2) → `10`
  - `"#o77"` (octal, base 8) → `63`
  - `"#d123"` (decimal, base 10) → `123`
  - `"#xff"` (hexadecimal, base 16) → `255`
- Leading and trailing whitespace is stripped
- Returns `nil` (`#f`) if string is not a valid number

**With radix** (2-36):

- Parses string as integer in given base
- No prefix needed: `(string->number "ff" 16)` → `255`
- Only integers supported (floats require base 10)
- Returns `nil` (`#f`) if invalid for that radix

**Examples:**

```lisp
;; Basic integers
(string->number "42")           ; => 42
(string->number "-123")         ; => -123
(string->number "  +99  ")      ; => 99

;; Floats
(string->number "3.14")         ; => 3.14
(string->number "1e10")         ; => 1e10
(string->number "-2.5e-3")      ; => -0.0025

;; Radix prefixes
(string->number "#b1010")       ; => 10
(string->number "#o77")         ; => 63
(string->number "#xff")         ; => 255

;; Radix parameter
(string->number "1010" 2)       ; => 10
(string->number "ff" 16)        ; => 255
(string->number "77" 8)         ; => 63

;; Invalid input returns #f
(string->number "xyz")          ; => nil
(string->number "")             ; => nil
(string->number "12.34.56")     ; => nil
```

**R7RS Compliance:** Follows R7RS standard with optional radix parameter and `#f` return on failure.

#### `(number->string z)` / `(number->string z radix)`

Convert number to string representation with optional radix parameter.

**Without radix** (base 10):

- Formats integers: `255` → `"255"`
- Formats floats: `3.14` → `"3.14"`, `1e10` → `"1e+10"`

**With radix** (2-36):

- Formats integers in given base: `(number->string 255 16)` → `"ff"`
- Uses lowercase letters for digits > 9: `"0123456789abcdefghijklmnopqrstuvwxyz"`
- Floats only supported in base 10 (error otherwise)

**Examples:**

```lisp
;; Base 10 (default)
(number->string 42)             ; => "42"
(number->string -42)            ; => "-42"
(number->string 3.14)           ; => "3.14"
(number->string 1e10)           ; => "1e+10"

;; Binary (base 2)
(number->string 10 2)           ; => "1010"
(number->string 255 2)          ; => "11111111"

;; Octal (base 8)
(number->string 63 8)           ; => "77"
(number->string 255 8)          ; => "377"

;; Hexadecimal (base 16)
(number->string 255 16)         ; => "ff"
(number->string 16 16)          ; => "10"

;; Base 36 (maximum)
(number->string 35 36)          ; => "z"
(number->string 36 36)          ; => "10"

;; Round-trip conversion
(number->string (string->number "ff" 16) 16)   ; => "ff"
(string->number (number->string 255 16) 16)    ; => 255
```

**R7RS Compliance:** Follows R7RS standard with optional radix parameter (integers only for non-decimal bases).

### Boolean Functions

- `not` - Logical negation (returns #t for falsy values, #f for truthy values)

**Note:** `and` and `or` are special forms (see Special Forms section) that provide short-circuit evaluation.

### List Functions

- `car` - Get first element of list
- `cdr` - Get rest of list
- `cons` - Construct new list cell
- `set-car!` - Set first element of cons cell (mutating)
- `set-cdr!` - Set rest of cons cell (mutating)
- `list` - Create list from arguments
- `list-ref` - Get element at index (0-based)
- `reverse` - Reverse a list (returns new list with elements in reverse order)
- `append` - Concatenate multiple lists into a single list (returns new list)
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

### List Membership Functions

Functions for testing list membership. These return the tail of the list starting from the match, or nil if not found.

- `member` - Find element using structural equality (equal?)
- `memq` - Find element using pointer equality (eq?)

**Examples:**

```lisp
; Find element in list (returns tail starting from match)
(member 'b '(a b c d))          ; => (b c d)
(member 'x '(a b c))            ; => nil

; Works with strings using structural equality
(member "foo" '("bar" "foo" "baz"))  ; => ("foo" "baz")

; memq uses pointer equality (fast, but only reliable for symbols)
(memq 'b '(a b c d))            ; => (b c d)

; Test for membership (truthy if found)
(if (member item list) "found" "not found")
```

### Mapping Functions

Functions for transforming lists by applying a function to each element.

- `map` - Apply function to each element of list, returns new list of results
- `mapcar` - Same as map (Common Lisp compatibility)
- `filter` - Filter list elements that satisfy predicate, returns new list
- `apply` - Apply function to list of arguments

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
- `char?` - Check if character
- `vector?` - Check if vector
- `hash-table?` - Check if hash table
- `symbol?` - Check if symbol
- `list?` - Check if value is a list (nil or cons cell)
- `pair?` - Check if value is a cons cell (pair), returns `#t` for cons cells, `#f` otherwise (note: `nil` is NOT a pair)
- `error?` - Check if value is an error object

### Symbol Operations

Symbols are interned objects with a name and an optional docstring. The same symbol name always refers to the same object.

- `symbol?` - Check if value is a symbol
- `symbol->string` - Convert symbol to string
- `documentation` - Get symbol's docstring (see [Docstrings](#docstrings))
- `set-documentation!` - Set symbol's docstring (see [Docstrings](#docstrings))

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

### Evaluation Functions

- `eval` - Evaluate an expression in the current environment
- `bound?` - Check if a symbol is bound to a value

**Examples:**

```lisp
; eval evaluates quoted expressions
(eval '(+ 1 2 3))           ; => 6
(eval 'my-var)              ; => value of my-var

; Useful for evaluating symbols to get their bound values
(define my-func (lambda (x) (* x 2)))
(eval 'my-func)             ; => #<lambda my-func ...>

; bound? checks if a symbol has a binding
(define x 42)
(bound? 'x)                 ; => #t
(bound? 'undefined-var)     ; => nil
```

### Time Functions

- `current-time-ms` - Return current time in milliseconds

**Examples:**

```lisp
; Get current time for timing operations
(define start (current-time-ms))
; ... do some work ...
(define elapsed (- (current-time-ms) start))
(format nil "Elapsed: ~A ms" elapsed)
```

### Error Handling Functions

The condition system provides Emacs Lisp-style error handling with typed errors, catch/handle, and guaranteed cleanup.

#### Error Creation and Signaling

- `signal` - Raise a typed error (arguments: error-type-symbol, data)
- `error` - Raise a generic error (arguments: message-string)

#### Error Introspection

- `error?` - Check if value is an error object
- `error-type` - Get error type symbol from error object
- `error-message` - Get error message string from error object
- `error-stack` - Get call stack trace from error object (list of function names)
- `error-data` - Get error data payload from error object

**Examples:**

```lisp
; Raise a typed error
(signal 'division-by-zero "Cannot divide by zero")
; => ERROR: [division-by-zero] Cannot divide by zero

; Raise a generic error
(error "Something went wrong")
; => ERROR: [error] Something went wrong

; Catch and inspect an error
(define err nil)
(condition-case e
    (signal 'my-error "test message")
  (error (define err e)))

(error? err)           ; => #t
(error-type err)       ; => my-error
(error-message err)    ; => "test message"
(error-data err)       ; => "test message"
(error-stack err)      ; => ("signal")

; Error with custom data
(condition-case e
    (signal 'file-error '("cannot open" "file.txt" 404))
  (error (error-data e)))  ; => ("cannot open" "file.txt" 404)
```

### Regex Functions (PCRE2)

- `regex-match?` - Test if string matches pattern (arguments: pattern, string)
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
- `delete-file` - Delete a file from the filesystem (filename) - returns nil on success, error if file doesn't exist or cannot be deleted
- `load` - Load and evaluate a Lisp file (filename) - returns the result of the last expression evaluated, or an error if loading fails

### Path Expansion Functions

Cross-platform file path expansion utilities.

#### `(home-directory)`

Get the user's home directory path.

**Returns:**

- String with home directory path
- `nil` if home directory cannot be determined

**Platform behavior:**

- **Unix/Linux/macOS:** Uses `$HOME` environment variable
- **Windows:** Uses `%USERPROFILE%` or `%HOMEDRIVE%%HOMEPATH%`

**Examples:**

```lisp
(home-directory)                    ; => "/home/alice" (Unix)
(home-directory)                    ; => "C:\\Users\\Alice" (Windows)

; Use in paths
(define config-dir (concat (home-directory) "/.config"))
; => "/home/alice/.config"
```

#### `(expand-path path)`

Expand `~/` prefix in file paths to the user's home directory.

**Arguments:**

- `path` (string) - File path, may start with `~/`

**Returns:**

- String with expanded path (if path starts with `~/`)
- Original string (if path does not start with `~/`)
- Error if argument is not a string

**Behavior:**

- Detects `~/` at start of path
- Replaces `~/` with home directory from `home-directory`
- Handles cross-platform path separators
- Works with both forward and backslashes after `~`

**Examples:**

```lisp
; Basic expansion
(expand-path "~/config.lisp")
; => "/home/alice/config.lisp" (Unix)
; => "C:\\Users\\Alice\\config.lisp" (Windows)

; Subdirectories
(expand-path "~/Documents/notes.txt")
; => "/home/alice/Documents/notes.txt" (Unix)

; No expansion (no ~/ prefix)
(expand-path "/etc/config")         ; => "/etc/config"
(expand-path "relative/path")       ; => "relative/path"
(expand-path "./local.lisp")        ; => "./local.lisp"

; Just ~ expands to home directory
(expand-path "~")                   ; => "/home/alice"

; Use with file I/O
(define file (open (expand-path "~/my-config.lisp") "r"))
(load (expand-path "~/scripts/init.lisp"))
```

**Use Cases:**

- Reading/writing user configuration files
- Loading user-specific scripts
- Saving data to user directories
- Cross-platform file operations

**Error Handling:**

- Returns error if home directory cannot be determined
- Returns error if argument is not a string

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

Following traditional Lisp semantics:

- **Falsy**: Only `nil` (and `#f`, which is identical to `nil`)
- **Truthy**: Everything else, including:
  - Numbers: `0`, `0.0`, `42`, `-1`
  - Strings: `""`, `"hello"`
  - Collections: Empty vectors `#()`, empty hash tables, non-empty collections
  - Booleans: `#t`

**Note**: Unlike some Lisps, both integer `0` and float `0.0` are **truthy**. Only `nil` is false.

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

Telnet Lisp implements an Emacs Lisp-style condition system with typed errors, catch/handle, and guaranteed cleanup. The system provides:

- **Typed errors**: Errors have symbol-based types (e.g., `'division-by-zero`, `'file-error`)
- **Error catching**: `condition-case` catches and handles specific error types
- **Guaranteed cleanup**: `unwind-protect` ensures cleanup code always runs
- **Error introspection**: Full access to error type, message, data, and call stack
- **Automatic propagation**: Uncaught errors propagate with call stack traces

### Signal and Error Creation

**`signal`** - Raise a typed error:

```lisp
(signal 'division-by-zero "Cannot divide by zero")
; => ERROR: [division-by-zero] Cannot divide by zero

(signal 'file-error "Cannot open file")
; => ERROR: [file-error] Cannot open file

; With data payload
(signal 'file-error '("cannot open" "file.txt" 404))
; => ERROR: [file-error] ("cannot open" "file.txt" 404)
```

**`error`** - Convenience function for generic errors:

```lisp
(error "Something went wrong")
; => ERROR: [error] Something went wrong
```

### Condition-Case (Error Catching)

**Syntax:** `(condition-case VAR BODYFORM HANDLERS...)`

Catch and handle specific error types:

```lisp
; Basic error catching
(condition-case e
    (/ 10 0)
  (division-by-zero "Caught division by zero"))
; => "Caught division by zero"

; Multiple handlers
(condition-case e
    (some-risky-operation)
  (file-error "File problem")
  (network-error "Network problem")
  (error "Other error"))  ; 'error catches everything

; Access the error object
(condition-case err
    (signal 'my-error "test message")
  (error (error-message err)))
; => "test message"

; Handler with multiple expressions (implicit progn)
(condition-case e
    (signal 'test-error "boom")
  (test-error
    (define handled #t)
    (define result "recovered")
    result))
; => "recovered"
```

**Handler matching:**

- Handlers are checked in order
- First matching handler executes
- `'error` symbol catches all error types
- More specific handlers take precedence

### Unwind-Protect (Guaranteed Cleanup)

**Syntax:** `(unwind-protect BODYFORM CLEANUP...)`

Guarantee cleanup code execution (like try-finally):

```lisp
; Cleanup always runs
(define file (open "data.txt" "r"))
(unwind-protect
    (read-line file)
  (close file))  ; Always closes, even on error

; Multiple cleanup forms
(define cleanup-count 0)
(unwind-protect
    (signal 'test-error "boom")
  (define cleanup-count (+ cleanup-count 1))
  (define cleanup-count (+ cleanup-count 10)))
cleanup-count  ; => 11 (cleanup ran despite error)

; Returns body result even if error
(condition-case e
    (unwind-protect
        (signal 'test-error "boom")
      (define cleaned-up #t))
  (error "caught"))
cleaned-up  ; => #t
```

### Error Introspection

Caught errors can be inspected:

```lisp
(define my-err nil)
(condition-case e
    (signal 'custom-error "test message")
  (error (define my-err e)))

(error? my-err)           ; => #t
(error-type my-err)       ; => custom-error
(error-message my-err)    ; => "test message"
(error-data my-err)       ; => "test message"
(error-stack my-err)      ; => ("signal")
```

### Practical Examples

**Safe division:**

```lisp
(define (safe-divide a b)
  (condition-case err
      (if (= b 0)
          (signal 'division-by-zero "cannot divide by zero")
          (/ a b))
    (division-by-zero "Error: division by zero")
    (error (concat "Unexpected error: " (error-message err)))))

(safe-divide 10 2)  ; => 5
(safe-divide 10 0)  ; => "Error: division by zero"
```

**Resource management:**

```lisp
(define (process-file filename)
  (define file (open filename "r"))
  (unwind-protect
      (progn
        (define content (read-line file))
        (if (null? content)
            (signal 'empty-file "File is empty")
            content))
    (close file)))  ; Always closes
```

**Nested error handling:**

```lisp
(condition-case outer
    (condition-case inner
        (signal 'inner-error "from inner")
      (other-error "inner handler"))
  (inner-error "outer caught it"))
; => "outer caught it"
```

### Error Propagation and Call Stacks

Uncaught errors automatically propagate with call stack traces:

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

### Common Error Types

Standard error types (user-extensible):

- `'error` - Generic error (catch-all)
- `'division-by-zero` - Division by zero
- `'wrong-type-argument` - Wrong type passed to function
- `'wrong-number-of-arguments` - Arity mismatch
- `'void-variable` - Undefined symbol
- `'file-error` - File I/O errors
- `'range-error` - Out of bounds access

You can define custom error types using any symbol:

```lisp
(signal 'my-custom-error "Custom error message")
```

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
(if 0 "truthy" "falsy")              ; => "truthy" (only nil is falsy!)
(if "" "truthy" "falsy")             ; => "truthy" (empty strings are truthy!)
```

### String Operations

```lisp
; Concatenation
(concat "Hello" " " "World")         ; => "Hello World"

; Split by pattern
(split "a,b,c" ",")                  ; => ("a" "b" "c")
(split "foo*bar*baz" "*")            ; => ("foo" "bar" "baz")

; Join strings
(join '("a" "b" "c") ",")            ; => "a,b,c"
(join '("hello" "world") " ")        ; => "hello world"

; String comparisons
(string=? "hello" "hello")           ; => 1
(string<? "abc" "def")               ; => 1
(string-contains? "hello world" "world") ; => 1
(string-index "hello world" "world") ; => 6
(string-index "hello world" "xyz")   ; => nil
(string-match? "hello" "h*o")        ; => 1
(string-prefix? "hel" "hello")       ; => 1
(string-prefix? "lis" "lisp")        ; => 1

; UTF-8 string operations
(length "Hello, 世界! 🌍")            ; => 12 (grapheme clusters)
(substring "Hello, 世界!" 7 9)        ; => "世界"
(string-ref "Hello, 世界!" 7)        ; => "世"

; String transformations
(string-replace "hello world" "world" "universe")  ; => "hello universe"
(string-replace "hello" "l" "L")                   ; => "heLLo"
(string-upcase "hello world")                      ; => "HELLO WORLD"
(string-downcase "HELLO WORLD")                    ; => "hello world"
(string-trim "  hello world  ")                    ; => "hello world"
(string-trim "\t  spaced  \n")                     ; => "spaced"
```

### Regex Operations

**Note:** Regex replace functions use the argument order: `(regex-replace PATTERN STRING REPLACEMENT)`

```lisp
; Basic regex matching
(regex-match? "\\d+" "hello123")      ; => 1
(regex-match? "^hello$" "hello")      ; => 1

; Finding matches
(regex-find "\\d+" "abc123def")      ; => "123"
(regex-find-all "[a-z]+" "hello world")  ; => ("hello" "world")

; Extracting capture groups
(regex-extract "(\\d+)-(\\d+)-(\\d+)" "2025-10-24")  ; => ("2025" "10" "24")

; Replacing text - replaces ALL occurrences (both functions behave the same)
; Arguments: (regex-replace PATTERN STRING REPLACEMENT)
(regex-replace "\\d+" "a1b2c3" "X")  ; => "aXbXcX" (replaces all)
(regex-replace-all "\\s+" "hello  world" "_")  ; => "hello_world"

; Replace only matched portion, not entire string
(regex-replace ",$" "hello," "X")    ; => "helloX" (replaces comma only)
(regex-replace "a+$" "baaa" "")      ; => "b" (removes trailing a's)

; Splitting by regex
(regex-split "," "apple,banana,cherry")  ; => ("apple" "banana" "cherry")

; Escaping special characters
(regex-escape "test.string*")        ; => "test\\.string\\*"

; Validating patterns
(regex-valid? "\\d+")               ; => 1
(regex-valid? "[invalid")           ; => nil
```

### Boolean Operations

`and` and `or` are special forms with short-circuit evaluation:

```lisp
; and - returns last value if all truthy, or first falsy value
(and)                                ; => 1 (empty and is true)
(and 1 2 3)                          ; => 3 (returns last truthy value)
(and 1 nil 3)                        ; => nil (short-circuits at nil)
(and #f (/ 1 0))                     ; => #f (doesn't evaluate division)

; or - returns first truthy value, or last value if all falsy
(or)                                 ; => nil (empty or is false)
(or nil nil 5)                       ; => 5 (first truthy value)
(or 1 2 3)                           ; => 1 (short-circuits at first truthy)
(or #f "foo" (/ 1 0))                ; => "foo" (doesn't evaluate division)

; not - simple negation
(not nil)                            ; => 1
(not 5)                              ; => nil

; list? - check if value is a list (nil or cons)
(list? '(1 2 3))                     ; => 1
(list? nil)                          ; => 1
(list? 42)                           ; => nil

; pair? - check if value is a cons cell
(pair? '(1 . 2))                     ; => 1 (dotted pair)
(pair? '(1 2 3))                     ; => 1 (lists are chains of pairs)
(pair? (cons 1 2))                   ; => 1
(pair? nil)                          ; => nil (nil is NOT a pair)
(pair? 42)                           ; => nil
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

; Concatenate lists
(append '(1 2) '(3 4))               ; => (1 2 3 4)
(append '(1 2) '(3 4) '(5 6))        ; => (1 2 3 4 5 6)
(append '() '(1 2))                  ; => (1 2)
(append '(1 2) '())                  ; => (1 2)
(append)                             ; => nil
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

; Load and evaluate a Lisp file
(load "config.lisp")                 ; => result of last expression in file
(load "my-script.lisp")              ; => loads and executes all expressions

; Delete a file
(delete-file "temp.txt")             ; => nil (success) or error if file doesn't exist
```

### Vector Operations

```lisp
; Vector literals
#(1 2 3)                             ; => #(1 2 3)
#()                                  ; => #() (empty vector)
#(a b c)                             ; => #(a b c)
(vector-ref #(a b c) 1)              ; => b
(length #(1 2 3))                    ; => 3

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
(length vec)                         ; => 3

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
