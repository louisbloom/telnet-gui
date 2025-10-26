# Telnet Lisp Interpreter - Quick Start Guide

## Building the Project

Open MSYS2 UCRT64 terminal and run:

```bash
make
```

This creates:

- `build/liblisp.a` - The embeddable library
- `lisp-repl.exe` - The REPL application

## Running the REPL

Start the interactive REPL:

```bash
./lisp-repl.exe
```

Try these commands:

```lisp
>>> (+ 1 2 3)
6
>>> (define square (lambda (x) (* x x)))
#<lambda>
>>> (square 5)
25
>>> (concat "Hello" " " "World")
"Hello World"
>>> :quit
```

## Running Example Files

Execute a Lisp file:

```bash
./lisp-repl.exe examples/demo.lisp
```

## Quick Examples

### Variables and Functions

```lisp
(define x 42)
(define double (lambda (n) (* n 2)))
(double x)  ; => 84
```

### Conditionals

```lisp
(if (> 10 5) "yes" "no")  ; => "yes"
```

### String Operations

```lisp
(concat "Hello" " " "World")        ; => "Hello World"
(split "a,b,c" ",")                 ; => ("a" "b" "c")
(string-contains "hello" "ell")     ; => 1
```

### Lists

```lisp
(define nums (list 1 2 3 4 5))
(car nums)                          ; => 1
(cdr nums)                          ; => (2 3 4 5)
```

### Recursion

```lisp
(define factorial
  (lambda (n)
    (if (<= n 1)
      1
      (* n (factorial (- n 1))))))
(factorial 5)                       ; => 120
```

## Embedding in Your Application

```c
#include "lisp.h"

int main() {
    lisp_init();
    Environment* env = env_create_global();

    LispObject* result = lisp_eval_string("(+ 2 3)", env);
    char* output = lisp_print(result);
    printf("%s\n", output);  // Prints: 5
    // No need to free output - GC handles it

    env_free(env);
    lisp_cleanup();
    return 0;
}
```

Compile with:

```bash
gcc myapp.c -Iinclude -Lbuild -llisp -lm -lgc -lpcre2-8 -o myapp.exe
```

## REPL Commands

- `:quit` - Exit the REPL
- `:load <file>` - Load and execute a Lisp file

## Truthy/Falsy Values

- **Falsy**: `nil` and `""` (empty string)
- **Truthy**: Everything else (including `0`)

## Pattern Matching

Use wildcards in `split` and `string-match`:

- `*` matches zero or more characters
- `?` matches exactly one character
- `[abc]` matches any character in set
- `[a-z]` matches character ranges
- `[!abc]` matches anything NOT in set

```lisp
(string-match "hello" "h*o")   ; => 1
(split "a*b*c" "*")            ; => ("a" "b" "c")
```

## Regex Support

Powerful PCRE2 regex functions:

```lisp
(regex-match "\\d+" "hello123")  ; => 1
(regex-find-all "[a-z]+" "hello world")  ; => ("hello" "world")
(regex-extract "(\\d+)-(\\d+)-(\\d+)" "2025-10-24")  ; => ("2025" "10" "24")
(regex-replace-all "\\s+" "_" "hello  world")  ; => "hello_world"
```

## All Built-in Functions

**Arithmetic**: `+`, `-`, `*`, `/`

**Comparisons**: `>`, `<`, `=`, `>=`, `<=`

**String ops**: `concat`, `split`, `string=`, `string<`, `string>`, `string<=`, `string>=`, `string-contains`, `string-match`

**Regex**: `regex-match`, `regex-find`, `regex-find-all`, `regex-extract`, `regex-replace`, `regex-replace-all`, `regex-split`, `regex-escape`, `regex-valid?`

**Boolean**: `and`, `or`, `not`

**List ops**: `car`, `cdr`, `cons`, `list`

**Predicates**: `null`, `atom`

**Special forms**: `quote`, `if`, `define`, `lambda`, `let`

## More Examples

See the `examples/` directory for more complete examples:

- `examples/demo.lisp` - Comprehensive feature demonstration
- `examples/factorial.lisp` - Recursive factorial function
- `examples/strings.lisp` - String manipulation examples
- `examples/arithmetic.lisp` - Basic arithmetic operations
- `examples/regex.lisp` - Regex and pattern matching examples
