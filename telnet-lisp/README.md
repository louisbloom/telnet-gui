# Telnet Lisp

A minimal, embeddable Lisp interpreter library written in C. This implementation follows traditional Lisp naming conventions and provides a REPL for testing and demonstration.

**📖 [Language Reference](LANGUAGE_REFERENCE.md)** - Full language documentation with data types, functions, and examples

## Features

### Core Language

- **Data Types**: Numbers, integers, booleans, strings (UTF-8), characters, lists, vectors, hash tables, lambdas, errors
- **Special Forms**: `quote`, `quasiquote`, `if`, `define`, `set!`, `lambda`, `defmacro`, `let`/`let*`, `progn`, `do`, `cond`, `case`, `condition-case`, `unwind-protect`
- **Macros**: Code transformation with `defmacro`, quasiquote (`` ` ``), unquote (`,`), unquote-splicing (`,@`), and built-in `defun` macro
- **Error Handling**: Emacs Lisp-style condition system with `signal`, `condition-case`, `unwind-protect`, and error introspection
- **Functions**: Arithmetic, strings, lists, vectors, hash tables, regex (PCRE2), file I/O
- **Type Predicates**: `null?`, `atom?`, `integer?`, `boolean?`, `number?`, `string?`, `char?`, `vector?`, `hash-table?`, `error?`

See **[LANGUAGE_REFERENCE.md](LANGUAGE_REFERENCE.md)** for complete function listings and examples.

### Advanced Features

- **Condition System**: Emacs Lisp-style error handling with typed errors, catch/handle, and guaranteed cleanup
- **Tail Call Optimization**: Trampoline-based tail recursion enables efficient recursive algorithms without stack overflow
- **Lexical Scoping**: Lambdas capture their environment
- **First-Class Functions**: Functions can be passed as arguments and returned from functions
- **Closures**: Functions with captured variables
- **Regex Support**: Full PCRE2 integration for advanced pattern matching with UTF-8 support
- **UTF-8 Support**: Full Unicode string support with grapheme-based operations
  - `length` returns grapheme cluster count (human-visible characters)
  - `substring` and `string-ref` use grapheme-based indexing
  - Variation selectors and combining marks handled correctly
- **Memory Management**: Automatic garbage collection with Boehm GC

## Building

For build prerequisites and platform-specific setup (MSYS2, Linux, macOS), see the [main README](../README.md#building-from-source).

**Quick build:**

```bash
cd telnet-lisp
mkdir build && cd build
cmake ..
cmake --build .
```

**Running tests:** See [tests/README.md](tests/README.md)

## Usage

### Command Line Options

```bash
lisp-repl                 # Start interactive REPL
lisp-repl -c "CODE"       # Execute CODE and exit
lisp-repl FILE [FILE...]  # Execute FILE(s) and exit
lisp-repl -h, --help      # Show help message
```

### REPL Mode

```bash
./lisp-repl
```

REPL Commands:

- `:quit` - Exit the REPL
- `:load <filename>` - Load and execute a Lisp file

### Command-Line Execution

```bash
./lisp-repl -c "(+ 1 2 3)"                                    # => 6
./lisp-repl -c "(map (lambda (x) (* x 2)) '(1 2 3 4 5))"     # => (2 4 6 8 10)
./lisp-repl -c '(concat "hello" " " "world")'                 # => "hello world"
```

Exit code is 0 on success, 1 on error.

### Running Files

```bash
./lisp-repl script.lisp
```

## Embedding in Your Application

The library is self-contained and can be copied into any project. For detailed packaging instructions, see [PACKAGING.md](PACKAGING.md).

**CMake Integration:**

```cmake
add_subdirectory(libs/telnet-lisp)
target_link_libraries(myapp lisp)
```

**Using the Library:**

```c
#include "lisp.h"

int main() {
    lisp_init();
    Environment* env = env_create_global();

    LispObject* result = lisp_eval_string("(+ 1 2 3)", env);
    char* output = lisp_print(result);
    printf("%s\n", output);  // Prints: 6

    env_free(env);
    lisp_cleanup();
    return 0;
}
```

## Quick Start

```lisp
; Arithmetic and variables
(define x 10)
(+ x 20)                             ; => 30

; Functions
(define square (lambda (n) (* n n)))
(square 5)                           ; => 25

; Lists
(map (lambda (x) (* x 2)) '(1 2 3))  ; => (2 4 6)
(filter even? '(1 2 3 4 5 6))        ; => (2 4 6)

; Strings with Unicode
(length "Hello, 世界! 🌍")            ; => 12

; Conditionals
(if (> 10 5) "yes" "no")             ; => "yes"
```

**📖 For full documentation, see [LANGUAGE_REFERENCE.md](LANGUAGE_REFERENCE.md)**

## C API Reference

### Core Functions

| Function                        | Description                 |
| ------------------------------- | --------------------------- |
| `lisp_init()`                   | Initialize the interpreter  |
| `lisp_eval_string(code, env)`   | Parse and evaluate a string |
| `lisp_load_file(filename, env)` | Load and evaluate a file    |
| `lisp_cleanup()`                | Free global resources       |

### Parsing and Evaluation

| Function               | Description                           |
| ---------------------- | ------------------------------------- |
| `lisp_read(input)`     | Parse input into AST                  |
| `lisp_eval(expr, env)` | Evaluate an expression                |
| `lisp_print(obj)`      | Convert object to string (GC-managed) |

### Object Creation

| Function                   | Description                  |
| -------------------------- | ---------------------------- |
| `lisp_make_number(value)`  | Create floating-point number |
| `lisp_make_integer(value)` | Create integer               |
| `lisp_make_string(value)`  | Create string                |
| `lisp_make_symbol(name)`   | Create symbol                |
| `lisp_make_boolean(value)` | Create boolean               |
| `lisp_make_cons(car, cdr)` | Create cons cell             |
| `lisp_make_error(message)` | Create error object          |

### Environment Management

| Function                       | Description                              |
| ------------------------------ | ---------------------------------------- |
| `env_create(parent)`           | Create new environment                   |
| `env_create_global()`          | Create global environment with built-ins |
| `env_define(env, name, value)` | Define variable                          |
| `env_lookup(env, name)`        | Look up variable                         |
| `env_free(env)`                | Free environment                         |

## License

MIT License - see [LICENSE](../LICENSE) file for details.
