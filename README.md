# Telnet Lisp

A minimal, embeddable Lisp interpreter library written in C, designed to be integrated into GUI applications. This implementation follows traditional Lisp naming conventions and provides a REPL for testing and demonstration.

## Features

### Data Types

- **Numbers**: Double precision floating-point numbers
- **Integers**: 64-bit signed integers
- **Booleans**: True (#t) and false (#f)
- **Strings**: C-style null-terminated strings
- **Lists**: Cons cells for linked lists
- **Vectors**: Dynamic arrays (grow/shrink)
- **Hash Tables**: Key-value mappings
- **Symbols**: Variable names and function symbols
- **Lambda Functions**: User-defined closures with lexical scoping

### Special Forms

- `quote` - Quote expressions (also supported via `'expr` syntax)
- `if` - Conditional evaluation
- `define` - Define variables and functions
- `set!` - Mutate existing variables (works inside lambdas/hooks)
- `lambda` - Create anonymous functions with parameters
- `let` - Local variable bindings (parallel evaluation)
- `let*` - Local variable bindings (sequential evaluation, can reference previous bindings)
- `progn` - Evaluate multiple expressions sequentially and return last value

### Arithmetic Functions

- `+` - Add numbers (supports multiple arguments, integer + integer = integer, mixed = float)
- `-` - Subtract numbers (supports unary negation, integer - integer = integer, mixed = float)
- `*` - Multiply numbers (supports multiple arguments, integer \* integer = integer, mixed = float)
- `/` - Divide numbers (always returns float, supports unary reciprocal)

### Integer Operations

- `quotient` - Integer division (truncates to integer, e.g., quotient 10 3 → 3)

### Number Comparisons

- `>` - Greater than
- `<` - Less than
- `=` - Equal to
- `>=` - Greater than or equal
- `<=` - Less than or equal

### String Functions

- `concat` - Concatenate multiple strings
- `split` - Split string by pattern (supports wildcards: `*`, `?`, `[]`)

### String Comparisons

- `string=` - String equality
- `string<` - String less than (lexicographic)
- `string>` - String greater than (lexicographic)
- `string<=` - String less than or equal (lexicographic)
- `string>=` - String greater than or equal (lexicographic)

### String Predicates

- `string-contains` - Check if string contains substring
- `string-match` - Match string against wildcard pattern

### Boolean Functions

- `and` - Logical AND (returns last truthy value or nil)
- `or` - Logical OR (returns first truthy value or nil)
- `not` - Logical negation

### List Functions

- `car` - Get first element of list
- `cdr` - Get rest of list
- `cons` - Construct new list cell
- `list` - Create list from arguments
- `null?` - Check if list is empty
- `atom?` - Check if value is an atom (not a list)

### Vector Functions

- `make-vector` - Create a vector of specified size
- `vector-ref` - Get element at index
- `vector-set!` - Set element at index (mutating)
- `vector-length` - Get vector size
- `vector-push!` - Append to end (mutating)
- `vector-pop!` - Remove from end (mutating)
- `vector?` - Check if value is a vector

### Type Predicates

- `null?` - Check if nil
- `atom?` - Check if atom (not a list)
- `integer?` - Check if integer
- `boolean?` - Check if boolean
- `number?` - Check if number or integer
- `vector?` - Check if vector
- `hash-table?` - Check if hash table

### Regex Functions (PCRE2)

- `regex-match` - Test if string matches pattern
- `regex-find` - Find first regex match
- `regex-find-all` - Find all regex matches
- `regex-extract` - Extract capture groups
- `regex-replace` - Replace first match
- `regex-replace-all` - Replace all matches
- `regex-split` - Split string by regex pattern
- `regex-escape` - Escape special regex characters
- `regex-valid?` - Validate regex pattern syntax

### File I/O Functions

- `open` - Open a file (filename, mode) - returns file stream
- `close` - Close a file stream
- `read-line` - Read a line from file stream (returns string or nil at EOF)
- `write-line` - Write a line to file stream

### Advanced Features

- **Lexical Scoping**: Lambdas capture their environment
- **First-Class Functions**: Functions can be passed as arguments
- **Higher-Order Functions**: Functions that operate on functions
- **Closures**: Functions with captured variables
- **Global State Management**: Variables persist and can be updated across the REPL session
- **Quote Syntax**: `'expr` shorthand for `(quote expr)`
- **Truthy/Falsy**: JavaScript-like semantics (nil and empty strings are falsy)
- **Memory Management**: Automatic garbage collection with Boehm GC
- **Regex Support**: Full PCRE2 integration for advanced pattern matching
- **Wildcard Patterns**: Enhanced wildcard matching in `string-match` and `split`

## Building

## Windows Development

### MSYS2 UCRT64 Environment

This project is built for MSYS2 UCRT64 on Windows.

**⚠️ Compiler Requirements:**

- **Supported**: GCC (via MSYS2 UCRT64)
- **NOT Supported**: MSVC (Visual Studio)

**Installing MSYS2:**

1. Download MSYS2 from [https://www.msys2.org/](https://www.msys2.org/)
2. Run the installer and follow the setup wizard
3. Launch "MSYS2 UCRT64" from the Start menu
4. Update packages: `pacman -Syu` (may need to run twice)
5. Install toolchain: `pacman -S mingw-w64-ucrt-x86_64-toolchain`

**⚠️ CRITICAL REQUIREMENT**: The final executable must NOT depend on `msys-2.0.dll`

**Interactive Shell:**

```bash
C:\msys64\msys2_shell.cmd -defterm -here -no-start -ucrt64
```

**PowerShell Commands with UCRT64 Tools:**

```powershell
C:\msys64\msys2_shell.cmd -defterm -here -no-start -ucrt64 -c "<command>"
```

**Example Build Commands:**

```powershell
# Install dependencies
C:\msys64\msys2_shell.cmd -defterm -here -no-start -ucrt64 -c "pacman -S mingw-w64-ucrt-x86_64-gc mingw-w64-ucrt-x86_64-pcre2"

# Build project
C:\msys64\msys2_shell.cmd -defterm -here -no-start -ucrt64 -c "make"

# Format source code
C:\msys64\msys2_shell.cmd -defterm -here -no-start -ucrt64 -c "make format"

# Generate compile_commands.json for clangd
C:\msys64\msys2_shell.cmd -defterm -here -no-start -ucrt64 -c "mkdir -p build && cd build && cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
```

**Note**: The default MSYS2 installation path is `C:\msys64`

### Dependencies

Install required packages:

```bash
pacman -S mingw-w64-ucrt-x86_64-gc mingw-w64-ucrt-x86_64-pcre2 mingw-w64-ucrt-x86_64-toolchain mingw-w64-ucrt-x86_64-cmake
```

### Verification

After building, verify the executable has no `msys-2.0.dll` dependency:

```bash
# Check dependencies (should NOT show msys-2.0.dll)
ldd lisp-repl.exe

# Or use objdump
objdump -p lisp-repl.exe | grep DLL
```

### Build Instructions

**Quick Build:**

```bash
make
```

**Cross-platform Build:**

```bash
# Show platform information
make info

# Build for current platform
make clean && make

# Run tests
make test
```

**CMake Build (Alternative):**

```bash
mkdir build && cd build
cmake ..
make
```

**Installation:**

```bash
# Linux/macOS
sudo make install

# Windows (MSYS2)
make install

# Or use the install script
./install.sh        # Linux/macOS
install.bat         # Windows
```

## Usage

### REPL Mode

Run the REPL interactively:

```bash
# Linux/macOS
./lisp-repl

# Windows
./lisp-repl.exe
```

REPL Commands:

- `:quit` - Exit the REPL
- `:load <filename>` - Load and execute a Lisp file

### Running Files

Execute Lisp files directly:

```bash
# Linux/macOS
./lisp-repl script.lisp

# Windows
./lisp-repl.exe script.lisp
```

### Embedding in Your Application

Include the header and link with the library:

```c
#include "lisp.h"

int main() {
    // Initialize the interpreter (includes GC initialization)
    lisp_init();

    // Create a global environment
    Environment* env = env_create_global();

    // Evaluate a string
    LispObject* result = lisp_eval_string("(+ 1 2 3)", env);

    // Print the result
    char* output = lisp_print(result);
    printf("%s\n", output);  // Prints: 6
    // No need to free output - GC handles it

    // Load a file
    result = lisp_load_file("script.lisp", env);

    // Cleanup (GC handles most cleanup automatically)
    env_free(env);
    lisp_cleanup();

    return 0;
}
```

## Language Examples

### Basic Arithmetic

```lisp
(+ 1 2 3 4)        ; => 6 (integer)
(+ 1 2.5)          ; => 3.5 (float - mixed types)
(+ 1.5 2.5)        ; => 4.0 (float)
(- 10 3)           ; => 7 (integer)
(- 10 3.5)         ; => 6.5 (float - mixed types)
(* 2 3 4)          ; => 24 (integer)
(* 3 4.0)          ; => 12.0 (float - mixed types)
(/ 10 2)           ; => 5.0 (always float)
(quotient 10 3)    ; => 3 (integer division)
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
(if 0 "truthy" "falsy")              ; => "truthy" (0 is truthy!)
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
(string-contains "hello world" "world") ; => 1
(string-match "hello" "h*o")         ; => 1
```

### Regex Operations

```lisp
; Basic regex matching
(regex-match "\\d+" "hello123")      ; => 1
(regex-match "^hello$" "hello")      ; => 1

; Finding matches
(regex-find "\\d+" "abc123def")      ; => "123"
(regex-find-all "[a-z]+" "hello world")  ; => ("hello" "world")

; Extracting capture groups
(regex-extract "(\\d+)-(\\d+)-(\\d+)" "2025-10-24")  ; => ("2025" "10" "24")

; Replacing text
(regex-replace "\\d+" "X" "a1b2c3")  ; => "aXb2c3"
(regex-replace-all "\\s+" "_" "hello  world")  ; => "hello_world"

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

; Perfect for telnet-gui hooks!
(define message_count 0)
(define count_messages (lambda (data)
  (set! message_count (+ message_count 1))
  data))                             ; Return data unchanged

(register-user-input-hook count_messages)
message_count                        ; Increments as you type
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

## API Reference

### Simple API

- `int lisp_init()` - Initialize the interpreter
- `LispObject* lisp_eval_string(const char* code, Environment* env)` - Parse and evaluate a string
- `void lisp_cleanup()` - Free global resources

### Advanced API

- `LispObject* lisp_read(const char** input)` - Parse input into AST
- `LispObject* lisp_eval(LispObject* expr, Environment* env)` - Evaluate an expression
- `char* lisp_print(LispObject* obj)` - Convert object to string (caller must free)
- `LispObject* lisp_load_file(const char* filename, Environment* env)` - Load and evaluate a file

### Object Creation

- `LispObject* lisp_make_number(double value)`
- `LispObject* lisp_make_string(const char* value)`
- `LispObject* lisp_make_symbol(const char* name)`
- `LispObject* lisp_make_cons(LispObject* car, LispObject* cdr)`
- `LispObject* lisp_make_error(const char* message)`

### Environment Management

- `Environment* env_create(Environment* parent)` - Create new environment
- `Environment* env_create_global()` - Create global environment with built-ins
- `void env_define(Environment* env, const char* name, LispObject* value)` - Define variable
- `LispObject* env_lookup(Environment* env, const char* name)` - Look up variable
- `void env_free(Environment* env)` - Free environment

## Naming Conventions

Telnet Lisp follows modern Lisp naming conventions:

- **Predicates** use `?` suffix: `null?`, `vector?`, `integer?`, `boolean?`
- **Mutating functions** use `!` suffix: `set!`, `vector-set!`, `vector-push!`
- **Immutating functions** have no special suffix: `car`, `cdr`, `vector-ref`

## Truthy/Falsy Values

Following JavaScript-like semantics:

- **Falsy**: `nil`, `0` (number), `0` (integer), empty string `""`, boolean `#f`, empty vectors, empty hash tables
- **Truthy**: Everything else, including `0` numbers (except integer 0), non-empty strings, `#t`, non-empty vectors

## Pattern Matching

The `split` and `string-match` functions support enhanced wildcard patterns:

- `*` - Matches zero or more characters
- `?` - Matches exactly one character
- `[abc]` - Matches any character in set
- `[a-z]` - Matches character ranges
- `[!abc]` - Matches anything NOT in set

Examples:

```lisp
(string-match "hello" "h*o")         ; => 1
(string-match "hello" "h?llo")       ; => 1
(string-match "hello" "h??lo")       ; => 1
(string-match "hello" "h[aeiou]llo") ; => 1
(string-match "hello" "h[a-z]llo")   ; => 1
```

## Regex Support

Telnet Lisp includes powerful PCRE2 regex functions for advanced pattern matching:

- `regex-match` - Test if string matches regex pattern
- `regex-find` - Find first regex match
- `regex-find-all` - Find all regex matches
- `regex-extract` - Extract capture groups
- `regex-replace` - Replace first match with backreferences
- `regex-replace-all` - Replace all matches
- `regex-split` - Split by regex pattern
- `regex-escape` - Escape special regex characters
- `regex-valid?` - Validate regex patterns

See the `examples/` directory for comprehensive examples including `regex.lisp`, `progn.lisp`, `let_star.lisp`, `hooks_with_state.lisp`, and `file_io.lisp`.

## License

This is a demonstration project. Feel free to use and modify as needed.

## Future Enhancements

Potential additions for future versions:

### High Priority

- Additional integer operations (`remainder`, `even?`, `odd?`)
- Hash table iteration (keys, values, entries)
- Additional special forms (`cond`, `case`, `do`)
- Better error recovery and stack traces
- Tail call optimization for better recursion performance

### Medium Priority

- Additional string operations (substring, replace, upper/lowercase transformations)
- Structured data reading from files (S-expressions, JSON)
- Macros for metaprogramming
- More regex features (lookahead, lookbehind, non-capturing groups)
- Performance optimizations (bytecode compilation)

### Low Priority

- Additional list operations (reverse, append, map, filter, fold)
- Modules/packages for code organization
- Exception handling (`try-catch`)
- Namespaces for variable organization
