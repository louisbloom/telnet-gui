# Telnet LISP

A minimal, embeddable LISP interpreter library written in C, designed to be integrated into GUI applications. This implementation follows traditional LISP naming conventions and provides a REPL for testing and demonstration.

## Features

- **Data Types**: Numbers (double precision) and Strings
- **Symbols and Variables**: Full support for symbol binding and variable definitions
- **Functions**: Both built-in functions and user-defined lambdas
- **Arithmetic Operators**: `+`, `-`, `*`, `/`
- **String Manipulation**: `concat`, `split` (with wildcard pattern support)
- **Predicates**:
  - Number comparisons: `>`, `<`, `=`, `>=`, `<=`
  - String comparisons: `string=`, `string<`, `string>`, `string<=`, `string>=`
  - String operations: `string-contains`, `string-match`
  - Boolean operators: `and`, `or`, `not`
  - Type predicates: `null`, `atom`
- **Special Forms**: `quote`, `if`, `define`, `lambda`, `let`
- **List Operations**: `car`, `cdr`, `cons`, `list`
- **Quote Syntax**: `'expr` as syntactic sugar for `(quote expr)`
- **Truthy/Falsy**: JavaScript-like behavior (nil and empty strings are falsy)
- **Memory Management**: Automatic garbage collection with Boehm GC
- **Regex Support**: Powerful PCRE2 regex functions for pattern matching

## Building

## Windows Development

### MSYS2 UCRT64 Environment

This project is built for MSYS2 UCRT64 on Windows. Use the following commands:

**⚠️ CRITICAL REQUIREMENT**: The final executable must NOT depend on `msys-2.0.dll`

**Interactive Shell:**
```bash
C:\Users\tchristensen\scoop\apps\msys2\current\msys2_shell.cmd -defterm -here -no-start -ucrt64
```

**PowerShell Commands with UCRT64 Tools:**
```powershell
C:\Users\tchristensen\scoop\apps\msys2\current\msys2_shell.cmd -defterm -here -no-start -ucrt64 -c "<command>"
```

**Example Build Commands:**
```powershell
# Install dependencies
C:\Users\tchristensen\scoop\apps\msys2\current\msys2_shell.cmd -defterm -here -no-start -ucrt64 -c "pacman -S mingw-w64-ucrt-x86_64-gc mingw-w64-ucrt-x86_64-pcre2"

# Build project
C:\Users\tchristensen\scoop\apps\msys2\current\msys2_shell.cmd -defterm -here -no-start -ucrt64 -c "make"

# Generate compile_commands.json for clangd
C:\Users\tchristensen\scoop\apps\msys2\current\msys2_shell.cmd -defterm -here -no-start -ucrt64 -c "mkdir -p build && cd build && cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
```

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
- `:load <filename>` - Load and execute a LISP file

### Running Files

Execute LISP files directly:

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
(+ 1 2 3 4)        ; => 6
(- 10 3)           ; => 7
(* 2 3 4)          ; => 24
(/ 10 2)           ; => 5
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

## Truthy/Falsy Values

Following JavaScript-like semantics:
- **Falsy**: `nil` (the empty list) and `""` (empty string)
- **Truthy**: Everything else, including `0` and non-empty strings

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

Telnet LISP includes powerful PCRE2 regex functions for advanced pattern matching:

- `regex-match` - Test if string matches regex pattern
- `regex-find` - Find first regex match
- `regex-find-all` - Find all regex matches
- `regex-extract` - Extract capture groups
- `regex-replace` - Replace first match with backreferences
- `regex-replace-all` - Replace all matches
- `regex-split` - Split by regex pattern
- `regex-escape` - Escape special regex characters
- `regex-valid?` - Validate regex patterns

See the `examples/regex.lisp` file for comprehensive examples.

## License

This is a demonstration project. Feel free to use and modify as needed.

## Future Enhancements

Potential additions for future versions:
- More data types (integers, booleans, vectors)
- Macros
- Tail call optimization
- More string operations
- File I/O operations
- Error recovery and better error messages
- More regex features (lookahead, lookbehind)
