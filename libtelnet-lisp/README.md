# Telnet Lisp

A minimal, embeddable Lisp interpreter library written in C, designed to be integrated into GUI applications. This implementation follows traditional Lisp naming conventions and provides a REPL for testing and demonstration.

**📖 [Language Reference](LANGUAGE_REFERENCE.md)** - Complete language documentation with data types, functions, and examples

## Features

### Core Language

- **Data Types**: Numbers, integers, booleans, strings (UTF-8), lists, vectors, hash tables, lambdas
- **Special Forms**: `quote`, `if`, `define`, `set!`, `lambda`, `let`/`let*`, `progn`, `do`, `cond`, `case`
- **Functions**: Arithmetic, strings, lists, vectors, hash tables, regex (PCRE2), file I/O
- **Type Predicates**: `null?`, `atom?`, `integer?`, `boolean?`, `number?`, `string?`, `vector?`, `hash-table?`

See **[LANGUAGE_REFERENCE.md](LANGUAGE_REFERENCE.md)** for complete function listings and examples.

### Advanced Features

- **Lexical Scoping**: Lambdas capture their environment
- **First-Class Functions**: Functions can be passed as arguments
- **Higher-Order Functions**: Functions that operate on functions
- **Closures**: Functions with captured variables
- **Global State Management**: Variables persist and can be updated across the REPL session
- **Quote Syntax**: `'expr` shorthand for `(quote expr)`
- **Truthy/Falsy**: JavaScript-like semantics (nil and empty strings are falsy)
- **Memory Management**: Automatic garbage collection with Boehm GC
- **Regex Support**: Full PCRE2 integration for advanced pattern matching with UTF-8 support
- **Wildcard Patterns**: Enhanced wildcard matching in `string-match?` and `split`
- **UTF-8 Support**: Full Unicode string support with character-based operations (not byte-based)
  - Strings stored as UTF-8 byte sequences internally
  - UTF-8 aware string operations: `string-length`, `substring`, `string-ref`
  - Line ending support for Unix (`\n`), Windows (`\r\n`), and Mac (`\r`) in file I/O
  - PCRE2 regex compiled with UTF-8 and Unicode character property support

## Testing

The project includes a test suite with automated validation. See [tests/README.md](tests/README.md) for details.

```bash
# Run all tests
cd build && ctest

# Run specific test category
ctest -R basic

# Run with verbose output
ctest --verbose
```

## Building

### Prerequisites

Install required packages for your platform:

**Windows (MSYS2 UCRT64):**

```bash
pacman -S mingw-w64-ucrt-x86_64-gc mingw-w64-ucrt-x86_64-pcre2 mingw-w64-ucrt-x86_64-toolchain mingw-w64-ucrt-x86_64-cmake
```

**Linux/macOS:**

```bash
# Install via package manager
sudo apt-get install libgc-dev libpcre2-8-dev      # Linux
brew install bdw-gc pcre2                          # macOS
```

### Building

**Quick Build:**

```bash
make
```

**CMake Build:**

```bash
mkdir build && cd build
cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
cmake --build .
```

**Running Tests:**

```bash
make test          # Basic tests
cd build && ctest  # Full test suite (CMake)
```

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
# Adjust path for your MSYS2 installation
C:\msys64\msys2_shell.cmd -defterm -here -no-start -ucrt64
```

**PowerShell Commands with UCRT64 Tools:**

```powershell
# Adjust MSYS2 path for your installation
C:\msys64\msys2_shell.cmd -defterm -here -no-start -ucrt64 -c "<command>"

# If installed via Scoop
C:\Users\YourUser\scoop\apps\msys2\current\msys2_shell.cmd -defterm -here -no-start -ucrt64 -c "<command>"
```

**Example Build Commands:**

```powershell
# Find your MSYS2 installation path, then:
$MSYS2_ROOT = "C:\msys64"  # Or wherever MSYS2 is installed

# Install dependencies
& "$MSYS2_ROOT\msys2_shell.cmd" -defterm -here -no-start -ucrt64 -c "pacman -S mingw-w64-ucrt-x86_64-gc mingw-w64-ucrt-x86_64-pcre2"

# Build project
& "$MSYS2_ROOT\msys2_shell.cmd" -defterm -here -no-start -ucrt64 -c "make"

# Format source code
& "$MSYS2_ROOT\msys2_shell.cmd" -defterm -here -no-start -ucrt64 -c "make format"

# Generate compile_commands.json for clangd
& "$MSYS2_ROOT\msys2_shell.cmd" -defterm -here -no-start -ucrt64 -c "mkdir -p build && cd build && cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
```

**Note**: MSYS2 may be installed in different locations:

- Default: `C:\msys64\`
- Scoop: `C:\Users\<user>\scoop\apps\msys2\current\`
- Chocolatey: `C:\ProgramData\chocolatey\lib\msys2\`

### Verification

After building, verify the executable has no `msys-2.0.dll` dependency:

```bash
# Check dependencies (should NOT show msys-2.0.dll)
ldd lisp-repl.exe

# Or use objdump
objdump -p lisp-repl.exe | grep DLL
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

### Quick Start

The library is self-contained and can be copied into any project. For detailed packaging instructions, see [PACKAGING.md](PACKAGING.md).

**Simple Integration:**

```bash
# Copy the entire libtelnet-lisp directory into your project
cp -r /path/to/telnet-lisp/libtelnet-lisp ./libs/
```

**CMake Integration:**

```cmake
add_subdirectory(libs/libtelnet-lisp)
target_link_libraries(myapp liblisp)
```

**Using the Library:**

```c
#include "lisp.h"  # If copied into project
// OR #include <lisp.h>  # If installed system-wide

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

## Examples

The `tests/` directory contains validated examples that serve as both documentation and test cases. Each file demonstrates specific features with expected outputs:

**Basic Features:**

- `tests/basic/recursion.lisp` - Recursive functions and factorial
- `tests/basic/vectors.lisp` - Vector operations and manipulation
- `tests/basic/hash_tables.lisp` - Hash table operations
- `tests/basic/integers.lisp` - Integer arithmetic and type coercion
- `tests/basic/strings.lisp` - String operations

**Advanced Features:**

- `tests/advanced/named_functions.lisp` - Named function definitions
- `tests/advanced/do_loop.lisp` - Iteration with `do` loops
- `tests/advanced/cond_case.lisp` - Multi-way conditionals
- `tests/advanced/progn.lisp` - Sequential evaluation
- `tests/advanced/regex.lisp` - PCRE2 regex operations
- `tests/advanced/utf8.lisp` - UTF-8 string support
- `tests/advanced/file_io.lisp` - File reading and writing

**Regression Tests:**

- `tests/regression/core_features.lisp` - Core feature tests
- `tests/regression/multiline_parsing.lisp` - Multi-line expression parsing

Run the examples:

```bash
./lisp-repl.exe tests/basic/recursion.lisp
./lisp-repl.exe tests/advanced/cond_case.lisp
```

View all examples:

```bash
find tests -name "*.lisp"
```

## Quick Start

### Basic Example

```lisp
; Arithmetic and variables
(define x 10)
(define y 20)
(+ x y)                              ; => 30

; Functions
(define square (lambda (n) (* n n)))
(square 5)                           ; => 25

; Lists and strings
(list 1 2 3)                         ; => (1 2 3)
(concat "Hello" " " "World")         ; => "Hello World"

; Conditionals
(if (> 10 5) "yes" "no")             ; => "yes"
```

**📖 For complete language documentation with all functions and examples, see [LANGUAGE_REFERENCE.md](LANGUAGE_REFERENCE.md)**

**📁 For executable examples with expected outputs, see the `tests/` directory:**

- `tests/basic/` - Core functionality examples (recursion, vectors, strings, etc.)
- `tests/advanced/` - Advanced features (regex, file I/O, UTF-8, do loops, cond/case)
- `tests/regression/` - Regression test cases

Each test file is both executable code and documentation with expected outputs marked inline.

## API Reference

### Simple API

- `int lisp_init()` - Initialize the interpreter
- `LispObject* lisp_eval_string(const char* code, Environment* env)` - Parse and evaluate a string
- `void lisp_cleanup()` - Free global resources

### Advanced API

- `LispObject* lisp_read(const char** input)` - Parse input into AST
- `LispObject* lisp_eval(LispObject* expr, Environment* env)` - Evaluate an expression
- `char* lisp_print(LispObject* obj)` - Convert object to string (GC-managed, don't free)
- `void lisp_princ(LispObject* obj)` - Print object in human-readable form (no quotes for strings)
- `void lisp_prin1(LispObject* obj)` - Print object in readable representation (with quotes for strings)
- `void lisp_print_cl(LispObject* obj)` - Print object like `prin1` but adds newline before and after
- `LispObject* lisp_load_file(const char* filename, Environment* env)` - Load and evaluate a file

### Object Creation

- `LispObject* lisp_make_number(double value)`
- `LispObject* lisp_make_integer(long long value)`
- `LispObject* lisp_make_string(const char* value)`
- `LispObject* lisp_make_symbol(const char* name)`
- `LispObject* lisp_make_boolean(int value)`
- `LispObject* lisp_make_cons(LispObject* car, LispObject* cdr)`
- `LispObject* lisp_make_error(const char* message)`

### Environment Management

- `Environment* env_create(Environment* parent)` - Create new environment
- `Environment* env_create_global()` - Create global environment with built-ins
- `void env_define(Environment* env, const char* name, LispObject* value)` - Define variable
- `LispObject* env_lookup(Environment* env, const char* name)` - Look up variable
- `void env_free(Environment* env)` - Free environment


## License

This is a demonstration project. Feel free to use and modify as needed.

## Future Enhancements

Potential additions for future versions:

### Recently Completed ✨

- **UTF-8 Support**: Full Unicode string support with character-based operations (`string-length`, `substring`, `string-ref`)
- **Enhanced File I/O**: Support for Unix, Windows, and Mac line endings
- **Improved REPL File Mode**: Now prints all expression results when running files
- **New Data Types**: Integers, booleans, vectors, and hash tables with complete operations
- **Type Coercion**: Automatic integer/float promotion in arithmetic operations
- **Modern Naming Conventions**: Scheme-style predicates (`?` suffix) and mutating functions (`!` suffix)
- **Do Loop**: Efficient iteration construct for loops with variable updates
- **Integer Operations**: Remainder, even?, odd? predicates for number operations
- **Collection Iteration**: Hash table iteration (keys, values, entries) and list convenience functions (length, ref)
- **Conditional Forms**: cond (multi-way conditional) and case (pattern matching)
- **Error Recovery**: Lisp-level call stack traces show the call chain when errors occur

### Recent Bug Fixes (November 2024)

- **`make-vector` with initial value**: Fixed to properly initialize all vector elements with the provided initial value
- **`let*` body evaluation**: Fixed to evaluate all body expressions like `progn` (returns last value, not first)
- **`string?` predicate**: Confirmed working for type checking strings
- **`remainder` operation**: Confirmed working for integer modulo operations

### High Priority

- Tail call optimization for better recursion performance

### Medium Priority

- Macros for metaprogramming
- More regex features (lookahead, lookbehind, non-capturing groups)
- Performance optimizations (bytecode compilation)

### Low Priority

- Additional list operations (reverse, append, map, filter, fold)
- Modules/packages for code organization
- Exception handling (`try-catch`)
- Namespaces for variable organization
