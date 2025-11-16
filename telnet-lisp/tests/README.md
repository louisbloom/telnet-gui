# Test Suite

This directory contains the test suite for telnet-lisp. Tests serve dual purposes:

- **Test cases** - Automated validation of interpreter behavior
- **Examples** - Documentation of how to use each feature

For complete language documentation, see **[LANGUAGE_REFERENCE.md](../LANGUAGE_REFERENCE.md)**.

## Structure

```
tests/
├── basic/          # Core functionality tests
├── advanced/       # Advanced features
├── regression/     # Regression tests
├── test_runner.sh  # Test harness script
└── README.md       # This file
```

## Test Format

Each test file is a Lisp program that demonstrates features and validates behavior.

### Expected Output Format

Add comments to specify expected outputs:

```lisp
(+ 1 2 3)                 ; => 6
(define x 5)              ; ignore (define has no output)
(* x 10)                  ; => 50
(factorial 5)             ; => 120
```

### Comment Markers

- `; => value` - Exact expected output
- `; ignore` - Skip validation (for define, comments, etc.)
- Multiple expressions with `; =>` comments are validated in sequence

## Running Tests

### Using CMake (recommended)

```bash
cd build
cmake ..
cmake --build .
ctest                        # Run all tests
ctest -R basic               # Run tests matching "basic"
ctest --verbose              # Show detailed output
```

### Manual Testing

```bash
# Run a specific test
./lisp-repl.exe tests/basic/recursion.lisp

# Run with validation harness
./test_runner.sh tests/basic/recursion.lisp
```

## Adding New Tests

1. Create a `.lisp` file in the appropriate category
2. Add test expressions with expected output comments
3. The test harness automatically validates outputs
4. Tests are discovered automatically by CMake

### Example Test File

```lisp
; Test description
(+ 2 2)                     ; => 4
(- 10 5)                    ; => 5
(define square (lambda (x) (* x x)))
(square 5)                  ; => 25
```

## Categories

### basic/

Core functionality tests:

- **recursion.lisp** - Recursive functions (factorial, Fibonacci)
- **integers.lisp** - Integer arithmetic, type coercion, integer operations (quotient, remainder, even?, odd?)
- **vectors.lisp** - Vector operations (make-vector, vector-ref, vector-set!, vector-push!, vector-pop!, vector-length)
  - **Includes regression tests** for `make-vector` with initial values (bug fix: Nov 2024)
- **hash_tables.lisp** - Hash table operations (make-hash-table, hash-ref, hash-set!, hash-keys, hash-values, etc.)
- **strings.lisp** - String operations (concat, split, string-length, substring, string-replace, case conversion)
  - **Includes regression tests** for `string?` predicate (Nov 2024)

### advanced/

Advanced features:

- **named_functions.lisp** - Named function definitions with `define`
- **let_star.lisp** - Sequential let bindings with `let*`
  - **Includes regression tests** for `let*` body evaluation (bug fix: Nov 2024)
- **do_loop.lisp** - Iteration with `do` loops
- **cond_case.lisp** - Multi-way conditionals (`cond`, `case`)
- **progn.lisp** - Sequential evaluation with `progn`
- **regex.lisp** - PCRE2 regex operations (match, find, extract, replace, split, escape)
  - **Includes detailed parameter documentation** for `regex-replace` (Nov 2024)
- **utf8.lisp** - UTF-8 string support (character-based length, substring, string-ref)
- **file_io.lisp** - File reading and writing (open, close, read-line, write-line, read-sexp, read-json)

### regression/

Regression tests for:

- **core_features.lisp** - Core feature validation
- **multiline_parsing.lisp** - Multi-line expression parsing
- Bug fixes and edge cases

## Test Validation

The `test_runner.sh` harness:

1. Runs the Lisp file through the interpreter (`lisp-repl`)
2. Captures all output from the program
3. Extracts expected outputs from `; =>` comments
4. Compares actual vs expected output line by line
5. Reports pass/fail with details on mismatches
6. Provides meaningful error messages with context

### How It Works

```bash
# The test runner compares actual output with expected comments
# Example test file:
(+ 1 2 3)                 ; => 6

# The runner:
# 1. Runs: ./lisp-repl test_file.lisp
# 2. Captures output: "6"
# 3. Extracts expected: "6" (from ; => comment)
# 4. Compares: "6" == "6" → PASS
```

## Benefits

- **Dual purpose**: Tests serve as both validation and examples
- **Self-documenting**: Expected outputs show feature behavior
- **Maintainable**: One file per feature/functionality
- **Automated**: CMake discovers and runs all tests automatically
- **Validated**: Actual output checking, not just exit codes
- **Regression prevention**: Recent bug fixes have regression tests to prevent recurrence

## Recent Test Additions (November 2024)

New regression tests added to prevent future bugs:

- **`make-vector` initialization**: Test that `(make-vector size initial-value)` correctly initializes all elements
- **`let*` body evaluation**: Test that `let*` evaluates all body expressions and returns the last value
- **`string?` predicate**: Test that `string?` correctly identifies strings
- **`regex-replace` clarity**: Added parameter names and descriptions to avoid argument order confusion

## See Also

- **[LANGUAGE_REFERENCE.md](../LANGUAGE_REFERENCE.md)** - Complete language documentation
- **[README.md](../README.md)** - Project overview and C API reference
- **CMakeLists.txt** - Automatic test discovery configuration
