# Test Suite

This directory contains the test suite for telnet-lisp. Tests serve dual purposes:

- **Test cases** - Automated validation of interpreter behavior
- **Examples** - Documentation of how to use each feature

## Structure

```
tests/
├── basic/          # Core functionality tests
├── advanced/       # Advanced features
├── regression/     # Regression tests
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

- Arithmetic operations
- Basic data types
- Lists and vectors
- Hash tables
- String operations
- Predicates

### advanced/

Advanced features:

- Named functions
- Control structures (cond, case, loops)
- Lambda expressions
- File I/O
- Regex matching
- UTF-8 support

### regression/

Regression tests for:

- Multiline expression parsing
- Edge cases
- Complex scenarios
- Bug fixes

## Test Validation

The `test_runner.sh` harness:

- Runs the Lisp file through the interpreter
- Extracts expected outputs from comments
- Compares actual vs expected output
- Reports pass/fail with details
- Provides meaningful error messages

## Benefits

- **Dual purpose**: Tests serve as both validation and examples
- **Self-documenting**: Expected outputs show feature behavior
- **Maintainable**: One file per feature/functionality
- **Automated**: CMake discovers and runs all tests
- **Validated**: Actual output checking, not just exit codes
