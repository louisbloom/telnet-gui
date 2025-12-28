# Test Suite

This directory contains the test suite for telnet-lisp. Tests serve dual purposes:

- **Test cases** - Automated validation of interpreter behavior
- **Examples** - Documentation of how to use each feature

For complete language documentation, see **[LANGUAGE_REFERENCE.md](../LANGUAGE_REFERENCE.md)**.

## Structure

```
tests/
├── basic/           # Core functionality tests
├── advanced/        # Advanced features
├── regression/      # Regression tests
├── test-helpers.lisp # Assertion macros for tests
└── README.md        # This file
```

## Test Format

Tests use assertion macros from `test-helpers.lisp`:

```lisp
(load "telnet-lisp/tests/test-helpers.lisp")

(assert-equal (+ 1 2) 3 "addition works")
(assert-true (> 5 3) "5 is greater than 3")
(assert-false (null? '(1 2)) "non-empty list is not null")
(assert-error (/ 1 0) "division by zero throws error")
(assert-nil (assoc 'z '((a 1) (b 2))) "missing key returns nil")
```

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
./lisp-repl tests/basic/recursion.lisp
```

## Adding New Tests

1. Create a `.lisp` file in the appropriate category
2. Load test-helpers.lisp and use assertion macros
3. Tests are discovered automatically by CMake

### Example Test File

```lisp
(load "telnet-lisp/tests/test-helpers.lisp")

(assert-equal (+ 2 2) 4 "basic addition")
(assert-equal (* 3 3) 9 "basic multiplication")

(define square (lambda (x) (* x x)))
(assert-equal (square 5) 25 "square function")

(princ "All tests passed!\n")
```

## Categories

### basic/

Core functionality tests:

- **alists.lisp** - Association list operations (assoc, assq, alist-get)
- **and_or_list.lisp** - Boolean operators and list operations
- **characters.lisp** - Character type and operations
- **format.lisp** - String formatting with `format`
- **hash_tables.lisp** - Hash table operations (make-hash-table, hash-ref, hash-set!, hash-keys, hash-values)
- **integer_ops.lisp** - Integer operations (quotient, remainder, modulo)
- **integers.lisp** - Integer arithmetic, type coercion, even?, odd?
- **length.lisp** - Unified `length` function and `substring` with grapheme cluster support
- **list_reverse.lisp** - List reversal
- **path_expansion.lisp** - Path expansion (home-directory, expand-path)
- **printing.lisp** - Print functions (princ, prin1, print)
- **recursion.lisp** - Recursive functions (factorial, Fibonacci)
- **string_number_conversion.lisp** - String/number conversion
- **string-index.lisp** - String indexing operations
- **strings.lisp** - String operations (concat, split, length, substring, string-replace, case conversion)
- **symbols.lisp** - Symbol operations
- **vectors.lisp** - Vector operations (make-vector, vector-ref, vector-set!, vector-push!, vector-pop!, length)

### advanced/

Advanced features:

- **collection_iteration.lisp** - Collection iteration (dolist, dotimes)
- **cond_case.lisp** - Multi-way conditionals (`cond`, `case`)
- **condition-system.lisp** - Error handling (condition-case, unwind-protect)
- **do_loop.lisp** - Iteration with `do` loops
- **docstrings.lisp** - Documentation strings
- **equality.lisp** - Equality predicates (eq?, equal?, string=?)
- **file_io.lisp** - File reading and writing
- **let_star.lisp** - Sequential let bindings with `let*`
- **load.lisp** - File loading with `load`
- **macros.lisp** - Macro definition and expansion
- **named_functions.lisp** - Named function definitions with `define`
- **optional-parameters.lisp** - Optional and rest parameters
- **progn.lisp** - Sequential evaluation with `progn`
- **quasiquote.lisp** - Quasiquotation and unquote
- **regex.lisp** - PCRE2 regex operations (match, find, extract, replace, split)
- **tail_recursion.lisp** - Tail call optimization
- **utf8.lisp** - UTF-8 string support (grapheme-based length, substring, string-ref)

### regression/

Regression tests for bug fixes:

- **cond_case_multi_expr.lisp** - Multiple expressions in cond/case clauses
- **list_ref_type_check.lisp** - Type checking in list-ref
- **multiline_parsing.lisp** - Multi-line expression parsing
- **nested-unknown-hash-syntax.lisp** - Nested hash syntax handling
- **tail_call_unwrap.lisp** - Tail call result unwrapping

## Test Helpers

The `test-helpers.lisp` file provides assertion macros:

| Macro          | Usage                                  | Description            |
| -------------- | -------------------------------------- | ---------------------- |
| `assert-equal` | `(assert-equal actual expected "msg")` | Check equality         |
| `assert-true`  | `(assert-true condition "msg")`        | Check truthy value     |
| `assert-false` | `(assert-false condition "msg")`       | Check falsy value      |
| `assert-error` | `(assert-error expr "msg")`            | Check that expr throws |
| `assert-nil`   | `(assert-nil expr "msg")`              | Check for nil result   |

## See Also

- **[LANGUAGE_REFERENCE.md](../LANGUAGE_REFERENCE.md)** - Complete language documentation
- **[README.md](../README.md)** - Project overview and C API reference
- **CMakeLists.txt** - Automatic test discovery configuration
