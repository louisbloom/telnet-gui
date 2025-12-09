#include "../include/lisp.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>

/* Arithmetic operations */
static LispObject *builtin_add(LispObject *args, Environment *env);
static LispObject *builtin_subtract(LispObject *args, Environment *env);
static LispObject *builtin_multiply(LispObject *args, Environment *env);
static LispObject *builtin_divide(LispObject *args, Environment *env);

/* Number comparisons */
static LispObject *builtin_gt(LispObject *args, Environment *env);
static LispObject *builtin_lt(LispObject *args, Environment *env);
static LispObject *builtin_eq(LispObject *args, Environment *env);
static LispObject *builtin_gte(LispObject *args, Environment *env);
static LispObject *builtin_lte(LispObject *args, Environment *env);

/* String operations */
static LispObject *builtin_concat(LispObject *args, Environment *env);
static LispObject *builtin_number_to_string(LispObject *args, Environment *env);
static LispObject *builtin_string_to_number(LispObject *args, Environment *env);
static LispObject *builtin_split(LispObject *args, Environment *env);
static LispObject *builtin_string_lt(LispObject *args, Environment *env);
static LispObject *builtin_string_gt(LispObject *args, Environment *env);
static LispObject *builtin_string_lte(LispObject *args, Environment *env);
static LispObject *builtin_string_gte(LispObject *args, Environment *env);
static LispObject *builtin_string_contains(LispObject *args, Environment *env);
static LispObject *builtin_string_index(LispObject *args, Environment *env);
static LispObject *builtin_string_match(LispObject *args, Environment *env);
static LispObject *builtin_string_length(LispObject *args, Environment *env);
static LispObject *builtin_substring(LispObject *args, Environment *env);
static LispObject *builtin_string_ref(LispObject *args, Environment *env);
static LispObject *builtin_string_prefix_question(LispObject *args, Environment *env);
static LispObject *builtin_string_replace(LispObject *args, Environment *env);
static LispObject *builtin_string_upcase(LispObject *args, Environment *env);
static LispObject *builtin_string_downcase(LispObject *args, Environment *env);

/* Boolean operations */
static LispObject *builtin_not(LispObject *args, Environment *env);

/* List operations */
static LispObject *builtin_car(LispObject *args, Environment *env);
static LispObject *builtin_cdr(LispObject *args, Environment *env);
static LispObject *builtin_cons(LispObject *args, Environment *env);
static LispObject *builtin_list(LispObject *args, Environment *env);
static LispObject *builtin_list_length(LispObject *args, Environment *env);
static LispObject *builtin_list_ref(LispObject *args, Environment *env);
static LispObject *builtin_reverse(LispObject *args, Environment *env);
static LispObject *builtin_append(LispObject *args, Environment *env);

/* Predicates */
static LispObject *builtin_null_question(LispObject *args, Environment *env);
static LispObject *builtin_atom_question(LispObject *args, Environment *env);

/* Regex operations docstrings */
static const char *doc_regex_match = "Test if regex pattern matches string.\n"
                                     "\n"
                                     "## Parameters\n"
                                     "- `pattern` - PCRE2 regular expression pattern (string)\n"
                                     "- `string` - String to match against\n"
                                     "\n"
                                     "## Returns\n"
                                     "`#t` if pattern matches anywhere in string, `nil` otherwise.\n"
                                     "\n"
                                     "## Examples\n"
                                     "```lisp\n"
                                     "(regex-match \"[0-9]+\" \"abc123\")      ; => #t\n"
                                     "(regex-match \"^[0-9]+$\" \"abc123\")    ; => nil (not all digits)\n"
                                     "(regex-match \"hello\" \"hello world\")  ; => #t\n"
                                     "```\n"
                                     "\n"
                                     "## See Also\n"
                                     "- `regex-find` - Return the matched text\n"
                                     "- `regex-find-all` - Find all matches\n"
                                     "- `string-match?` - Wildcard pattern matching";

static const char *doc_regex_find = "Find first regex match in string.\n"
                                    "\n"
                                    "## Parameters\n"
                                    "- `pattern` - PCRE2 regular expression pattern (string)\n"
                                    "- `string` - String to search\n"
                                    "\n"
                                    "## Returns\n"
                                    "First matching substring, or `nil` if no match found.\n"
                                    "\n"
                                    "## Examples\n"
                                    "```lisp\n"
                                    "(regex-find \"[0-9]+\" \"abc123def456\")  ; => \"123\"\n"
                                    "(regex-find \"\\\\d+\" \"no digits\")       ; => nil\n"
                                    "(regex-find \"h.llo\" \"hello world\")    ; => \"hello\"\n"
                                    "```\n"
                                    "\n"
                                    "## See Also\n"
                                    "- `regex-match` - Test if pattern matches\n"
                                    "- `regex-find-all` - Find all matches\n"
                                    "- `regex-extract` - Extract capture groups";

static const char *doc_regex_find_all = "Find all regex matches in string.\n"
                                        "\n"
                                        "## Parameters\n"
                                        "- `pattern` - PCRE2 regular expression pattern (string)\n"
                                        "- `string` - String to search\n"
                                        "\n"
                                        "## Returns\n"
                                        "List of all matching substrings (in order), or empty list if no matches.\n"
                                        "\n"
                                        "## Examples\n"
                                        "```lisp\n"
                                        "(regex-find-all \"[0-9]+\" \"a1b22c333\")  ; => (\"1\" \"22\" \"333\")\n"
                                        "(regex-find-all \"\\\\w+\" \"hello world\")  ; => (\"hello\" \"world\")\n"
                                        "(regex-find-all \"x\" \"no match\")        ; => ()\n"
                                        "```\n"
                                        "\n"
                                        "## See Also\n"
                                        "- `regex-find` - Find first match only\n"
                                        "- `regex-split` - Split string by pattern";

static const char *doc_regex_extract =
    "Extract capture groups from regex match.\n"
    "\n"
    "## Parameters\n"
    "- `pattern` - PCRE2 regular expression with capture groups `(...)`\n"
    "- `string` - String to match against\n"
    "\n"
    "## Returns\n"
    "List of captured substrings (excluding group 0), or `nil` if no match.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(regex-extract \"(\\\\d+)-(\\\\d+)\" \"12-34\")     ; => (\"12\" \"34\")\n"
    "(regex-extract \"(\\\\w+)@(\\\\w+)\" \"user@host\")  ; => (\"user\" \"host\")\n"
    "(regex-extract \"(no)(match)\" \"text\")       ; => nil\n"
    "```\n"
    "\n"
    "## Notes\n"
    "Only returns capture groups (not the full match). Use parentheses `()` in pattern to define groups.";

static const char *doc_regex_replace =
    "Replace all regex matches in string.\n"
    "\n"
    "## Parameters\n"
    "- `pattern` - PCRE2 regular expression pattern (string)\n"
    "- `string` - String to search and replace in\n"
    "- `replacement` - Replacement string (can use `$1`, `$2` for captures)\n"
    "\n"
    "## Returns\n"
    "New string with all matches replaced.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(regex-replace \"[0-9]+\" \"a1b2c3\" \"X\")        ; => \"aXbXcX\"\n"
    "(regex-replace \"(\\\\w+)\" \"hello\" \"[$1]\")     ; => \"[hello]\"\n"
    "(regex-replace \"\\\\s+\" \"a  b  c\" \" \")        ; => \"a b c\"\n"
    "```\n"
    "\n"
    "## Notes\n"
    "Always replaces ALL occurrences. Use `$1`, `$2`, etc. to reference capture groups.\n"
    "\n"
    "## See Also\n"
    "- `regex-replace-all` - Alias for this function\n"
    "- `string-replace` - Literal string replacement (no regex)";

static const char *doc_regex_split = "Split string by regex pattern.\n"
                                     "\n"
                                     "## Parameters\n"
                                     "- `pattern` - PCRE2 regular expression pattern (string)\n"
                                     "- `string` - String to split\n"
                                     "\n"
                                     "## Returns\n"
                                     "List of substrings split by pattern matches.\n"
                                     "\n"
                                     "## Examples\n"
                                     "```lisp\n"
                                     "(regex-split \"[,;]\" \"a,b;c\")        ; => (\"a\" \"b\" \"c\")\n"
                                     "(regex-split \"\\\\s+\" \"a  b   c\")    ; => (\"a\" \"b\" \"c\")\n"
                                     "(regex-split \"\\\\d+\" \"a1b2c\")       ; => (\"a\" \"b\" \"c\")\n"
                                     "```\n"
                                     "\n"
                                     "## See Also\n"
                                     "- `split` - Split by literal string or wildcard\n"
                                     "- `regex-find-all` - Find all matches (doesn't split)";

static const char *doc_regex_escape = "Escape special regex characters in string.\n"
                                      "\n"
                                      "## Parameters\n"
                                      "- `string` - String to escape\n"
                                      "\n"
                                      "## Returns\n"
                                      "New string with regex metacharacters escaped.\n"
                                      "\n"
                                      "## Examples\n"
                                      "```lisp\n"
                                      "(regex-escape \"a.b\")         ; => \"a\\\\.b\"\n"
                                      "(regex-escape \"$100\")        ; => \"\\\\$100\"\n"
                                      "(regex-escape \"(test)\")      ; => \"\\\\(test\\\\)\"\n"
                                      "```\n"
                                      "\n"
                                      "## Notes\n"
                                      "Escapes these characters: `. ^ $ * + ? ( ) [ ] { } | \\`\n"
                                      "\n"
                                      "## Use Case\n"
                                      "Use this when you need to match user input literally in a regex pattern.";

static const char *doc_regex_valid = "Test if regex pattern is valid.\n"
                                     "\n"
                                     "## Parameters\n"
                                     "- `pattern` - PCRE2 regular expression pattern (string)\n"
                                     "\n"
                                     "## Returns\n"
                                     "`#t` if pattern compiles successfully, `nil` if invalid.\n"
                                     "\n"
                                     "## Examples\n"
                                     "```lisp\n"
                                     "(regex-valid? \"[0-9]+\")      ; => #t\n"
                                     "(regex-valid? \"[0-9\")        ; => nil (unclosed bracket)\n"
                                     "(regex-valid? \"(?P<name>\\\\w+)\")  ; => #t (named capture)\n"
                                     "```\n"
                                     "\n"
                                     "## Use Case\n"
                                     "Validate user-provided regex patterns before use.";

/* Regex operations */
static LispObject *builtin_regex_match(LispObject *args, Environment *env);
static LispObject *builtin_regex_find(LispObject *args, Environment *env);
static LispObject *builtin_regex_find_all(LispObject *args, Environment *env);
static LispObject *builtin_regex_extract(LispObject *args, Environment *env);
static LispObject *builtin_regex_replace(LispObject *args, Environment *env);
static LispObject *builtin_regex_replace_all(LispObject *args, Environment *env);
static LispObject *builtin_regex_split(LispObject *args, Environment *env);
static LispObject *builtin_regex_escape(LispObject *args, Environment *env);
static LispObject *builtin_regex_valid(LispObject *args, Environment *env);

/* Integer operations */
static LispObject *builtin_quotient(LispObject *args, Environment *env);
static LispObject *builtin_remainder(LispObject *args, Environment *env);
static LispObject *builtin_even_question(LispObject *args, Environment *env);
static LispObject *builtin_odd_question(LispObject *args, Environment *env);

/* Hash table operations */
static LispObject *builtin_make_hash_table(LispObject *args, Environment *env);
static LispObject *builtin_hash_ref(LispObject *args, Environment *env);
static LispObject *builtin_hash_set_bang(LispObject *args, Environment *env);
static LispObject *builtin_hash_remove_bang(LispObject *args, Environment *env);
static LispObject *builtin_hash_clear_bang(LispObject *args, Environment *env);
static LispObject *builtin_hash_count(LispObject *args, Environment *env);
static LispObject *builtin_hash_keys(LispObject *args, Environment *env);
static LispObject *builtin_hash_values(LispObject *args, Environment *env);
static LispObject *builtin_hash_entries(LispObject *args, Environment *env);

/* File I/O operations docstrings */
static const char *doc_open = "Open a file for reading or writing.\n"
                              "\n"
                              "## Parameters\n"
                              "- `filename` - Path to file (string)\n"
                              "- `mode` - Optional mode: `\"r\"` (read, default), `\"w\"` (write), `\"a\"` (append)\n"
                              "\n"
                              "## Returns\n"
                              "File stream object for use with `read-line`, `write-line`, `close`, etc.\n"
                              "Returns error if file cannot be opened.\n"
                              "\n"
                              "## Examples\n"
                              "```lisp\n"
                              "(define f (open \"test.txt\" \"r\"))  ; Open for reading\n"
                              "(define f (open \"out.txt\" \"w\"))   ; Open for writing\n"
                              "(define f (open \"log.txt\" \"a\"))   ; Open for appending\n"
                              "```\n"
                              "\n"
                              "## Notes\n"
                              "Always close files with `(close stream)` when done. Files are not auto-closed.\n"
                              "\n"
                              "## See Also\n"
                              "- `close` - Close file stream\n"
                              "- `read-line` - Read line from file\n"
                              "- `write-line` - Write line to file";

static const char *doc_close = "Close a file stream.\n"
                               "\n"
                               "## Parameters\n"
                               "- `stream` - File stream object from `open`\n"
                               "\n"
                               "## Returns\n"
                               "`nil`\n"
                               "\n"
                               "## Examples\n"
                               "```lisp\n"
                               "(define f (open \"test.txt\" \"r\"))\n"
                               "(read-line f)\n"
                               "(close f)\n"
                               "```\n"
                               "\n"
                               "## Notes\n"
                               "Always close files when done. Closing an already-closed stream is safe (no-op).\n"
                               "\n"
                               "## See Also\n"
                               "- `open` - Open file stream";

static const char *doc_read_line = "Read one line from file stream.\n"
                                   "\n"
                                   "## Parameters\n"
                                   "- `stream` - File stream object from `open`\n"
                                   "\n"
                                   "## Returns\n"
                                   "String containing the line (without newline), or `nil` at end-of-file.\n"
                                   "\n"
                                   "## Examples\n"
                                   "```lisp\n"
                                   "(define f (open \"test.txt\" \"r\"))\n"
                                   "(define line (read-line f))  ; => \"first line\"\n"
                                   "(read-line f)                ; => \"second line\"\n"
                                   "(read-line f)                ; => nil (EOF)\n"
                                   "(close f)\n"
                                   "```\n"
                                   "\n"
                                   "## Notes\n"
                                   "- Handles Unix (LF), Windows (CRLF), and old Mac (CR) line endings\n"
                                   "- Returns `nil` at end-of-file\n"
                                   "- Does not include the newline character in returned string\n"
                                   "\n"
                                   "## See Also\n"
                                   "- `write-line` - Write line to file\n"
                                   "- `read-sexp` - Read S-expressions from file";

static const char *doc_write_line = "Write string to file stream with newline.\n"
                                    "\n"
                                    "## Parameters\n"
                                    "- `stream` - File stream object from `open`\n"
                                    "- `text` - String to write\n"
                                    "\n"
                                    "## Returns\n"
                                    "The text that was written.\n"
                                    "\n"
                                    "## Examples\n"
                                    "```lisp\n"
                                    "(define f (open \"out.txt\" \"w\"))\n"
                                    "(write-line f \"first line\")\n"
                                    "(write-line f \"second line\")\n"
                                    "(close f)\n"
                                    "```\n"
                                    "\n"
                                    "## Notes\n"
                                    "- Automatically appends newline after text\n"
                                    "- Flushes output immediately\n"
                                    "\n"
                                    "## See Also\n"
                                    "- `read-line` - Read line from file";

static const char *doc_read_sexp = "Read S-expressions from file.\n"
                                   "\n"
                                   "## Parameters\n"
                                   "- `stream-or-filename` - File stream or filename string\n"
                                   "\n"
                                   "## Returns\n"
                                   "Single S-expression if file contains one, or list of all S-expressions.\n"
                                   "Returns error if parse fails.\n"
                                   "\n"
                                   "## Examples\n"
                                   "```lisp\n"
                                   "; File contains: (+ 1 2) (* 3 4)\n"
                                   "(read-sexp \"math.lisp\")  ; => ((+ 1 2) (* 3 4))\n"
                                   "\n"
                                   "; File contains: (define x 10)\n"
                                   "(read-sexp \"single.lisp\")  ; => (define x 10)\n"
                                   "```\n"
                                   "\n"
                                   "## Notes\n"
                                   "- Accepts filename string or open file stream\n"
                                   "- Auto-closes file if filename provided\n"
                                   "- Returns single expr if file has one, list if multiple\n"
                                   "\n"
                                   "## See Also\n"
                                   "- `load` - Load and evaluate file\n"
                                   "- `read-json` - Read JSON from file";

static const char *doc_read_json = "Read JSON from file (basic parser).\n"
                                   "\n"
                                   "## Parameters\n"
                                   "- `stream-or-filename` - File stream or filename string\n"
                                   "\n"
                                   "## Returns\n"
                                   "Lisp object representing JSON:\n"
                                   "- JSON object → hash table\n"
                                   "- JSON array → list (NOT YET SUPPORTED)\n"
                                   "- JSON string → string\n"
                                   "- JSON number → integer or float\n"
                                   "- JSON true → `#t`\n"
                                   "- JSON false/null → `nil`\n"
                                   "\n"
                                   "## Examples\n"
                                   "```lisp\n"
                                   "; File contains: {\"name\": \"Alice\", \"age\": 30}\n"
                                   "(define obj (read-json \"data.json\"))\n"
                                   "(hash-ref obj \"name\")  ; => \"Alice\"\n"
                                   "(hash-ref obj \"age\")   ; => 30\n"
                                   "```\n"
                                   "\n"
                                   "## Limitations\n"
                                   "- Basic implementation: nested objects and arrays not yet supported\n"
                                   "- Only top-level objects work reliably\n"
                                   "\n"
                                   "## See Also\n"
                                   "- `read-sexp` - Read Lisp S-expressions from file";

static const char *doc_delete_file =
    "Delete a file from filesystem.\n"
    "\n"
    "## Parameters\n"
    "- `filename` - Path to file (string)\n"
    "\n"
    "## Returns\n"
    "`nil` on success, error if deletion fails.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(delete-file \"temp.txt\")  ; => nil\n"
    "(delete-file \"/no/such/file\")  ; => Error: ...\n"
    "```\n"
    "\n"
    "## Notes\n"
    "Permanent operation - deleted files cannot be recovered.\n"
    "\n"
    "## Errors\n"
    "Returns error with system message if deletion fails (permission denied, file not found, etc.)";

static const char *doc_load = "Load and evaluate Lisp file.\n"
                              "\n"
                              "## Parameters\n"
                              "- `filename` - Path to Lisp file (string)\n"
                              "\n"
                              "## Returns\n"
                              "Result of last expression in file, or error if load fails.\n"
                              "\n"
                              "## Examples\n"
                              "```lisp\n"
                              "(load \"utils.lisp\")       ; Load utility functions\n"
                              "(load \"~/.lisprc\")         ; Load config file\n"
                              "```\n"
                              "\n"
                              "## Notes\n"
                              "- Evaluates all expressions in file sequentially\n"
                              "- Returns value of last expression\n"
                              "- File must be valid Lisp syntax\n"
                              "- Definitions are added to current environment\n"
                              "- Use `expand-path` to handle `~/` in paths\n"
                              "\n"
                              "## See Also\n"
                              "- `read-sexp` - Read without evaluating\n"
                              "- `expand-path` - Expand `~/ paths";

/* File I/O operations */
static LispObject *builtin_open(LispObject *args, Environment *env);
static LispObject *builtin_close(LispObject *args, Environment *env);
static LispObject *builtin_read_line(LispObject *args, Environment *env);
static LispObject *builtin_write_line(LispObject *args, Environment *env);
static LispObject *builtin_read_sexp(LispObject *args, Environment *env);
static LispObject *builtin_read_json(LispObject *args, Environment *env);
static LispObject *builtin_delete_file(LispObject *args, Environment *env);
static LispObject *builtin_load(LispObject *args, Environment *env);

/* Common Lisp printing operations */
static LispObject *builtin_princ(LispObject *args, Environment *env);
static LispObject *builtin_prin1(LispObject *args, Environment *env);
static LispObject *builtin_print_cl(LispObject *args, Environment *env);
static LispObject *builtin_format(LispObject *args, Environment *env);
static LispObject *builtin_terpri(LispObject *args, Environment *env);

/* Type predicates */
static LispObject *builtin_integer_question(LispObject *args, Environment *env);
static LispObject *builtin_boolean_question(LispObject *args, Environment *env);
static LispObject *builtin_number_question(LispObject *args, Environment *env);
static LispObject *builtin_vector_question(LispObject *args, Environment *env);
static LispObject *builtin_hash_table_question(LispObject *args, Environment *env);
static LispObject *builtin_string_question(LispObject *args, Environment *env);
static LispObject *builtin_symbol_question(LispObject *args, Environment *env);
static LispObject *builtin_list_question(LispObject *args, Environment *env);

/* Symbol operations */
static LispObject *builtin_symbol_to_string(LispObject *args, Environment *env);

/* Vector operations */
static LispObject *builtin_make_vector(LispObject *args, Environment *env);
static LispObject *builtin_vector_ref(LispObject *args, Environment *env);
static LispObject *builtin_vector_set_bang(LispObject *args, Environment *env);
static LispObject *builtin_vector_length(LispObject *args, Environment *env);
static LispObject *builtin_vector_push_bang(LispObject *args, Environment *env);
static LispObject *builtin_vector_pop_bang(LispObject *args, Environment *env);

/* Alist and mapping operations */
static LispObject *builtin_assoc(LispObject *args, Environment *env);
static LispObject *builtin_assq(LispObject *args, Environment *env);
static LispObject *builtin_assv(LispObject *args, Environment *env);
static LispObject *builtin_alist_get(LispObject *args, Environment *env);
static LispObject *builtin_map(LispObject *args, Environment *env);
static LispObject *builtin_mapcar(LispObject *args, Environment *env);

/* Error introspection and handling */
static LispObject *builtin_error_question(LispObject *args, Environment *env);
static LispObject *builtin_error_type(LispObject *args, Environment *env);
static LispObject *builtin_error_message(LispObject *args, Environment *env);
static LispObject *builtin_error_stack(LispObject *args, Environment *env);
static LispObject *builtin_error_data(LispObject *args, Environment *env);
static LispObject *builtin_signal(LispObject *args, Environment *env);
static LispObject *builtin_error(LispObject *args, Environment *env);

/* Docstring introspection */
static LispObject *builtin_lambda_docstring(LispObject *args, Environment *env);
static LispObject *builtin_macro_docstring(LispObject *args, Environment *env);
static LispObject *builtin_documentation(LispObject *args, Environment *env);

/* Equality predicates */
static LispObject *builtin_eq_predicate(LispObject *args, Environment *env);
static LispObject *builtin_equal_predicate(LispObject *args, Environment *env);
static LispObject *builtin_string_eq_predicate(LispObject *args, Environment *env);

/* Path expansion functions */
static LispObject *builtin_home_directory(LispObject *args, Environment *env);
static LispObject *builtin_expand_path(LispObject *args, Environment *env);

/* Helper for wildcard matching */
static int match_char_class(const char **pattern, char c);
static int wildcard_match(const char *pattern, const char *str);

/* ===========================================================================
 * Docstrings for Built-in Functions
 * =========================================================================== */

/* Arithmetic operations */
static const char *doc_add = "Add numbers together.\n"
                             "\n"
                             "## Parameters\n"
                             "- `numbers...` - Zero or more numbers to add\n"
                             "\n"
                             "## Returns\n"
                             "Sum of all numbers. Returns `0` if no arguments provided.\n"
                             "If all arguments are integers, returns integer; otherwise returns float.\n"
                             "\n"
                             "## Examples\n"
                             "```lisp\n"
                             "(+)              ; => 0\n"
                             "(+ 1 2 3)        ; => 6 (integer)\n"
                             "(+ 10 5)         ; => 15 (integer)\n"
                             "(+ 1.5 2.5)      ; => 4.0 (float)\n"
                             "(+ 1 2.5)        ; => 3.5 (mixed integer/float -> float)\n"
                             "```";

static const char *doc_subtract =
    "Subtract numbers or negate a single number.\n"
    "\n"
    "## Parameters\n"
    "- `number` - With one argument, returns the negation\n"
    "- `minuend subtrahends...` - With multiple arguments, subtracts all subsequent numbers from the first\n"
    "\n"
    "## Returns\n"
    "Result of subtraction. Returns integer if all arguments are integers; otherwise returns float.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(- 5)            ; => -5 (unary negation)\n"
    "(- 10 3)         ; => 7 (integer)\n"
    "(- 10 3.5)       ; => 6.5 (float - mixed types)\n"
    "(- 100 20 30)    ; => 50 (multiple subtractions)\n"
    "```";

static const char *doc_multiply = "Multiply numbers together.\n"
                                  "\n"
                                  "## Parameters\n"
                                  "- `numbers...` - Zero or more numbers to multiply\n"
                                  "\n"
                                  "## Returns\n"
                                  "Product of all numbers. Returns `1` if no arguments provided.\n"
                                  "If all arguments are integers, returns integer; otherwise returns float.\n"
                                  "\n"
                                  "## Examples\n"
                                  "```lisp\n"
                                  "(*)              ; => 1\n"
                                  "(* 2 3 4)        ; => 24 (integer)\n"
                                  "(* 3 4.0)        ; => 12.0 (float - mixed types)\n"
                                  "(* 5 -2)         ; => -10\n"
                                  "```";

static const char *doc_divide =
    "Divide numbers or compute reciprocal.\n"
    "\n"
    "## Parameters\n"
    "- `number` - With one argument, returns the reciprocal (1/number)\n"
    "- `dividend divisors...` - With multiple arguments, divides the first number by all subsequent numbers\n"
    "\n"
    "## Returns\n"
    "Result of division. **Always returns a float**, even for integer arguments.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(/ 2)            ; => 0.5 (reciprocal: 1/2)\n"
    "(/ 10 2)         ; => 5.0 (always float)\n"
    "(/ 10 3)         ; => 3.333333...\n"
    "(/ 100 2 5)      ; => 10.0 (multiple divisions: 100/2/5)\n"
    "```";

static const char *doc_quotient = "Integer division - divide and truncate to integer.\n"
                                  "\n"
                                  "## Parameters\n"
                                  "- `dividend` - The number to be divided (integer or float)\n"
                                  "- `divisor` - The number to divide by (integer or float)\n"
                                  "\n"
                                  "## Returns\n"
                                  "Integer result of division, truncated toward zero.\n"
                                  "\n"
                                  "## Examples\n"
                                  "```lisp\n"
                                  "(quotient 10 3)      ; => 3\n"
                                  "(quotient 17 5)      ; => 3\n"
                                  "(quotient -17 5)     ; => -3\n"
                                  "(quotient 10.8 3.2)  ; => 3\n"
                                  "```\n"
                                  "\n"
                                  "## See Also\n"
                                  "- `remainder` - Get the remainder of integer division\n"
                                  "- `/` - Regular division (returns float)";

static const char *doc_remainder = "Integer remainder (modulo operation).\n"
                                   "\n"
                                   "## Parameters\n"
                                   "- `dividend` - The number to be divided (integer or float)\n"
                                   "- `divisor` - The number to divide by (integer or float)\n"
                                   "\n"
                                   "## Returns\n"
                                   "Integer remainder after division.\n"
                                   "\n"
                                   "## Examples\n"
                                   "```lisp\n"
                                   "(remainder 17 5)     ; => 2\n"
                                   "(remainder 10 3)     ; => 1\n"
                                   "(remainder 20 5)     ; => 0\n"
                                   "(remainder -17 5)    ; => -2\n"
                                   "```\n"
                                   "\n"
                                   "## See Also\n"
                                   "- `quotient` - Integer division\n"
                                   "- `/` - Regular division (returns float)";

/* String operations */
static const char *doc_concat =
    "Concatenate strings together.\n"
    "\n"
    "## Parameters\n"
    "- `strings...` - Zero or more strings to concatenate\n"
    "\n"
    "## Returns\n"
    "A new string formed by concatenating all arguments. Returns empty string `\"\"` if no arguments provided.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(concat \"Hello\" \" \" \"World\")     ; => \"Hello World\"\n"
    "(concat \"foo\" \"bar\" \"baz\")      ; => \"foobarbaz\"\n"
    "(concat)                          ; => \"\"\n"
    "(concat \"test\")                   ; => \"test\"\n"
    "```";

static const char *doc_substring =
    "Extract a substring by character indices (UTF-8 aware).\n"
    "\n"
    "## Parameters\n"
    "- `string` - The input string\n"
    "- `start` - Starting character index (0-based, inclusive)\n"
    "- `end` - Ending character index (0-based, exclusive)\n"
    "\n"
    "## Returns\n"
    "A new string containing characters from `start` to `end-1`. Returns empty string if `start >= end`.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(substring \"Hello\" 0 5)          ; => \"Hello\"\n"
    "(substring \"Hello\" 1 4)          ; => \"ell\"\n"
    "(substring \"Hello, 世界!\" 7 9)    ; => \"世界\" (UTF-8 aware)\n"
    "```\n"
    "\n"
    "## Notes\n"
    "- Indices are **character** positions, not byte positions\n"
    "- Works correctly with multi-byte UTF-8 characters\n"
    "- Index out of bounds returns error";

static const char *doc_string_length = "Get the character count of a string (UTF-8 aware).\n"
                                       "\n"
                                       "## Parameters\n"
                                       "- `string` - The input string\n"
                                       "\n"
                                       "## Returns\n"
                                       "Number of characters in the string (not byte count).\n"
                                       "\n"
                                       "## Examples\n"
                                       "```lisp\n"
                                       "(string-length \"Hello\")          ; => 5\n"
                                       "(string-length \"世界\")            ; => 2 (UTF-8 characters)\n"
                                       "(string-length \"Hello, 世界! 🌍\")  ; => 15 (not byte count)\n"
                                       "(string-length \"\")               ; => 0\n"
                                       "```\n"
                                       "\n"
                                       "## Notes\n"
                                       "- Returns **character** count, not byte count\n"
                                       "- Correctly handles multi-byte UTF-8 characters";

/* List operations */
static const char *doc_car = "Get the first element of a list.\n"
                             "\n"
                             "## Parameters\n"
                             "- `list` - A non-empty list\n"
                             "\n"
                             "## Returns\n"
                             "The first element of the list.\n"
                             "\n"
                             "## Examples\n"
                             "```lisp\n"
                             "(car '(1 2 3))           ; => 1\n"
                             "(car '(\"a\" \"b\"))        ; => \"a\"\n"
                             "(car (list 10 20 30))    ; => 10\n"
                             "```\n"
                             "\n"
                             "## See Also\n"
                             "- `cdr` - Get the rest of a list\n"
                             "- `cons` - Construct a new list cell";

static const char *doc_cdr =
    "Get the rest of a list (all elements except the first).\n"
    "\n"
    "## Parameters\n"
    "- `list` - A non-empty list\n"
    "\n"
    "## Returns\n"
    "A list containing all elements except the first. Returns `nil` if the list has only one element.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(cdr '(1 2 3))           ; => (2 3)\n"
    "(cdr '(1))               ; => nil\n"
    "(cdr '(\"a\" \"b\" \"c\"))    ; => (\"b\" \"c\")\n"
    "```\n"
    "\n"
    "## See Also\n"
    "- `car` - Get the first element of a list\n"
    "- `cons` - Construct a new list cell";

static const char *doc_cons = "Construct a new list cell (cons cell).\n"
                              "\n"
                              "## Parameters\n"
                              "- `element` - The value to place in the car (first position)\n"
                              "- `list` - The list to place in the cdr (rest position)\n"
                              "\n"
                              "## Returns\n"
                              "A new list with `element` as the first item and `list` as the rest.\n"
                              "\n"
                              "## Examples\n"
                              "```lisp\n"
                              "(cons 1 '(2 3))          ; => (1 2 3)\n"
                              "(cons 'a nil)            ; => (a)\n"
                              "(cons 1 (cons 2 '()))    ; => (1 2)\n"
                              "```\n"
                              "\n"
                              "## See Also\n"
                              "- `car` - Get the first element\n"
                              "- `cdr` - Get the rest\n"
                              "- `list` - Create a list from multiple elements";

static const char *doc_list = "Create a list from the given elements.\n"
                              "\n"
                              "## Parameters\n"
                              "- `elements...` - Zero or more elements to put in the list\n"
                              "\n"
                              "## Returns\n"
                              "A new list containing all the provided elements. Returns `nil` for empty list.\n"
                              "\n"
                              "## Examples\n"
                              "```lisp\n"
                              "(list 1 2 3)             ; => (1 2 3)\n"
                              "(list \"a\" \"b\" \"c\")      ; => (\"a\" \"b\" \"c\")\n"
                              "(list)                   ; => nil\n"
                              "(list (+ 1 2) (* 3 4))   ; => (3 12) (evaluates arguments)\n"
                              "```\n"
                              "\n"
                              "## See Also\n"
                              "- `cons` - Construct a single list cell\n"
                              "- `append` - Concatenate lists";

static const char *doc_append =
    "Concatenate multiple lists into a single list.\n"
    "\n"
    "## Parameters\n"
    "- `lists...` - Zero or more lists to concatenate\n"
    "\n"
    "## Returns\n"
    "A new list containing all elements from all input lists in order. Returns `nil` if no arguments provided.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(append '(1 2) '(3 4))               ; => (1 2 3 4)\n"
    "(append '(1 2) '(3 4) '(5 6))        ; => (1 2 3 4 5 6)\n"
    "(append '() '(1 2))                  ; => (1 2)\n"
    "(append '(1 2) '())                  ; => (1 2)\n"
    "(append)                             ; => nil\n"
    "```\n"
    "\n"
    "## Notes\n"
    "- Returns a **new list** (does not modify input lists)\n"
    "- Works with any number of lists";

/* Type predicates */
static const char *doc_null_question = "Check if a value is nil (null/empty).\n"
                                       "\n"
                                       "## Parameters\n"
                                       "- `value` - Any value to test\n"
                                       "\n"
                                       "## Returns\n"
                                       "`#t` if the value is `nil`, `#f` otherwise.\n"
                                       "\n"
                                       "## Examples\n"
                                       "```lisp\n"
                                       "(null? nil)          ; => #t\n"
                                       "(null? #f)           ; => #t (nil and #f are the same)\n"
                                       "(null? '())          ; => #t (empty list is nil)\n"
                                       "(null? 0)            ; => #f (0 is truthy)\n"
                                       "(null? \"\")           ; => #f (empty string is truthy)\n"
                                       "```";

static const char *doc_atom_question = "Check if a value is an atom (not a list).\n"
                                       "\n"
                                       "## Parameters\n"
                                       "- `value` - Any value to test\n"
                                       "\n"
                                       "## Returns\n"
                                       "`#t` if the value is an atom (not a cons cell), `#f` otherwise.\n"
                                       "\n"
                                       "## Examples\n"
                                       "```lisp\n"
                                       "(atom? 42)           ; => #t\n"
                                       "(atom? \"hello\")      ; => #t\n"
                                       "(atom? nil)          ; => #t\n"
                                       "(atom? '(1 2 3))     ; => #f (lists are not atoms)\n"
                                       "```";

static const char *doc_integer_question = "Check if a value is an integer.\n"
                                          "\n"
                                          "## Parameters\n"
                                          "- `value` - Any value to test\n"
                                          "\n"
                                          "## Returns\n"
                                          "`#t` if the value is a 64-bit integer, `#f` otherwise.\n"
                                          "\n"
                                          "## Examples\n"
                                          "```lisp\n"
                                          "(integer? 42)        ; => #t\n"
                                          "(integer? -100)      ; => #t\n"
                                          "(integer? 3.14)      ; => #f (float)\n"
                                          "(integer? \"42\")      ; => #f (string)\n"
                                          "```";

static const char *doc_number_question = "Check if a value is a number (integer or float).\n"
                                         "\n"
                                         "## Parameters\n"
                                         "- `value` - Any value to test\n"
                                         "\n"
                                         "## Returns\n"
                                         "`#t` if the value is an integer or float, `#f` otherwise.\n"
                                         "\n"
                                         "## Examples\n"
                                         "```lisp\n"
                                         "(number? 42)         ; => #t (integer)\n"
                                         "(number? 3.14)       ; => #t (float)\n"
                                         "(number? \"42\")       ; => #f (string)\n"
                                         "```";

static const char *doc_string_question = "Check if a value is a string.\n"
                                         "\n"
                                         "## Parameters\n"
                                         "- `value` - Any value to test\n"
                                         "\n"
                                         "## Returns\n"
                                         "`#t` if the value is a string, `#f` otherwise.\n"
                                         "\n"
                                         "## Examples\n"
                                         "```lisp\n"
                                         "(string? \"hello\")    ; => #t\n"
                                         "(string? \"\")         ; => #t (empty string)\n"
                                         "(string? 'hello)     ; => #f (symbol)\n"
                                         "(string? 42)         ; => #f\n"
                                         "```";

static const char *doc_symbol_question = "Check if a value is a symbol.\n"
                                         "\n"
                                         "## Parameters\n"
                                         "- `value` - Any value to test\n"
                                         "\n"
                                         "## Returns\n"
                                         "`#t` if the value is a symbol, `#f` otherwise.\n"
                                         "\n"
                                         "## Examples\n"
                                         "```lisp\n"
                                         "(symbol? 'foo)       ; => #t\n"
                                         "(symbol? '+)         ; => #t\n"
                                         "(symbol? \"foo\")      ; => #f (string)\n"
                                         "(symbol? 42)         ; => #f\n"
                                         "```";

static const char *doc_boolean_question = "Check if a value is a boolean (#t or #f).\n"
                                          "\n"
                                          "## Parameters\n"
                                          "- `value` - Any value to test\n"
                                          "\n"
                                          "## Returns\n"
                                          "`#t` if the value is `#t` or `#f`, `#f` otherwise.\n"
                                          "\n"
                                          "## Examples\n"
                                          "```lisp\n"
                                          "(boolean? #t)        ; => #t\n"
                                          "(boolean? #f)        ; => #t\n"
                                          "(boolean? nil)       ; => #t (nil is #f)\n"
                                          "(boolean? 1)         ; => #f\n"
                                          "```";

static const char *doc_list_question = "Check if a value is a list (nil or cons cell).\n"
                                       "\n"
                                       "## Parameters\n"
                                       "- `value` - Any value to test\n"
                                       "\n"
                                       "## Returns\n"
                                       "`#t` if the value is `nil` or a cons cell, `#f` otherwise.\n"
                                       "\n"
                                       "## Examples\n"
                                       "```lisp\n"
                                       "(list? '(1 2 3))     ; => #t\n"
                                       "(list? nil)          ; => #t\n"
                                       "(list? '())          ; => #t\n"
                                       "(list? 42)           ; => #f\n"
                                       "```";

static const char *doc_vector_question = "Check if a value is a vector.\n"
                                         "\n"
                                         "## Parameters\n"
                                         "- `value` - Any value to test\n"
                                         "\n"
                                         "## Returns\n"
                                         "`#t` if the value is a vector, `#f` otherwise.\n"
                                         "\n"
                                         "## Examples\n"
                                         "```lisp\n"
                                         "(vector? (make-vector 5))    ; => #t\n"
                                         "(vector? #())                ; => #t (empty vector literal)\n"
                                         "(vector? '(1 2 3))           ; => #f (list)\n"
                                         "```";

/* Comparison operators */
static const char *doc_gt = "Test if numbers are in strictly decreasing order.\n"
                            "\n"
                            "## Parameters\n"
                            "- `n1 n2 ...` - Two or more numbers to compare\n"
                            "\n"
                            "## Returns\n"
                            "`#t` if each number is strictly greater than the next, `#f` otherwise.\n"
                            "\n"
                            "## Examples\n"
                            "```lisp\n"
                            "(> 5 3)              ; => #t\n"
                            "(> 10 5 2)           ; => #t\n"
                            "(> 5 5)              ; => #f (not strictly greater)\n"
                            "(> 3 5)              ; => #f\n"
                            "```";

static const char *doc_lt = "Test if numbers are in strictly increasing order.\n"
                            "\n"
                            "## Parameters\n"
                            "- `n1 n2 ...` - Two or more numbers to compare\n"
                            "\n"
                            "## Returns\n"
                            "`#t` if each number is strictly less than the next, `#f` otherwise.\n"
                            "\n"
                            "## Examples\n"
                            "```lisp\n"
                            "(< 3 5)              ; => #t\n"
                            "(< 1 2 3 4)          ; => #t\n"
                            "(< 5 5)              ; => #f (not strictly less)\n"
                            "(< 5 3)              ; => #f\n"
                            "```";

static const char *doc_eq = "Test if numbers are all equal.\n"
                            "\n"
                            "## Parameters\n"
                            "- `n1 n2 ...` - Two or more numbers to compare\n"
                            "\n"
                            "## Returns\n"
                            "`#t` if all numbers are equal, `#f` otherwise. Works with integers and floats.\n"
                            "\n"
                            "## Examples\n"
                            "```lisp\n"
                            "(= 5 5)              ; => #t\n"
                            "(= 5 5 5)            ; => #t\n"
                            "(= 5.0 5)            ; => #t (cross-type comparison)\n"
                            "(= 5 3)              ; => #f\n"
                            "```";

static const char *doc_gte = "Test if numbers are in non-increasing order (greater than or equal).\n"
                             "\n"
                             "## Parameters\n"
                             "- `n1 n2 ...` - Two or more numbers to compare\n"
                             "\n"
                             "## Returns\n"
                             "`#t` if each number is greater than or equal to the next, `#f` otherwise.\n"
                             "\n"
                             "## Examples\n"
                             "```lisp\n"
                             "(>= 5 3)             ; => #t\n"
                             "(>= 5 5)             ; => #t (equal is OK)\n"
                             "(>= 10 5 5 2)        ; => #t\n"
                             "(>= 3 5)             ; => #f\n"
                             "```";

static const char *doc_lte = "Test if numbers are in non-decreasing order (less than or equal).\n"
                             "\n"
                             "## Parameters\n"
                             "- `n1 n2 ...` - Two or more numbers to compare\n"
                             "\n"
                             "## Returns\n"
                             "`#t` if each number is less than or equal to the next, `#f` otherwise.\n"
                             "\n"
                             "## Examples\n"
                             "```lisp\n"
                             "(<= 3 5)             ; => #t\n"
                             "(<= 5 5)             ; => #t (equal is OK)\n"
                             "(<= 1 2 2 3)         ; => #t\n"
                             "(<= 5 3)             ; => #f\n"
                             "```";

/* Equality predicates */
static const char *doc_eq_predicate = "Test pointer equality (same object in memory).\n"
                                      "\n"
                                      "## Parameters\n"
                                      "- `obj1` - First object\n"
                                      "- `obj2` - Second object\n"
                                      "\n"
                                      "## Returns\n"
                                      "`#t` if both objects are the same object in memory, `#f` otherwise.\n"
                                      "\n"
                                      "## Examples\n"
                                      "```lisp\n"
                                      "(eq? 'foo 'foo)          ; => #t (symbols are interned)\n"
                                      "(eq? '(1 2) '(1 2))      ; => #f (different list objects)\n"
                                      "(define x '(1 2))\n"
                                      "(define y x)\n"
                                      "(eq? x y)                ; => #t (same object)\n"
                                      "```\n"
                                      "\n"
                                      "## Notes\n"
                                      "- Fast pointer comparison\n"
                                      "- Reliable for symbols (always interned)\n"
                                      "- NOT reliable for numbers or strings\n"
                                      "- Use `equal?` for structural equality";

static const char *doc_equal_predicate = "Test deep structural equality.\n"
                                         "\n"
                                         "## Parameters\n"
                                         "- `obj1` - First object\n"
                                         "- `obj2` - Second object\n"
                                         "\n"
                                         "## Returns\n"
                                         "`#t` if objects have the same structure and values, `#f` otherwise.\n"
                                         "\n"
                                         "## Examples\n"
                                         "```lisp\n"
                                         "(equal? '(1 2 3) '(1 2 3))      ; => #t (same values)\n"
                                         "(equal? \"abc\" \"abc\")            ; => #t (same string)\n"
                                         "(equal? '(1 (2 3)) '(1 (2 3)))  ; => #t (nested lists)\n"
                                         "(equal? 1 1.0)                  ; => #f (different types)\n"
                                         "```\n"
                                         "\n"
                                         "## Notes\n"
                                         "- Recursive comparison of lists, vectors, hash tables\n"
                                         "- Default choice for \"are these values the same?\" checks";

static const char *doc_string_eq_predicate = "Test string equality.\n"
                                             "\n"
                                             "## Parameters\n"
                                             "- `str1` - First string\n"
                                             "- `str2` - Second string\n"
                                             "\n"
                                             "## Returns\n"
                                             "`#t` if strings have identical character sequences, `#f` otherwise.\n"
                                             "\n"
                                             "## Examples\n"
                                             "```lisp\n"
                                             "(string=? \"foo\" \"foo\")     ; => #t\n"
                                             "(string=? \"foo\" \"bar\")     ; => #f\n"
                                             "(string=? \"\" \"\")           ; => #t\n"
                                             "```";

/* Boolean operations */
static const char *doc_not = "Logical negation.\n"
                             "\n"
                             "## Parameters\n"
                             "- `value` - Value to negate\n"
                             "\n"
                             "## Returns\n"
                             "`#t` if value is falsy (nil), `#f` if value is truthy.\n"
                             "\n"
                             "## Examples\n"
                             "```lisp\n"
                             "(not nil)            ; => #t\n"
                             "(not #f)             ; => #t\n"
                             "(not #t)             ; => #f\n"
                             "(not 0)              ; => #f (0 is truthy!)\n"
                             "(not \"\")             ; => #f (empty string is truthy!)\n"
                             "```\n"
                             "\n"
                             "## Notes\n"
                             "Only `nil` (and `#f`) are falsy in telnet-lisp";

/* Vector operations */
static const char *doc_make_vector =
    "Create a new vector with specified size and optional initial value.\n"
    "\n"
    "## Parameters\n"
    "- `size` - Number of elements (integer)\n"
    "- `initial-value` - Optional value to initialize all elements (defaults to `nil`)\n"
    "\n"
    "## Returns\n"
    "A new vector with `size` elements, all initialized to `initial-value`.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(make-vector 5)          ; => #(nil nil nil nil nil)\n"
    "(make-vector 3 0)        ; => #(0 0 0)\n"
    "(make-vector 4 \"x\")      ; => #(\"x\" \"x\" \"x\" \"x\")\n"
    "```";

static const char *doc_vector_ref = "Get element at index from vector.\n"
                                    "\n"
                                    "## Parameters\n"
                                    "- `vector` - The vector to access\n"
                                    "- `index` - Zero-based index (integer)\n"
                                    "\n"
                                    "## Returns\n"
                                    "The element at the specified index.\n"
                                    "\n"
                                    "## Examples\n"
                                    "```lisp\n"
                                    "(define v (make-vector 3 0))\n"
                                    "(vector-ref v 0)         ; => 0\n"
                                    "(vector-ref v 2)         ; => 0\n"
                                    "```\n"
                                    "\n"
                                    "## Errors\n"
                                    "Returns error if index is out of bounds.";

static const char *doc_vector_set_bang = "Set element at index in vector (mutating operation).\n"
                                         "\n"
                                         "## Parameters\n"
                                         "- `vector` - The vector to modify\n"
                                         "- `index` - Zero-based index (integer)\n"
                                         "- `value` - Value to store at index\n"
                                         "\n"
                                         "## Returns\n"
                                         "The value that was set.\n"
                                         "\n"
                                         "## Examples\n"
                                         "```lisp\n"
                                         "(define v (make-vector 3))\n"
                                         "(vector-set! v 0 \"a\")    ; => \"a\"\n"
                                         "(vector-set! v 1 \"b\")    ; => \"b\"\n"
                                         "v                        ; => #(\"a\" \"b\" nil)\n"
                                         "```\n"
                                         "\n"
                                         "## Notes\n"
                                         "- Modifies vector in place\n"
                                         "- Returns error if index out of bounds";

static const char *doc_vector_length = "Get the number of elements in a vector.\n"
                                       "\n"
                                       "## Parameters\n"
                                       "- `vector` - The vector to measure\n"
                                       "\n"
                                       "## Returns\n"
                                       "Integer count of elements in the vector.\n"
                                       "\n"
                                       "## Examples\n"
                                       "```lisp\n"
                                       "(vector-length (make-vector 5))  ; => 5\n"
                                       "(vector-length #())              ; => 0\n"
                                       "```";

static const char *doc_vector_push_bang = "Append element to end of vector (mutating operation).\n"
                                          "\n"
                                          "## Parameters\n"
                                          "- `vector` - The vector to modify\n"
                                          "- `value` - Value to append\n"
                                          "\n"
                                          "## Returns\n"
                                          "The value that was pushed.\n"
                                          "\n"
                                          "## Examples\n"
                                          "```lisp\n"
                                          "(define v (make-vector 0))\n"
                                          "(vector-push! v 10)      ; => 10\n"
                                          "(vector-push! v 20)      ; => 20\n"
                                          "v                        ; => #(10 20)\n"
                                          "```\n"
                                          "\n"
                                          "## Notes\n"
                                          "Vector grows dynamically to accommodate new elements.";

static const char *doc_vector_pop_bang = "Remove and return last element from vector (mutating operation).\n"
                                         "\n"
                                         "## Parameters\n"
                                         "- `vector` - The vector to modify\n"
                                         "\n"
                                         "## Returns\n"
                                         "The element that was removed from the end.\n"
                                         "\n"
                                         "## Examples\n"
                                         "```lisp\n"
                                         "(define v #(10 20 30))\n"
                                         "(vector-pop! v)          ; => 30\n"
                                         "(vector-pop! v)          ; => 20\n"
                                         "v                        ; => #(10)\n"
                                         "```\n"
                                         "\n"
                                         "## Errors\n"
                                         "Returns error if vector is empty.";

/* Hash table operations */
static const char *doc_make_hash_table = "Create a new empty hash table.\n"
                                         "\n"
                                         "## Returns\n"
                                         "A new hash table with no entries.\n"
                                         "\n"
                                         "## Examples\n"
                                         "```lisp\n"
                                         "(define ht (make-hash-table))\n"
                                         "(hash-set! ht \"key\" \"value\")\n"
                                         "(hash-ref ht \"key\")         ; => \"value\"\n"
                                         "```";

static const char *doc_hash_ref = "Get value for key from hash table.\n"
                                  "\n"
                                  "## Parameters\n"
                                  "- `hash-table` - The hash table to query\n"
                                  "- `key` - The key to look up\n"
                                  "\n"
                                  "## Returns\n"
                                  "The value associated with `key`, or `nil` if key not found.\n"
                                  "\n"
                                  "## Examples\n"
                                  "```lisp\n"
                                  "(define ht (make-hash-table))\n"
                                  "(hash-set! ht \"name\" \"Alice\")\n"
                                  "(hash-ref ht \"name\")        ; => \"Alice\"\n"
                                  "(hash-ref ht \"missing\")     ; => nil\n"
                                  "```";

static const char *doc_hash_set_bang = "Set key-value pair in hash table (mutating operation).\n"
                                       "\n"
                                       "## Parameters\n"
                                       "- `hash-table` - The hash table to modify\n"
                                       "- `key` - The key to set\n"
                                       "- `value` - The value to associate with key\n"
                                       "\n"
                                       "## Returns\n"
                                       "The value that was set.\n"
                                       "\n"
                                       "## Examples\n"
                                       "```lisp\n"
                                       "(define ht (make-hash-table))\n"
                                       "(hash-set! ht \"name\" \"Alice\")    ; => \"Alice\"\n"
                                       "(hash-set! ht \"age\" 30)           ; => 30\n"
                                       "(hash-ref ht \"name\")              ; => \"Alice\"\n"
                                       "```\n"
                                       "\n"
                                       "## Notes\n"
                                       "Modifies hash table in place. If key already exists, value is updated.";

static const char *doc_hash_remove_bang = "Remove key-value pair from hash table (mutating operation).\n"
                                          "\n"
                                          "## Parameters\n"
                                          "- `hash-table` - The hash table to modify\n"
                                          "- `key` - The key to remove\n"
                                          "\n"
                                          "## Returns\n"
                                          "`#t` if key was found and removed, `nil` if key wasn't present.\n"
                                          "\n"
                                          "## Examples\n"
                                          "```lisp\n"
                                          "(define ht (make-hash-table))\n"
                                          "(hash-set! ht \"key\" \"value\")\n"
                                          "(hash-remove! ht \"key\")       ; => #t\n"
                                          "(hash-remove! ht \"missing\")   ; => nil\n"
                                          "```";

static const char *doc_hash_count = "Get the number of key-value pairs in hash table.\n"
                                    "\n"
                                    "## Parameters\n"
                                    "- `hash-table` - The hash table to measure\n"
                                    "\n"
                                    "## Returns\n"
                                    "Integer count of entries in the hash table.\n"
                                    "\n"
                                    "## Examples\n"
                                    "```lisp\n"
                                    "(define ht (make-hash-table))\n"
                                    "(hash-count ht)                  ; => 0\n"
                                    "(hash-set! ht \"a\" 1)\n"
                                    "(hash-set! ht \"b\" 2)\n"
                                    "(hash-count ht)                  ; => 2\n"
                                    "```";

static const char *doc_hash_keys = "Get list of all keys in hash table.\n"
                                   "\n"
                                   "## Parameters\n"
                                   "- `hash-table` - The hash table to query\n"
                                   "\n"
                                   "## Returns\n"
                                   "List of all keys in the hash table. Order is not guaranteed.\n"
                                   "\n"
                                   "## Examples\n"
                                   "```lisp\n"
                                   "(define ht (make-hash-table))\n"
                                   "(hash-set! ht \"name\" \"Alice\")\n"
                                   "(hash-set! ht \"age\" 30)\n"
                                   "(hash-keys ht)               ; => (\"name\" \"age\") or (\"age\" \"name\")\n"
                                   "```";

static const char *doc_hash_values = "Get list of all values in hash table.\n"
                                     "\n"
                                     "## Parameters\n"
                                     "- `hash-table` - The hash table to query\n"
                                     "\n"
                                     "## Returns\n"
                                     "List of all values in the hash table. Order is not guaranteed.\n"
                                     "\n"
                                     "## Examples\n"
                                     "```lisp\n"
                                     "(define ht (make-hash-table))\n"
                                     "(hash-set! ht \"name\" \"Alice\")\n"
                                     "(hash-set! ht \"age\" 30)\n"
                                     "(hash-values ht)             ; => (\"Alice\" 30) or (30 \"Alice\")\n"
                                     "```";

static const char *doc_hash_entries = "Get list of all key-value pairs as cons cells.\n"
                                      "\n"
                                      "## Parameters\n"
                                      "- `hash-table` - The hash table to query\n"
                                      "\n"
                                      "## Returns\n"
                                      "List of `(key . value)` pairs. Order is not guaranteed.\n"
                                      "\n"
                                      "## Examples\n"
                                      "```lisp\n"
                                      "(define ht (make-hash-table))\n"
                                      "(hash-set! ht \"name\" \"Alice\")\n"
                                      "(hash-set! ht \"age\" 30)\n"
                                      "(hash-entries ht)            ; => ((\"name\" . \"Alice\") (\"age\" . 30))\n"
                                      "```\n"
                                      "\n"
                                      "## Notes\n"
                                      "Useful for iterating over hash tables with association list functions.";

/* List operations */
static const char *doc_map = "Apply function to each element of list, return new list of results.\n"
                             "\n"
                             "## Parameters\n"
                             "- `function` - Function to apply to each element\n"
                             "- `list` - List to process\n"
                             "\n"
                             "## Returns\n"
                             "New list containing results of applying `function` to each element.\n"
                             "\n"
                             "## Examples\n"
                             "```lisp\n"
                             "(map (lambda (x) (* x 2)) '(1 2 3 4))    ; => (2 4 6 8)\n"
                             "(map car '((1 . 2) (3 . 4) (5 . 6)))     ; => (1 3 5)\n"
                             "(map string-upcase '(\"a\" \"b\" \"c\"))      ; => (\"A\" \"B\" \"C\")\n"
                             "```";

static const char *doc_list_length = "Get the number of elements in a list.\n"
                                     "\n"
                                     "## Parameters\n"
                                     "- `list` - The list to measure\n"
                                     "\n"
                                     "## Returns\n"
                                     "Integer count of elements in the list.\n"
                                     "\n"
                                     "## Examples\n"
                                     "```lisp\n"
                                     "(list-length '(1 2 3))       ; => 3\n"
                                     "(list-length '())            ; => 0\n"
                                     "(list-length '(a))           ; => 1\n"
                                     "```";

static const char *doc_list_ref = "Get element at index from list (0-based).\n"
                                  "\n"
                                  "## Parameters\n"
                                  "- `list` - The list to access\n"
                                  "- `index` - Zero-based index (integer)\n"
                                  "\n"
                                  "## Returns\n"
                                  "The element at the specified index.\n"
                                  "\n"
                                  "## Examples\n"
                                  "```lisp\n"
                                  "(list-ref '(a b c) 0)        ; => a\n"
                                  "(list-ref '(a b c) 2)        ; => c\n"
                                  "(list-ref '(1 2 3 4) 3)      ; => 4\n"
                                  "```\n"
                                  "\n"
                                  "## Errors\n"
                                  "Returns error if index is out of bounds.";

static const char *doc_reverse = "Reverse a list.\n"
                                 "\n"
                                 "## Parameters\n"
                                 "- `list` - The list to reverse\n"
                                 "\n"
                                 "## Returns\n"
                                 "New list with elements in reverse order.\n"
                                 "\n"
                                 "## Examples\n"
                                 "```lisp\n"
                                 "(reverse '(1 2 3))           ; => (3 2 1)\n"
                                 "(reverse '(a))               ; => (a)\n"
                                 "(reverse '())                ; => nil\n"
                                 "```\n"
                                 "\n"
                                 "## Notes\n"
                                 "Returns a new list (does not modify input).";

/* String operations */
static const char *doc_split = "Split string by pattern (supports wildcards).\n"
                               "\n"
                               "## Parameters\n"
                               "- `string` - The string to split\n"
                               "- `pattern` - Pattern to split on (supports wildcards: `*`, `?`, `[abc]`)\n"
                               "\n"
                               "## Returns\n"
                               "List of strings split by pattern.\n"
                               "\n"
                               "## Examples\n"
                               "```lisp\n"
                               "(split \"a,b,c\" \",\")               ; => (\"a\" \"b\" \"c\")\n"
                               "(split \"foo*bar*baz\" \"*\")         ; => (\"foo\" \"bar\" \"baz\")\n"
                               "(split \"hello world\" \" \")        ; => (\"hello\" \"world\")\n"
                               "```";

static const char *doc_string_replace = "Replace all occurrences of substring in string.\n"
                                        "\n"
                                        "## Parameters\n"
                                        "- `string` - The input string\n"
                                        "- `old` - Substring to find\n"
                                        "- `new` - Substring to replace with\n"
                                        "\n"
                                        "## Returns\n"
                                        "New string with all occurrences of `old` replaced by `new`.\n"
                                        "\n"
                                        "## Examples\n"
                                        "```lisp\n"
                                        "(string-replace \"hello world\" \"world\" \"universe\")\n"
                                        "  ; => \"hello universe\"\n"
                                        "(string-replace \"hello\" \"l\" \"L\")\n"
                                        "  ; => \"heLLo\" (all occurrences)\n"
                                        "```";

static const char *doc_string_upcase = "Convert string to uppercase (ASCII only).\n"
                                       "\n"
                                       "## Parameters\n"
                                       "- `string` - The input string\n"
                                       "\n"
                                       "## Returns\n"
                                       "New string with all ASCII letters converted to uppercase.\n"
                                       "\n"
                                       "## Examples\n"
                                       "```lisp\n"
                                       "(string-upcase \"hello world\")    ; => \"HELLO WORLD\"\n"
                                       "(string-upcase \"Hello123\")       ; => \"HELLO123\"\n"
                                       "```\n"
                                       "\n"
                                       "## Notes\n"
                                       "Only converts ASCII letters (a-z). Non-ASCII characters unchanged.";

static const char *doc_string_downcase = "Convert string to lowercase (ASCII only).\n"
                                         "\n"
                                         "## Parameters\n"
                                         "- `string` - The input string\n"
                                         "\n"
                                         "## Returns\n"
                                         "New string with all ASCII letters converted to lowercase.\n"
                                         "\n"
                                         "## Examples\n"
                                         "```lisp\n"
                                         "(string-downcase \"HELLO WORLD\")  ; => \"hello world\"\n"
                                         "(string-downcase \"Hello123\")     ; => \"hello123\"\n"
                                         "```\n"
                                         "\n"
                                         "## Notes\n"
                                         "Only converts ASCII letters (A-Z). Non-ASCII characters unchanged.";

/* String conversion operations */
static const char *doc_number_to_string = "Convert number to string with optional radix.\n"
                                          "\n"
                                          "## Parameters\n"
                                          "- `number` - Integer or float to convert\n"
                                          "- `radix` - Optional base (2-36, defaults to 10)\n"
                                          "\n"
                                          "## Returns\n"
                                          "String representation of the number.\n"
                                          "\n"
                                          "## Examples\n"
                                          "```lisp\n"
                                          "(number->string 42)          ; => \"42\"\n"
                                          "(number->string 3.14159)     ; => \"3.14159\"\n"
                                          "(number->string 255 16)      ; => \"ff\" (hexadecimal)\n"
                                          "(number->string 8 2)         ; => \"1000\" (binary)\n"
                                          "```\n"
                                          "\n"
                                          "## Notes\n"
                                          "- Floats only supported in base 10\n"
                                          "- For other bases, number is converted to integer first\n"
                                          "\n"
                                          "## See Also\n"
                                          "- `string->number` - Convert string to number";

static const char *doc_string_to_number =
    "Convert string to number with optional radix.\n"
    "\n"
    "## Parameters\n"
    "- `string` - String to parse\n"
    "- `radix` - Optional base (2-36, defaults to 10)\n"
    "\n"
    "## Returns\n"
    "Number parsed from string, or `nil` if invalid.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(string->number \"42\")           ; => 42\n"
    "(string->number \"3.14\")         ; => 3.14\n"
    "(string->number \"ff\" 16)        ; => 255 (hexadecimal)\n"
    "(string->number \"#xff\")         ; => 255 (auto-detect hex prefix)\n"
    "(string->number \"invalid\")      ; => nil\n"
    "```\n"
    "\n"
    "## Notes\n"
    "- Supports prefixes: `#b` (binary), `#o` (octal), `#d` (decimal), `#x` (hex)\n"
    "- Floats only supported in base 10\n"
    "- Ignores leading/trailing whitespace\n"
    "\n"
    "## See Also\n"
    "- `number->string` - Convert number to string";

/* String comparison operations */
static const char *doc_string_lt = "Test if strings are in lexicographic order (less than).\n"
                                   "\n"
                                   "## Parameters\n"
                                   "- `str1` - First string\n"
                                   "- `str2` - Second string\n"
                                   "\n"
                                   "## Returns\n"
                                   "`#t` if `str1` is lexicographically less than `str2`, `nil` otherwise.\n"
                                   "\n"
                                   "## Examples\n"
                                   "```lisp\n"
                                   "(string<? \"apple\" \"banana\")     ; => #t\n"
                                   "(string<? \"zebra\" \"ant\")        ; => nil\n"
                                   "(string<? \"abc\" \"abc\")          ; => nil (equal)\n"
                                   "```\n"
                                   "\n"
                                   "## Notes\n"
                                   "Comparison is byte-by-byte (ASCII/UTF-8 code point order).\n"
                                   "\n"
                                   "## See Also\n"
                                   "- `string>?` - Greater than\n"
                                   "- `string=?` - Equality";

static const char *doc_string_gt = "Test if strings are in reverse lexicographic order (greater than).\n"
                                   "\n"
                                   "## Parameters\n"
                                   "- `str1` - First string\n"
                                   "- `str2` - Second string\n"
                                   "\n"
                                   "## Returns\n"
                                   "`#t` if `str1` is lexicographically greater than `str2`, `nil` otherwise.\n"
                                   "\n"
                                   "## Examples\n"
                                   "```lisp\n"
                                   "(string>? \"banana\" \"apple\")     ; => #t\n"
                                   "(string>? \"ant\" \"zebra\")        ; => nil\n"
                                   "(string>? \"abc\" \"abc\")          ; => nil (equal)\n"
                                   "```";

static const char *doc_string_lte =
    "Test if strings are in non-decreasing lexicographic order (less than or equal).\n"
    "\n"
    "## Parameters\n"
    "- `str1` - First string\n"
    "- `str2` - Second string\n"
    "\n"
    "## Returns\n"
    "`#t` if `str1` is lexicographically less than or equal to `str2`, `nil` otherwise.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(string<=? \"apple\" \"banana\")    ; => #t\n"
    "(string<=? \"abc\" \"abc\")         ; => #t (equal)\n"
    "(string<=? \"zebra\" \"ant\")       ; => nil\n"
    "```";

static const char *doc_string_gte =
    "Test if strings are in non-increasing lexicographic order (greater than or equal).\n"
    "\n"
    "## Parameters\n"
    "- `str1` - First string\n"
    "- `str2` - Second string\n"
    "\n"
    "## Returns\n"
    "`#t` if `str1` is lexicographically greater than or equal to `str2`, `nil` otherwise.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(string>=? \"banana\" \"apple\")    ; => #t\n"
    "(string>=? \"abc\" \"abc\")         ; => #t (equal)\n"
    "(string>=? \"ant\" \"zebra\")       ; => nil\n"
    "```";

/* String search operations */
static const char *doc_string_contains = "Test if string contains substring.\n"
                                         "\n"
                                         "## Parameters\n"
                                         "- `haystack` - String to search in\n"
                                         "- `needle` - Substring to find\n"
                                         "\n"
                                         "## Returns\n"
                                         "`#t` if `needle` is found anywhere in `haystack`, `nil` otherwise.\n"
                                         "\n"
                                         "## Examples\n"
                                         "```lisp\n"
                                         "(string-contains? \"hello world\" \"world\")   ; => #t\n"
                                         "(string-contains? \"hello world\" \"planet\")  ; => nil\n"
                                         "(string-contains? \"test\" \"\")               ; => #t (empty string)\n"
                                         "```\n"
                                         "\n"
                                         "## See Also\n"
                                         "- `string-index` - Find position of substring\n"
                                         "- `string-prefix?` - Test if string starts with prefix";

static const char *doc_string_index = "Find character index of first occurrence of substring.\n"
                                      "\n"
                                      "## Parameters\n"
                                      "- `haystack` - String to search in\n"
                                      "- `needle` - Substring to find\n"
                                      "\n"
                                      "## Returns\n"
                                      "Character index (0-based) of first occurrence, or `nil` if not found.\n"
                                      "\n"
                                      "## Examples\n"
                                      "```lisp\n"
                                      "(string-index \"hello world\" \"world\")    ; => 6\n"
                                      "(string-index \"hello\" \"l\")              ; => 2 (first 'l')\n"
                                      "(string-index \"test\" \"xyz\")             ; => nil\n"
                                      "```\n"
                                      "\n"
                                      "## Notes\n"
                                      "Returns character index, not byte offset (UTF-8 aware).\n"
                                      "\n"
                                      "## See Also\n"
                                      "- `string-contains?` - Test for substring presence";

static const char *doc_string_match = "Test if string matches wildcard pattern.\n"
                                      "\n"
                                      "## Parameters\n"
                                      "- `string` - String to test\n"
                                      "- `pattern` - Wildcard pattern\n"
                                      "\n"
                                      "## Returns\n"
                                      "`#t` if string matches pattern, `nil` otherwise.\n"
                                      "\n"
                                      "## Pattern Syntax\n"
                                      "- `*` - Match zero or more characters\n"
                                      "- `?` - Match exactly one character\n"
                                      "- `[abc]` - Match any character in set\n"
                                      "- `[a-z]` - Match any character in range\n"
                                      "- `[!abc]` - Match any character NOT in set\n"
                                      "\n"
                                      "## Examples\n"
                                      "```lisp\n"
                                      "(string-match? \"hello\" \"h*o\")        ; => #t\n"
                                      "(string-match? \"test.txt\" \"*.txt\")   ; => #t\n"
                                      "(string-match? \"file\" \"f?le\")        ; => #t\n"
                                      "(string-match? \"abc\" \"[a-z]*\")       ; => #t\n"
                                      "```\n"
                                      "\n"
                                      "## See Also\n"
                                      "- `regex-match` - Full regex pattern matching\n"
                                      "- `string-prefix?` - Test for exact prefix";

static const char *doc_string_ref = "Get character at index from string (UTF-8 aware).\n"
                                    "\n"
                                    "## Parameters\n"
                                    "- `string` - The input string\n"
                                    "- `index` - Character index (0-based)\n"
                                    "\n"
                                    "## Returns\n"
                                    "Single-character string at the specified index.\n"
                                    "\n"
                                    "## Examples\n"
                                    "```lisp\n"
                                    "(string-ref \"hello\" 0)          ; => \"h\"\n"
                                    "(string-ref \"hello\" 4)          ; => \"o\"\n"
                                    "(string-ref \"世界\" 0)            ; => \"世\" (UTF-8)\n"
                                    "```\n"
                                    "\n"
                                    "## Notes\n"
                                    "- Index is **character** position, not byte position\n"
                                    "- Works correctly with multi-byte UTF-8 characters\n"
                                    "- Returns error if index out of bounds\n"
                                    "\n"
                                    "## See Also\n"
                                    "- `substring` - Extract substring by range\n"
                                    "- `string-length` - Get character count";

static const char *doc_string_prefix = "Test if string starts with prefix.\n"
                                       "\n"
                                       "## Parameters\n"
                                       "- `prefix` - Prefix to test for\n"
                                       "- `string` - String to test\n"
                                       "\n"
                                       "## Returns\n"
                                       "`#t` if `string` starts with `prefix`, `nil` otherwise.\n"
                                       "\n"
                                       "## Examples\n"
                                       "```lisp\n"
                                       "(string-prefix? \"hello\" \"hello world\")   ; => #t\n"
                                       "(string-prefix? \"world\" \"hello world\")   ; => nil\n"
                                       "(string-prefix? \"\" \"anything\")           ; => #t (empty prefix)\n"
                                       "```\n"
                                       "\n"
                                       "## See Also\n"
                                       "- `string-contains?` - Test for substring anywhere\n"
                                       "- `string-match?` - Wildcard pattern matching";

/* Association list (alist) operations */
static const char *doc_assoc =
    "Find key-value pair in association list using structural equality.\n"
    "\n"
    "## Parameters\n"
    "- `key` - Key to search for\n"
    "- `alist` - Association list (list of `(key . value)` pairs)\n"
    "\n"
    "## Returns\n"
    "The first `(key . value)` pair where key matches (using `equal?`), or `nil` if not found.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(assoc 'name '((name . \"Alice\") (age . 30)))     ; => (name . \"Alice\")\n"
    "(assoc 'city '((name . \"Bob\") (age . 25)))       ; => nil\n"
    "(assoc '(1 2) '(((1 2) . \"pair\") (3 . \"x\")))   ; => ((1 2) . \"pair\")\n"
    "```\n"
    "\n"
    "## Notes\n"
    "Uses deep structural comparison (`equal?`). For pointer equality, use `assq`.\n"
    "\n"
    "## See Also\n"
    "- `assq` - Pointer equality search\n"
    "- `assv` - Value equality search\n"
    "- `alist-get` - Get value directly";

static const char *doc_assq = "Find key-value pair in association list using pointer equality.\n"
                              "\n"
                              "## Parameters\n"
                              "- `key` - Key to search for\n"
                              "- `alist` - Association list (list of `(key . value)` pairs)\n"
                              "\n"
                              "## Returns\n"
                              "The first `(key . value)` pair where key matches (using `eq?`), or `nil` if not found.\n"
                              "\n"
                              "## Examples\n"
                              "```lisp\n"
                              "(assq 'name '((name . \"Alice\") (age . 30)))      ; => (name . \"Alice\")\n"
                              "(assq 'city '((name . \"Bob\") (age . 25)))        ; => nil\n"
                              "```\n"
                              "\n"
                              "## Notes\n"
                              "- Uses pointer equality (`eq?`) - fastest but only reliable for symbols\n"
                              "- Symbols are always interned, so `assq` is safe for symbol keys\n"
                              "- NOT reliable for numbers or strings (use `assoc` instead)\n"
                              "\n"
                              "## See Also\n"
                              "- `assoc` - Structural equality search\n"
                              "- `eq?` - Pointer equality predicate";

static const char *doc_assv =
    "Find key-value pair in association list using value equality.\n"
    "\n"
    "## Parameters\n"
    "- `key` - Key to search for\n"
    "- `alist` - Association list (list of `(key . value)` pairs)\n"
    "\n"
    "## Returns\n"
    "The first `(key . value)` pair where key matches (using `eqv?`), or `nil` if not found.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(assv 42 '((42 . \"answer\") (10 . \"ten\")))       ; => (42 . \"answer\")\n"
    "(assv 'key '((key . \"value\") (x . \"y\")))        ; => (key . \"value\")\n"
    "```\n"
    "\n"
    "## Notes\n"
    "Uses `eqv?` equality (same as `equal?` in this implementation).\n"
    "\n"
    "## See Also\n"
    "- `assoc` - Structural equality (equivalent in this implementation)\n"
    "- `assq` - Pointer equality";

static const char *doc_alist_get = "Get value for key from association list with optional default.\n"
                                   "\n"
                                   "## Parameters\n"
                                   "- `key` - Key to search for\n"
                                   "- `alist` - Association list (list of `(key . value)` pairs)\n"
                                   "- `default` - Optional default value (defaults to `nil`)\n"
                                   "\n"
                                   "## Returns\n"
                                   "The value (cdr) of the first matching pair, or `default` if key not found.\n"
                                   "\n"
                                   "## Examples\n"
                                   "```lisp\n"
                                   "(alist-get 'name '((name . \"Alice\") (age . 30)))        ; => \"Alice\"\n"
                                   "(alist-get 'city '((name . \"Bob\")) \"Unknown\")          ; => \"Unknown\"\n"
                                   "(alist-get 'missing '((a . 1) (b . 2)))                  ; => nil\n"
                                   "```\n"
                                   "\n"
                                   "## Notes\n"
                                   "Returns the **value** directly, not the `(key . value)` pair.\n"
                                   "\n"
                                   "## See Also\n"
                                   "- `assoc` - Get full key-value pair\n"
                                   "- `hash-ref` - Hash table lookup";

/* Miscellaneous predicates and operations */
static const char *doc_even_question = "Test if number is even.\n"
                                       "\n"
                                       "## Parameters\n"
                                       "- `n` - Number to test (integer or float)\n"
                                       "\n"
                                       "## Returns\n"
                                       "`#t` if number is even, `#f` if odd.\n"
                                       "\n"
                                       "## Examples\n"
                                       "```lisp\n"
                                       "(even? 4)    ; => #t\n"
                                       "(even? 5)    ; => #f\n"
                                       "(even? 0)    ; => #t\n"
                                       "(even? -2)   ; => #t\n"
                                       "(even? 3.0)  ; => #f (converts to integer)\n"
                                       "```\n"
                                       "\n"
                                       "## Notes\n"
                                       "Converts floats to integers by truncation before testing.\n"
                                       "\n"
                                       "## See Also\n"
                                       "- `odd?` - Test if number is odd\n"
                                       "- `quotient` - Integer division\n"
                                       "- `remainder` - Integer remainder";

static const char *doc_odd_question = "Test if number is odd.\n"
                                      "\n"
                                      "## Parameters\n"
                                      "- `n` - Number to test (integer or float)\n"
                                      "\n"
                                      "## Returns\n"
                                      "`#t` if number is odd, `#f` if even.\n"
                                      "\n"
                                      "## Examples\n"
                                      "```lisp\n"
                                      "(odd? 5)     ; => #t\n"
                                      "(odd? 4)     ; => #f\n"
                                      "(odd? 1)     ; => #t\n"
                                      "(odd? -3)    ; => #t\n"
                                      "(odd? 2.0)   ; => #f (converts to integer)\n"
                                      "```\n"
                                      "\n"
                                      "## Notes\n"
                                      "Converts floats to integers by truncation before testing.\n"
                                      "\n"
                                      "## See Also\n"
                                      "- `even?` - Test if number is even\n"
                                      "- `quotient` - Integer division\n"
                                      "- `remainder` - Integer remainder";

static const char *doc_hash_table_question = "Test if value is a hash table.\n"
                                             "\n"
                                             "## Parameters\n"
                                             "- `value` - Value to test\n"
                                             "\n"
                                             "## Returns\n"
                                             "`#t` if value is a hash table, `nil` otherwise.\n"
                                             "\n"
                                             "## Examples\n"
                                             "```lisp\n"
                                             "(define ht (make-hash-table))\n"
                                             "(hash-table? ht)        ; => #t\n"
                                             "(hash-table? '(1 2 3))  ; => nil\n"
                                             "(hash-table? 42)        ; => nil\n"
                                             "```\n"
                                             "\n"
                                             "## See Also\n"
                                             "- `make-hash-table` - Create hash table\n"
                                             "- `vector?` - Test if value is vector\n"
                                             "- `list?` - Test if value is list";

static const char *doc_symbol_to_string = "Convert symbol to string.\n"
                                          "\n"
                                          "## Parameters\n"
                                          "- `symbol` - Symbol to convert\n"
                                          "\n"
                                          "## Returns\n"
                                          "String containing the symbol's name.\n"
                                          "\n"
                                          "## Examples\n"
                                          "```lisp\n"
                                          "(symbol->string 'hello)      ; => \"hello\"\n"
                                          "(symbol->string '+)           ; => \"+\"\n"
                                          "(symbol->string 'my-var)      ; => \"my-var\"\n"
                                          "\n"
                                          "; Using with variables\n"
                                          "(define x 'test)\n"
                                          "(symbol->string x)            ; => \"test\"\n"
                                          "```\n"
                                          "\n"
                                          "## Errors\n"
                                          "Returns error if argument is not a symbol.\n"
                                          "\n"
                                          "## See Also\n"
                                          "- `symbol?` - Test if value is symbol";

static const char *doc_hash_clear_bang =
    "Remove all entries from hash table (mutating).\n"
    "\n"
    "## Parameters\n"
    "- `table` - Hash table to clear\n"
    "\n"
    "## Returns\n"
    "`nil`\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(define ht (make-hash-table))\n"
    "(hash-set! ht \"key1\" 10)\n"
    "(hash-set! ht \"key2\" 20)\n"
    "(hash-count ht)         ; => 2\n"
    "(hash-clear! ht)\n"
    "(hash-count ht)         ; => 0\n"
    "```\n"
    "\n"
    "## Notes\n"
    "This is a mutating operation (note the `!` suffix). The hash table is modified in place.\n"
    "\n"
    "## See Also\n"
    "- `hash-remove!` - Remove single entry\n"
    "- `hash-count` - Get entry count\n"
    "- `make-hash-table` - Create new hash table";

/* Path expansion functions */
static const char *doc_home_directory =
    "Get the user's home directory path.\n"
    "\n"
    "## Parameters\n"
    "None.\n"
    "\n"
    "## Returns\n"
    "String with home directory path, or `nil` if home directory cannot be determined.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(home-directory)                    ; => \"/home/alice\" (Unix)\n"
    "(home-directory)                    ; => \"C:\\\\Users\\\\Alice\" (Windows)\n"
    "\n"
    "; Use in paths\n"
    "(define config-dir (concat (home-directory) \"/.config\"))\n"
    "; => \"/home/alice/.config\"\n"
    "```\n"
    "\n"
    "## Platform Behavior\n"
    "- **Unix/Linux/macOS**: Uses `$HOME` environment variable\n"
    "- **Windows**: Uses `%USERPROFILE%` or `%HOMEDRIVE%%HOMEPATH%`\n"
    "\n"
    "## See Also\n"
    "- `expand-path` - Expand `~/` prefix in paths";

static const char *doc_expand_path =
    "Expand `~/` prefix in file paths to the user's home directory.\n"
    "\n"
    "## Parameters\n"
    "- `path` - File path (string), may start with `~/`\n"
    "\n"
    "## Returns\n"
    "String with expanded path (if path starts with `~/`), or original string (if path does not start with `~/`).\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "; Basic expansion\n"
    "(expand-path \"~/config.lisp\")\n"
    "; => \"/home/alice/config.lisp\" (Unix)\n"
    "; => \"C:\\\\Users\\\\Alice\\\\config.lisp\" (Windows)\n"
    "\n"
    "; Subdirectories\n"
    "(expand-path \"~/Documents/notes.txt\")\n"
    "; => \"/home/alice/Documents/notes.txt\" (Unix)\n"
    "\n"
    "; No expansion (no ~/ prefix)\n"
    "(expand-path \"/etc/config\")         ; => \"/etc/config\"\n"
    "(expand-path \"relative/path\")       ; => \"relative/path\"\n"
    "(expand-path \"./local.lisp\")        ; => \"./local.lisp\"\n"
    "\n"
    "; Just ~ expands to home directory\n"
    "(expand-path \"~\")                   ; => \"/home/alice\"\n"
    "\n"
    "; Use with file I/O\n"
    "(define file (open (expand-path \"~/my-config.lisp\") \"r\"))\n"
    "(load (expand-path \"~/scripts/init.lisp\"))\n"
    "```\n"
    "\n"
    "## Notes\n"
    "- Detects `~/` at start of path\n"
    "- Replaces `~/` with home directory from `home-directory`\n"
    "- Handles cross-platform path separators\n"
    "- Works with both forward and backslashes after `~`\n"
    "\n"
    "## Errors\n"
    "Returns error if:\n"
    "- Home directory cannot be determined\n"
    "- Argument is not a string\n"
    "\n"
    "## Use Cases\n"
    "- Reading/writing user configuration files\n"
    "- Loading user-specific scripts\n"
    "- Saving data to user directories\n"
    "- Cross-platform file operations\n"
    "\n"
    "## See Also\n"
    "- `home-directory` - Get home directory path\n"
    "- `open` - Open file\n"
    "- `load` - Load Lisp file";

/* Error handling functions */
static const char *doc_error_question = "Test if value is an error object.\n"
                                        "\n"
                                        "## Parameters\n"
                                        "- `value` - Value to test\n"
                                        "\n"
                                        "## Returns\n"
                                        "`#t` if value is an error object, `#f` otherwise.\n"
                                        "\n"
                                        "## Examples\n"
                                        "```lisp\n"
                                        "(define err nil)\n"
                                        "(condition-case e\n"
                                        "    (signal 'test-error \"boom\")\n"
                                        "  (error (set! err e)))\n"
                                        "\n"
                                        "(error? err)            ; => #t\n"
                                        "(error? 42)             ; => #f\n"
                                        "(error? \"string\")       ; => #f\n"
                                        "```\n"
                                        "\n"
                                        "## See Also\n"
                                        "- `error-type` - Get error type symbol\n"
                                        "- `error-message` - Get error message\n"
                                        "- `signal` - Raise typed error";

static const char *doc_error_type =
    "Get error type symbol from error object.\n"
    "\n"
    "## Parameters\n"
    "- `error` - Error object\n"
    "\n"
    "## Returns\n"
    "Symbol representing the error type (e.g., `'error`, `'division-by-zero`, `'file-error`).\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(define err nil)\n"
    "(condition-case e\n"
    "    (signal 'my-error \"test\")\n"
    "  (error (set! err e)))\n"
    "\n"
    "(error-type err)        ; => my-error\n"
    "\n"
    "; Different error types\n"
    "(condition-case e\n"
    "    (/ 10 0)\n"
    "  (error (error-type e)))  ; => division-by-zero\n"
    "```\n"
    "\n"
    "## Errors\n"
    "Returns error if argument is not an error object.\n"
    "\n"
    "## See Also\n"
    "- `error?` - Test if value is error\n"
    "- `error-message` - Get error message\n"
    "- `signal` - Raise typed error";

static const char *doc_error_message = "Get error message string from error object.\n"
                                       "\n"
                                       "## Parameters\n"
                                       "- `error` - Error object\n"
                                       "\n"
                                       "## Returns\n"
                                       "String containing the human-readable error message.\n"
                                       "\n"
                                       "## Examples\n"
                                       "```lisp\n"
                                       "(define err nil)\n"
                                       "(condition-case e\n"
                                       "    (signal 'custom-error \"Something went wrong\")\n"
                                       "  (error (set! err e)))\n"
                                       "\n"
                                       "(error-message err)     ; => \"Something went wrong\"\n"
                                       "\n"
                                       "; With division-by-zero error\n"
                                       "(condition-case e\n"
                                       "    (/ 10 0)\n"
                                       "  (error (error-message e)))  ; => \"Division by zero\"\n"
                                       "```\n"
                                       "\n"
                                       "## Errors\n"
                                       "Returns error if argument is not an error object.\n"
                                       "\n"
                                       "## See Also\n"
                                       "- `error-type` - Get error type\n"
                                       "- `error-data` - Get error data\n"
                                       "- `error-stack` - Get call stack";

static const char *doc_error_stack =
    "Get call stack trace from error object.\n"
    "\n"
    "## Parameters\n"
    "- `error` - Error object\n"
    "\n"
    "## Returns\n"
    "List of function names in the call stack, or `nil` if no stack trace.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(define outer (lambda (x) (middle x)))\n"
    "(define middle (lambda (x) (inner x)))\n"
    "(define inner (lambda (x) (/ x 0)))\n"
    "\n"
    "(define err nil)\n"
    "(condition-case e\n"
    "    (outer 10)\n"
    "  (error (set! err e)))\n"
    "\n"
    "(error-stack err)       ; => (\"/\" \"inner\" \"middle\" \"outer\")\n"
    "```\n"
    "\n"
    "## Notes\n"
    "Stack traces show the sequence of function calls from innermost (where error occurred) to outermost.\n"
    "\n"
    "## Errors\n"
    "Returns error if argument is not an error object.\n"
    "\n"
    "## See Also\n"
    "- `error-message` - Get error message\n"
    "- `error-type` - Get error type";

static const char *doc_error_data = "Get error data payload from error object.\n"
                                    "\n"
                                    "## Parameters\n"
                                    "- `error` - Error object\n"
                                    "\n"
                                    "## Returns\n"
                                    "The data associated with the error, or `nil` if no data.\n"
                                    "\n"
                                    "## Examples\n"
                                    "```lisp\n"
                                    "; Error with simple data\n"
                                    "(define err nil)\n"
                                    "(condition-case e\n"
                                    "    (signal 'test-error \"message\")\n"
                                    "  (error (set! err e)))\n"
                                    "\n"
                                    "(error-data err)        ; => \"message\"\n"
                                    "\n"
                                    "; Error with complex data\n"
                                    "(condition-case e\n"
                                    "    (signal 'file-error '(\"cannot open\" \"file.txt\" 404))\n"
                                    "  (error (error-data e)))  ; => (\"cannot open\" \"file.txt\" 404)\n"
                                    "```\n"
                                    "\n"
                                    "## Notes\n"
                                    "Error data can be any Lisp value: string, number, list, etc.\n"
                                    "\n"
                                    "## Errors\n"
                                    "Returns error if argument is not an error object.\n"
                                    "\n"
                                    "## See Also\n"
                                    "- `error-message` - Get error message\n"
                                    "- `error-type` - Get error type\n"
                                    "- `signal` - Raise error with data";

static const char *doc_signal = "Raise a typed error (Emacs Lisp-style exception).\n"
                                "\n"
                                "## Parameters\n"
                                "- `error-type` - Symbol identifying the error type\n"
                                "- `data` - Optional data payload (any Lisp value)\n"
                                "\n"
                                "## Returns\n"
                                "Does not return (raises error that must be caught with `condition-case`).\n"
                                "\n"
                                "## Examples\n"
                                "```lisp\n"
                                "; Simple error\n"
                                "(signal 'my-error \"Something went wrong\")\n"
                                "; => ERROR: [my-error] Something went wrong\n"
                                "\n"
                                "; Error with structured data\n"
                                "(signal 'file-error '(\"cannot open\" \"file.txt\" 404))\n"
                                "; => ERROR: [file-error] file-error: (\"cannot open\" \"file.txt\" 404)\n"
                                "\n"
                                "; Catching errors\n"
                                "(condition-case e\n"
                                "    (signal 'division-by-zero \"Cannot divide by zero\")\n"
                                "  (division-by-zero \"Caught division error\")\n"
                                "  (error \"Caught other error\"))\n"
                                "; => \"Caught division error\"\n"
                                "```\n"
                                "\n"
                                "## Common Error Types\n"
                                "- `'error` - Generic error (catch-all)\n"
                                "- `'division-by-zero` - Division by zero\n"
                                "- `'wrong-type-argument` - Wrong type passed to function\n"
                                "- `'wrong-number-of-arguments` - Arity mismatch\n"
                                "- `'void-variable` - Undefined symbol\n"
                                "- `'file-error` - File I/O errors\n"
                                "- `'range-error` - Out of bounds access\n"
                                "\n"
                                "You can define custom error types using any symbol.\n"
                                "\n"
                                "## Notes\n"
                                "If `data` is a string, it's used as the error message. Otherwise, a message is "
                                "constructed from the error type and data.\n"
                                "\n"
                                "## See Also\n"
                                "- `error` - Raise generic error (convenience function)\n"
                                "- `condition-case` - Catch and handle errors\n"
                                "- `error-type` - Get error type from error object";

static const char *doc_error =
    "Raise a generic error with given message (convenience function).\n"
    "\n"
    "## Parameters\n"
    "- `message` - Error message string (or any value, will be converted to string)\n"
    "\n"
    "## Returns\n"
    "Does not return (raises error that must be caught with `condition-case`).\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "(error \"Something went wrong\")\n"
    "; => ERROR: [error] Something went wrong\n"
    "\n"
    "; With non-string message\n"
    "(error 404)\n"
    "; => ERROR: [error] 404\n"
    "\n"
    "; Catching errors\n"
    "(condition-case e\n"
    "    (error \"test error\")\n"
    "  (error (error-message e)))\n"
    "; => \"test error\"\n"
    "```\n"
    "\n"
    "## Notes\n"
    "This is equivalent to `(signal 'error message)`. For typed errors, use `signal` instead.\n"
    "\n"
    "## See Also\n"
    "- `signal` - Raise typed error\n"
    "- `condition-case` - Catch and handle errors\n"
    "- `error-message` - Get error message from error object";

/* Docstring introspection functions */
static const char *doc_lambda_docstring =
    "Get docstring from lambda object.\n"
    "\n"
    "## Parameters\n"
    "- `lambda` - Lambda function object\n"
    "\n"
    "## Returns\n"
    "String containing the lambda's documentation, or `nil` if no docstring exists.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "; Lambda with docstring\n"
    "(define greet\n"
    "  (lambda (name)\n"
    "    \"Greet a person by NAME.\"\n"
    "    (concat \"Hello, \" name \"!\")))\n"
    "\n"
    "(lambda-docstring greet)\n"
    "; => \"Greet a person by NAME.\"\n"
    "\n"
    "; Lambda without docstring\n"
    "(define double (lambda (x) (* x 2)))\n"
    "(lambda-docstring double)  ; => nil\n"
    "```\n"
    "\n"
    "## Notes\n"
    "Docstrings follow Emacs Lisp conventions and use CommonMark (Markdown) format.\n"
    "\n"
    "## Errors\n"
    "Returns error if argument is not a lambda.\n"
    "\n"
    "## See Also\n"
    "- `documentation` - Get docstring via symbol\n"
    "- `macro-docstring` - Get macro docstring";

static const char *doc_macro_docstring =
    "Get docstring from macro object.\n"
    "\n"
    "## Parameters\n"
    "- `macro` - Macro object\n"
    "\n"
    "## Returns\n"
    "String containing the macro's documentation, or `nil` if no docstring exists.\n"
    "\n"
    "## Examples\n"
    "```lisp\n"
    "; Macro with docstring\n"
    "(defmacro when (condition . body)\n"
    "  \"Execute BODY when CONDITION is true.\"\n"
    "  `(if ,condition (progn ,@body) nil))\n"
    "\n"
    "(macro-docstring when)\n"
    "; => \"Execute BODY when CONDITION is true.\"\n"
    "\n"
    "; Macro without docstring\n"
    "(defmacro unless (condition . body)\n"
    "  `(if ,condition nil (progn ,@body)))\n"
    "\n"
    "(macro-docstring unless)  ; => nil\n"
    "```\n"
    "\n"
    "## Notes\n"
    "Docstrings follow Emacs Lisp conventions and use CommonMark (Markdown) format.\n"
    "\n"
    "## Errors\n"
    "Returns error if argument is not a macro.\n"
    "\n"
    "## See Also\n"
    "- `documentation` - Get docstring via symbol\n"
    "- `lambda-docstring` - Get lambda docstring";

static const char *doc_documentation = "Get documentation string for function, macro, or built-in bound to symbol.\n"
                                       "\n"
                                       "## Parameters\n"
                                       "- `symbol` - Symbol name (quoted)\n"
                                       "\n"
                                       "## Returns\n"
                                       "String containing the documentation, or `nil` if:\n"
                                       "- No docstring exists\n"
                                       "- Symbol is unbound\n"
                                       "- Symbol's value is not a function, macro, or built-in\n"
                                       "\n"
                                       "## Examples\n"
                                       "```lisp\n"
                                       "; User-defined function\n"
                                       "(define calculate-area\n"
                                       "  (lambda (width height)\n"
                                       "    \"Calculate the area of a rectangle.\n"
                                       "\n"
                                       "    ## Parameters\n"
                                       "    - `width` - Width of the rectangle\n"
                                       "    - `height` - Height of the rectangle\n"
                                       "\n"
                                       "    ## Returns\n"
                                       "    The area as a number.\"\n"
                                       "    (* width height)))\n"
                                       "\n"
                                       "(documentation 'calculate-area)\n"
                                       "; => \"Calculate the area of a rectangle.\\n\\n    ## Parameters...\"\n"
                                       "\n"
                                       "; Built-in function\n"
                                       "(documentation 'car)\n"
                                       "; => \"Get the first element of a list (the car).\\n\\n## Parameters...\"\n"
                                       "\n"
                                       "; Macro\n"
                                       "(defmacro when (condition . body)\n"
                                       "  \"Execute BODY when CONDITION is true.\"\n"
                                       "  `(if ,condition (progn ,@body) nil))\n"
                                       "\n"
                                       "(documentation 'when)\n"
                                       "; => \"Execute BODY when CONDITION is true.\"\n"
                                       "\n"
                                       "; Undefined symbol\n"
                                       "(documentation 'nonexistent)  ; => ERROR: Undefined symbol\n"
                                       "\n"
                                       "; Function without docstring\n"
                                       "(define no-doc (lambda (x) (* x 2)))\n"
                                       "(documentation 'no-doc)       ; => nil\n"
                                       "```\n"
                                       "\n"
                                       "## Notes\n"
                                       "- Similar to Emacs Lisp's `documentation` function\n"
                                       "- Works with lambdas, macros, and built-in functions\n"
                                       "- Docstrings use CommonMark (Markdown) format\n"
                                       "\n"
                                       "## Errors\n"
                                       "Returns error if:\n"
                                       "- Argument is not a symbol\n"
                                       "- Symbol is undefined\n"
                                       "\n"
                                       "## See Also\n"
                                       "- `lambda-docstring` - Get docstring from lambda object\n"
                                       "- `macro-docstring` - Get docstring from macro object";

/* Printing functions */
static const char *doc_princ = "Print object in human-readable form (without quotes on strings).\n"
                               "\n"
                               "## Parameters\n"
                               "- `object` - Any object to print\n"
                               "\n"
                               "## Returns\n"
                               "The object that was printed.\n"
                               "\n"
                               "## Examples\n"
                               "```lisp\n"
                               "(princ \"Hello\")         ; Prints: Hello (no quotes)\n"
                               "(princ 42)              ; Prints: 42\n"
                               "(princ '(1 2 3))        ; Prints: (1 2 3)\n"
                               "```\n"
                               "\n"
                               "## Notes\n"
                               "Output goes to stdout. Strings print without surrounding quotes.";

static const char *doc_prin1 = "Print object in readable representation (with quotes on strings).\n"
                               "\n"
                               "## Parameters\n"
                               "- `object` - Any object to print\n"
                               "\n"
                               "## Returns\n"
                               "The object that was printed.\n"
                               "\n"
                               "## Examples\n"
                               "```lisp\n"
                               "(prin1 \"Hello\")         ; Prints: \"Hello\" (with quotes)\n"
                               "(prin1 42)              ; Prints: 42\n"
                               "(prin1 '(1 2 3))        ; Prints: (1 2 3)\n"
                               "```\n"
                               "\n"
                               "## Notes\n"
                               "Output goes to stdout. Strings print with surrounding quotes.";

static const char *doc_print = "Print object like prin1 but with newlines before and after.\n"
                               "\n"
                               "## Parameters\n"
                               "- `object` - Any object to print\n"
                               "\n"
                               "## Returns\n"
                               "The object that was printed.\n"
                               "\n"
                               "## Examples\n"
                               "```lisp\n"
                               "(print \"Hello\")         ; Prints: \\n\"Hello\"\\n\n"
                               "(print 42)              ; Prints: \\n42\\n\n"
                               "```\n"
                               "\n"
                               "## Notes\n"
                               "Output goes to stdout. Adds newline before and after output.";

static const char *doc_format = "Formatted output with directives (Common Lisp style).\n"
                                "\n"
                                "## Parameters\n"
                                "- `destination` - `nil` (return string) or `#t` (print to stdout)\n"
                                "- `format-string` - Format string with directives\n"
                                "- `args...` - Arguments to format\n"
                                "\n"
                                "## Returns\n"
                                "Formatted string if `destination` is `nil`, otherwise `nil`.\n"
                                "\n"
                                "## Format Directives\n"
                                "- `~A` or `~a` - Aesthetic (princ-style, no quotes)\n"
                                "- `~S` or `~s` - S-expression (prin1-style, with quotes)\n"
                                "- `~%` - Newline\n"
                                "- `~~` - Literal tilde (~)\n"
                                "\n"
                                "## Examples\n"
                                "```lisp\n"
                                "(format nil \"Hello, ~A!\" \"World\")     ; => \"Hello, World!\"\n"
                                "(format nil \"~A + ~A = ~A\" 2 3 5)     ; => \"2 + 3 = 5\"\n"
                                "(format nil \"String: ~S\" \"test\")     ; => \"String: \\\"test\\\"\"\n"
                                "(format nil \"Line 1~%Line 2\")         ; => \"Line 1\\nLine 2\"\n"
                                "(format #t \"Hello!~%\" )               ; Prints: Hello!\\n\n"
                                "                                       ; => nil\n"
                                "```";

static const char *doc_terpri = "Print newline (terminate print).\n"
                                "\n"
                                "## Returns\n"
                                "`nil`\n"
                                "\n"
                                "## Examples\n"
                                "```lisp\n"
                                "(princ \"Hello\")\n"
                                "(terpri)                ; Prints newline\n"
                                "(princ \"World\")\n"
                                "```\n"
                                "\n"
                                "## Notes\n"
                                "Useful for adding newlines between output.";

void register_builtins(Environment *env) {
    env_define(env, "+", lisp_make_builtin(builtin_add, "+", doc_add));
    env_define(env, "-", lisp_make_builtin(builtin_subtract, "-", doc_subtract));
    env_define(env, "*", lisp_make_builtin(builtin_multiply, "*", doc_multiply));
    env_define(env, "/", lisp_make_builtin(builtin_divide, "/", doc_divide));

    env_define(env, ">", lisp_make_builtin(builtin_gt, ">", doc_gt));
    env_define(env, "<", lisp_make_builtin(builtin_lt, "<", doc_lt));
    env_define(env, "=", lisp_make_builtin(builtin_eq, "=", doc_eq));
    env_define(env, ">=", lisp_make_builtin(builtin_gte, ">=", doc_gte));
    env_define(env, "<=", lisp_make_builtin(builtin_lte, "<=", doc_lte));

    env_define(env, "concat", lisp_make_builtin(builtin_concat, "concat", doc_concat));
    env_define(env, "number->string",
               lisp_make_builtin(builtin_number_to_string, "number->string", doc_number_to_string));
    env_define(env, "string->number",
               lisp_make_builtin(builtin_string_to_number, "string->number", doc_string_to_number));
    env_define(env, "split", lisp_make_builtin(builtin_split, "split", doc_split));
    env_define(env, "string<?", lisp_make_builtin(builtin_string_lt, "string<?", doc_string_lt));
    env_define(env, "string>?", lisp_make_builtin(builtin_string_gt, "string>?", doc_string_gt));
    env_define(env, "string<=?", lisp_make_builtin(builtin_string_lte, "string<=?", doc_string_lte));
    env_define(env, "string>=?", lisp_make_builtin(builtin_string_gte, "string>=?", doc_string_gte));
    env_define(env, "string-contains?",
               lisp_make_builtin(builtin_string_contains, "string-contains?", doc_string_contains));
    env_define(env, "string-index", lisp_make_builtin(builtin_string_index, "string-index", doc_string_index));
    env_define(env, "string-match?", lisp_make_builtin(builtin_string_match, "string-match?", doc_string_match));
    env_define(env, "string-length", lisp_make_builtin(builtin_string_length, "string-length", doc_string_length));
    env_define(env, "substring", lisp_make_builtin(builtin_substring, "substring", doc_substring));
    env_define(env, "string-ref", lisp_make_builtin(builtin_string_ref, "string-ref", doc_string_ref));
    env_define(env, "string-prefix?",
               lisp_make_builtin(builtin_string_prefix_question, "string-prefix?", doc_string_prefix));
    env_define(env, "string-replace", lisp_make_builtin(builtin_string_replace, "string-replace", doc_string_replace));
    env_define(env, "string-upcase", lisp_make_builtin(builtin_string_upcase, "string-upcase", doc_string_upcase));
    env_define(env, "string-downcase",
               lisp_make_builtin(builtin_string_downcase, "string-downcase", doc_string_downcase));

    env_define(env, "not", lisp_make_builtin(builtin_not, "not", doc_not));

    env_define(env, "car", lisp_make_builtin(builtin_car, "car", doc_car));
    env_define(env, "cdr", lisp_make_builtin(builtin_cdr, "cdr", doc_cdr));
    env_define(env, "cons", lisp_make_builtin(builtin_cons, "cons", doc_cons));
    env_define(env, "list", lisp_make_builtin(builtin_list, "list", doc_list));
    env_define(env, "list-length", lisp_make_builtin(builtin_list_length, "list-length", doc_list_length));
    env_define(env, "list-ref", lisp_make_builtin(builtin_list_ref, "list-ref", doc_list_ref));
    env_define(env, "reverse", lisp_make_builtin(builtin_reverse, "reverse", doc_reverse));
    env_define(env, "append", lisp_make_builtin(builtin_append, "append", doc_append));

    /* Alist operations */
    env_define(env, "assoc", lisp_make_builtin(builtin_assoc, "assoc", doc_assoc));
    env_define(env, "assq", lisp_make_builtin(builtin_assq, "assq", doc_assq));
    env_define(env, "assv", lisp_make_builtin(builtin_assv, "assv", doc_assv));
    env_define(env, "alist-get", lisp_make_builtin(builtin_alist_get, "alist-get", doc_alist_get));

    /* Equality predicates */
    env_define(env, "eq?", lisp_make_builtin(builtin_eq_predicate, "eq?", doc_eq_predicate));
    env_define(env, "equal?", lisp_make_builtin(builtin_equal_predicate, "equal?", doc_equal_predicate));
    env_define(env, "string=?", lisp_make_builtin(builtin_string_eq_predicate, "string=?", doc_string_eq_predicate));

    /* Mapping operations */
    env_define(env, "map", lisp_make_builtin(builtin_map, "map", doc_map));
    env_define(env, "mapcar", lisp_make_builtin(builtin_mapcar, "mapcar", doc_map));

    env_define(env, "null?", lisp_make_builtin(builtin_null_question, "null?", doc_null_question));
    env_define(env, "atom?", lisp_make_builtin(builtin_atom_question, "atom?", doc_atom_question));

    env_define(env, "regex-match", lisp_make_builtin(builtin_regex_match, "regex-match", doc_regex_match));
    env_define(env, "regex-find", lisp_make_builtin(builtin_regex_find, "regex-find", doc_regex_find));
    env_define(env, "regex-find-all", lisp_make_builtin(builtin_regex_find_all, "regex-find-all", doc_regex_find_all));
    env_define(env, "regex-extract", lisp_make_builtin(builtin_regex_extract, "regex-extract", doc_regex_extract));
    env_define(env, "regex-replace", lisp_make_builtin(builtin_regex_replace, "regex-replace", doc_regex_replace));
    env_define(env, "regex-replace-all",
               lisp_make_builtin(builtin_regex_replace_all, "regex-replace-all", doc_regex_replace));
    env_define(env, "regex-split", lisp_make_builtin(builtin_regex_split, "regex-split", doc_regex_split));
    env_define(env, "regex-escape", lisp_make_builtin(builtin_regex_escape, "regex-escape", doc_regex_escape));
    env_define(env, "regex-valid?", lisp_make_builtin(builtin_regex_valid, "regex-valid?", doc_regex_valid));

    /* File I/O functions */
    env_define(env, "open", lisp_make_builtin(builtin_open, "open", doc_open));
    env_define(env, "close", lisp_make_builtin(builtin_close, "close", doc_close));
    env_define(env, "read-line", lisp_make_builtin(builtin_read_line, "read-line", doc_read_line));
    env_define(env, "write-line", lisp_make_builtin(builtin_write_line, "write-line", doc_write_line));
    env_define(env, "read-sexp", lisp_make_builtin(builtin_read_sexp, "read-sexp", doc_read_sexp));
    env_define(env, "read-json", lisp_make_builtin(builtin_read_json, "read-json", doc_read_json));
    env_define(env, "delete-file", lisp_make_builtin(builtin_delete_file, "delete-file", doc_delete_file));
    env_define(env, "load", lisp_make_builtin(builtin_load, "load", doc_load));

    /* Path expansion functions */
    env_define(env, "home-directory", lisp_make_builtin(builtin_home_directory, "home-directory", doc_home_directory));
    env_define(env, "expand-path", lisp_make_builtin(builtin_expand_path, "expand-path", doc_expand_path));

    /* Common Lisp printing functions */
    env_define(env, "princ", lisp_make_builtin(builtin_princ, "princ", doc_princ));
    env_define(env, "prin1", lisp_make_builtin(builtin_prin1, "prin1", doc_prin1));
    env_define(env, "print", lisp_make_builtin(builtin_print_cl, "print", doc_print));
    env_define(env, "format", lisp_make_builtin(builtin_format, "format", doc_format));
    env_define(env, "terpri", lisp_make_builtin(builtin_terpri, "terpri", doc_terpri));

    /* Type predicates */
    env_define(env, "integer?", lisp_make_builtin(builtin_integer_question, "integer?", doc_integer_question));
    env_define(env, "boolean?", lisp_make_builtin(builtin_boolean_question, "boolean?", doc_boolean_question));
    env_define(env, "number?", lisp_make_builtin(builtin_number_question, "number?", doc_number_question));
    env_define(env, "vector?", lisp_make_builtin(builtin_vector_question, "vector?", doc_vector_question));
    env_define(env, "hash-table?",
               lisp_make_builtin(builtin_hash_table_question, "hash-table?", doc_hash_table_question));
    env_define(env, "string?", lisp_make_builtin(builtin_string_question, "string?", doc_string_question));
    env_define(env, "symbol?", lisp_make_builtin(builtin_symbol_question, "symbol?", doc_symbol_question));
    env_define(env, "list?", lisp_make_builtin(builtin_list_question, "list?", doc_list_question));

    /* Symbol operations */
    env_define(env, "symbol->string",
               lisp_make_builtin(builtin_symbol_to_string, "symbol->string", doc_symbol_to_string));

    /* Vector operations */
    env_define(env, "make-vector", lisp_make_builtin(builtin_make_vector, "make-vector", doc_make_vector));
    env_define(env, "vector-ref", lisp_make_builtin(builtin_vector_ref, "vector-ref", doc_vector_ref));
    env_define(env, "vector-set!", lisp_make_builtin(builtin_vector_set_bang, "vector-set!", doc_vector_set_bang));
    env_define(env, "vector-length", lisp_make_builtin(builtin_vector_length, "vector-length", doc_vector_length));
    env_define(env, "vector-push!", lisp_make_builtin(builtin_vector_push_bang, "vector-push!", doc_vector_push_bang));
    env_define(env, "vector-pop!", lisp_make_builtin(builtin_vector_pop_bang, "vector-pop!", doc_vector_pop_bang));

    /* Integer operations */
    env_define(env, "quotient", lisp_make_builtin(builtin_quotient, "quotient", doc_quotient));
    env_define(env, "remainder", lisp_make_builtin(builtin_remainder, "remainder", doc_remainder));
    env_define(env, "even?", lisp_make_builtin(builtin_even_question, "even?", doc_even_question));
    env_define(env, "odd?", lisp_make_builtin(builtin_odd_question, "odd?", doc_odd_question));

    /* Hash table operations */
    env_define(env, "make-hash-table",
               lisp_make_builtin(builtin_make_hash_table, "make-hash-table", doc_make_hash_table));
    env_define(env, "hash-ref", lisp_make_builtin(builtin_hash_ref, "hash-ref", doc_hash_ref));
    env_define(env, "hash-set!", lisp_make_builtin(builtin_hash_set_bang, "hash-set!", doc_hash_set_bang));
    env_define(env, "hash-remove!", lisp_make_builtin(builtin_hash_remove_bang, "hash-remove!", doc_hash_remove_bang));
    env_define(env, "hash-clear!", lisp_make_builtin(builtin_hash_clear_bang, "hash-clear!", doc_hash_clear_bang));
    env_define(env, "hash-count", lisp_make_builtin(builtin_hash_count, "hash-count", doc_hash_count));
    env_define(env, "hash-keys", lisp_make_builtin(builtin_hash_keys, "hash-keys", doc_hash_keys));
    env_define(env, "hash-values", lisp_make_builtin(builtin_hash_values, "hash-values", doc_hash_values));
    env_define(env, "hash-entries", lisp_make_builtin(builtin_hash_entries, "hash-entries", doc_hash_entries));

    /* Error introspection and handling */
    env_define(env, "error?", lisp_make_builtin(builtin_error_question, "error?", doc_error_question));
    env_define(env, "error-type", lisp_make_builtin(builtin_error_type, "error-type", doc_error_type));
    env_define(env, "error-message", lisp_make_builtin(builtin_error_message, "error-message", doc_error_message));
    env_define(env, "error-stack", lisp_make_builtin(builtin_error_stack, "error-stack", doc_error_stack));
    env_define(env, "error-data", lisp_make_builtin(builtin_error_data, "error-data", doc_error_data));
    env_define(env, "signal", lisp_make_builtin(builtin_signal, "signal", doc_signal));
    env_define(env, "error", lisp_make_builtin(builtin_error, "error", doc_error));

    /* Docstring introspection functions */
    env_define(env, "lambda-docstring",
               lisp_make_builtin(builtin_lambda_docstring, "lambda-docstring", doc_lambda_docstring));
    env_define(env, "macro-docstring",
               lisp_make_builtin(builtin_macro_docstring, "macro-docstring", doc_macro_docstring));
    env_define(env, "documentation", lisp_make_builtin(builtin_documentation, "documentation", doc_documentation));
}

/* Helper function to get numeric value */
static double get_numeric_value(LispObject *obj, int *is_integer) {
    if (obj->type == LISP_INTEGER) {
        *is_integer = 1;
        return (double)obj->value.integer;
    } else if (obj->type == LISP_NUMBER) {
        *is_integer = 0;
        return obj->value.number;
    }
    return 0.0;
}

/* Arithmetic operations */
static LispObject *builtin_add(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_integer(0); /* Return 0 for (+), not 0.0 */
    }

    /* Check if all integers */
    int all_integers = 1;
    double sum = 0;
    int first_is_integer = 0;
    double first_val = 0;

    LispObject *first = lisp_car(args);
    first_val = get_numeric_value(first, &first_is_integer);
    all_integers = first_is_integer;
    sum = first_val;
    args = lisp_cdr(args);

    while (args != NIL && args != NULL) {
        LispObject *arg = lisp_car(args);
        int arg_is_integer = 0;
        double val = get_numeric_value(arg, &arg_is_integer);
        if (arg_is_integer == 0 && arg->type != LISP_NUMBER && arg->type != LISP_INTEGER) {
            return lisp_make_error("+ requires numbers");
        }
        if (!arg_is_integer) {
            all_integers = 0;
        }
        sum += val;
        args = lisp_cdr(args);
    }

    if (all_integers) {
        return lisp_make_integer((long long)sum);
    } else {
        return lisp_make_number(sum);
    }
}

static LispObject *builtin_subtract(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("- requires at least one argument");
    }

    LispObject *first = lisp_car(args);
    int first_is_integer = 0;
    double result = get_numeric_value(first, &first_is_integer);
    int all_integers = first_is_integer;

    args = lisp_cdr(args);

    if (args == NIL) {
        /* Unary negation */
        if (first_is_integer) {
            return lisp_make_integer((long long)-result);
        } else {
            return lisp_make_number(-result);
        }
    }

    while (args != NIL && args != NULL) {
        LispObject *arg = lisp_car(args);
        int arg_is_integer = 0;
        double val = get_numeric_value(arg, &arg_is_integer);
        if (arg_is_integer == 0 && arg->type != LISP_NUMBER && arg->type != LISP_INTEGER) {
            return lisp_make_error("- requires numbers");
        }
        if (!arg_is_integer) {
            all_integers = 0;
        }
        result -= val;
        args = lisp_cdr(args);
    }

    if (all_integers) {
        return lisp_make_integer((long long)result);
    } else {
        return lisp_make_number(result);
    }
}

static LispObject *builtin_multiply(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_integer(1); /* Return 1 for (*), not 1.0 */
    }

    int all_integers = 1;
    double product = 1;

    while (args != NIL && args != NULL) {
        LispObject *arg = lisp_car(args);
        int arg_is_integer = 0;
        double val = get_numeric_value(arg, &arg_is_integer);
        if (arg_is_integer == 0 && arg->type != LISP_NUMBER && arg->type != LISP_INTEGER) {
            return lisp_make_error("* requires numbers");
        }
        if (!arg_is_integer) {
            all_integers = 0;
        }
        product *= val;
        args = lisp_cdr(args);
    }

    if (all_integers) {
        return lisp_make_integer((long long)product);
    } else {
        return lisp_make_number(product);
    }
}

static LispObject *builtin_divide(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("/ requires at least one argument");
    }

    LispObject *first = lisp_car(args);
    int first_is_integer;
    double result = get_numeric_value(first, &first_is_integer);
    args = lisp_cdr(args);

    if (args == NIL) {
        /* Unary reciprocal */
        if (result == 0) {
            return lisp_make_error("Division by zero");
        }
        return lisp_make_number(1.0 / result); /* Always return float */
    }

    /* Division always returns float */
    while (args != NIL && args != NULL) {
        LispObject *arg = lisp_car(args);
        int arg_is_integer = 0;
        double val = get_numeric_value(arg, &arg_is_integer);
        if (arg_is_integer == 0 && arg->type != LISP_NUMBER && arg->type != LISP_INTEGER) {
            return lisp_make_error("/ requires numbers");
        }
        if (val == 0) {
            return lisp_make_error("Division by zero");
        }
        result /= val;
        args = lisp_cdr(args);
    }

    return lisp_make_number(result); /* Always return float */
}

static LispObject *builtin_quotient(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("quotient requires 2 arguments");
    }

    LispObject *first = lisp_car(args);
    LispObject *second = lisp_car(lisp_cdr(args));

    if (first->type != LISP_INTEGER && first->type != LISP_NUMBER) {
        return lisp_make_error("quotient requires numbers");
    }
    if (second->type != LISP_INTEGER && second->type != LISP_NUMBER) {
        return lisp_make_error("quotient requires numbers");
    }

    int first_is_integer;
    int second_is_integer;
    double first_val = get_numeric_value(first, &first_is_integer);
    double second_val = get_numeric_value(second, &second_is_integer);

    if (second_val == 0) {
        return lisp_make_error("Division by zero");
    }

    /* Truncate to integer */
    long long result = (long long)(first_val / second_val);
    return lisp_make_integer(result);
}

static LispObject *builtin_remainder(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("remainder requires 2 arguments");
    }

    LispObject *first = lisp_car(args);
    LispObject *second = lisp_car(lisp_cdr(args));

    if (first->type != LISP_INTEGER && first->type != LISP_NUMBER) {
        return lisp_make_error("remainder requires numbers");
    }
    if (second->type != LISP_INTEGER && second->type != LISP_NUMBER) {
        return lisp_make_error("remainder requires numbers");
    }

    int first_is_integer;
    int second_is_integer;
    double first_val = get_numeric_value(first, &first_is_integer);
    double second_val = get_numeric_value(second, &second_is_integer);

    if (second_val == 0) {
        return lisp_make_error("Division by zero");
    }

    long long result = (long long)first_val % (long long)second_val;
    return lisp_make_integer(result);
}

static LispObject *builtin_even_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("even? requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    int arg_is_integer;
    double arg_val = get_numeric_value(arg, &arg_is_integer);

    if (arg->type != LISP_INTEGER && arg->type != LISP_NUMBER) {
        return lisp_make_error("even? requires a number");
    }

    long long val = (long long)arg_val;
    if ((val & 1) == 0) {
        return lisp_make_boolean(1);
    }
    return lisp_make_boolean(0);
}

static LispObject *builtin_odd_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("odd? requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    int arg_is_integer;
    double arg_val = get_numeric_value(arg, &arg_is_integer);

    if (arg->type != LISP_INTEGER && arg->type != LISP_NUMBER) {
        return lisp_make_error("odd? requires a number");
    }

    long long val = (long long)arg_val;
    if ((val & 1) == 1) {
        return lisp_make_boolean(1);
    }
    return lisp_make_boolean(0);
}

/* Number comparisons */
static LispObject *builtin_gt(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("> requires at least 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    int a_is_integer = 0;
    int b_is_integer = 0;
    double a_val = get_numeric_value(a, &a_is_integer);
    double b_val = get_numeric_value(b, &b_is_integer);

    if ((!a_is_integer && a->type != LISP_NUMBER && a->type != LISP_INTEGER) ||
        (!b_is_integer && b->type != LISP_NUMBER && b->type != LISP_INTEGER)) {
        return lisp_make_error("> requires numbers");
    }

    return (a_val > b_val) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_lt(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("< requires at least 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    int a_is_integer = 0;
    int b_is_integer = 0;
    double a_val = get_numeric_value(a, &a_is_integer);
    double b_val = get_numeric_value(b, &b_is_integer);

    if ((!a_is_integer && a->type != LISP_NUMBER && a->type != LISP_INTEGER) ||
        (!b_is_integer && b->type != LISP_NUMBER && b->type != LISP_INTEGER)) {
        return lisp_make_error("< requires numbers");
    }

    return (a_val < b_val) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_eq(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("= requires at least 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    int a_is_integer = 0;
    int b_is_integer = 0;
    double a_val = get_numeric_value(a, &a_is_integer);
    double b_val = get_numeric_value(b, &b_is_integer);

    if ((!a_is_integer && a->type != LISP_NUMBER && a->type != LISP_INTEGER) ||
        (!b_is_integer && b->type != LISP_NUMBER && b->type != LISP_INTEGER)) {
        return lisp_make_error("= requires numbers");
    }

    return (a_val == b_val) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_gte(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error(">= requires at least 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    int a_is_integer = 0;
    int b_is_integer = 0;
    double a_val = get_numeric_value(a, &a_is_integer);
    double b_val = get_numeric_value(b, &b_is_integer);

    if ((!a_is_integer && a->type != LISP_NUMBER && a->type != LISP_INTEGER) ||
        (!b_is_integer && b->type != LISP_NUMBER && b->type != LISP_INTEGER)) {
        return lisp_make_error(">= requires numbers");
    }

    return (a_val >= b_val) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_lte(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("<= requires at least 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    int a_is_integer = 0;
    int b_is_integer = 0;
    double a_val = get_numeric_value(a, &a_is_integer);
    double b_val = get_numeric_value(b, &b_is_integer);

    if ((!a_is_integer && a->type != LISP_NUMBER && a->type != LISP_INTEGER) ||
        (!b_is_integer && b->type != LISP_NUMBER && b->type != LISP_INTEGER)) {
        return lisp_make_error("<= requires numbers");
    }

    return (a_val <= b_val) ? lisp_make_number(1) : NIL;
}

/* String operations */
static LispObject *builtin_concat(LispObject *args, Environment *env) {
    (void)env;
    size_t total_len = 0;

    /* Calculate total length */
    LispObject *curr = args;
    while (curr != NIL && curr != NULL) {
        LispObject *arg = lisp_car(curr);
        if (arg->type != LISP_STRING) {
            return lisp_make_error("concat requires strings");
        }
        total_len += strlen(arg->value.string);
        curr = lisp_cdr(curr);
    }

    /* Concatenate */
    char *result = GC_malloc(total_len + 1);
    result[0] = '\0';

    curr = args;
    while (curr != NIL && curr != NULL) {
        LispObject *arg = lisp_car(curr);
        strcat(result, arg->value.string);
        curr = lisp_cdr(curr);
    }

    LispObject *obj = lisp_make_string(result);
    return obj;
}

static LispObject *builtin_number_to_string(LispObject *args, Environment *env) {
    (void)env;
    if (args == NULL || args == NIL) {
        return lisp_make_error("number->string: expected at least 1 argument");
    }

    /* Parse arguments: number (required), radix (optional) */
    LispObject *num = lisp_car(args);
    LispObject *radix_obj = (lisp_cdr(args) != NIL) ? lisp_car(lisp_cdr(args)) : NIL;

    if (num == NULL || num == NIL) {
        return lisp_make_error("number->string: first argument cannot be nil");
    }

    /* Validate number argument */
    if (num->type != LISP_INTEGER && num->type != LISP_NUMBER) {
        return lisp_make_error("number->string: first argument must be a number");
    }

    int radix = 10; /* default base */

    /* Parse optional radix */
    if (radix_obj != NIL) {
        if (radix_obj->type != LISP_INTEGER) {
            return lisp_make_error("number->string: radix must be an integer");
        }
        radix = (int)radix_obj->value.integer;
        if (radix < 2 || radix > 36) {
            return lisp_make_error("number->string: radix must be between 2 and 36");
        }
    }

    /* Float formatting (only base 10) */
    if (num->type == LISP_NUMBER) {
        if (radix != 10) {
            return lisp_make_error("number->string: floats only supported in base 10");
        }
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "%.15g", num->value.number);
        return lisp_make_string(buffer);
    }

    /* Integer formatting with arbitrary radix */
    long long value = num->value.integer;

    /* Special case: zero */
    if (value == 0) {
        return lisp_make_string("0");
    }

    char buffer[128];
    int pos = 0;
    int negative = (value < 0);

    /* Handle negative numbers */
    if (negative) {
        value = -value;
    }

    /* Convert to given radix */
    const char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
    char temp[128];
    int temp_pos = 0;

    while (value > 0) {
        temp[temp_pos++] = digits[value % radix];
        value /= radix;
    }

    /* Add sign */
    if (negative) {
        buffer[pos++] = '-';
    }

    /* Reverse digits */
    while (temp_pos > 0) {
        buffer[pos++] = temp[--temp_pos];
    }

    buffer[pos] = '\0';
    return lisp_make_string(buffer);
}

static LispObject *builtin_string_to_number(LispObject *args, Environment *env) {
    (void)env;

    if (args == NULL || args == NIL) {
        return lisp_make_error("string->number: expected at least 1 argument");
    }

    /* Parse arguments: string (required), radix (optional) */
    LispObject *str_obj = lisp_car(args);
    LispObject *radix_obj = (lisp_cdr(args) != NIL) ? lisp_car(lisp_cdr(args)) : NIL;

    /* Validate string argument */
    if (str_obj == NULL || str_obj == NIL) {
        return lisp_make_error("string->number: first argument cannot be nil");
    }
    if (str_obj->type != LISP_STRING) {
        return lisp_make_error("string->number: first argument must be a string");
    }

    const char *str = str_obj->value.string;
    int radix = 10; /* default base */

    /* Parse optional radix */
    if (radix_obj != NIL) {
        if (radix_obj->type != LISP_INTEGER) {
            return lisp_make_error("string->number: radix must be an integer");
        }
        radix = (int)radix_obj->value.integer;
        if (radix < 2 || radix > 36) {
            return lisp_make_error("string->number: radix must be between 2 and 36");
        }
    }

    /* Skip leading whitespace */
    while (*str && isspace((unsigned char)*str)) {
        str++;
    }

    /* Empty string returns #f */
    if (*str == '\0') {
        return NIL;
    }

    /* Handle radix prefix (only if radix not explicitly specified) */
    if (radix == 10 && str[0] == '#' && str[1]) {
        char prefix = str[1];
        if (prefix == 'b' || prefix == 'B') {
            radix = 2;
            str += 2;
        } else if (prefix == 'o' || prefix == 'O') {
            radix = 8;
            str += 2;
        } else if (prefix == 'd' || prefix == 'D') {
            radix = 10;
            str += 2;
        } else if (prefix == 'x' || prefix == 'X') {
            radix = 16;
            str += 2;
        }
    }

    /* Try parsing as float (only for base 10) */
    if (radix == 10 && (strchr(str, '.') != NULL || strchr(str, 'e') != NULL || strchr(str, 'E') != NULL)) {
        char *endptr;
        errno = 0;
        double value = strtod(str, &endptr);

        /* Skip trailing whitespace */
        while (*endptr && isspace((unsigned char)*endptr)) {
            endptr++;
        }

        /* Return float if parse succeeded, else #f */
        if (*endptr == '\0' && errno != ERANGE) {
            return lisp_make_number(value);
        }
        return NIL;
    }

    /* Try parsing as integer */
    char *endptr;
    errno = 0;
    long long value = strtoll(str, &endptr, radix);

    /* Skip trailing whitespace */
    while (*endptr && isspace((unsigned char)*endptr)) {
        endptr++;
    }

    /* Return integer if parse succeeded, else #f */
    if (*endptr != '\0' || errno == ERANGE) {
        return NIL;
    }

    return lisp_make_integer(value);
}

static int match_char_class(const char **pattern, char c) {
    const char *p = *pattern + 1; /* Skip '[' */
    int negate = 0;
    int match = 0;

    if (*p == '!') {
        negate = 1;
        p++;
    }

    while (*p && *p != ']') {
        if (*(p + 1) == '-' && *(p + 2) != ']' && *(p + 2) != '\0') {
            /* Range */
            if (c >= *p && c <= *(p + 2)) {
                match = 1;
            }
            p += 3;
        } else {
            /* Single character */
            if (c == *p) {
                match = 1;
            }
            p++;
        }
    }

    if (*p == ']') {
        *pattern = p + 1;
    }

    return negate ? !match : match;
}

static int wildcard_match(const char *pattern, const char *str) {
    while (*pattern && *str) {
        if (*pattern == '*') {
            pattern++;
            if (*pattern == '\0')
                return 1;
            while (*str) {
                if (wildcard_match(pattern, str))
                    return 1;
                str++;
            }
            return 0;
        } else if (*pattern == '?') {
            pattern++;
            str++;
        } else if (*pattern == '[') {
            if (match_char_class(&pattern, *str)) {
                str++;
            } else {
                return 0;
            }
        } else if (*pattern == *str) {
            pattern++;
            str++;
        } else {
            return 0;
        }
    }

    while (*pattern == '*')
        pattern++;
    return (*pattern == '\0' && *str == '\0');
}

static LispObject *builtin_split(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("split requires 2 arguments");
    }

    LispObject *str_obj = lisp_car(args);
    LispObject *pattern_obj = lisp_car(lisp_cdr(args));

    if (str_obj->type != LISP_STRING || pattern_obj->type != LISP_STRING) {
        return lisp_make_error("split requires strings");
    }

    const char *str = str_obj->value.string;
    const char *pattern = pattern_obj->value.string;
    size_t pattern_len = strlen(pattern);

    /* Handle empty pattern */
    if (pattern_len == 0) {
        return lisp_make_cons(lisp_make_string(str), NIL);
    }

    LispObject *result = NIL;
    LispObject *tail = NULL;

    const char *start = str;
    const char *p = str;

    /* Check if pattern contains wildcards */
    int has_wildcards = (strchr(pattern, '*') != NULL || strchr(pattern, '?') != NULL);

    while (*p) {
        int match = 0;

        if (has_wildcards) {
            match = wildcard_match(pattern, p);
        } else {
            /* Literal string match */
            match = (strncmp(p, pattern, pattern_len) == 0);
        }

        if (match) {
            /* Found match */
            size_t len = p - start;
            char *token = GC_malloc(len + 1);
            strncpy(token, start, len);
            token[len] = '\0';

            LispObject *token_obj = lisp_make_string(token);

            LispObject *new_cons = lisp_make_cons(token_obj, NIL);
            if (result == NIL) {
                result = new_cons;
                tail = new_cons;
            } else {
                tail->value.cons.cdr = new_cons;
                tail = new_cons;
            }

            /* Skip pattern */
            p += pattern_len;
            start = p;
        } else {
            p++;
        }
    }

    /* Add remaining */
    if (*start || result != NIL) {
        LispObject *token_obj = lisp_make_string(start);
        LispObject *new_cons = lisp_make_cons(token_obj, NIL);
        if (result == NIL) {
            result = new_cons;
        } else {
            tail->value.cons.cdr = new_cons;
        }
    }

    return result;
}

static LispObject *builtin_string_lt(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string<? requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string<? requires strings");
    }

    return (strcmp(a->value.string, b->value.string) < 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_gt(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string>? requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string>? requires strings");
    }

    return (strcmp(a->value.string, b->value.string) > 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_lte(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string<=? requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string<=? requires strings");
    }

    return (strcmp(a->value.string, b->value.string) <= 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_gte(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string>=? requires 2 arguments");
    }

    LispObject *a = lisp_car(args);
    LispObject *b = lisp_car(lisp_cdr(args));

    if (a->type != LISP_STRING || b->type != LISP_STRING) {
        return lisp_make_error("string>=? requires strings");
    }

    return (strcmp(a->value.string, b->value.string) >= 0) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_contains(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-contains? requires 2 arguments");
    }

    LispObject *haystack = lisp_car(args);
    LispObject *needle = lisp_car(lisp_cdr(args));

    if (haystack->type != LISP_STRING || needle->type != LISP_STRING) {
        return lisp_make_error("string-contains? requires strings");
    }

    return (strstr(haystack->value.string, needle->value.string) != NULL) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_index(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-index requires 2 arguments");
    }

    LispObject *haystack = lisp_car(args);
    LispObject *needle = lisp_car(lisp_cdr(args));

    if (haystack->type != LISP_STRING || needle->type != LISP_STRING) {
        return lisp_make_error("string-index requires strings");
    }

    /* Find byte offset where needle occurs in haystack */
    char *found = strstr(haystack->value.string, needle->value.string);
    if (found == NULL) {
        return NIL;
    }

    /* Count UTF-8 characters from start to found position */
    int char_index = 0;
    const char *ptr = haystack->value.string;
    while (ptr < found) {
        ptr = utf8_next_char(ptr);
        if (ptr == NULL) {
            break;
        }
        char_index++;
    }

    return lisp_make_integer(char_index);
}

static LispObject *builtin_string_match(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-match? requires 2 arguments");
    }

    LispObject *str = lisp_car(args);
    LispObject *pattern = lisp_car(lisp_cdr(args));

    if (str->type != LISP_STRING || pattern->type != LISP_STRING) {
        return lisp_make_error("string-match? requires strings");
    }

    return wildcard_match(pattern->value.string, str->value.string) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_prefix_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-prefix? requires 2 arguments");
    }

    LispObject *prefix = lisp_car(args);
    LispObject *str = lisp_car(lisp_cdr(args));

    if (prefix->type != LISP_STRING || str->type != LISP_STRING) {
        return lisp_make_error("string-prefix? requires strings");
    }

    size_t prefix_len = strlen(prefix->value.string);
    size_t str_len = strlen(str->value.string);

    /* If prefix is longer than string, it can't be a prefix */
    if (prefix_len > str_len) {
        return NIL;
    }

    /* Use strncmp to check if prefix matches the beginning of str */
    return (strncmp(prefix->value.string, str->value.string, prefix_len) == 0) ? lisp_make_number(1) : NIL;
}

/* UTF-8 String operations */
static LispObject *builtin_string_length(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("string-length requires 1 argument");
    }

    LispObject *str_obj = lisp_car(args);
    if (str_obj->type != LISP_STRING) {
        return lisp_make_error("string-length requires a string");
    }

    size_t char_count = utf8_strlen(str_obj->value.string);
    return lisp_make_integer((long long)char_count);
}

static LispObject *builtin_substring(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("substring requires at least 2 arguments");
    }

    LispObject *str_obj = lisp_car(args);
    LispObject *start_obj = lisp_car(lisp_cdr(args));
    LispObject *end_obj = lisp_cdr(lisp_cdr(args)) != NIL ? lisp_car(lisp_cdr(lisp_cdr(args))) : NIL;

    if (str_obj->type != LISP_STRING) {
        return lisp_make_error("substring requires a string");
    }
    if (start_obj->type != LISP_INTEGER) {
        return lisp_make_error("substring requires integer start index");
    }

    long long start = start_obj->value.integer;
    long long end;

    if (end_obj == NIL || end_obj == NULL) {
        /* No end specified, use string length */
        end = utf8_strlen(str_obj->value.string);
    } else {
        if (end_obj->type != LISP_INTEGER) {
            return lisp_make_error("substring requires integer end index");
        }
        end = end_obj->value.integer;
    }

    if (start < 0 || end < 0 || start > end) {
        return lisp_make_error("substring: invalid start/end indices");
    }

    size_t start_offset = utf8_byte_offset(str_obj->value.string, start);
    size_t end_offset = utf8_byte_offset(str_obj->value.string, end);

    size_t result_len = end_offset - start_offset;
    char *result = GC_malloc(result_len + 1);
    memcpy(result, str_obj->value.string + start_offset, result_len);
    result[result_len] = '\0';

    return lisp_make_string(result);
}

static LispObject *builtin_string_ref(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("string-ref requires 2 arguments");
    }

    LispObject *str_obj = lisp_car(args);
    LispObject *index_obj = lisp_car(lisp_cdr(args));

    if (str_obj->type != LISP_STRING) {
        return lisp_make_error("string-ref requires a string");
    }
    if (index_obj->type != LISP_INTEGER) {
        return lisp_make_error("string-ref requires an integer index");
    }

    long long index = index_obj->value.integer;
    if (index < 0) {
        return lisp_make_error("string-ref: negative index");
    }

    size_t char_count = utf8_strlen(str_obj->value.string);
    if (index >= (long long)char_count) {
        return lisp_make_error("string-ref: index out of bounds");
    }

    const char *char_ptr = utf8_char_at(str_obj->value.string, index);
    if (char_ptr == NULL) {
        return lisp_make_error("string-ref: invalid character at index");
    }

    int bytes = utf8_char_bytes(char_ptr);
    char *result = GC_malloc(bytes + 1);
    memcpy(result, char_ptr, bytes);
    result[bytes] = '\0';

    return lisp_make_string(result);
}

/* String replace */
static LispObject *builtin_string_replace(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL || lisp_cdr(lisp_cdr(args)) == NIL) {
        return lisp_make_error("string-replace requires 3 arguments");
    }

    LispObject *str_obj = lisp_car(args);
    LispObject *old_obj = lisp_car(lisp_cdr(args));
    LispObject *new_obj = lisp_car(lisp_cdr(lisp_cdr(args)));

    if (old_obj->type != LISP_STRING || new_obj->type != LISP_STRING || str_obj->type != LISP_STRING) {
        return lisp_make_error("string-replace requires strings");
    }

    const char *old_str = old_obj->value.string;
    const char *new_str = new_obj->value.string;
    const char *str = str_obj->value.string;

    /* If old string is empty, return original string */
    if (old_str[0] == '\0') {
        return lisp_make_string(GC_strdup(str));
    }

    /* Count occurrences to estimate result size */
    size_t old_len = strlen(old_str);
    size_t new_len = strlen(new_str);
    size_t str_len = strlen(str);
    size_t count = 0;
    const char *pos = str;
    while ((pos = strstr(pos, old_str)) != NULL) {
        count++;
        pos += old_len;
    }

    /* Calculate result size */
    size_t result_len = str_len + (new_len - old_len) * count;
    char *result = GC_malloc(result_len + 1);
    char *result_ptr = result;
    const char *str_ptr = str;

    /* Replace all occurrences */
    while ((pos = strstr(str_ptr, old_str)) != NULL) {
        /* Copy part before match */
        size_t before_len = pos - str_ptr;
        memcpy(result_ptr, str_ptr, before_len);
        result_ptr += before_len;

        /* Copy new string */
        memcpy(result_ptr, new_str, new_len);
        result_ptr += new_len;

        /* Advance past old string */
        str_ptr = pos + old_len;
    }

    /* Copy remaining part */
    size_t remaining = strlen(str_ptr);
    memcpy(result_ptr, str_ptr, remaining);
    result_ptr += remaining;
    *result_ptr = '\0';

    return lisp_make_string(result);
}

/* String upcase */
static LispObject *builtin_string_upcase(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("string-upcase requires 1 argument");
    }

    LispObject *str_obj = lisp_car(args);
    if (str_obj->type != LISP_STRING) {
        return lisp_make_error("string-upcase requires a string");
    }

    const char *str = str_obj->value.string;
    size_t len = strlen(str);
    char *result = GC_malloc(len + 1);
    const char *src = str;
    char *dst = result;

    /* Convert each character */
    while (*src) {
        int codepoint = utf8_get_codepoint(src);
        if (codepoint >= 0 && codepoint < 128) {
            /* ASCII - use toupper */
            *dst = toupper((unsigned char)*src);
            src++;
            dst++;
        } else {
            /* Unicode - preserve as-is (full Unicode case conversion requires tables) */
            int bytes = utf8_char_bytes(src);
            memcpy(dst, src, bytes);
            src += bytes;
            dst += bytes;
        }
    }
    *dst = '\0';

    return lisp_make_string(result);
}

/* String downcase */
static LispObject *builtin_string_downcase(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("string-downcase requires 1 argument");
    }

    LispObject *str_obj = lisp_car(args);
    if (str_obj->type != LISP_STRING) {
        return lisp_make_error("string-downcase requires a string");
    }

    const char *str = str_obj->value.string;
    size_t len = strlen(str);
    char *result = GC_malloc(len + 1);
    const char *src = str;
    char *dst = result;

    /* Convert each character */
    while (*src) {
        int codepoint = utf8_get_codepoint(src);
        if (codepoint >= 0 && codepoint < 128) {
            /* ASCII - use tolower */
            *dst = tolower((unsigned char)*src);
            src++;
            dst++;
        } else {
            /* Unicode - preserve as-is (full Unicode case conversion requires tables) */
            int bytes = utf8_char_bytes(src);
            memcpy(dst, src, bytes);
            src += bytes;
            dst += bytes;
        }
    }
    *dst = '\0';

    return lisp_make_string(result);
}

/* Boolean operations */
static LispObject *builtin_not(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("not requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    return lisp_is_truthy(arg) ? NIL : lisp_make_number(1);
}

/* List operations */
static LispObject *builtin_car(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("car requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    if (arg == NIL) {
        return NIL;
    }

    if (arg->type != LISP_CONS) {
        return lisp_make_error("car requires a list");
    }

    return arg->value.cons.car;
}

static LispObject *builtin_cdr(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("cdr requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    if (arg == NIL) {
        return NIL;
    }

    if (arg->type != LISP_CONS) {
        return lisp_make_error("cdr requires a list");
    }

    return arg->value.cons.cdr;
}

static LispObject *builtin_cons(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("cons requires 2 arguments");
    }

    LispObject *car = lisp_car(args);
    LispObject *cdr = lisp_car(lisp_cdr(args));

    return lisp_make_cons(car, cdr);
}

static LispObject *builtin_list(LispObject *args, Environment *env) {
    (void)env;
    return args;
}

static LispObject *builtin_list_length(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("list-length requires 1 argument");
    }

    LispObject *lst = lisp_car(args);
    long long count = 0;

    while (lst != NIL && lst != NULL) {
        count++;
        lst = lst->value.cons.cdr;
    }

    return lisp_make_integer(count);
}

static LispObject *builtin_list_ref(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("list-ref requires 2 arguments");
    }

    LispObject *lst = lisp_car(args);
    LispObject *index_obj = lisp_car(lisp_cdr(args));

    int index_is_integer;
    double index_val = get_numeric_value(index_obj, &index_is_integer);

    if (index_obj->type != LISP_INTEGER && index_obj->type != LISP_NUMBER) {
        return lisp_make_error("list-ref index must be a number");
    }

    long long index = (long long)index_val;
    if (index < 0) {
        return lisp_make_error("list-ref index must be non-negative");
    }

    /* Traverse the list, checking type at each step */
    for (long long i = 0; i < index && lst != NIL && lst != NULL; i++) {
        /* Type check before accessing cons fields */
        if (lst->type != LISP_CONS) {
            return lisp_make_error("list-ref: not a proper list");
        }
        lst = lst->value.cons.cdr;
    }

    if (lst == NIL || lst == NULL) {
        return lisp_make_error("list-ref index out of bounds");
    }

    /* Final type check before accessing car */
    if (lst->type != LISP_CONS) {
        return lisp_make_error("list-ref: not a proper list");
    }

    return lst->value.cons.car;
}

static LispObject *builtin_reverse(LispObject *args, Environment *env) {
    (void)env;

    if (args == NULL || args == NIL) {
        return lisp_make_error("reverse: expected 1 argument");
    }

    LispObject *lst = lisp_car(args);

    /* Handle empty list */
    if (lst == NIL || lst == NULL) {
        return NIL;
    }

    /* Validate it's a list */
    if (lst->type != LISP_CONS) {
        return lisp_make_error("reverse: argument must be a list");
    }

    /* Build reversed list iteratively */
    LispObject *result = NIL;
    while (lst != NIL && lst != NULL && lst->type == LISP_CONS) {
        result = lisp_make_cons(lisp_car(lst), result);
        lst = lisp_cdr(lst);
    }

    return result;
}

static LispObject *builtin_append(LispObject *args, Environment *env) {
    (void)env;

    /* No arguments: return empty list */
    if (args == NIL) {
        return NIL;
    }

    /* Build result by concatenating all argument lists */
    LispObject *result = NIL;
    LispObject *result_tail = NIL;

    /* Iterate through each argument */
    LispObject *current_arg = args;
    while (current_arg != NIL) {
        LispObject *list = lisp_car(current_arg);

        /* Each argument must be a list (or NIL) */
        if (list != NIL && list->type != LISP_CONS) {
            return lisp_make_error("append requires list arguments");
        }

        /* Copy elements from this list */
        LispObject *elem = list;
        while (elem != NIL && elem->type == LISP_CONS) {
            LispObject *new_cons = lisp_make_cons(lisp_car(elem), NIL);

            if (result == NIL) {
                result = new_cons;
                result_tail = new_cons;
            } else {
                result_tail->value.cons.cdr = new_cons;
                result_tail = new_cons;
            }

            elem = lisp_cdr(elem);
        }

        current_arg = lisp_cdr(current_arg);
    }

    return result;
}

/* Predicates */
static LispObject *builtin_null_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("null? requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    return (arg == NIL || arg == NULL) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_atom_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("atom? requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    return (arg->type != LISP_CONS) ? lisp_make_number(1) : NIL;
}

/* Regex functions */
static LispObject *builtin_regex_match(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("regex-match requires 2 arguments");
    }

    LispObject *pattern_obj = lisp_car(args);
    LispObject *string_obj = lisp_car(lisp_cdr(args));

    if (pattern_obj->type != LISP_STRING || string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-match requires strings");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "regex-match: %s", error_msg);
        return lisp_make_error(error);
    }

    pcre2_match_data *match_data = execute_regex(re, string_obj->value.string);
    int result = (match_data != NULL);

    free_regex_resources(re, match_data);

    return result ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_regex_find(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("regex-find requires 2 arguments");
    }

    LispObject *pattern_obj = lisp_car(args);
    LispObject *string_obj = lisp_car(lisp_cdr(args));

    if (pattern_obj->type != LISP_STRING || string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-find requires strings");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "regex-find: %s", error_msg);
        return lisp_make_error(error);
    }

    pcre2_match_data *match_data = execute_regex(re, string_obj->value.string);

    if (match_data == NULL) {
        free_regex_resources(re, NULL);
        return NIL;
    }

    char *matched = extract_capture(match_data, string_obj->value.string, 0);
    LispObject *result = matched ? lisp_make_string(matched) : NIL;

    free_regex_resources(re, match_data);

    return result;
}

static LispObject *builtin_regex_find_all(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("regex-find-all requires 2 arguments");
    }

    LispObject *pattern_obj = lisp_car(args);
    LispObject *string_obj = lisp_car(lisp_cdr(args));

    if (pattern_obj->type != LISP_STRING || string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-find-all requires strings");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "regex-find-all: %s", error_msg);
        return lisp_make_error(error);
    }

    LispObject *result = NIL;
    LispObject *tail = NULL;

    const char *subject = string_obj->value.string;
    size_t offset = 0;
    size_t subject_len = strlen(subject);

    while (offset < subject_len) {
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        int rc = pcre2_match(re, (PCRE2_SPTR)subject, subject_len, offset, 0, match_data, NULL);

        if (rc < 0) {
            pcre2_match_data_free(match_data);
            break;
        }

        char *matched = extract_capture(match_data, subject, 0);
        if (matched) {
            LispObject *match_obj = lisp_make_string(matched);
            LispObject *new_cons = lisp_make_cons(match_obj, NIL);

            if (result == NIL) {
                result = new_cons;
                tail = new_cons;
            } else {
                tail->value.cons.cdr = new_cons;
                tail = new_cons;
            }
        }

        PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
        offset = ovector[1];

        pcre2_match_data_free(match_data);

        if (offset == ovector[0]) {
            offset++; /* Avoid infinite loop on zero-length match */
        }
    }

    pcre2_code_free(re);

    return result;
}

static LispObject *builtin_regex_extract(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("regex-extract requires 2 arguments");
    }

    LispObject *pattern_obj = lisp_car(args);
    LispObject *string_obj = lisp_car(lisp_cdr(args));

    if (pattern_obj->type != LISP_STRING || string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-extract requires strings");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "regex-extract: %s", error_msg);
        return lisp_make_error(error);
    }

    pcre2_match_data *match_data = execute_regex(re, string_obj->value.string);

    if (match_data == NULL) {
        free_regex_resources(re, NULL);
        return NIL;
    }

    int capture_count = get_capture_count(re);
    LispObject *result = NIL;
    LispObject *tail = NULL;

    /* Extract capture groups (skip group 0 which is the whole match) */
    for (int i = 1; i <= capture_count; i++) {
        char *captured = extract_capture(match_data, string_obj->value.string, i);
        if (captured) {
            LispObject *cap_obj = lisp_make_string(captured);
            LispObject *new_cons = lisp_make_cons(cap_obj, NIL);

            if (result == NIL) {
                result = new_cons;
                tail = new_cons;
            } else {
                tail->value.cons.cdr = new_cons;
                tail = new_cons;
            }
        }
    }

    free_regex_resources(re, match_data);

    return result;
}

static LispObject *builtin_regex_replace(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL || lisp_cdr(lisp_cdr(args)) == NIL) {
        return lisp_make_error("regex-replace requires 3 arguments");
    }

    LispObject *pattern_obj = lisp_car(args);
    LispObject *string_obj = lisp_car(lisp_cdr(args));
    LispObject *replacement_obj = lisp_car(lisp_cdr(lisp_cdr(args)));

    if (pattern_obj->type != LISP_STRING || replacement_obj->type != LISP_STRING || string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-replace requires strings");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "regex-replace: %s", error_msg);
        return lisp_make_error(error);
    }

    size_t output_buffer_size = strlen(string_obj->value.string) * 2 + 256;
    size_t output_len = output_buffer_size;
    PCRE2_UCHAR *output = GC_malloc(output_buffer_size + 1); // +1 for null terminator

    int rc = pcre2_substitute(re, (PCRE2_SPTR)string_obj->value.string, PCRE2_ZERO_TERMINATED, 0, /* start offset */
                              PCRE2_SUBSTITUTE_GLOBAL, /* options - replace all */
                              NULL,                    /* match data */
                              NULL,                    /* match context */
                              (PCRE2_SPTR)replacement_obj->value.string, PCRE2_ZERO_TERMINATED, output, &output_len);

    pcre2_code_free(re);

    if (rc < 0) {
        char error[256];
        snprintf(error, sizeof(error), "regex-replace: substitution failed (error code: %d)", rc);
        return lisp_make_error(error);
    }

    /* Ensure null termination */
    output[output_len] = '\0';

    return lisp_make_string((char *)output);
}

static LispObject *builtin_regex_replace_all(LispObject *args, Environment *env) {
    /* Same as regex-replace since we use PCRE2_SUBSTITUTE_GLOBAL */
    return builtin_regex_replace(args, env);
}

static LispObject *builtin_regex_split(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("regex-split requires 2 arguments");
    }

    LispObject *pattern_obj = lisp_car(args);
    LispObject *string_obj = lisp_car(lisp_cdr(args));

    if (pattern_obj->type != LISP_STRING || string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-split requires strings");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "regex-split: %s", error_msg);
        return lisp_make_error(error);
    }

    LispObject *result = NIL;
    LispObject *tail = NULL;

    const char *subject = string_obj->value.string;
    size_t offset = 0;
    size_t last_end = 0;
    size_t subject_len = strlen(subject);

    while (offset <= subject_len) {
        pcre2_match_data *match_data = pcre2_match_data_create_from_pattern(re, NULL);

        int rc = pcre2_match(re, (PCRE2_SPTR)subject, subject_len, offset, 0, match_data, NULL);

        if (rc < 0) {
            pcre2_match_data_free(match_data);
            break;
        }

        PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
        size_t match_start = ovector[0];
        size_t match_end = ovector[1];

        /* Add substring before match */
        size_t part_len = match_start - last_end;
        char *part = GC_malloc(part_len + 1);
        strncpy(part, subject + last_end, part_len);
        part[part_len] = '\0';

        LispObject *part_obj = lisp_make_string(part);
        LispObject *new_cons = lisp_make_cons(part_obj, NIL);

        if (result == NIL) {
            result = new_cons;
            tail = new_cons;
        } else {
            tail->value.cons.cdr = new_cons;
            tail = new_cons;
        }

        last_end = match_end;
        offset = match_end;

        pcre2_match_data_free(match_data);

        if (offset == match_start) {
            offset++; /* Avoid infinite loop */
        }
    }

    /* Add remaining substring */
    if (last_end <= subject_len) {
        char *part = GC_malloc(subject_len - last_end + 1);
        strcpy(part, subject + last_end);

        LispObject *part_obj = lisp_make_string(part);
        LispObject *new_cons = lisp_make_cons(part_obj, NIL);

        if (result == NIL) {
            result = new_cons;
        } else {
            tail->value.cons.cdr = new_cons;
        }
    }

    pcre2_code_free(re);

    return result;
}

static LispObject *builtin_regex_escape(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("regex-escape requires 1 argument");
    }

    LispObject *string_obj = lisp_car(args);

    if (string_obj->type != LISP_STRING) {
        return lisp_make_error("regex-escape requires a string");
    }

    const char *str = string_obj->value.string;
    size_t len = strlen(str);
    char *escaped = GC_malloc(len * 2 + 1);
    size_t j = 0;

    const char *special = ".^$*+?()[]{}|\\";

    for (size_t i = 0; i < len; i++) {
        if (strchr(special, str[i])) {
            escaped[j++] = '\\';
        }
        escaped[j++] = str[i];
    }
    escaped[j] = '\0';

    return lisp_make_string(escaped);
}

static LispObject *builtin_regex_valid(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("regex-valid? requires 1 argument");
    }

    LispObject *pattern_obj = lisp_car(args);

    if (pattern_obj->type != LISP_STRING) {
        return lisp_make_error("regex-valid? requires a string");
    }

    char *error_msg = NULL;
    pcre2_code *re = compile_regex_pattern(pattern_obj->value.string, &error_msg);

    if (re == NULL) {
        return NIL;
    }

    pcre2_code_free(re);
    return lisp_make_number(1);
}

/* File I/O operations */

/* Helper function to create a file stream object */
LispObject *lisp_make_file_stream(FILE *file) {
    LispObject *obj = GC_malloc(sizeof(LispObject));
    obj->type = LISP_FILE_STREAM;
    obj->value.file = file;
    return obj;
}

static LispObject *builtin_open(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("open requires at least 1 argument");
    }

    LispObject *filename_obj = lisp_car(args);
    if (filename_obj->type != LISP_STRING) {
        return lisp_make_error("open requires a string filename");
    }

    /* Default mode is "r" (read) */
    const char *mode = "r";
    if (lisp_cdr(args) != NIL && lisp_cdr(args) != NULL) {
        LispObject *mode_obj = lisp_car(lisp_cdr(args));
        if (mode_obj->type != LISP_STRING) {
            return lisp_make_error("open mode must be a string");
        }
        mode = mode_obj->value.string;
    }

    FILE *file = fopen(filename_obj->value.string, mode);
    if (file == NULL) {
        char error[512];
        snprintf(error, sizeof(error), "open: cannot open file '%s'", filename_obj->value.string);
        return lisp_make_error(error);
    }

    return lisp_make_file_stream(file);
}

static LispObject *builtin_close(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("close requires 1 argument");
    }

    LispObject *stream_obj = lisp_car(args);
    if (stream_obj->type != LISP_FILE_STREAM) {
        return lisp_make_error("close requires a file stream");
    }

    if (stream_obj->value.file != NULL) {
        fclose(stream_obj->value.file);
        stream_obj->value.file = NULL;
    }

    return NIL;
}

static LispObject *builtin_read_line(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("read-line requires 1 argument");
    }

    LispObject *stream_obj = lisp_car(args);
    if (stream_obj->type != LISP_FILE_STREAM) {
        return lisp_make_error("read-line requires a file stream");
    }

    FILE *file = stream_obj->value.file;
    if (file == NULL) {
        return lisp_make_error("read-line: file is closed");
    }

    /* Read line with dynamic buffer */
    size_t size = 256;
    char *buffer = GC_malloc(size);
    size_t pos = 0;

    int c;
    while ((c = fgetc(file)) != EOF) {
        if (c == '\n') {
            /* Unix line ending */
            break;
        } else if (c == '\r') {
            /* Check for Windows (\r\n) or old Mac (\r) line ending */
            int next_c = fgetc(file);
            if (next_c == '\n') {
                /* Windows line ending \r\n */
                break;
            } else if (next_c != EOF) {
                /* Old Mac line ending \r, put back the character */
                ungetc(next_c, file);
            }
            /* We got \r, now break */
            break;
        }

        if (pos >= size - 1) {
            size *= 2;
            char *new_buffer = GC_malloc(size);
            strncpy(new_buffer, buffer, pos);
            buffer = new_buffer;
        }

        buffer[pos++] = c;
    }

    /* Check for EOF without newline */
    if (pos == 0 && c == EOF) {
        return NIL; /* End of file */
    }

    buffer[pos] = '\0';
    return lisp_make_string(buffer);
}

static LispObject *builtin_write_line(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("write-line requires at least 1 argument");
    }

    LispObject *stream_obj = lisp_car(args);
    if (stream_obj->type != LISP_FILE_STREAM) {
        return lisp_make_error("write-line requires a file stream");
    }

    FILE *file = stream_obj->value.file;
    if (file == NULL) {
        return lisp_make_error("write-line: file is closed");
    }

    LispObject *rest = lisp_cdr(args);
    if (rest == NIL) {
        return lisp_make_error("write-line requires a string to write");
    }

    LispObject *text_obj = lisp_car(rest);
    if (text_obj->type != LISP_STRING) {
        return lisp_make_error("write-line requires a string");
    }

    fprintf(file, "%s\n", text_obj->value.string);
    fflush(file);

    return text_obj;
}

/* Read S-expressions from file */
static LispObject *builtin_read_sexp(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("read-sexp requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    FILE *file = NULL;
    int should_close = 0;

    /* Check if argument is file stream or filename */
    if (arg->type == LISP_FILE_STREAM) {
        file = arg->value.file;
        if (file == NULL) {
            return lisp_make_error("read-sexp: file is closed");
        }
    } else if (arg->type == LISP_STRING) {
        /* Open file */
        file = fopen(arg->value.string, "r");
        if (file == NULL) {
            char error[512];
            snprintf(error, sizeof(error), "read-sexp: cannot open file '%s'", arg->value.string);
            return lisp_make_error(error);
        }
        should_close = 1;
    } else {
        return lisp_make_error("read-sexp requires a filename (string) or file stream");
    }

    /* Read entire file */
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (size < 0) {
        if (should_close) {
            fclose(file);
        }
        return lisp_make_error("read-sexp: cannot determine file size");
    }

    char *buffer = GC_malloc(size + 1);
    size_t read_size = fread(buffer, 1, size, file);
    buffer[read_size] = '\0';

    if (should_close) {
        fclose(file);
    }

    /* Parse all expressions */
    const char *input = buffer;
    LispObject *result = NIL;
    LispObject *last = NIL;

    while (*input) {
        /* Skip whitespace and comments */
        while (*input == ' ' || *input == '\t' || *input == '\n' || *input == '\r' || *input == ';') {
            if (*input == ';') {
                while (*input && *input != '\n') {
                    input++;
                }
            } else {
                input++;
            }
        }

        if (*input == '\0') {
            break;
        }

        /* Parse expression */
        LispObject *expr = lisp_read(&input);
        if (expr == NULL) {
            break;
        }

        if (expr->type == LISP_ERROR) {
            return expr;
        }

        /* Add to result list */
        LispObject *cell = lisp_make_cons(expr, NIL);
        if (result == NIL) {
            result = cell;
            last = cell;
        } else {
            last->value.cons.cdr = cell;
            last = cell;
        }
    }

    /* Return single expression if only one, otherwise return list */
    if (result != NIL && lisp_cdr(result) == NIL) {
        return lisp_car(result);
    }

    return result;
}

/* Read JSON from file - basic JSON parser */
static LispObject *builtin_read_json(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("read-json requires 1 argument");
    }

    LispObject *arg = lisp_car(args);
    FILE *file = NULL;
    int should_close = 0;

    /* Check if argument is file stream or filename */
    if (arg->type == LISP_FILE_STREAM) {
        file = arg->value.file;
        if (file == NULL) {
            return lisp_make_error("read-json: file is closed");
        }
    } else if (arg->type == LISP_STRING) {
        /* Open file */
        file = fopen(arg->value.string, "r");
        if (file == NULL) {
            char error[512];
            snprintf(error, sizeof(error), "read-json: cannot open file '%s'", arg->value.string);
            return lisp_make_error(error);
        }
        should_close = 1;
    } else {
        return lisp_make_error("read-json requires a filename (string) or file stream");
    }

    /* Read entire file */
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (size < 0) {
        if (should_close) {
            fclose(file);
        }
        return lisp_make_error("read-json: cannot determine file size");
    }

    char *buffer = GC_malloc(size + 1);
    size_t read_size = fread(buffer, 1, size, file);
    buffer[read_size] = '\0';

    if (should_close) {
        fclose(file);
    }

    /* Simple JSON parser - this is a basic implementation */
    /* Skip whitespace */
    const char *p = buffer;
    while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) {
        p++;
    }

    if (*p == '\0') {
        return NIL;
    }

    /* Parse JSON value */
    LispObject *result = NULL;
    if (*p == '{') {
        /* JSON object -> hash table */
        result = lisp_make_hash_table();
        p++; /* Skip '{' */
        while (*p && *p != '}') {
            /* Skip whitespace */
            while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) {
                p++;
            }
            if (*p == '}') {
                break;
            }

            /* Parse key (must be string) */
            if (*p != '"') {
                return lisp_make_error("read-json: object key must be a string");
            }
            p++; /* Skip '"' */
            const char *key_start = p;
            while (*p && *p != '"') {
                if (*p == '\\' && *(p + 1)) {
                    p += 2; /* Skip escape sequence */
                } else {
                    p++;
                }
            }
            if (*p != '"') {
                return lisp_make_error("read-json: unterminated string");
            }
            size_t key_len = p - key_start;
            char *key = GC_malloc(key_len + 1);
            memcpy(key, key_start, key_len);
            key[key_len] = '\0';
            p++; /* Skip '"' */

            /* Skip whitespace */
            while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) {
                p++;
            }
            if (*p != ':') {
                return lisp_make_error("read-json: expected ':' after key");
            }
            p++; /* Skip ':' */

            /* Skip whitespace */
            while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) {
                p++;
            }

            /* Parse value (recursive) */
            LispObject *json_value = NULL;
            if (*p == '"') {
                /* String */
                p++;
                const char *val_start = p;
                while (*p && *p != '"') {
                    if (*p == '\\' && *(p + 1)) {
                        p += 2;
                    } else {
                        p++;
                    }
                }
                if (*p != '"') {
                    return lisp_make_error("read-json: unterminated string");
                }
                size_t val_len = p - val_start;
                char *val_str = GC_malloc(val_len + 1);
                memcpy(val_str, val_start, val_len);
                val_str[val_len] = '\0';
                json_value = lisp_make_string(val_str);
                p++; /* Skip '"' */
            } else if (*p == '{') {
                /* Nested object - simplified: return error for now */
                return lisp_make_error("read-json: nested objects not yet supported");
            } else if (*p == '[') {
                /* Array - simplified: return error for now */
                return lisp_make_error("read-json: arrays not yet supported");
            } else if (strncmp(p, "true", 4) == 0) {
                json_value = lisp_make_boolean(1);
                p += 4;
            } else if (strncmp(p, "false", 5) == 0) {
                json_value = NIL;
                p += 5;
            } else if (strncmp(p, "null", 4) == 0) {
                json_value = NIL;
                p += 4;
            } else if (isdigit(*p) || *p == '-') {
                /* Number - simplified parsing */
                const char *num_start = p;
                while (*p && (isdigit(*p) || *p == '.' || *p == '-' || *p == '+' || *p == 'e' || *p == 'E')) {
                    p++;
                }
                size_t num_len = p - num_start;
                char *num_str = GC_malloc(num_len + 1);
                memcpy(num_str, num_start, num_len);
                num_str[num_len] = '\0';
                if (strchr(num_str, '.') != NULL) {
                    json_value = lisp_make_number(atof(num_str));
                } else {
                    json_value = lisp_make_integer(atoll(num_str));
                }
            } else {
                return lisp_make_error("read-json: unexpected character");
            }

            /* Store in hash table */
            hash_table_set_entry(result, key, json_value);

            /* Skip whitespace */
            while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) {
                p++;
            }
            if (*p == ',') {
                p++; /* Skip ',' */
            } else if (*p != '}') {
                return lisp_make_error("read-json: expected ',' or '}'");
            }
        }
        if (*p == '}') {
            p++;
        }
    } else if (*p == '"') {
        /* JSON string */
        p++;
        const char *str_start = p;
        while (*p && *p != '"') {
            if (*p == '\\' && *(p + 1)) {
                p += 2;
            } else {
                p++;
            }
        }
        if (*p != '"') {
            return lisp_make_error("read-json: unterminated string");
        }
        size_t str_len = p - str_start;
        char *str = GC_malloc(str_len + 1);
        memcpy(str, str_start, str_len);
        str[str_len] = '\0';
        result = lisp_make_string(str);
        p++;
    } else if (strncmp(p, "true", 4) == 0) {
        result = lisp_make_boolean(1);
    } else if (strncmp(p, "false", 5) == 0) {
        result = NIL;
    } else if (strncmp(p, "null", 4) == 0) {
        result = NIL;
    } else if (isdigit(*p) || *p == '-') {
        /* Number */
        const char *num_start = p;
        while (*p && (isdigit(*p) || *p == '.' || *p == '-' || *p == '+' || *p == 'e' || *p == 'E')) {
            p++;
        }
        size_t num_len = p - num_start;
        char *num_str = GC_malloc(num_len + 1);
        memcpy(num_str, num_start, num_len);
        num_str[num_len] = '\0';
        if (strchr(num_str, '.') != NULL) {
            result = lisp_make_number(atof(num_str));
        } else {
            result = lisp_make_integer(atoll(num_str));
        }
    } else {
        return lisp_make_error("read-json: unsupported JSON value type");
    }

    return result;
}

/* Load and evaluate a Lisp file */
static LispObject *builtin_load(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("load requires 1 argument");
    }

    LispObject *filename_obj = lisp_car(args);
    if (filename_obj->type != LISP_STRING) {
        return lisp_make_error("load requires a string filename");
    }

    LispObject *result = lisp_load_file(filename_obj->value.string, env);

    /* Return the result of the last expression evaluated, or nil if error */
    if (result && result->type == LISP_ERROR) {
        return result;
    }

    /* Return the last evaluated expression, or nil if file was empty */
    return result ? result : NIL;
}

/* Delete a file */
static LispObject *builtin_delete_file(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("delete-file requires 1 argument");
    }

    LispObject *filename_obj = lisp_car(args);
    if (filename_obj->type != LISP_STRING) {
        return lisp_make_error("delete-file requires a string filename");
    }

    /* Attempt to delete the file */
    if (remove(filename_obj->value.string) == 0) {
        /* Success */
        return NIL;
    } else {
        /* Failure - return error with errno message */
        char error[512];
        snprintf(error, sizeof(error), "delete-file: failed to delete '%s': %s", filename_obj->value.string,
                 strerror(errno));
        return lisp_make_error(error);
    }
}

static LispObject *builtin_princ(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("princ requires 1 argument");
    }

    LispObject *obj = lisp_car(args);
    lisp_princ(obj);

    /* Return the object (Common Lisp convention) */
    return obj;
}

static LispObject *builtin_prin1(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("prin1 requires 1 argument");
    }

    LispObject *obj = lisp_car(args);
    lisp_prin1(obj);

    /* Return the object (Common Lisp convention) */
    return obj;
}

static LispObject *builtin_print_cl(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("print requires 1 argument");
    }

    LispObject *obj = lisp_car(args);
    lisp_print_cl(obj);

    /* Return the object (Common Lisp convention) */
    return obj;
}

static LispObject *builtin_format(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("format requires at least 2 arguments");
    }

    LispObject *dest = lisp_car(args);
    LispObject *format_str_obj = lisp_car(lisp_cdr(args));

    if (format_str_obj->type != LISP_STRING) {
        return lisp_make_error("format requires a string as format argument");
    }

    const char *format_str = format_str_obj->value.string;
    LispObject *format_args = lisp_cdr(lisp_cdr(args));

    /* Build output string */
    char *output = GC_malloc(4096);
    size_t output_len = 0;
    size_t output_capacity = 4096;

    LispObject *current_arg = format_args;

    for (const char *p = format_str; *p; p++) {
        if (*p == '~' && *(p + 1)) {
            p++;
            char directive = *p;

            if (directive == '%') {
                /* Newline */
                if (output_len + 1 >= output_capacity) {
                    output_capacity *= 2;
                    char *new_output = GC_malloc(output_capacity);
                    memcpy(new_output, output, output_len);
                    output = new_output;
                }
                output[output_len++] = '\n';
            } else if (directive == '~') {
                /* Literal tilde */
                if (output_len + 1 >= output_capacity) {
                    output_capacity *= 2;
                    char *new_output = GC_malloc(output_capacity);
                    memcpy(new_output, output, output_len);
                    output = new_output;
                }
                output[output_len++] = '~';
            } else if (directive == 'A' || directive == 'a') {
                /* Aesthetic - princ style (no quotes) */
                if (current_arg == NIL) {
                    return lisp_make_error("format: not enough arguments for format directives");
                }
                LispObject *arg = lisp_car(current_arg);
                char *arg_str = lisp_print(arg);
                /* Remove quotes from strings for aesthetic output */
                if (arg->type == LISP_STRING) {
                    arg_str = arg->value.string;
                }
                size_t arg_len = strlen(arg_str);
                if (output_len + arg_len >= output_capacity) {
                    while (output_len + arg_len >= output_capacity) {
                        output_capacity *= 2;
                    }
                    char *new_output = GC_malloc(output_capacity);
                    memcpy(new_output, output, output_len);
                    output = new_output;
                }
                memcpy(output + output_len, arg_str, arg_len);
                output_len += arg_len;
                current_arg = lisp_cdr(current_arg);
            } else if (directive == 'S' || directive == 's') {
                /* S-expression - prin1 style (with quotes) */
                if (current_arg == NIL) {
                    return lisp_make_error("format: not enough arguments for format directives");
                }
                LispObject *arg = lisp_car(current_arg);
                char *arg_str = lisp_print(arg);
                size_t arg_len = strlen(arg_str);
                if (output_len + arg_len >= output_capacity) {
                    while (output_len + arg_len >= output_capacity) {
                        output_capacity *= 2;
                    }
                    char *new_output = GC_malloc(output_capacity);
                    memcpy(new_output, output, output_len);
                    output = new_output;
                }
                memcpy(output + output_len, arg_str, arg_len);
                output_len += arg_len;
                current_arg = lisp_cdr(current_arg);
            } else {
                /* Unknown directive - just output it */
                if (output_len + 2 >= output_capacity) {
                    output_capacity *= 2;
                    char *new_output = GC_malloc(output_capacity);
                    memcpy(new_output, output, output_len);
                    output = new_output;
                }
                output[output_len++] = '~';
                output[output_len++] = directive;
            }
        } else {
            /* Regular character */
            if (output_len + 1 >= output_capacity) {
                output_capacity *= 2;
                char *new_output = GC_malloc(output_capacity);
                memcpy(new_output, output, output_len);
                output = new_output;
            }
            output[output_len++] = *p;
        }
    }

    output[output_len] = '\0';

    /* Handle destination */
    if (dest == NIL) {
        /* Return as string */
        return lisp_make_string(output);
    } else if (dest->type == LISP_BOOLEAN && dest->value.boolean) {
        /* Output to stdout */
        printf("%s", output);
        fflush(stdout);
        return NIL;
    } else {
        return lisp_make_error("format: invalid destination (use nil for string or #t for stdout)");
    }
}

static LispObject *builtin_terpri(LispObject *args, Environment *env) {
    (void)env;
    (void)args;
    printf("\n");
    fflush(stdout);
    return NIL;
}

/* Path expansion functions */

/* Get user's home directory path (cross-platform)
 * Unix/Linux/macOS: $HOME
 * Windows: %USERPROFILE% or %HOMEDRIVE%%HOMEPATH%
 * Returns: String with home directory or NIL if not found
 */
static LispObject *builtin_home_directory(LispObject *args, Environment *env) {
    (void)args; /* Takes no arguments */
    (void)env;

    const char *home = NULL;

#if defined(_WIN32) || defined(_WIN64)
    /* Windows: Try USERPROFILE first */
    home = getenv("USERPROFILE");

    /* Fallback: HOMEDRIVE + HOMEPATH */
    if (home == NULL) {
        const char *homedrive = getenv("HOMEDRIVE");
        const char *homepath = getenv("HOMEPATH");

        if (homedrive != NULL && homepath != NULL) {
            size_t len = strlen(homedrive) + strlen(homepath) + 1;
            char *combined = GC_malloc(len);
            snprintf(combined, len, "%s%s", homedrive, homepath);
            home = combined;
        }
    }
#else
    /* Unix/Linux/macOS: Use HOME */
    home = getenv("HOME");
#endif

    if (home == NULL) {
        return NIL; /* No home directory found */
    }

    return lisp_make_string(home);
}

/* Expand ~/ in file paths to home directory (cross-platform)
 * Takes: String (file path)
 * Returns: String (expanded path) or original if no ~/ prefix
 * Example: (expand-path "~/config.lisp") => "/home/user/config.lisp"
 */
static LispObject *builtin_expand_path(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("expand-path requires 1 argument");
    }

    LispObject *path_obj = lisp_car(args);
    if (path_obj->type != LISP_STRING) {
        return lisp_make_error("expand-path requires a string argument");
    }

    const char *path = path_obj->value.string;

    /* Check if path starts with ~/ */
    if (path[0] != '~' || (path[1] != '/' && path[1] != '\\' && path[1] != '\0')) {
        /* Not a ~/ path - return original */
        return path_obj;
    }

    /* Get home directory */
    LispObject *home_obj = builtin_home_directory(NIL, env);
    if (home_obj == NIL || home_obj->type != LISP_STRING) {
        /* No home directory - return error */
        return lisp_make_error("expand-path: cannot determine home directory");
    }

    const char *home = home_obj->value.string;

    /* Calculate expanded path length */
    /* If path is just "~", use home directory directly */
    if (path[1] == '\0') {
        return home_obj;
    }

    /* Skip ~/ or ~\ */
    const char *rest = path + 2;

    /* Build expanded path: home + / + rest */
    size_t home_len = strlen(home);
    size_t rest_len = strlen(rest);
    size_t total_len = home_len + 1 + rest_len + 1;

    char *expanded = GC_malloc(total_len);

#if defined(_WIN32) || defined(_WIN64)
    /* Windows: Use backslash separator */
    snprintf(expanded, total_len, "%s\\%s", home, rest);
#else
    /* Unix: Use forward slash separator */
    snprintf(expanded, total_len, "%s/%s", home, rest);
#endif

    return lisp_make_string(expanded);
}

/* Type predicates */
static LispObject *builtin_integer_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("integer? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_INTEGER) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_boolean_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("boolean? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_BOOLEAN) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_number_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("number? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_NUMBER || arg->type == LISP_INTEGER) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_vector_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("vector? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_VECTOR) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_hash_table_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("hash-table? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_HASH_TABLE) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("string? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_STRING) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_symbol_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("symbol? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    return (arg->type == LISP_SYMBOL) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_list_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("list? requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    /* A list is either NIL or a cons cell */
    return (arg == NIL || arg->type == LISP_CONS) ? lisp_make_number(1) : NIL;
}

/* Symbol operations */

static LispObject *builtin_symbol_to_string(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("symbol->string requires 1 argument");
    }
    LispObject *arg = lisp_car(args);
    if (arg->type != LISP_SYMBOL) {
        return lisp_make_error("symbol->string requires a symbol argument");
    }
    return lisp_make_string(arg->value.symbol);
}

/* Vector operations */
static LispObject *builtin_make_vector(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("make-vector requires at least 1 argument");
    }
    LispObject *size_obj = lisp_car(args);
    if (size_obj->type != LISP_NUMBER && size_obj->type != LISP_INTEGER) {
        return lisp_make_error("make-vector size must be a number");
    }
    size_t size = (size_t)(size_obj->type == LISP_INTEGER ? size_obj->value.integer : size_obj->value.number);

    LispObject *vec = lisp_make_vector(size);

    /* If initial value is provided, set all elements to it */
    if (lisp_cdr(args) != NIL) {
        LispObject *init_val = lisp_car(lisp_cdr(args));
        for (size_t i = 0; i < size; i++) {
            vec->value.vector.items[i] = init_val;
        }
        /* Set the size to match the capacity when initializing */
        vec->value.vector.size = size;
    }

    return vec;
}

static LispObject *builtin_vector_ref(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("vector-ref requires 2 arguments");
    }
    LispObject *vec_obj = lisp_car(args);
    if (vec_obj->type != LISP_VECTOR) {
        return lisp_make_error("vector-ref requires a vector");
    }
    LispObject *idx_obj = lisp_car(lisp_cdr(args));
    if (idx_obj->type != LISP_NUMBER && idx_obj->type != LISP_INTEGER) {
        return lisp_make_error("vector-ref index must be a number");
    }
    size_t idx = (size_t)(idx_obj->type == LISP_INTEGER ? idx_obj->value.integer : idx_obj->value.number);
    if (idx >= vec_obj->value.vector.size) {
        return lisp_make_error("vector-ref: index out of bounds");
    }
    return vec_obj->value.vector.items[idx];
}

static LispObject *builtin_vector_set_bang(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL || lisp_cdr(lisp_cdr(args)) == NIL) {
        return lisp_make_error("vector-set! requires 3 arguments");
    }
    LispObject *vec_obj = lisp_car(args);
    if (vec_obj->type != LISP_VECTOR) {
        return lisp_make_error("vector-set! requires a vector");
    }
    LispObject *idx_obj = lisp_car(lisp_cdr(args));
    if (idx_obj->type != LISP_NUMBER && idx_obj->type != LISP_INTEGER) {
        return lisp_make_error("vector-set! index must be a number");
    }
    size_t idx = (size_t)(idx_obj->type == LISP_INTEGER ? idx_obj->value.integer : idx_obj->value.number);
    if (idx >= vec_obj->value.vector.size) {
        vec_obj->value.vector.size = idx + 1;
        /* Expand capacity if needed */
        if (vec_obj->value.vector.size > vec_obj->value.vector.capacity) {
            size_t new_capacity = vec_obj->value.vector.capacity;
            while (new_capacity < vec_obj->value.vector.size) {
                new_capacity *= 2;
            }
            LispObject **new_items = GC_malloc(sizeof(LispObject *) * new_capacity);
            for (size_t i = 0; i < vec_obj->value.vector.size - 1; i++) {
                new_items[i] = vec_obj->value.vector.items[i];
            }
            for (size_t i = vec_obj->value.vector.size - 1; i < new_capacity; i++) {
                new_items[i] = NIL;
            }
            vec_obj->value.vector.items = new_items;
            vec_obj->value.vector.capacity = new_capacity;
        }
    }
    LispObject *value = lisp_car(lisp_cdr(lisp_cdr(args)));
    vec_obj->value.vector.items[idx] = value;
    return value;
}

static LispObject *builtin_vector_length(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("vector-length requires 1 argument");
    }
    LispObject *vec_obj = lisp_car(args);
    if (vec_obj->type != LISP_VECTOR) {
        return lisp_make_error("vector-length requires a vector");
    }
    return lisp_make_number((double)vec_obj->value.vector.size);
}

static LispObject *builtin_vector_push_bang(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("vector-push! requires 2 arguments");
    }
    LispObject *vec_obj = lisp_car(args);
    if (vec_obj->type != LISP_VECTOR) {
        return lisp_make_error("vector-push! requires a vector");
    }
    LispObject *value = lisp_car(lisp_cdr(args));
    /* Check if we need to expand */
    if (vec_obj->value.vector.size >= vec_obj->value.vector.capacity) {
        size_t new_capacity = vec_obj->value.vector.capacity * 2;
        LispObject **new_items = GC_malloc(sizeof(LispObject *) * new_capacity);
        for (size_t i = 0; i < vec_obj->value.vector.size; i++) {
            new_items[i] = vec_obj->value.vector.items[i];
        }
        for (size_t i = vec_obj->value.vector.size; i < new_capacity; i++) {
            new_items[i] = NIL;
        }
        vec_obj->value.vector.items = new_items;
        vec_obj->value.vector.capacity = new_capacity;
    }
    vec_obj->value.vector.items[vec_obj->value.vector.size] = value;
    vec_obj->value.vector.size++;
    return value;
}

static LispObject *builtin_vector_pop_bang(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("vector-pop! requires 1 argument");
    }
    LispObject *vec_obj = lisp_car(args);
    if (vec_obj->type != LISP_VECTOR) {
        return lisp_make_error("vector-pop! requires a vector");
    }
    if (vec_obj->value.vector.size == 0) {
        return lisp_make_error("vector-pop!: cannot pop from empty vector");
    }
    vec_obj->value.vector.size--;
    return vec_obj->value.vector.items[vec_obj->value.vector.size];
}

/* Hash table operations */
static LispObject *builtin_make_hash_table(LispObject *args, Environment *env) {
    (void)env;
    (void)args;
    return lisp_make_hash_table();
}

static LispObject *builtin_hash_ref(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("hash-ref requires 2 arguments");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-ref requires a hash table");
    }

    LispObject *key_obj = lisp_car(lisp_cdr(args));
    if (key_obj->type != LISP_STRING && key_obj->type != LISP_SYMBOL) {
        return lisp_make_error("hash-ref key must be a string or symbol");
    }

    const char *key = (key_obj->type == LISP_STRING) ? key_obj->value.string : key_obj->value.symbol;
    struct HashEntry *entry = hash_table_get_entry(table, key);

    if (entry) {
        return entry->value;
    }

    return NIL; /* Key not found */
}

static LispObject *builtin_hash_set_bang(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL || lisp_cdr(lisp_cdr(args)) == NIL) {
        return lisp_make_error("hash-set! requires 3 arguments");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-set! requires a hash table");
    }

    LispObject *key_obj = lisp_car(lisp_cdr(args));
    if (key_obj->type != LISP_STRING && key_obj->type != LISP_SYMBOL) {
        return lisp_make_error("hash-set! key must be a string or symbol");
    }

    const char *key = (key_obj->type == LISP_STRING) ? key_obj->value.string : key_obj->value.symbol;
    LispObject *value = lisp_car(lisp_cdr(lisp_cdr(args)));

    hash_table_set_entry(table, key, value);
    return value;
}

static LispObject *builtin_hash_remove_bang(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("hash-remove! requires 2 arguments");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-remove! requires a hash table");
    }

    LispObject *key_obj = lisp_car(lisp_cdr(args));
    if (key_obj->type != LISP_STRING && key_obj->type != LISP_SYMBOL) {
        return lisp_make_error("hash-remove! key must be a string or symbol");
    }

    const char *key = (key_obj->type == LISP_STRING) ? key_obj->value.string : key_obj->value.symbol;
    int removed = hash_table_remove_entry(table, key);

    return removed ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_hash_clear_bang(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("hash-clear! requires 1 argument");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-clear! requires a hash table");
    }

    hash_table_clear(table);
    return NIL;
}

static LispObject *builtin_hash_count(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("hash-count requires 1 argument");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-count requires a hash table");
    }

    return lisp_make_number((double)table->value.hash_table.entry_count);
}

static LispObject *builtin_hash_keys(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("hash-keys requires 1 argument");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-keys requires a hash table");
    }

    struct HashEntry **buckets = (struct HashEntry **)table->value.hash_table.buckets;
    size_t bucket_count = table->value.hash_table.bucket_count;

    LispObject *result = NIL;
    LispObject *tail = NULL;

    for (size_t i = 0; i < bucket_count; i++) {
        struct HashEntry *entry = buckets[i];
        while (entry != NULL) {
            LispObject *key_obj = lisp_make_string(entry->key);
            LispObject *new_cons = lisp_make_cons(key_obj, NIL);

            if (result == NIL) {
                result = new_cons;
                tail = new_cons;
            } else {
                tail->value.cons.cdr = new_cons;
                tail = new_cons;
            }

            entry = entry->next;
        }
    }

    return result;
}

static LispObject *builtin_hash_values(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("hash-values requires 1 argument");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-values requires a hash table");
    }

    struct HashEntry **buckets = (struct HashEntry **)table->value.hash_table.buckets;
    size_t bucket_count = table->value.hash_table.bucket_count;

    LispObject *result = NIL;
    LispObject *tail = NULL;

    for (size_t i = 0; i < bucket_count; i++) {
        struct HashEntry *entry = buckets[i];
        while (entry != NULL) {
            LispObject *value_obj = entry->value;
            LispObject *new_cons = lisp_make_cons(value_obj, NIL);

            if (result == NIL) {
                result = new_cons;
                tail = new_cons;
            } else {
                tail->value.cons.cdr = new_cons;
                tail = new_cons;
            }

            entry = entry->next;
        }
    }

    return result;
}

static LispObject *builtin_hash_entries(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("hash-entries requires 1 argument");
    }

    LispObject *table = lisp_car(args);
    if (table->type != LISP_HASH_TABLE) {
        return lisp_make_error("hash-entries requires a hash table");
    }

    struct HashEntry **buckets = (struct HashEntry **)table->value.hash_table.buckets;
    size_t bucket_count = table->value.hash_table.bucket_count;

    LispObject *result = NIL;
    LispObject *tail = NULL;

    for (size_t i = 0; i < bucket_count; i++) {
        struct HashEntry *entry = buckets[i];
        while (entry != NULL) {
            LispObject *key_obj = lisp_make_string(entry->key);
            LispObject *value_obj = entry->value;

            /* Create (key . value) pair */
            LispObject *pair = lisp_make_cons(key_obj, value_obj);
            LispObject *new_cons = lisp_make_cons(pair, NIL);

            if (result == NIL) {
                result = new_cons;
                tail = new_cons;
            } else {
                tail->value.cons.cdr = new_cons;
                tail = new_cons;
            }

            entry = entry->next;
        }
    }

    return result;
}

/* Alist operations */

/* Helper function to check deep structural equality (used by assoc, equal?, etc.) */
static int objects_equal_recursive(LispObject *a, LispObject *b) {
    /* Fast path: pointer equality */
    if (a == b)
        return 1;

    /* NIL checks */
    if (a == NIL || b == NIL)
        return 0;

    /* Type mismatch */
    if (a->type != b->type)
        return 0;

    switch (a->type) {
    case LISP_NUMBER:
        return a->value.number == b->value.number;

    case LISP_INTEGER:
        return a->value.integer == b->value.integer;

    case LISP_STRING:
        return strcmp(a->value.string, b->value.string) == 0;

    case LISP_SYMBOL:
        return strcmp(a->value.symbol, b->value.symbol) == 0;

    case LISP_BOOLEAN:
        return a->value.boolean == b->value.boolean;

    case LISP_CONS:
        /* Recursive list comparison */
        return objects_equal_recursive(a->value.cons.car, b->value.cons.car) &&
               objects_equal_recursive(a->value.cons.cdr, b->value.cons.cdr);

    case LISP_VECTOR:
        /* Compare vector lengths */
        if (a->value.vector.size != b->value.vector.size) {
            return 0;
        }
        /* Compare each element */
        for (size_t i = 0; i < a->value.vector.size; i++) {
            if (!objects_equal_recursive(a->value.vector.items[i], b->value.vector.items[i])) {
                return 0;
            }
        }
        return 1;

    case LISP_HASH_TABLE:
        /* Compare hash table sizes */
        if (a->value.hash_table.entry_count != b->value.hash_table.entry_count) {
            return 0;
        }
        /* Compare each key-value pair */
        /* Iterate through all buckets in hash table a */
        struct HashEntry **a_buckets = (struct HashEntry **)a->value.hash_table.buckets;
        for (size_t i = 0; i < a->value.hash_table.bucket_count; i++) {
            struct HashEntry *entry = a_buckets[i];
            while (entry != NULL) {
                /* Look up the key in hash table b */
                struct HashEntry *b_entry = hash_table_get_entry(b, entry->key);
                if (b_entry == NULL) {
                    return 0; /* Key doesn't exist in b */
                }
                /* Compare values */
                if (!objects_equal_recursive(entry->value, b_entry->value)) {
                    return 0;
                }
                entry = entry->next;
            }
        }
        return 1;

    default:
        /* For other types (lambdas, builtins, etc.), use pointer equality */
        return 0;
    }
}

static LispObject *builtin_assoc(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("assoc requires 2 arguments");
    }

    LispObject *key = lisp_car(args);
    LispObject *alist = lisp_car(lisp_cdr(args));

    /* Iterate through association list */
    while (alist != NIL && alist != NULL) {
        if (alist->type != LISP_CONS) {
            return lisp_make_error("assoc requires an association list");
        }

        LispObject *pair = lisp_car(alist);
        if (pair != NIL && pair->type == LISP_CONS) {
            LispObject *pair_key = lisp_car(pair);
            if (objects_equal_recursive(key, pair_key)) {
                return pair;
            }
        }

        alist = lisp_cdr(alist);
    }

    return NIL;
}

static LispObject *builtin_assq(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("assq requires 2 arguments");
    }

    LispObject *key = lisp_car(args);
    LispObject *alist = lisp_car(lisp_cdr(args));

    /* Iterate through association list using pointer equality (eq) */
    while (alist != NIL && alist != NULL) {
        if (alist->type != LISP_CONS) {
            return lisp_make_error("assq requires an association list");
        }

        LispObject *pair = lisp_car(alist);
        if (pair != NIL && pair->type == LISP_CONS) {
            LispObject *pair_key = lisp_car(pair);
            if (key == pair_key) {
                return pair;
            }
        }

        alist = lisp_cdr(alist);
    }

    return NIL;
}

static LispObject *builtin_assv(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("assv requires 2 arguments");
    }

    LispObject *key = lisp_car(args);
    LispObject *alist = lisp_car(lisp_cdr(args));

    /* Iterate through association list using eqv equality (same as assoc for our purposes) */
    while (alist != NIL && alist != NULL) {
        if (alist->type != LISP_CONS) {
            return lisp_make_error("assv requires an association list");
        }

        LispObject *pair = lisp_car(alist);
        if (pair != NIL && pair->type == LISP_CONS) {
            LispObject *pair_key = lisp_car(pair);
            if (objects_equal_recursive(key, pair_key)) {
                return pair;
            }
        }

        alist = lisp_cdr(alist);
    }

    return NIL;
}

static LispObject *builtin_alist_get(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("alist-get requires at least 2 arguments");
    }

    LispObject *key = lisp_car(args);
    LispObject *alist = lisp_car(lisp_cdr(args));
    LispObject *default_val = NIL;

    /* Optional third argument is default value */
    if (lisp_cdr(lisp_cdr(args)) != NIL) {
        default_val = lisp_car(lisp_cdr(lisp_cdr(args)));
    }

    /* Iterate through association list */
    while (alist != NIL && alist != NULL) {
        if (alist->type != LISP_CONS) {
            return lisp_make_error("alist-get requires an association list");
        }

        LispObject *pair = lisp_car(alist);
        if (pair != NIL && pair->type == LISP_CONS) {
            LispObject *pair_key = lisp_car(pair);
            if (objects_equal_recursive(key, pair_key)) {
                return lisp_cdr(pair);
            }
        }

        alist = lisp_cdr(alist);
    }

    return default_val;
}

/* Equality predicates */

static LispObject *builtin_eq_predicate(LispObject *args, Environment *env) {
    (void)env;
    /* Validate exactly 2 arguments */
    if (args == NIL || args->value.cons.cdr == NIL || args->value.cons.cdr->value.cons.cdr != NIL) {
        return lisp_make_error("eq? expects exactly 2 arguments");
    }

    LispObject *a = args->value.cons.car;
    LispObject *b = args->value.cons.cdr->value.cons.car;

    /* Pointer equality - same object in memory */
    return (a == b) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_equal_predicate(LispObject *args, Environment *env) {
    (void)env;
    /* Validate exactly 2 arguments */
    if (args == NIL || args->value.cons.cdr == NIL || args->value.cons.cdr->value.cons.cdr != NIL) {
        return lisp_make_error("equal? expects exactly 2 arguments");
    }

    LispObject *a = args->value.cons.car;
    LispObject *b = args->value.cons.cdr->value.cons.car;

    /* Use recursive structural equality */
    return objects_equal_recursive(a, b) ? lisp_make_number(1) : NIL;
}

static LispObject *builtin_string_eq_predicate(LispObject *args, Environment *env) {
    (void)env;
    /* Validate exactly 2 arguments */
    if (args == NIL || args->value.cons.cdr == NIL || args->value.cons.cdr->value.cons.cdr != NIL) {
        return lisp_make_error("string=? expects exactly 2 arguments");
    }

    LispObject *a = args->value.cons.car;
    LispObject *b = args->value.cons.cdr->value.cons.car;

    /* Type validation */
    if (a->type != LISP_STRING) {
        return lisp_make_error("string=?: first argument must be a string");
    }
    if (b->type != LISP_STRING) {
        return lisp_make_error("string=?: second argument must be a string");
    }

    /* String comparison */
    return (strcmp(a->value.string, b->value.string) == 0) ? lisp_make_number(1) : NIL;
}

/* Mapping operations */

static LispObject *builtin_map(LispObject *args, Environment *env) {
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("map requires at least 2 arguments");
    }

    LispObject *func = lisp_car(args);
    LispObject *list = lisp_car(lisp_cdr(args));

    if (func->type != LISP_BUILTIN && func->type != LISP_LAMBDA) {
        return lisp_make_error("map requires a function as first argument");
    }

    LispObject *result = NIL;
    LispObject *tail = NULL;

    while (list != NIL && list != NULL) {
        if (list->type != LISP_CONS) {
            return lisp_make_error("map requires a list");
        }

        LispObject *item = lisp_car(list);
        LispObject *func_args = lisp_make_cons(item, NIL);

        LispObject *mapped;
        if (func->type == LISP_BUILTIN) {
            mapped = func->value.builtin.func(func_args, env);
        } else {
            /* Lambda function - manually apply it */
            Environment *lambda_env = env_create(func->value.lambda.closure);

            /* Bind parameter to argument */
            LispObject *params = func->value.lambda.params;
            if (params == NIL || params->type != LISP_CONS) {
                return lisp_make_error("map: lambda must have at least one parameter");
            }

            LispObject *param = lisp_car(params);
            if (param->type != LISP_SYMBOL) {
                return lisp_make_error("map: lambda parameter must be a symbol");
            }

            /* Bind the parameter to the item */
            env_define(lambda_env, param->value.symbol, item);

            /* Check for extra parameters (should only have one for map) */
            if (lisp_cdr(params) != NIL) {
                return lisp_make_error("map: lambda should take exactly one argument");
            }

            /* Evaluate lambda body */
            LispObject *body = func->value.lambda.body;
            mapped = NIL;
            while (body != NIL && body != NULL) {
                mapped = lisp_eval(lisp_car(body), lambda_env);
                if (mapped->type == LISP_ERROR) {
                    env_free(lambda_env);
                    return mapped;
                }
                body = lisp_cdr(body);
            }

            env_free(lambda_env);
        }

        if (mapped->type == LISP_ERROR) {
            return mapped;
        }

        LispObject *new_cons = lisp_make_cons(mapped, NIL);
        if (result == NIL) {
            result = new_cons;
            tail = new_cons;
        } else {
            tail->value.cons.cdr = new_cons;
            tail = new_cons;
        }

        list = lisp_cdr(list);
    }

    return result;
}

static LispObject *builtin_mapcar(LispObject *args, Environment *env) {
    /* mapcar is the same as map in this implementation */
    return builtin_map(args, env);
}

/* Error introspection and handling functions */

/* error? - Test if object is an error */
static LispObject *builtin_error_question(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_boolean(0);
    }
    LispObject *obj = lisp_car(args);
    return lisp_make_boolean(obj->type == LISP_ERROR);
}

/* error-type - Get error type symbol */
static LispObject *builtin_error_type(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_typed_error_simple("wrong-number-of-arguments", "error-type requires 1 argument", env);
    }
    LispObject *obj = lisp_car(args);
    if (obj->type != LISP_ERROR) {
        return lisp_make_typed_error_simple("wrong-type-argument", "error-type: argument must be an error", env);
    }
    /* Return error type (guaranteed to be a symbol) */
    if (obj->value.error_with_stack.error_type != NULL) {
        return obj->value.error_with_stack.error_type;
    }
    /* Fallback to 'error symbol if somehow NULL */
    return sym_error;
}

/* error-message - Get error message string */
static LispObject *builtin_error_message(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_typed_error_simple("wrong-number-of-arguments", "error-message requires 1 argument", env);
    }
    LispObject *obj = lisp_car(args);
    if (obj->type != LISP_ERROR) {
        return lisp_make_typed_error_simple("wrong-type-argument", "error-message: argument must be an error", env);
    }
    return lisp_make_string(obj->value.error_with_stack.message);
}

/* error-stack - Get error stack trace */
static LispObject *builtin_error_stack(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_typed_error_simple("wrong-number-of-arguments", "error-stack requires 1 argument", env);
    }
    LispObject *obj = lisp_car(args);
    if (obj->type != LISP_ERROR) {
        return lisp_make_typed_error_simple("wrong-type-argument", "error-stack: argument must be an error", env);
    }
    LispObject *stack = obj->value.error_with_stack.stack_trace;
    return (stack != NULL) ? stack : NIL;
}

/* error-data - Get error data payload */
static LispObject *builtin_error_data(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_typed_error_simple("wrong-number-of-arguments", "error-data requires 1 argument", env);
    }
    LispObject *obj = lisp_car(args);
    if (obj->type != LISP_ERROR) {
        return lisp_make_typed_error_simple("wrong-type-argument", "error-data: argument must be an error", env);
    }
    LispObject *data = obj->value.error_with_stack.data;
    return (data != NULL) ? data : NIL;
}

/* signal - Raise a typed error
 * (signal ERROR-SYMBOL DATA)
 */
static LispObject *builtin_signal(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_typed_error_simple("wrong-number-of-arguments", "signal requires at least 1 argument", env);
    }

    LispObject *error_type = lisp_car(args);
    if (error_type->type != LISP_SYMBOL) {
        return lisp_make_typed_error_simple("wrong-type-argument", "signal: first argument must be a symbol", env);
    }

    /* Get optional data argument */
    LispObject *rest = lisp_cdr(args);
    LispObject *data = (rest != NIL && rest != NULL) ? lisp_car(rest) : NIL;

    /* Build error message from data */
    char message[512];
    if (data != NIL && data->type == LISP_STRING) {
        snprintf(message, sizeof(message), "%s", data->value.string);
    } else if (data != NIL) {
        char *data_str = lisp_print(data);
        snprintf(message, sizeof(message), "%s: %s", error_type->value.symbol, data_str);
    } else {
        snprintf(message, sizeof(message), "%s", error_type->value.symbol);
    }

    return lisp_make_typed_error(error_type, message, data, env);
}

/* error - Convenience function to signal generic error
 * (error MESSAGE)
 */
static LispObject *builtin_error(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_typed_error_simple("wrong-number-of-arguments", "error requires 1 argument", env);
    }

    LispObject *message_obj = lisp_car(args);
    const char *message;

    if (message_obj->type == LISP_STRING) {
        message = message_obj->value.string;
    } else {
        message = lisp_print(message_obj);
    }

    return lisp_make_typed_error_simple("error", message, env);
}

/* lambda-docstring - Get docstring from lambda object
 * (lambda-docstring LAMBDA)
 * Returns the documentation string of a lambda, or nil if none exists.
 */
static LispObject *builtin_lambda_docstring(LispObject *args, Environment *env) {
    (void)env;

    if (args == NIL || lisp_cdr(args) != NIL) {
        return lisp_make_error("lambda-docstring requires exactly 1 argument");
    }

    LispObject *func = lisp_car(args);

    if (func->type != LISP_LAMBDA) {
        return lisp_make_error("lambda-docstring requires a lambda");
    }

    if (func->value.lambda.docstring == NULL) {
        return NIL;
    }

    return lisp_make_string(func->value.lambda.docstring);
}

/* macro-docstring - Get docstring from macro object
 * (macro-docstring MACRO)
 * Returns the documentation string of a macro, or nil if none exists.
 */
static LispObject *builtin_macro_docstring(LispObject *args, Environment *env) {
    (void)env;

    if (args == NIL || lisp_cdr(args) != NIL) {
        return lisp_make_error("macro-docstring requires exactly 1 argument");
    }

    LispObject *macro = lisp_car(args);

    if (macro->type != LISP_MACRO) {
        return lisp_make_error("macro-docstring requires a macro");
    }

    if (macro->value.macro.docstring == NULL) {
        return NIL;
    }

    return lisp_make_string(macro->value.macro.docstring);
}

/* documentation - Get documentation string for a symbol
 * (documentation SYMBOL)
 * Returns the documentation string of the function or macro bound to SYMBOL,
 * or nil if none exists or if the symbol is unbound.
 * Similar to Emacs Lisp's documentation function.
 */
static LispObject *builtin_documentation(LispObject *args, Environment *env) {
    if (args == NIL) {
        return lisp_make_error("documentation requires at least 1 argument");
    }

    LispObject *symbol = lisp_car(args);

    if (symbol->type != LISP_SYMBOL) {
        return lisp_make_error("documentation requires a symbol");
    }

    /* Look up the symbol's value */
    LispObject *value = env_lookup(env, symbol->value.symbol);

    if (value == NULL) {
        return lisp_make_error("Undefined symbol");
    }

    /* Return docstring based on type */
    if (value->type == LISP_LAMBDA) {
        if (value->value.lambda.docstring == NULL) {
            return NIL;
        }
        return lisp_make_string(value->value.lambda.docstring);
    } else if (value->type == LISP_MACRO) {
        if (value->value.macro.docstring == NULL) {
            return NIL;
        }
        return lisp_make_string(value->value.macro.docstring);
    } else if (value->type == LISP_BUILTIN) {
        if (value->value.builtin.docstring == NULL) {
            return NIL;
        }
        return lisp_make_string(value->value.builtin.docstring);
    }

    return NIL;
}
