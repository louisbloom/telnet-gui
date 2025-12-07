;; File I/O examples using standard Lisp functions

(load "tests/test-helpers.lisp")

;; Test files will be cleaned up using unwind-protect
(unwind-protect
  (progn
    ;; ============================================
    ;; Example 1: Writing to a file
    ;; ============================================

    ;; Open a file for writing
    (define logfile (open "output.log" "w"))

    ;; Write multiple lines
    (write-line logfile "Log entry 1")
    (write-line logfile "Log entry 2")
    (write-line logfile "Log entry 3")

    ;; Always close when done
    (close logfile)

    ;; ============================================
    ;; Example 2: Reading from a file
    ;; ============================================

    ;; Open for reading
    (define datafile (open "output.log" "r"))

    ;; Read lines one by one
    (assert-equal (read-line datafile) "Log entry 1" "read first line")
    (assert-equal (read-line datafile) "Log entry 2" "read second line")
    (assert-equal (read-line datafile) "Log entry 3" "read third line")
    (assert-nil (read-line datafile) "EOF returns nil")

    (close datafile)

    ;; ============================================
    ;; Example 3: Writing different modes
    ;; ============================================

    ;; Write mode (creates or overwrites)
    (define f1 (open "file1.txt" "w"))
    (write-line f1 "This creates or overwrites")
    (close f1)

    ;; Append mode
    (define f2 (open "file1.txt" "a"))
    (write-line f2 "This appends to the file")
    (close f2)

    ;; Read mode (default)
    (define f3 (open "file1.txt" "r"))
    (assert-equal (read-line f3) "This creates or overwrites" "read overwritten line")
    (assert-equal (read-line f3) "This appends to the file" "read appended line")
    (close f3)

    ;; ============================================
    ;; Example 6: Read S-expressions from file
    ;; ============================================

    ;; Create a test file with S-expressions
    (define sexp_file (open "test.lisp" "w"))
    (write-line sexp_file "(define x 42)")
    (write-line sexp_file "(define y 10)")
    (write-line sexp_file "(+ x y)")
    (close sexp_file)

    ;; Read all S-expressions from file
    (read-sexp "test.lisp")

    ;; Read from file stream
    (define file (open "test.lisp" "r"))
    (read-sexp file)
    (close file)

    ;; ============================================
    ;; Example 7: Read JSON from file
    ;; ============================================

    ;; Create a test JSON file
    (define json_file (open "test.json" "w"))
    (write-line json_file "{\"name\": \"Alice\", \"age\": 30, \"active\": true}")
    (close json_file)

    ;; Read JSON from file
    (define data (read-json "test.json"))
    (assert-equal (hash-ref data "name") "Alice" "JSON name field")
    (assert-equal (hash-ref data "age") 30 "JSON age field")
    (assert-true (hash-ref data "active") "JSON active field")

    ;; Read JSON from file stream
    (define file2 (open "test.json" "r"))
    (define data2 (read-json file2))
    (close file2)

    ;; Simple JSON values
    (define json_str_file (open "string.json" "w"))
    (write-line json_str_file "\"hello world\"")
    (close json_str_file)

    (assert-equal (read-json "string.json") "hello world" "JSON string value")

    (define json_num_file (open "number.json" "w"))
    (write-line json_num_file "42")
    (close json_num_file)

    (assert-equal (read-json "number.json") 42 "JSON number value")

    (define json_bool_file (open "bool.json" "w"))
    (write-line json_bool_file "true")
    (close json_bool_file)

    (assert-true (read-json "bool.json") "JSON boolean value"))

  ;; Cleanup code always runs
  (progn
    (file-delete "output.log")
    (file-delete "file1.txt")
    (file-delete "test.lisp")
    (file-delete "test.json")
    (file-delete "string.json")
    (file-delete "number.json")
    (file-delete "bool.json")))
