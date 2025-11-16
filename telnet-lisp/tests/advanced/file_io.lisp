;; File I/O examples using standard Lisp functions
;;
;; Note: File deletion is not yet supported.
;; Generated files (like file1.txt, output.log) from these tests
;; remain in the directory and are not cleaned up automatically.

;; ============================================
;; Example 1: Writing to a file
;; ============================================

;; Open a file for writing
(define logfile (open "output.log" "w"))

;; Write multiple lines
(write-line logfile "Log entry 1")
(write-line logfile "Log entry 2")
(write-line logfile "Log entry 3")

;; Note: output.log and file1.txt files are created but not deleted

;; Always close when done
(close logfile)

;; ============================================
;; Example 2: Reading from a file
;; ============================================

;; Open for reading
(define datafile (open "output.log" "r"))

;; Read lines one by one
(read-line datafile)  ; => "Log entry 1"
(read-line datafile)  ; => "Log entry 2"
(read-line datafile)  ; => "Log entry 3"
(read-line datafile) ; => nil (end of file)

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
(read-line f3)  ; => "This creates or overwrites"
(read-line f3)  ; => "This appends to the file"
(close f3)

;; ============================================
;; Example 4: Read entire file
;; ============================================

(define read_all (lambda (filename)
  (define file (open filename "r"))
  (define lines nil)

  (progn
    (define line (read-line file))
    (while (not (null line))  ; Would need while builtin
      (set! lines (cons line lines))
      (set! line (read-line file))))

  (close file)
  lines))  ; Returns list of lines

;; ============================================
;; Example 5: Copy file
;; ============================================

(define copy_file (lambda (src dest)
  (define infile (open src "r"))
  (define outfile (open dest "w"))

  (progn
    (define line (read-line infile))
    (while (not (null line))
      (write-line outfile line)
      (set! line (read-line infile))))

  (close infile)
  (close outfile)
  dest))

(copy_file "input.txt" "output.txt")

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
(read-sexp "test.lisp")  ; => Returns list of expressions or single expression

;; Read from file stream
(define file (open "test.lisp" "r"))
(read-sexp file)  ; => Returns list of expressions or single expression
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
(hash-ref data "name")   ; => "Alice"
(hash-ref data "age")    ; => 30
(hash-ref data "active") ; => 1 (true)

;; Read JSON from file stream
(define file2 (open "test.json" "r"))
(define data2 (read-json file2))
(close file2)

;; Simple JSON values
(define json_str_file (open "string.json" "w"))
(write-line json_str_file "\"hello world\"")
(close json_str_file)

(read-json "string.json")  ; => "hello world"

(define json_num_file (open "number.json" "w"))
(write-line json_num_file "42")
(close json_num_file)

(read-json "number.json")  ; => 42

(define json_bool_file (open "bool.json" "w"))
(write-line json_bool_file "true")
(close json_bool_file)

(read-json "bool.json")  ; => 1 (true)
