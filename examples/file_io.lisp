;; File I/O examples using standard Lisp functions

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
