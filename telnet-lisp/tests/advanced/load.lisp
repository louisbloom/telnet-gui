;; Test for load built-in function
;;
;; Tests loading and evaluating Lisp files

(unwind-protect
  (progn
    ;; ============================================
    ;; Test 1: Load a file with a single expression
    ;; ============================================

    ;; Create a test file with a single expression
    (define test_file1 (open "load_test1.lisp" "w"))
    (write-line test_file1 "(+ 1 2 3)")
    (close test_file1)

    ;; Load and evaluate the file
    (load "load_test1.lisp")  ; => 6

    ;; ============================================
    ;; Test 2: Load a file with multiple expressions
    ;; ============================================

    ;; Create a test file with multiple expressions
    (define test_file2 (open "load_test2.lisp" "w"))
    (write-line test_file2 "(define x 42)")
    (write-line test_file2 "(define y 10)")
    (write-line test_file2 "(+ x y)")
    (close test_file2)

    ;; Load and evaluate - should return result of last expression
    (load "load_test2.lisp")  ; => 52

    ;; Verify variables were defined
    x  ; => 42
    y  ; => 10

    ;; ============================================
    ;; Test 3: Load a file that defines functions
    ;; ============================================

    ;; Create a test file with function definitions
    (define test_file3 (open "load_test3.lisp" "w"))
    (write-line test_file3 "(define square (lambda (n) (* n n)))")
    (write-line test_file3 "(square 5)")
    (close test_file3)

    ;; Load and evaluate
    (load "load_test3.lisp")  ; => 25

    ;; Verify function was defined
    (square 7)  ; => 49

    ;; ============================================
    ;; Test 4: Load a file with nested expressions
    ;; ============================================

    ;; Create a test file with nested expressions
    (define test_file4 (open "load_test4.lisp" "w"))
    (write-line test_file4 "(define result (+ (* 2 3) (* 4 5)))")
    (write-line test_file4 "result")
    (close test_file4)

    ;; Load and evaluate
    (load "load_test4.lisp")  ; => 26

    ;; Verify variable was defined
    result  ; => 26

    ;; ============================================
    ;; Test 5: Load a file that returns a string
    ;; ============================================

    ;; Create a test file that returns a string
    (define test_file5 (open "load_test5.lisp" "w"))
    (write-line test_file5 "(concat \"Hello\" \" \" \"World\")")
    (close test_file5)

    ;; Load and evaluate
    (load "load_test5.lisp")  ; => "Hello World"

    ;; ============================================
    ;; Test 6: Load a file with empty result
    ;; ============================================

    ;; Create a test file with only definitions (no return value)
    (define test_file6 (open "load_test6.lisp" "w"))
    (write-line test_file6 "(define z 100)")
    (close test_file6)

    ;; Load and evaluate - should return nil or last expression result
    (load "load_test6.lisp")  ; => nil (or the result of define, which is typically nil)

    ;; Verify variable was defined
    z  ; => 100

    ;; ============================================
    ;; Test 7: Load a file with list result
    ;; ============================================

    ;; Create a test file that returns a list
    (define test_file7 (open "load_test7.lisp" "w"))
    (write-line test_file7 "(list 1 2 3 4 5)")
    (close test_file7)

    ;; Load and evaluate
    (load "load_test7.lisp")  ; => (1 2 3 4 5)

    ;; ============================================
    ;; Test 8: Error handling - non-existent file
    ;; ============================================

    ;; Try to load a non-existent file - should return error
    (load "nonexistent_file.lisp")  ; => ERROR: Cannot open file

    ;; ============================================
    ;; Test 9: Load a file that loads another file
    ;; ============================================

    ;; Create a helper file
    (define helper_file (open "helper.lisp" "w"))
    (write-line helper_file "(define helper_var 999)")
    (write-line helper_file "helper_var")
    (close helper_file)

    ;; Create a main file that loads the helper
    (define main_file (open "main.lisp" "w"))
    (write-line main_file "(load \"helper.lisp\")")
    (write-line main_file "(+ helper_var 1)")
    (close main_file)

    ;; Load the main file
    (load "main.lisp")  ; => 1000

    ;; Verify helper variable was defined
    helper_var  ; => 999

    ;; ============================================
    ;; Test 10: Load a file with comments
    ;; ============================================

    ;; Create a test file with comments
    (define test_file10 (open "load_test10.lisp" "w"))
    (write-line test_file10 "; This is a comment")
    (write-line test_file10 "(define commented 42)")
    (write-line test_file10 "; Another comment")
    (write-line test_file10 "commented")
    (close test_file10)

    ;; Load and evaluate - comments should be ignored
    (load "load_test10.lisp")  ; => 42

    ;; Verify variable was defined
    commented  ; => 42
  )
  ;; ============================================
  ;; Cleanup: Delete all temporary test files
  ;; This runs regardless of errors in the tests above
  ;; ============================================
  (progn
    (delete-file "load_test1.lisp")   ; ignore
    (delete-file "load_test2.lisp")   ; ignore
    (delete-file "load_test3.lisp")   ; ignore
    (delete-file "load_test4.lisp")   ; ignore
    (delete-file "load_test5.lisp")   ; ignore
    (delete-file "load_test6.lisp")   ; ignore
    (delete-file "load_test7.lisp")   ; ignore
    (delete-file "load_test10.lisp")  ; ignore
    (delete-file "helper.lisp")       ; ignore
    (delete-file "main.lisp")         ; ignore
  )
)
