;; Test for load built-in function
;;
;; Tests loading and evaluating Lisp files

(load "tests/test-helpers.lisp")

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
    (assert-equal (load "load_test1.lisp") 6 "load single expression file")

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
    (assert-equal (load "load_test2.lisp") 52 "load multiple expressions file")

    ;; Verify variables were defined
    (assert-equal x 42 "variable x defined from loaded file")
    (assert-equal y 10 "variable y defined from loaded file")

    ;; ============================================
    ;; Test 3: Load a file that defines functions
    ;; ============================================

    ;; Create a test file with function definitions
    (define test_file3 (open "load_test3.lisp" "w"))
    (write-line test_file3 "(define square (lambda (n) (* n n)))")
    (write-line test_file3 "(square 5)")
    (close test_file3)

    ;; Load and evaluate
    (assert-equal (load "load_test3.lisp") 25 "load function definition file")

    ;; Verify function was defined
    (assert-equal (square 7) 49 "function defined from loaded file")

    ;; ============================================
    ;; Test 4: Load a file with nested expressions
    ;; ============================================

    ;; Create a test file with nested expressions
    (define test_file4 (open "load_test4.lisp" "w"))
    (write-line test_file4 "(define result (+ (* 2 3) (* 4 5)))")
    (write-line test_file4 "result")
    (close test_file4)

    ;; Load and evaluate
    (assert-equal (load "load_test4.lisp") 26 "load nested expressions file")

    ;; Verify variable was defined
    (assert-equal result 26 "variable result defined from loaded file")

    ;; ============================================
    ;; Test 5: Load a file that returns a string
    ;; ============================================

    ;; Create a test file that returns a string
    (define test_file5 (open "load_test5.lisp" "w"))
    (write-line test_file5 "(concat \"Hello\" \" \" \"World\")")
    (close test_file5)

    ;; Load and evaluate
    (assert-equal (load "load_test5.lisp") "Hello World" "load file returning string")

    ;; ============================================
    ;; Test 6: Load a file with empty result
    ;; ============================================

    ;; Create a test file with only definitions (no return value)
    (define test_file6 (open "load_test6.lisp" "w"))
    (write-line test_file6 "(define z 100)")
    (close test_file6)

    ;; Load and evaluate - should return value of last expression
    (assert-equal (load "load_test6.lisp") 100 "load file with define returns last value")

    ;; Verify variable was defined
    (assert-equal z 100 "variable z defined from loaded file")

    ;; ============================================
    ;; Test 7: Load a file with list result
    ;; ============================================

    ;; Create a test file that returns a list
    (define test_file7 (open "load_test7.lisp" "w"))
    (write-line test_file7 "(list 1 2 3 4 5)")
    (close test_file7)

    ;; Load and evaluate
    (assert-equal (load "load_test7.lisp") '(1 2 3 4 5) "load file returning list")

    ;; ============================================
    ;; Test 8: Error handling - non-existent file
    ;; ============================================

    ;; Try to load a non-existent file - should return error
    (assert-error (load "nonexistent_file.lisp") "load non-existent file")

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
    (assert-equal (load "main.lisp") 1000 "load file that loads another file")

    ;; Verify helper variable was defined
    (assert-equal helper_var 999 "helper variable defined")

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
    (assert-equal (load "load_test10.lisp") 42 "load file with comments")

    ;; Verify variable was defined
    (assert-equal commented 42 "variable commented defined")
    )
  ;; ============================================
  ;; Cleanup: Delete all temporary test files
  ;; This runs regardless of errors in the tests above
  ;; ============================================
  (progn
    (file-delete "load_test1.lisp")
    (file-delete "load_test2.lisp")
    (file-delete "load_test3.lisp")
    (file-delete "load_test4.lisp")
    (file-delete "load_test5.lisp")
    (file-delete "load_test6.lisp")
    (file-delete "load_test7.lisp")
    (file-delete "load_test10.lisp")
    (file-delete "helper.lisp")
    (file-delete "main.lisp")))
