                                        ; Docstring support tests
                                        ; Tests documentation string extraction for lambdas, macros, and variables

                                        ; ===========================================
                                        ; Lambda Docstrings
                                        ; ===========================================

                                        ; Lambda with docstring
(define add-nums
  (lambda (x y)
    "Add X and Y together.

    Returns the sum of the two numbers."
    (+ x y)))

(documentation 'add-nums) ; => "Add X and Y together.\n\n    Returns the sum of the two numbers."

                                        ; Lambda without docstring
(define no-doc
  (lambda (x) (* x 2)))

(documentation 'no-doc) ; => nil

                                        ; Single-expression lambda (no docstring)
(define single-expr
  (lambda (x) (+ x 1)))

(documentation 'single-expr) ; => nil

                                        ; Anonymous lambda with docstring
(define anon
  (lambda (x y)
    "Multiply X by Y."
    (* x y)))

(documentation 'anon) ; => "Multiply X by Y."

                                        ; Multi-line docstring (CommonMark format)
(define format-text
  (lambda (text)
    "Format text using CommonMark.

    ## Usage

    - Pass a string
    - Returns formatted version

    **Note**: This is just an example."
    (string-append "Formatted: " text)))

(documentation 'format-text) ; => "Format text using CommonMark.\n\n    ## Usage\n\n    - Pass a string\n    - Returns formatted version\n\n    **Note**: This is just an example."

                                        ; ===========================================
                                        ; Macro Docstrings
                                        ; ===========================================

(defmacro when-test (condition . body)
  "Execute BODY when CONDITION is true.

  This is a simple conditional macro."
  `(if ,condition (progn ,@body) nil))

(documentation 'when-test) ; => "Execute BODY when CONDITION is true.\n\n  This is a simple conditional macro."

                                        ; Macro without docstring
(defmacro unless-test (condition . body)
  `(if ,condition nil (progn ,@body)))

(documentation 'unless-test) ; => nil

                                        ; ===========================================
                                        ; bound? tests
                                        ; ===========================================

(bound? 'car) ; => #t
(bound? 'nonexistent-symbol-xyz) ; => #f
(define bound-test-var 123)
(bound? 'bound-test-var) ; => #t

                                        ; ===========================================
                                        ; set-documentation! tests
                                        ; ===========================================

(define doc-test-var 42)
(set-documentation! 'doc-test-var "The answer to everything.")
(documentation 'doc-test-var) ; => "The answer to everything."

                                        ; ===========================================
                                        ; defvar tests
                                        ; ===========================================

(defvar defvar-test 100 "A test variable defined with defvar.")
defvar-test ; => 100
(documentation 'defvar-test) ; => "A test variable defined with defvar."

                                        ; defvar doesn't rebind if already bound
(defvar defvar-test 999 "New docstring")
defvar-test ; => 100

                                        ; defvar without docstring
(defvar defvar-no-doc 50)
defvar-no-doc ; => 50
(documentation 'defvar-no-doc) ; => nil

                                        ; defvar with no value defaults to nil
(defvar defvar-nil-test)
defvar-nil-test ; => nil

                                        ; ===========================================
                                        ; defconst tests
                                        ; ===========================================

(defconst const-test 3.14159 "Pi, the ratio of circumference to diameter.")
const-test ; => 3.14159
(documentation 'const-test) ; => "Pi, the ratio of circumference to diameter."

                                        ; defconst always rebinds (unlike defvar)
(defconst const-test 2.71828 "Euler's number")
const-test ; => 2.71828
(documentation 'const-test) ; => "Euler's number"

                                        ; ===========================================
                                        ; defalias tests
                                        ; ===========================================

(defalias my-add + "Alias for the + function.")
(my-add 1 2 3) ; => 6
(documentation 'my-add) ; => "Alias for the + function."

                                        ; ===========================================
                                        ; doc and doc-set! aliases
                                        ; ===========================================

(define alias-test 123)
(doc-set! 'alias-test "Set via doc-set! alias")
(doc 'alias-test) ; => "Set via doc-set! alias"

                                        ; ===========================================
                                        ; concat alias for string-append
                                        ; ===========================================

(concat "Hello, " "World!") ; => "Hello, World!"
(string-append "a" "b" "c") ; => "abc"

                                        ; ===========================================
                                        ; Edge Cases
                                        ; ===========================================

                                        ; String-only body (NOT a docstring - it's the return value)
(define return-string
  (lambda () "Just a string"))

(documentation 'return-string) ; => nil

(return-string) ; => "Just a string"

                                        ; Docstring preservation across closures
(define make-multiplier
  (lambda (factor)
    "Create a function that multiplies by FACTOR."
    (lambda (x)
      "Inner lambda - multiply X by captured factor."
      (* x factor))))

(documentation 'make-multiplier) ; => "Create a function that multiplies by FACTOR."

(define times-5 (make-multiplier 5))

(times-5 10) ; => 50

                                        ; ===========================================
                                        ; Verify Functions Work Correctly
                                        ; ===========================================

                                        ; Lambda with docstring still works
(add-nums 3 4) ; => 7

                                        ; Macro with docstring still works
(when-test 1
  (+ 1 2)
  (+ 3 4)) ; => 7

(when-test nil
  (+ 1 2)) ; => nil

                                        ; Lambda without docstring still works
(no-doc 5) ; => 10

                                        ; Macro without docstring still works
(unless-test nil
  (+ 10 20)) ; => 30

                                        ; ===========================================
                                        ; Error Cases (wrapped in condition-case)
                                        ; ===========================================

                                        ; documentation requires symbol
(condition-case err
  (documentation 42)
  (error 1)) ; => 1

                                        ; documentation on undefined symbol
(condition-case err
  (documentation 'undefined-symbol)
  (error 1)) ; => 1

                                        ; bound? requires symbol
(condition-case err
  (bound? 42)
  (error 1)) ; => 1

                                        ; set-documentation! requires symbol
(condition-case err
  (set-documentation! 42 "doc")
  (error 1)) ; => 1

                                        ; set-documentation! requires string
(condition-case err
  (set-documentation! 'doc-test-var 42)
  (error 1)) ; => 1

                                        ; set-documentation! requires bound symbol
(condition-case err
  (set-documentation! 'unbound-xyz "doc")
  (error 1)) ; => 1
