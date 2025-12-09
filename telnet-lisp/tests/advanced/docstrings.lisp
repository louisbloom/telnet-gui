					; Docstring support tests
					; Tests documentation string extraction for lambdas and macros

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

(lambda-docstring anon) ; => "Multiply X by Y."

					; Multi-line docstring (CommonMark format)
(define format-text
  (lambda (text)
    "Format text using CommonMark.

    ## Usage

    - Pass a string
    - Returns formatted version

    **Note**: This is just an example."
    (concat "Formatted: " text)))

(documentation 'format-text) ; => "Format text using CommonMark.\n\n    ## Usage\n\n    - Pass a string\n    - Returns formatted version\n\n    **Note**: This is just an example."

					; ===========================================
					; Macro Docstrings
					; ===========================================

(defmacro when (condition . body)
  "Execute BODY when CONDITION is true.

  This is a simple conditional macro."
  `(if ,condition (progn ,@body) nil))

(documentation 'when) ; => "Execute BODY when CONDITION is true.\n\n  This is a simple conditional macro."

					; Macro without docstring
(defmacro unless (condition . body)
  `(if ,condition nil (progn ,@body)))

(documentation 'unless) ; => nil

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

(lambda-docstring times-5) ; => "Inner lambda - multiply X by captured factor."

(times-5 10) ; => 50

					; ===========================================
					; Verify Functions Work Correctly
					; ===========================================

					; Lambda with docstring still works
(add-nums 3 4) ; => 7

					; Macro with docstring still works
(when 1
  (+ 1 2)
  (+ 3 4)) ; => 7

(when nil
  (+ 1 2)) ; => nil

					; Lambda without docstring still works
(no-doc 5) ; => 10

					; Macro without docstring still works
(unless nil
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

					; lambda-docstring requires lambda
(condition-case err
  (lambda-docstring "not-a-lambda")
  (error 1)) ; => 1

					; lambda-docstring on lambda without docstring returns nil
(lambda-docstring no-doc) ; => nil

					; macro-docstring requires macro
(condition-case err
  (macro-docstring "not-a-macro")
  (error 1)) ; => 1

					; macro-docstring on macro without docstring returns nil
(macro-docstring unless) ; => nil
