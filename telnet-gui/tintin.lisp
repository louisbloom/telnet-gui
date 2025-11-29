;; TinTin++ Implementation - Test-Driven Development
;; Building incrementally, one test at a time

;; ============================================================================
;; DATA STRUCTURES
;; ============================================================================

(define *tintin-aliases* (make-hash-table))
(define *tintin-variables* (make-hash-table))
(define *tintin-speedwalk-enabled* #t)

;; ============================================================================
;; UTILITY FUNCTIONS
;; NOTE: These functions should ideally be built into telnet-lisp natively
;; ============================================================================

;; Convert string of digits to number (should be native: string->number)
(defun tintin-string->number (str)
  (if (not (string? str))
      0
      (let ((num 0))
	(do ((i 0 (+ i 1)))
	    ((>= i (string-length str)) num)
	  (let ((digit-ch (substring str i (+ i 1))))
	    (set! num (+ (* num 10)
			 (cond
			   ((string= digit-ch "0") 0)
			   ((string= digit-ch "1") 1)
			   ((string= digit-ch "2") 2)
			   ((string= digit-ch "3") 3)
			   ((string= digit-ch "4") 4)
			   ((string= digit-ch "5") 5)
			   ((string= digit-ch "6") 6)
			   ((string= digit-ch "7") 7)
			   ((string= digit-ch "8") 8)
			   ((string= digit-ch "9") 9)
			   (#t 0)))))))))

;; Reverse a list (should be native: reverse)
(defun tintin-reverse-list (lst)
  (let ((result '()))
    (do ((i 0 (+ i 1)))
        ((>= i (list-length lst)) result)
      (set! result (cons (list-ref lst i) result)))))

;; ============================================================================
;; TEST 1: COMMAND SEPARATOR
;; ============================================================================

(defun tintin-split-commands (str)
  (if (not (string? str))
      '()
      (split str ";")))

;; ============================================================================
;; TEST 2: SPEEDWALK
;; ============================================================================

;; Check if character is a digit
(defun tintin-is-digit? (ch)
  (and (string? ch)
       (= (string-length ch) 1)
       (or (string= ch "0") (string= ch "1") (string= ch "2") (string= ch "3")
	   (string= ch "4") (string= ch "5") (string= ch "6") (string= ch "7")
	   (string= ch "8") (string= ch "9"))))

;; Check if a string is a valid direction
(defun tintin-is-direction? (str)
  (or (string= str "n") (string= str "e") (string= str "s") (string= str "w")
      (string= str "u") (string= str "d")
      (string= str "ne") (string= str "nw") (string= str "se") (string= str "sw")))

;; Expand speedwalk string like "3n2e" to "n;n;n;e;e"
(defun tintin-expand-speedwalk (input)
  (if (or (not (string? input)) (not *tintin-speedwalk-enabled*))
      input
      (let ((len (string-length input))
	    (pos 0)
	    (result '()))
	(do ()
	    ((>= pos len))
	  (let ((count-str "")
		(direction ""))
	    ;; Collect digits for count
	    (do ()
		((or (>= pos len) (not (tintin-is-digit? (substring input pos (+ pos 1)))))
		 nil)
	      (set! count-str (concat count-str (substring input pos (+ pos 1))))
	      (set! pos (+ pos 1)))

	    ;; Get direction (1 or 2 characters)
	    (if (< pos len)
		(let ((ch1 (substring input pos (+ pos 1))))
		  ;; Try 2-char direction first
		  (if (and (< (+ pos 1) len)
			   (tintin-is-direction? (concat ch1 (substring input (+ pos 1) (+ pos 2)))))
		      (progn
			(set! direction (concat ch1 (substring input (+ pos 1) (+ pos 2))))
			(set! pos (+ pos 2)))
		      ;; Try 1-char direction
		      (if (tintin-is-direction? ch1)
			  (progn
			    (set! direction ch1)
			    (set! pos (+ pos 1)))
			  ;; Not a valid direction, skip it
			  (set! pos (+ pos 1))))))

	    ;; Expand direction N times
	    (if (not (string= direction ""))
		(let ((count (if (string= count-str "") 1 (tintin-string->number count-str))))
		  (do ((i 0 (+ i 1)))
		      ((>= i count))
		    (set! result (cons direction result)))))))

	;; Join results with semicolons
	(let ((reversed (tintin-reverse-list result))
	      (output ""))
	  (do ((i 0 (+ i 1)))
	      ((>= i (list-length reversed)) output)
	    (set! output (concat output
				 (if (> i 0) ";" "")
				 (list-ref reversed i))))))))

;; ============================================================================
;; TEST 3: ALIAS CREATION
;; ============================================================================

;; Extract text between braces
(defun tintin-extract-braced (str start-pos)
  (if (>= start-pos (string-length str))
      nil
      (let ((pos start-pos)
	    (len (string-length str)))
	;; Find opening brace
	(do ()
	    ((or (>= pos len) (string= (string-ref str pos) "{"))
	     (if (>= pos len)
		 nil
		 ;; Extract until closing brace
		 (let ((depth 1)
		       (start (+ pos 1))
		       (end-pos (+ pos 1)))
		   (do ()
		       ((or (>= end-pos len) (= depth 0))
			(if (= depth 0)
			    (cons (substring str start (- end-pos 1)) end-pos)
			    nil))
		     (let ((ch (string-ref str end-pos)))
		       (if (string= ch "{")
			   (set! depth (+ depth 1))
			   (if (string= ch "}")
			       (set! depth (- depth 1))))
		       (set! end-pos (+ end-pos 1)))))))
	  (set! pos (+ pos 1))))))

;; Parse #alias {name} {commands} {priority}
(defun tintin-parse-alias (input)
  (if (not (string-prefix? "#alias " input))
      nil
      (let ((name-data (tintin-extract-braced input 7)))
	(if (not name-data)
	    nil
	    (let ((name (car name-data))
		  (cmd-data (tintin-extract-braced input (cdr name-data))))
	      (if (not cmd-data)
		  nil
		  (let ((commands (car cmd-data))
			(priority-data (tintin-extract-braced input (cdr cmd-data))))
		    (let ((priority (if priority-data
					(string->number (car priority-data))
					5)))
		      (list name commands priority)))))))))

;; Match a pattern against input and extract placeholder values
;; Returns list of extracted values or nil if no match
;; Example: (tintin-match-pattern "k %1 with %2" "k orc with sword") => ("orc" "sword")
(defun tintin-match-pattern (pattern input)
  (let ((pattern-parts (split pattern " "))
        (input-parts (split input " ")))
    (if (not (= (list-length pattern-parts) (list-length input-parts)))
	nil
	(let ((matches '())
	      (success #t))
	  (do ((i 0 (+ i 1)))
	      ((or (>= i (list-length pattern-parts)) (not success))
	       (if success (tintin-reverse-list matches) nil))
	    (let ((p-part (list-ref pattern-parts i))
		  (i-part (list-ref input-parts i)))
	      (if (string-prefix? "%" p-part)
		  ;; Placeholder - capture the value
		  (set! matches (cons i-part matches))
		  ;; Literal - must match exactly
		  (if (not (string= p-part i-part))
		      (set! success #f)))))))))

;; Parse #variable {name} {value}
(defun tintin-parse-variable (input)
  (if (not (string-prefix? "#variable " input))
      nil
      (let ((name-data (tintin-extract-braced input 10)))
	(if (not name-data)
	    nil
	    (let ((name (car name-data))
		  (value-data (tintin-extract-braced input (cdr name-data))))
	      (if (not value-data)
		  nil
		  (list name (car value-data))))))))

;; Expand $variable references in a string
(defun tintin-expand-variables (str)
  (if (not (string? str))
      str
      (let ((result str)
	    (var-names (hash-keys *tintin-variables*)))
	;; Replace each variable
	(do ((i 0 (+ i 1)))
	    ((>= i (list-length var-names)) result)
	  (let* ((var-name (list-ref var-names i))
		 (var-value (hash-ref *tintin-variables* var-name)))
	    (set! result (string-replace (concat "$" var-name) var-value result)))))))

;; Process a single command
(defun tintin-process-command (cmd)
  (if (not (string? cmd))
      ""
      ;; First check for #variable command
      (let ((var-data (tintin-parse-variable cmd)))
	(if var-data
	    (progn
	      (let ((name (list-ref var-data 0))
		    (value (list-ref var-data 1)))
		(hash-set! *tintin-variables* name value)
		(if (symbol? 'terminal-echo)
		    (terminal-echo (concat "Variable '" name "' set to '" value "'\r\n"))))
	      "")
	    ;; Not a variable, check for #alias
	    (let ((alias-data (tintin-parse-alias cmd)))
	      (if alias-data
		  (progn
		    (let ((name (list-ref alias-data 0))
			  (commands (list-ref alias-data 1))
			  (priority (list-ref alias-data 2)))
		      (hash-set! *tintin-aliases* name (list commands priority))
		      (if (symbol? 'terminal-echo)
			  (terminal-echo (concat "Alias '" name "' created (priority: "
						 (number->string priority) ")\r\n"))))
		    "")
		  ;; Not alias or variable creation - expand variables first, then check aliases
		  (let ((expanded-cmd (tintin-expand-variables cmd)))
		    (let ((words (split expanded-cmd " "))
			  (first-word (car (split expanded-cmd " "))))
		      (let ((alias-entry (hash-ref *tintin-aliases* first-word)))
			(if alias-entry
			    ;; Simple match - expand alias
			    (let ((template (car alias-entry))
				  (args (cdr words)))
			      (let ((result template))
				;; First, replace %0 with all arguments
				(if (> (list-length args) 0)
				    (let ((all-args ""))
				      (do ((i 0 (+ i 1)))
					  ((>= i (list-length args)))
					(set! all-args (concat all-args
							       (if (> i 0) " " "")
							       (list-ref args i))))
				      (set! result (string-replace "%0" all-args result))))
				;; Then replace %1, %2, etc. with individual args
				(do ((i 0 (+ i 1)))
				    ((>= i (list-length args)) result)
				  (set! result (string-replace
						(concat "%" (number->string (+ i 1)))
						(list-ref args i)
						result)))))
			    ;; No simple match - try pattern matching
			    (let ((alias-names (hash-keys *tintin-aliases*))
				  (matched-result nil))
			      (do ((i 0 (+ i 1)))
				  ((or (>= i (list-length alias-names)) matched-result)
				   (if matched-result matched-result expanded-cmd))
				(let* ((pattern (list-ref alias-names i))
				       (match-values (tintin-match-pattern pattern expanded-cmd)))
				  (if match-values
				      (let ((alias-data (hash-ref *tintin-aliases* pattern)))
					(let* ((template (car alias-data))
					       (result template))
					  ;; Replace %1, %2, etc. with matched values
					  (do ((j 0 (+ j 1)))
					      ((>= j (list-length match-values)))
					    (set! result (string-replace
							  (concat "%" (number->string (+ j 1)))
							  (list-ref match-values j)
							  result)))
					  (set! matched-result result)))))))))))))))))

;; ============================================================================
;; TEST 7: FULL INPUT PROCESSING
;; ============================================================================

;; Process a full input line (split by semicolons, process each command)
(defun tintin-process-input (input)
  (if (not (string? input))
      ""
      (let ((commands (tintin-split-commands input))
	    (results '()))
	;; Process each command and collect results
	(do ((i 0 (+ i 1)))
	    ((>= i (list-length commands)))
	  (let ((processed (tintin-process-command (list-ref commands i))))
	    (if (not (string= processed ""))
		(set! results (cons processed results)))))
	;; Reverse and join results with semicolons
	(let ((reversed-results (tintin-reverse-list results))
	      (output ""))
	  (do ((i 0 (+ i 1)))
	      ((>= i (list-length reversed-results)) output)
	    (set! output (concat output
				 (if (> i 0) ";" "")
				 (list-ref reversed-results i))))))))
