(in-package :c-parse)

(fiveam:in-suite* c-parse)

(defun run-tests ()
  (let ((results (fiveam:run 'c-parse)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(defun regex-character-class-to-esrap-liquid (regex)
  (lex-rule-dump (parse-with-garbage 'lex-rule-character-class regex)))
(fiveam:test what
  (fiveam:is (char= (c-parse-parse 'lex-char-or-escaped-char "\\\\")
		    #\\))
  (fiveam:is (char= (c-parse-parse 'lex-char-or-escaped-char "\\t")
		    #\Tab))
  (fiveam:is (string= (c-parse-parse 'lex-string "\"234\\t234\"")
		      "234	234"))
  (fiveam:is (string= (char-to-escaped-char #\Newline)
		      "\\n"))
  (fiveam:is (string= (char-to-escaped-char #\s)
		      "s"))
  (fiveam:is (char= (parse-with-garbage
		     (regex-character-class-to-esrap-liquid "[^a-zA-Z_0-9]")
		     "$"))
	     #\$)
  (fiveam:is (char= (parse-with-garbage
		     (regex-character-class-to-esrap-liquid "[a-zA-Z_0-9]")
		     "S"))
	     #\S))

(defun test-parse-rules (&optional (rules *lex-rules-lines*))
  (mapcar (lambda (text)
	    (parse-with-garbage 'lex-rule-start text))
	  rules))


;;run split-lex-2 to set the dynamic variables
(defun test-lines (&optional (rule 'lex-rule-start) (rules *lex-rules-lines*))
  (let ((correct 0)
	(wrong 0))
    (terpri)
    (mapc (lambda (text)
	    (let* ((obj (parse-with-garbage rule text))
		   (a (princ-to-string 
		       obj)))
	      (flet ((dump ()
		       (princ a)
		       (terpri)
		       (princ text)
		       (terpri)))
		(cond ((string-a-prefix-b-p
			a
			text)
		       (progn
			 (format t "~%same:~%") 
			 (dump))
		       (incf correct))
		      (t
		       (incf wrong)
		       (format t "~%DIFFERENT:~%")
		       (dump)
	;	       (inspect obj)
		       )))))
	  rules)
    (format t "correct: ~a wrong: ~a~%" correct wrong)
    (list wrong correct)))

(defun teststuff ()
  (test-lines)
  (test-lines 'lex-rule-start
	      (mapcar 'spec-lex-rule-rule
		      (mapcar
		       (lambda (item)
			 (parse-with-garbage 'lex-line-def item))
		       *lex-definitions-lines*))))

(defun test-things (&optional not-pretty)
  (let ((*print-raw* not-pretty))
    (teststuff))
  (values))



;;;end
;;(string-thing lex-token-type yacc-token-type)
;;;;
(defun lex (string &optional (stream *standard-output*))
  (let ((start 0))
    (loop
       (multiple-value-bind (result len)
	   (parse-with-garbage 'lexer-foo string :start start)
	 (when (zerop len)
	   (return))
	 (destructuring-bind (string-thing ignorable yacc-token-type) result
	   (declare (ignorable string-thing yacc-token-type ignorable))
	   ;;(write-char (char-code-object yacc-token-type) stream)
	   (princ (stringy (car result)) stream)
	   )
	 (incf start len)))))

(defun lex2 (string)
  (with-output-to-string (stream)
    (lex string stream)))

;;FIXME:: hack -> using unicode characters to represent tokens, thus simplifyng tokens
#+nil
(progn
  (defparameter *char-code-pointer* nil)
  (defparameter *objects-to-characters* nil)
  (defun reset-char-code-object-table ()
    (setf *char-code-pointer* 32)
    (setf *objects-to-characters* (make-hash-table :test 'equal)))
  (reset-char-code-object-table)
  (defun char-code-object (obj)
    (let ((there? (gethash obj *objects-to-characters*)))
      (unless there?
	(let ((new (code-char *char-code-pointer*)))
	  (setf (gethash obj *objects-to-characters*)
		new)
	  (setf there? new))
	(incf *char-code-pointer*))
      there?)))

(define-c-parse-rule left-recursion? ()
  (progn-v left-recursion?
	   #\(
	   character
	   #\)))

;;32 -> 126 inclusive
;;0 -> 126 - 32 = 94
;;0 -> 94 inclusive = mod 95
(defun ascii-increment (char)
  (let ((code (char-code char)))
    (code-char (+ 32 (mod (+ 1 (- code 32)) 95)))))

#+nil
(get-directives 'per-iter
		(alexandria:read-file-into-string
		 (ensure-cached-no-directives
		  "/home/imac/install/src/emacs-mirror/emacs-master/src/xdisp.c")
		 ))
(defparameter *cpp-test-path* "/home/imac/install/src/emacs-mirror/emacs-master/src/bytecode.c")
(defun reroot-cpp (&optional (path *cpp-test-path*))
  (reroot path :prefix "_cpp_"))

(defun cpp-it (&optional (path *cpp-test-path*))
  (uiop:run-program (print (cppbar path))
		    :output *standard-output* :error-output *standard-output*))

(defun cpp-include-directories-foo ()
  (stringy (mapcar (lambda (x)
		     (format nil " -I~a " x))
		   *include-directories*)))

(defun cppbar (&optional (path *cpp-test-path*))
  (let ((infile (uiop:unix-namestring path))
	(outfile (uiop:unix-namestring (reroot-cpp path)))
	(flags (stringy
		(list
		 ;;from man cpp
		 ;;" -CC " ;;preserve comments
		 " -P " ;;no line information
		 ;;"-fdebug-cpp" ;;token information?
		 ))))
    (format nil "cpp ~a ~a ~a -o ~a " (cpp-include-directories-foo) flags infile outfile)))
