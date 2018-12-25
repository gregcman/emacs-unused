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

(defun parse-with-garbage (rule text)
  (c-parse-parse rule text :junk-allowed t))

(defun test-parse-rules (&optional (rules *lex-rules-lines*))
  (mapcar (lambda (text)
	    (parse-with-garbage 'lex-rule-start text))
	  rules))

(define-c-parse-rule lex-line-def ()
  (cap :def-name (v lex-token-string))
  (v whitespace)
  ;;(cap :rule (v lex-rule-start))
  (list
   (recap :def-name)
   (stringify (postimes character))))
(defun spec-lex-rule-rule (spec)
  (second spec))
(defun spec-lex-rule-name (spec)
  (first spec))

(define-c-parse-rule lex-line-rule ()
  (prog1-v lex-rule-start
	   whitespace))

;;*defs* is a list of ("name" "rule")
(defun split-lex-line-def (&optional (item "NS [a-zA-Z_]"))
  (destructuring-bind (name rule-string) (parse-with-garbage 'lex-line-def item)
    (list name (parse-with-garbage 'lex-rule-start rule-string))))

(defun split-lex-line-rule (&optional (string "asd[a-zA-Z_]fasd   {return; /* */}"))
  (multiple-value-bind (form end)
      (parse-with-garbage 'lex-line-rule string)
    ;;;FIXME::assumes that } terminates the line, which for this file does
    (let ((last-bracket (position #\} string :from-end t)))
      (list form
	    (subseq string (1+ end)
		    last-bracket)))))

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

;;(string-a-prefix-b-p "a" "ab") -> T
;;(string-a-prefix-b-p "ac" "ab") -> 
(defun string-a-prefix-b-p (a b)
  "test whether string a is a prefix of b"
  (when (> (length a)
	   (length b))
    ;;(error "a is longer than b")
    (return-from string-a-prefix-b-p nil)
    )
  (dotimes (index (length a))
    (unless (char= (aref a index)
		   (aref b index))
      (return-from string-a-prefix-b-p nil)))
  t)

(defun setup ()
  (split-lex2)
  (values))
(setup)
(defun test-things (&optional not-pretty)
  (let ((*print-raw* not-pretty))
    (teststuff))
  (values))

(defparameter *processed-definitions* (mapcar 'split-lex-line-def
					      *lex-definitions-lines*))
(defun pipeline (&optional (def "hello [90]"))
  (compile-to-esrap-liquid (split-lex-line-def def)))
(defun compile-to-esrap-liquid (item)
  (destructuring-bind (name rule) item
    (let ((form `(define-c-parse-rule ,(find-lex-symbol name) ()
		   ,(lex-rule-dump rule))))
      form)))
(defun load-processed-definitions ()
  (mapcar
   'compile-to-esrap-liquid
   *processed-definitions*))
(defparameter *processed-rules* (mapcar 'split-lex-line-rule
					*lex-rules-lines*))
