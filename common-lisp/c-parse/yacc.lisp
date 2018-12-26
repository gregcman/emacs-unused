(in-package :c-parse)

(define-c-parse-rule yacc-token-line ()
  (v "%token")
  (times (progn-v whitespace
		  lex-token-string)))
(define-c-parse-rule yacc-start-line ()
  (v "%start")
  (progn-v whitespace
	   lex-token-string))

(defun foobar65 ()
  "return (values #<list-of-tokens> start)"
  (values (alexandria:flatten
	   (mapcar (lambda (line)
		     (parse-with-garbage 'yacc-token-line line))
		   *yacc-tokens-lines*))
	  (first
	   (alexandria:flatten
	    (mapcar (lambda (line)
		      (parse-with-garbage 'yacc-start-line line))
		    *yacc-tokens-lines*)))))

(defparameter *yacc-token-strings* nil)
(defparameter *yacc-start-string* nil)
(defun foobar67 ()
  (setf (values *yacc-token-strings* *yacc-start-string*)
	(foobar65)))

;;FIXME::does not handle // comments
(define-c-parse-rule yacc-whitespace-or-comment ()
  (|| whitespace
      lex-yacc-multiline-comment))
(define-c-parse-rule yacc-whitespace-or-comments ()
  (postimes yacc-whitespace-or-comment))

(define-c-parse-rule yacc-grammar-one-expansion ()
  (v yacc-whitespace-or-comments)
  (|| lex-read-char
      lex-token-string))
(define-c-parse-rule yacc-rule-aux ()
  (times yacc-grammar-one-expansion))
(define-c-parse-rule yacc-rule ()
  (list* (prog1 (v lex-token-string)
	   (v yacc-whitespace-or-comments))
	 (prog1 (list*
		 (progn
		   (v #\:)
		   (v yacc-rule-aux))
		 (times (progn-v yacc-whitespace-or-comments
				 #\|
				 yacc-rule-aux)))
	   (v yacc-whitespace-or-comments)
	   (v #\;)
	   (v #\Newline))))
(define-c-parse-rule yacc-rules ()
  (times yacc-rule))

(defun foobar68 ()
  (parse-with-garbage 'yacc-rules *yacc-definitions*))

(defparameter *yacc-grammar* nil)
(defun foobar69 ()
  (setf *yacc-grammar* (foobar68)))

(defparameter *yacc-terminal-chars* nil) ;;because characters are terminals. FIXME:documentation
(defparameter *yacc-package* (make-package "YACC-SYMBOLS"))
(defun yacc-symbol (x)
  (if x
      (let ((string (etypecase x
		      (string (format nil "string%~a" x))
		      (character
		       (pushnew x *yacc-terminal-chars*)
		       (format nil "character%-~a" x))
		      (symbol (format nil "symbol%~a" x)))))
	(intern string *yacc-package*))
      x))
#+nil
"A  parser  consumes  the  output  of  a  lexer,  that  produces  a  stream  of  terminals.   CL-Yacc
expects the lexer to be a function of no arguments (a
thunk
) that returns two values:  the next
terminal symbol, and the value of the symbol, which will be passed to the action associated with
a production.  At the end of the input, the lexer should return
nil
."
(defun lex-for-cl-yacc (string)
  (let ((start 0))
    (lambda ()
      (block out
	(tagbody try-again
	   (multiple-value-bind (result len)
	       (parse-with-garbage 'lexer-foo string :start start)
	     (incf start len)
	     (when (zerop len)
	       (return-from out (values nil nil)))
	     (destructuring-bind (string-thing ignorable yacc-token-type) result
	       (declare (ignorable string-thing yacc-token-type ignorable))
	       ;;(write-char (char-code-object yacc-token-type) stream)
	       ;;(princ (stringy (car result)) stream)
	       
	       ;;to skip over whitespace
	       (when (not yacc-token-type)
		 (go try-again))
	       (return-from out
		 (values yacc-token-type
			 string-thing))
	       
	       )))))))
(defparameter *yacc-start-symbol* (yacc-symbol *yacc-start-string*))
(defparameter *yacc-grammar-symbols* (tree-map 'yacc-symbol *yacc-grammar*))
(defparameter *yacc-token-symbols* (tree-map 'yacc-symbol
					     (append *yacc-token-strings*
						     *yacc-terminal-chars*)))

(defun tree-map (fn tree)
  "replace each list-element in tree with (funcall fn list-element)"
  ;;(tree-map (lambda (x) (* x x)) '(1 2 (2 3) . foo)) -> (1 4 (4 9) . FOO)
  (cond ((atom tree) (funcall fn tree))
	(t (cons (tree-map fn (first tree))
		 (let ((rest (rest tree)))
		   (if (and rest
			    (listp rest))
		       (tree-map fn rest)
		       rest))))))

(utility:etouq
  `(define-parser *c*
     (:start-symbol ,*yacc-start-symbol*)
     (:terminals ,*yacc-token-symbols*)
     ,@*yacc-grammar-symbols*))
(defun parsefoobar (string)
  (yacc:parse-with-lexer (lex-for-cl-yacc string) *c*))
