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
