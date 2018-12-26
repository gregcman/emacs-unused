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
