(defpackage :c-parse
  (:use :cl :esrap-liquid))
(in-package :c-parse)

(defparameter *path* (asdf:system-source-directory :c-parse))
;;generated via grepping .h and .c files for "#include <"
(defparameter *lex-txt-path* (merge-pathnames "lex.txt" *path*))
(defparameter *yacc-txt-path* (merge-pathnames "yacc.txt" *path*))
(defparameter *lex-txt* (alexandria:read-file-into-string *lex-txt-path*))
(defparameter *yacc-txt* (alexandria:read-file-into-string *yacc-txt-path*))
(defparameter *lex-txt2*
  (remove-if #'whitespace-string
	     (split-sequence:split-sequence #\Newline *lex-txt*)))

(defun print-list (&optional (data *lex-txt2*))
  (dolist (item data)
    (print item)))
(defun princ-list (&optional (data *lex-txt2*))
  (dolist (item data)
    (terpri)
    (princ item)))

(defun whitespace-string (str)
  "return t if its all spaces or empty"
  (dotimes (i (length str))
    (unless (char= #\Space (aref str i))
      (return-from whitespace-string nil)))
  t)
;;https://docs.oracle.com/cd/E19504-01/802-5880/lex-6/index.html
;;The mandatory rules section opens with the delimiter %%.
;;If a routines section follows, another %% delimiter ends the rules section.
;;The %% delimiters must be entered at the beginning of a line, that is, without leading blanks.
;;If there is no second delimiter, the rules section is presumed to continue to the end of the program. 
(defun split-lex (&optional (lex *lex-txt2*))
  ;;divide the lex.txt into terminals and patterns.
  ;;ignore the c code for check_type and comment, instead hand-coding those
  (values
   (let (;;this is where c code starts and definitions
	 (first-end (position "%{" lex :test 'string=))
	 ;;skip over the variables at the beginning for the lex program
	 (start (position-if (lambda (str)
			       (not (char= (aref str 0)
					   #\%)))
			     lex)))
     ;;terminals, called definitions
     (subseq lex start first-end))
   (let* ((first-end (position "%%" lex :test 'string=))
	  (second-end (position "%%" lex :test 'string= :start (+ 1 first-end))))
     ;;patterns, called rules
     (subseq lex (+ 1 first-end) second-end))))
;;http://dinosaur.compilertools.net/lex/index.html <- detailed explanation of lex file format
(defparameter *lex-strings* nil)
(defparameter *lex-patterns* nil)
(defun split-lex2 (&optional (lex *lex-txt2*))
  (setf (values *lex-strings*
		*lex-patterns*)
	(split-lex lex)))

(define-esrap-env c-parse)
(define-c-parse-rule lex-yacc-token-char ()
  (|| #\_
   (character-ranges
    (#\a #\z)
    (#\A #\Z))))
(define-c-parse-rule lex-yacc-token ()
  (postimes lex-yacc-token-char))
(define-c-parse-rule spaces+ ()
  (length (postimes #\Space))
  nil)

(defun stringify (seq)
  "coerce sequence into a string"
  (coerce seq 'string))

(define-c-parse-rule lex-string ()
  (cap "what" (v lex-yacc-token))
  (v spaces+)
  (list (stringify (recap "what"))))

#+nil
(progn ;;example parsing rule
  (define-c-parse-rule char ()
    (|| (v #\a)))
  (defun what ()
    (c-parse-parse 'char "b")))

;;http://dinosaur.compilertools.net/lex/index.html
;;" \ [ ] ^ - ? . * + | ( ) $ / { } % < > ;;operators that need to be escaped
;;Another use of the quoting mechanism is to get a blank into an expression;
;;normally, as explained above, blanks or tabs end a rule.
;;Any blank character not contained within [] (see below) must be quoted.
;;Several normal C escapes with \ are recognized: \n is newline, \t is tab, and \b is backspace.
;;To enter \ itself, use \\. Since newline is illegal in an expression, \n must be used;
;;it is not required to escape tab and backspace. Every character but blank, tab, newline and the list above is always a text character. 

;;\ - and ^ ;;special characters for []

;;x        the character "x"
;;"x"      an "x", even if x is an operator.
;;\x       an "x", even if x is an operator.
;;[xy]     the character x or y.
;;[x-z]    the characters x, y or z.
;;[^x]     any character but x.
;;.        any character but newline.
;;^x       an x at the beginning of a line.
;;<y>x     an x when Lex is in start condition y.             ;;ignore
;;x$       an x at the end of a line.
;;x?       an optional x.
;;x*       0,1,2, ... instances of x.
;;x+       1,2,3, ... instances of x.
;;x|y      an x or a y.
;;(x)      an x.
;;x/y      an x but only if followed by y.
;;{xx}     the translation of xx from the definitions section.
;;x{m,n}   m through n occurrences of x

;;| repeats the lex rule to the next listed rule
(utility:eval-always
  (defparameter *lex-special-chars*
    '((#\t #\tab)
      (#\n #\Newline)
      (#\b #\backspace))))

(defun escaped-char-to-char (char)
  ;;Several normal C escapes with \ are recognized: \n is newline, \t is tab, and \b is backspace.
  (utility:etouq
    `(case char
       ,@*lex-special-chars*
       (otherwise char))))

(define-c-parse-rule lex-number ()
  (read-from-string (stringify
		     (times 
		      (character-ranges
		       (#\0 #\9)) :from 1))))

(define-c-parse-rule lex-char-or-escaped-char ()
  (|| lex-char
      (progn (v #\\)
	     (let ((char (v character)))
	       (escaped-char-to-char char)))))
(utility:eval-always
  (defparameter *lex-regex-operators*
    (coerce
     "\"\\[]^-?.*+|()$/{}%<>"
     'list)))

(defun char-to-escaped-char (char)
  "return a string representing the char as either just a char or an escape sequence"
  (let ((escaped-char 
	 (utility:etouq
	   `(case char
	      ,@(mapcar 'reverse *lex-special-chars*)
	      (,*lex-regex-operators* char)
	      (otherwise nil)))))
    (if escaped-char
	(format nil "\\~A" escaped-char)
	(string char))))

(define-c-parse-rule lex-char ()
  ;;" \ [ ] ^ - ? . * + | ( ) $ / { } % < > ;;operators that need to be escaped
  (! (utility:etouq `(|| ,@*lex-regex-operators*)))
  (v character))

(define-c-parse-rule lex-string ()
  (v #\")
  (cap :answer (stringify (times lex-char-or-escaped-char)))
  (v #\")
  (recap :answer))

(struct-to-clos:struct->class
 (defstruct lex-character-range
   start
   end))
(defun print-lex-character-range (stream object)
  (format stream "[~a-~a]"
	  (lex-character-range-start object)
	  (lex-character-range-end object)))
(set-pprint-dispatch 'lex-character-range 'print-lex-character-range)

(define-c-parse-rule lex-character-range ()
  ;;http://dinosaur.compilertools.net/lex/index.html
  ;;The - character indicates ranges.
  (cap :start (v lex-char-or-escaped-char))
  (v #\-)
  (cap :end (v lex-char-or-escaped-char))
  (make-lex-character-range
   :start (recap :start)
   :end (recap :end)))

(struct-to-clos:struct->class
 (defstruct lex-character-class
   negated-p
   chars))

(define-c-parse-rule lex-character-class ()
  ;;http://dinosaur.compilertools.net/lex/index.html
  ;;In character classes, the ^ operator must appear as the first character after the left bracket;
  ;;it indicates that the resulting string is to be complemented with respect to the computer character set. Thus
  (v #\[)
  (cap :negation (? #\^))
  (cap :chars
       (times (|| lex-character-range
		  lex-char-or-escaped-char)))
  (v #\])
  (make-lex-character-class
   :negated-p (recap :negation)
   :chars (recap :chars)))
