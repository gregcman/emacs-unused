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

(define-c-parse-rule lex-token-string ()
  (stringify (v lex-yacc-token)))

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
;;^x       an x at the beginning of a line.                   ;;ignore
;;<y>x     an x when Lex is in start condition y.             ;;ignore
;;x$       an x at the end of a line.                         ;;ignore
;;x?       an optional x.
;;x*       0,1,2, ... instances of x.
;;x+       1,2,3, ... instances of x.
;;x|y      an x or a y.
;;(x)      an x.
;;x/y      an x but only if followed by y.                    ;;ignore
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
		     (postimes 
		      (character-ranges
		       (#\0 #\9))))))

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
  (progm #\"
	 (stringify (utility:etouq
		      `(times (|| lex-char-or-escaped-char
				  (|| ,@(set-difference *lex-regex-operators*
							'(#\" #\\)))))))
	 #\"))

(progn
  (struct-to-clos:struct->class
   (defstruct lex-character-range
     start
     end))
  (defun print-lex-character-range (stream object)
    (format stream "~a-~a"
	    (char-to-escaped-char (lex-character-range-start object))
	    (char-to-escaped-char (lex-character-range-end object))))
  (set-pprint-dispatch 'lex-character-range 'print-lex-character-range))

(define-c-parse-rule lex-character-range ()
  ;;http://dinosaur.compilertools.net/lex/index.html
  ;;The - character indicates ranges.
  (cap :start (v lex-char-or-escaped-char))
  (v #\-)
  (cap :end (v lex-char-or-escaped-char))
  (make-lex-character-range
   :start (recap :start)
   :end (recap :end)))

(defmacro with-write-parens ((stream) &body body)
  `(prog2
       (write-char #\( ,stream)
       (progn ,@body)
     (write-char #\) ,stream)))

(progn
  (struct-to-clos:struct->class
   (defstruct lex-character-class
     negated-p
     chars))
  (defun print-lex-character-class (stream object)
    (with-write-parens (stream)
      (write-char #\[ stream)
      (when (lex-character-class-negated-p object)
	(write-char #\^ stream))
      (dolist (item (lex-character-class-chars object))
	(etypecase item
	  (character (write-string (char-to-escaped-char item)
				   stream))
	  (lex-character-range 
	   (print-lex-character-range stream item))))
      (write-char #\] stream)))
  (set-pprint-dispatch 'lex-character-class 'print-lex-character-class))

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
(progn
  (defparameter *lex-rule-repeat-infinity* :infinity
    "signify that the rule should repeat forever")
  (struct-to-clos:struct->class
   (defstruct lex-rule-repeat
     rule
     min
     (max *lex-rule-repeat-infinity*)))
  (defun print-lex-rule-repeat (stream object)
    (with-write-parens (stream)
      (write object :stream stream)
      (let ((min (lex-rule-repeat-min object))
	    (max (lex-rule-repeat-max object)))
	(flet ((single-char (x)
		 (write-char x stream)))
	  (cond ((and (= min 0)
		      (= max 1))
		 (single-char #\?))
		((and (= min 0)
		      (eql max *lex-rule-repeat-infinity*))
		 (single-char #\*))
		((and (= min 1)
		      (eql max *lex-rule-repeat-infinity*))
		 (single-char #\+))
		(t 
		 (format stream "{~a,~a}" min max)))))))
  (set-pprint-dispatch 'lex-rule-repeat 'print-lex-rule-repeat))

(define-c-parse-rule lex-rule-? (rule)
  (v #\?)
  (make-lex-rule-repeat
   :rule rule
   :min 0
   :max 1))
(define-c-parse-rule lex-rule-* (rule)
  (v #\*)
  (make-lex-rule-repeat
   :rule rule
   :min 0
   :max *lex-rule-repeat-infinity*))
(define-c-parse-rule lex-rule-+ (rule)
  (v #\+)
  (make-lex-rule-repeat
   :rule rule
   :min 1
   :max *lex-rule-repeat-infinity*))

(progn
  (struct-to-clos:struct->class
   (defstruct lex-rule-or
     first
     second))
  (defun print-lex-rule-or (stream object)
    (with-write-parens (stream)
      (format stream "~a|~a"
	      (lex-rule-or-first object)
	      (lex-rule-or-second object))))
  (set-pprint-dispatch 'lex-rule-or 'print-lex-rule-or))
(define-c-parse-rule lex-rule-vertical-bar (rule)
  (v #\|)
  (cap :arg1 (v lex-rule))
  (make-lex-rule-or
   :first rule
   :second (recap :arg1)))

(define-c-parse-rule lex-rule-parentheses ()
  (progm #\(
	 lex-rule
	 #\)))

(progn
  (struct-to-clos:struct->class
   (defstruct lex-rule-reference
     string))
  (defun print-lex-rule-reference (stream object)
    ;;FIXME::what characters can tokens consist of?
    (with-write-parens (stream)
      (format stream "{~a}"
	      (lex-rule-reference-string object))))
  (set-pprint-dispatch 'lex-rule-reference 'print-lex-rule-reference))

(define-c-parse-rule lex-rule-definition ()
  (make-lex-rule-reference
   :string
   (progm #\{
	  lex-token-string
	  #\})))
(define-c-parse-rule lex-rule-occurences (rule)
  (v #\{)
  (cap :min (v lex-number))
  (v #\,)
  (cap :max (v lex-number))
  (v #\})
  (make-lex-rule-repeat
   :rule rule
   :min (recap :min)
   :max (recap :max)))

(define-c-parse-rule lex-rule ()
  (postimes
   (let ((rule (||
		lex-char-or-escaped-char
		lex-character-class
		lex-string
		(v #\.)
		lex-rule-parentheses
		lex-rule-definition)))
     (print rule)
     (print (|| (v lex-rule-? rule)
		(v lex-rule-* rule)
		(v lex-rule-+ rule)
		(v lex-rule-vertical-bar rule)
		(v lex-rule-occurences rule)
		(progn rule))))))
