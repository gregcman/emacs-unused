(defpackage :c-parse
  (:use :cl :esrap-liquid))
(in-package :c-parse)

(defparameter *path* (asdf:system-source-directory :c-parse))
;;generated via grepping .h and .c files for "#include <"
(defparameter *lex-txt-path* (merge-pathnames "lex.txt" *path*))
(defparameter *yacc-txt-path* (merge-pathnames "yacc.txt" *path*))
(defparameter *lex-txt* (alexandria:read-file-into-string *lex-txt-path*))
(defparameter *yacc-txt* (alexandria:read-file-into-string *yacc-txt-path*))
(defun whitespace-string (str)
  "return t if its all spaces or empty"
  (dotimes (i (length str))
    (unless (char= #\Space (aref str i))
      (return-from whitespace-string nil)))
  t)
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
      (#\b #\backspace)
      (#\v #\vt)
      (#\f #\formfeed) ;;FIXME - see below
      (#\r #\return) ;;FIXME -> what chars are allowed?
      )))

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
  ;;FIXME:misnomer. not a regular expression
  (defparameter *lex-regex-operators*
    (coerce
     "\"\\[]^-?.*+|()$/{}%<>"
     'list)))

(flet ((escape (escaped-char char)
	 (if escaped-char
	     (format nil "\\~A" escaped-char)
	     (string char))))
  ;;;;different contexts have different escape seqences 
  (defun char-to-escaped-char-string (char)
    ;;used in string rule
    (let ((escaped-char 
	   (utility:etouq
	     `(case char
		,@(mapcar 'reverse *lex-special-chars*)
		(otherwise nil)))))
      (escape escaped-char char)))
  (defun char-to-escaped-char (char)
    ;;used as expression
    (let ((escaped-char 
	   (utility:etouq
	     `(case char
		,@(mapcar 'reverse *lex-special-chars*)
		(,*lex-regex-operators* char)
		(otherwise nil)))))
      (escape escaped-char char)))
  (defun char-to-escaped-char-character-class (char)
    ;;used in character class
    (let ((escaped-char
	   (utility::etouq
	     `(case char
		(#\\ #\\)
		(#\] #\])
		(#\? #\?) ;;FIXME::characters added here on a case by case basis?
		,@(mapcar 'reverse *lex-special-chars*)
		(otherwise nil)))))
      (escape escaped-char char))))

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
	    (char-to-escaped-char-character-class (lex-character-range-start object))
	    (char-to-escaped-char-character-class (lex-character-range-end object))))
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
     (chars nil)))
  (defun print-lex-character-class (stream object)
    (;;with-write-parens (stream)
     progn
      (write-char #\[ stream)
      (when (lex-character-class-negated-p object)
	(write-char #\^ stream))
      (dolist (item (lex-character-class-chars object))
	(etypecase item
	  (character
	   (write-string (char-to-escaped-char-character-class item)
			 stream))
	  (lex-character-range 
	   (print-lex-character-range stream item))))
      (write-char #\] stream)))
  (set-pprint-dispatch 'lex-character-class 'print-lex-character-class))
(defun set-character-class-char (obj &rest data)
  (setf (lex-character-class-chars obj) data))

(define-c-parse-rule lex-rule-character-class ()
  ;;http://dinosaur.compilertools.net/lex/index.html
  ;;In character classes, the ^ operator must appear as the first character after the left bracket;
  ;;it indicates that the resulting string is to be complemented with respect to the computer character set. Thus
  (v #\[)
  (cap :negation (? #\^))
  (cap :chars
       ;;FIXME::what characters are allowed where?
       (utility:etouq
	 `(times (|| lex-character-range
		     lex-char-or-escaped-char
		     ,@(set-difference *lex-regex-operators*
				       '(#\]))))))
  (v #\])
  (make-lex-character-class
   :negated-p (recap :negation)
   :chars (recap :chars)))
(defparameter *print-raw* nil
  "toggle printing lex-sequence as a dot or a string. ")
(progn
  (defparameter *lex-rule-repeat-infinity* :infinity
    "signify that the rule should repeat forever")
  (struct-to-clos:struct->class
   (defstruct lex-rule-repeat
     rule
     min
     (max *lex-rule-repeat-infinity*)))
  (defun print-lex-rule-repeat (stream object)
    (;;with-write-parens (stream)
      progn
      (write (lex-rule-repeat-rule object) :stream stream)
      (let ((min (lex-rule-repeat-min object))
	    (max (lex-rule-repeat-max object)))
	(flet ((single-char (x)
		 (write-char x stream)))
	  (cond ((and
		  (not *print-raw*)
		  (eql min 0)
		  (eql max 1))
		 (single-char #\?))
		((and
		  (not *print-raw*)
		  (eql min 0)
		  (eql max *lex-rule-repeat-infinity*))
		 (single-char #\*))
		((and
		  (not *print-raw*)
		  (eql min 1)
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
    (;;with-write-parens (stream)
     progn
      (format stream "~a|~a"
	      (lex-rule-or-first object)
	      (lex-rule-or-second object))))
  (set-pprint-dispatch 'lex-rule-or 'print-lex-rule-or))
(define-c-parse-rule lex-rule-vertical-bar (rule)
  (v #\|)
  (cap :arg1 (v lex-rule-sequence))
  (make-lex-rule-or
   :first rule
   :second (recap :arg1)))

(progn
  (struct-to-clos:struct->class
   (defstruct lex-rule-reference
     string))
  (defun print-lex-rule-reference (stream object)
    ;;FIXME::what characters can tokens consist of?
    (;;with-write-parens (stream)
     progn
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

(define-c-parse-rule whitespace ()
  (postimes (|| #\Newline #\Space #\tab)))
(progn
  ;;FIXME::lex-rule, which handles sequences, is becoming dumping ground for
  ;;irregular lex syntax like strings and the dot ->.
  (struct-to-clos:struct->class
   (defstruct lex-rule
     data
     ;;dot
     (print-as-dot nil)
     ;;characters
     (with-parens nil)
     ;;strings and chars
     (string-print-as-char-p nil)
     string-data
     (string-p nil)))
  (defun print-lex-rule (stream object)
    ;;FIXME::what characters can tokens consist of?
    (flet ((print-stuff ()
	     (dolist (item (lex-rule-data object))
	       (format stream "~a" item))))
      (cond (;;for the . operator
	     (and (not *print-raw*)
		  (lex-rule-print-as-dot object))
	     ;;FIXME::dots are converted into lex-rule sequences.
	     ;;have separate special object for shortening?
	     (write-char #\. stream))
	    (;; for strings and characters
	     (and (not *print-raw*)
		  (lex-rule-string-p object))
	     (let ((str (lex-rule-string-data object)))
	       (cond ((and (= 1 (length str))
			   (lex-rule-string-print-as-char-p object))
		      (write-string (char-to-escaped-char (aref str 0))
				    stream))
		     (t
		      (write-char #\" stream)
		      (let ((str str))
			(dotimes (index (length str))
			  (write-string (char-to-escaped-char-string (aref str index))
					stream)))
		      (write-char #\" stream)))))
	    (;;if this was read with parentheses?
	     t
	     (if (lex-rule-with-parens object)
		 (with-write-parens (stream)
		   (print-stuff))
		 (print-stuff))))))
  (set-pprint-dispatch 'lex-rule 'print-lex-rule))
(define-c-parse-rule lex-rule-parentheses ()
  (let ((lex-rule-sequence
	 (progm #\(
		lex-rule-sequence
		#\))))
    (setf (lex-rule-with-parens lex-rule-sequence) t)
    lex-rule-sequence))


(define-c-parse-rule lex-rule-all-but-newline-rule ()
  (v #\.)
  (make-lex-rule
   :print-as-dot t
   :data
   (list
    (match-one-char
     #\Newline
     (make-lex-character-class
      :negated-p t)))))

(defun match-one-char (char &optional (character-class-rule
				       (make-lex-character-class)))
  "create a sequence rule that matches one character"
  (set-character-class-char
   character-class-rule
   char)
  (make-lex-rule-repeat
   :rule character-class-rule
   :min 1
   :max 1))
;;the string object covers both strings and individual characters
(defun match-string (string &optional (print-as-char nil))
  (make-lex-rule
   :string-data string
   :string-p t
   :string-print-as-char-p print-as-char
   :data
   (map 'list
	(lambda (char)
	  (match-one-char char))
	string)))
(define-c-parse-rule lex-rule-string ()
  (match-string (v lex-string)))
(define-c-parse-rule lex-rule-char ()
  (match-string (string (v lex-char-or-escaped-char)) t))

(define-c-parse-rule lex-rule-sequence (&optional (toplevel nil))
  (make-lex-rule
   :data
   (postimes
    (progn
      (when toplevel
	(! whitespace))
      (let ((rule
	     (||
	      lex-rule-char
	      lex-rule-character-class
	      lex-rule-string
	      lex-rule-all-but-newline-rule
	      lex-rule-parentheses
	      lex-rule-definition)))
	;;;
	(block out
	  (loop
	     (setf rule
		   (|| (v lex-rule-? rule)
		       (v lex-rule-* rule)
		       (v lex-rule-+ rule)
		       (v lex-rule-vertical-bar rule)
		       (v lex-rule-occurences rule)
		       (return-from out rule))))))))))

(define-c-parse-rule lex-rule-start ()
  (v lex-rule-sequence t))

(defun parse-with-garbage (rule text)
  (c-parse-parse rule text :junk-allowed t))

(defun test-parse-rules (&optional (rules *lex-patterns*))
  (mapcar (lambda (text)
	    (parse-with-garbage 'lex-rule-start text))
	  rules))

(define-c-parse-rule lex-def ()
  (cap :def-name (v lex-token-string))
  (v whitespace)
  ;;(cap :rule (v lex-rule-start))
  (stringify (postimes character)))

;;need to evaluate this before testing
(defparameter *defs* nil)
(defun set-deps ()
  (setf *defs*
	(mapcar
	 (lambda (item)
	   (parse-with-garbage 'lex-def item))
	 *lex-strings*)))

;;run split-lex-2 to set the dynamic variables
(defun test-lines (&optional (rule 'lex-rule-start) (rules *lex-patterns*))
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
    (values)))

(defun teststuff ()
  (test-lines)
  (test-lines 'lex-rule-start *defs*))

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
  (set-deps)
  (values))

(defun test-things (&optional not-pretty)
  (let ((*print-raw* not-pretty))
    (teststuff))
  (values))

;;character classes
;;strings <- can be replaced by a special lex-rule with all character-classes of length 1
;;numerical repetition
;;references
;;sequencing
;;options
;;all-but-newline <- not necessary? a character class?

;;"foo" -> ([f]{1,1}[o]{1,1}[o]{1,1})
;;. -> ([^\n]{1,1})

;;lex-rule-sequence sequencing -> concatenate + list-v?
;;lex-rule-or option -> ||
;;lex-rule-repeat repeat -> times
;;lex-character-class ->  [! with character] characters, || character-ranges
;;references -> references to other rules
