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
  (times lex-yacc-token-char :from 1))
(define-c-parse-rule spaces+ ()
  (length (times #\Space :from 1))
  nil)

(defun stringify (seq)
  "coerce sequence into a string"
  (coerce seq 'string))

(define-c-parse-rule lex-string ()
  (cap "what" (v lex-yacc-token))
  (v spaces+)
  (list (stringify (recap "what"))))

(defun parse-lex-string (&optional (str "O   [0-7]"))
  (c-parse-parse ))

(define-c-parse-rule char ()
  (|| (v #\a)))

(defun what ()
  (c-parse-parse 'char "b"))
