(in-package :c-parse)

(defparameter *path* (asdf:system-source-directory :c-parse))
;;generated via grepping .h and .c files for "#include <"
(defun whitespace-string (str)
  "return t if its all spaces or empty"
  (dotimes (i (length str))
    (unless (char= #\Space (aref str i))
      (return-from whitespace-string nil)))
  t)
(defun file-lines-no-whitespace-lines (string)
  (remove-if #'whitespace-string
	     (split-sequence:split-sequence #\Newline string)))
(defun print-list (&optional (data *lex-txt2*))
  (dolist (item data)
    (print item)))
(defun princ-list (&optional (data *lex-txt2*))
  (dolist (item data)
    (terpri)
    (princ item)))

;;both the lex and yacc file are separated into 3 sections by two "%%"
;;for use with *lex-txt2* and *yacc-txt2*
(defun %%-positions (data)
  (let ((first-end (position "%%" data :test 'string=)))
    (values first-end
	    (position "%%" data :test 'string= :start (+ 1 first-end)))))

(define-esrap-env c-parse)
(define-c-parse-rule lex-yacc-token-char ()
  (|| #\_
      (character-ranges
       (#\a #\z)
       (#\A #\Z))))
(define-c-parse-rule lex-yacc-token ()
  (postimes lex-yacc-token-char))

(defun stringify (seq)
  "coerce sequence into a string"
  (coerce seq 'string))
(define-c-parse-rule lex-token-string ()
  ;;happens to be same for yacc. FIXME:: proper names for things?
  (stringify (v lex-yacc-token)))
