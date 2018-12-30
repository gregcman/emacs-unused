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

(defmacro parse-with-garbage (rule text &rest rest &key &allow-other-keys)
  `(c-parse-parse ,rule ,text :junk-allowed t ,@rest))

(defun stringy (tree)
  ;;turn a tree of nil's and characters produced by esrap-liquid into a
  ;;string
  (with-output-to-string (stream)
    (labels ((rec (node)
	       (when node
		 (if (atom node)
		     (princ node stream)
		     (progn (rec (car node))
			    (rec (cdr node)))))))
      (rec tree))))

(defun concatenate-string (&rest rest)
  (%concatenate-string rest))
(defun %concatenate-string (rest)
  (apply 'concatenate 'string rest))

;;;yacc and lex comments are the same?
(define-c-parse-rule lex-yacc-multiline-comment ()
  (progn-v
   "/*"
   lex-comment-end))
(define-c-parse-rule lex-comment-end-token ()
  (progn (v #\*)
	 (v #\/)))
(define-c-parse-rule lex-comment-end ()
  (prog1 (postimes
	  (progn (! lex-comment-end-token)
		 (v character))
	  )
    (v lex-comment-end-token))
   nil
  )
(defparameter *emacs-src-root-path* "/home/imac/install/src/emacs-mirror/emacs-master/")
(defun emacsify-path (&optional (path "src/lisp.h"))
  (merge-pathnames path *emacs-src-root-path*))

(defparameter *testpath*
  (emacsify-path "src/lisp.h")
  #+nil
  "/home/imac/install/src/pycparser-master/examples/c_files/funky.c"
  #+nil
  "/home/imac/install/src/pycparser-master/examples/c_files/hash.c")

;;FIXME:: where to put test files?
(defparameter *text-test-file*
  (alexandria:read-file-into-string
   (emacsify-path "src/lisp.h")
   #+nil
   (emacsify-path "src/keymap.h")))
