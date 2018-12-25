(defpackage :c-parse
  (:use :cl))
(in-package :c-parse)

(defparameter *path* (asdf:system-source-directory :c-parse))
;;generated via grepping .h and .c files for "#include <"
(defparameter *lex-txt-path* (merge-pathnames "lex.txt" *path*))
(defparameter *yacc-txt-path* (merge-pathnames "yacc.txt" *path*))

