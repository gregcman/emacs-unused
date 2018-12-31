(defpackage :run-temacs
  (:use :cl :utility))
(in-package :run-temacs)

(defparameter *shared-lib-path* "/home/imac/install/src/emacs-mirror/emacs-master/src/temacs.so")
(defun load-temacs-executable ()
  (cffi:load-foreign-library *shared-lib-path*))

(cffi:defcfun "runthetemacs" :int
  (argc :int)
  (argv (:pointer (:pointer :char))))

(defun invoke-temacs-main (&rest strings)
  (load-temacs-executable)
  (call-with-foreign-strings
   (list* "./temacs" strings)
   (lambda (foreign-strings)
     (call-with-foreign-array-of-foreign-strings
      foreign-strings
      (lambda (argc argv)
	(runthetemacs argc argv))))))

;;
(defun call-with-foreign-array-of-foreign-strings
    (foreign-strings
     &optional
       (fun (lambda (argc argv)
	      ;;(print argv)
	      (dotimes (i argc)
		;;(print i)
		(let ((string-pointer (cffi:mem-aref argv :pointer i)))
		  ;;(print string-pointer)
		  (print (cffi:foreign-string-to-lisp
			  string-pointer)))))))
  (let ((len (length foreign-strings)))
    (cffi:with-foreign-object
     (argv :pointer len)
     (dotimes (i len)
       ;;(print i)
       (let ((string (elt foreign-strings i)))
	 (setf (cffi:mem-aref argv :pointer i)
	       string)))
     (let ((argc len))
       (funcall fun argc argv)))))
(defun call-with-foreign-strings
    (&optional
       (strings '("hello" "world"))
       (fun
					;	#+nil
	(lambda (strings)
	  (call-with-foreign-array-of-foreign-strings strings))
	#+nil
	(lambda (strings)
	  (dolist (string strings)
	    (print (cffi:foreign-string-to-lisp string))))))
  (labels ((rec (acc strings)
	     (if strings
		 (cffi:with-foreign-string (string (car strings))
		   (rec (cons string acc)
			(cdr strings)))
		 (funcall fun (nreverse acc)))))
    (rec nil strings)))
