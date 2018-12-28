(in-package :c-parse)

;;;;Make a subdirectory that mimics the real directories

(defparameter *cache* "/home/imac/install/src/filecache")
(defun re-root-real-path (path &optional (base *cache*))
  "change the root of path to base, making sure that therer can be no outside reference.
only works if path actually exists."
  (let ((truename (uiop:truename* path)))
    (unless truename
	(error "does not exist ~s" path))
    (unless (uiop:absolute-pathname-p path)
      (error "not absolute:~s" path))
    (concatenate
     'string
     base
     (uiop:unix-namestring
      truename))))

(defun touch-cached-directories-and-files (path)
  (let ((reroot (re-root-real-path path)))
    (ensure-directories-exist reroot)
    ;;if its a file, touch it
    (unless (uiop:directory-pathname-p reroot)
      (touch-file reroot))
    reroot))
(defun reroot (path)
  (touch-cached-directories-and-files path))

(defun touch-file (&optional (path "/home/imac/install/src/touch.txt"))
  (with-open-file (stream path :if-does-not-exist :create)))

;;;;C preprocessor
;;;;ignore trigraphs
;;;;non-portably use newlines to iindicate line breaks, not mac or windows
;;;;lines which end in backslash are joined
;;LF -> unix, CR LF ->DOS,VMS, CR ->classic mac os
;;just support unix? uiop:read-file-lines

(defun line-attach-p (line)
  (let ((len (length line)))
    (unless (zerop len)
      (char= #\\ (uiop:last-char line)))))
(defun all-but-last-char-string (str)
  (let ((len (length str)))
    (if (zerop len)
	""
	(subseq str 0 (1- len)))))
(defun attach (lines end)
  (apply 'concatenate 'string
	 (nconc (mapcar 'all-but-last-char-string lines)
		(list end))))
(defun join-lines-list (&optional (file-lines '("bar\\" "foo")))
  (let ((acc nil)
	(lines-acc))
    (dolist (line file-lines)
      (if (line-attach-p line)
	  (push line lines-acc)
	  (progn
	    (push (attach (nreverse lines-acc) line)
		  acc)
	    (setf lines-acc nil))))
    (when lines-acc
      (push (attach (nreverse (butlast lines-acc))
		    (car (last lines-acc)))
	    acc))
    (nreverse acc)))
(defun join-lines (file)
  (let* ((file-lines (uiop:read-file-lines file))
	 (list (join-lines-list file-lines)))
    (with-open-file (output (reroot file) :direction :output :if-exists :overwrite)
      (let ((len (list-length list))
	    (count 0))
	(dolist (line list)
	  (when (< 0 count (1- len))
	    (write-char #\Newline output))
	  (write-string line output)
	  (incf count))))))

(define-c-parse-rule //comment ()
  (progn-v #\/
	   #\/
	   (times (progn (! #\Newline)
			 (v character)))))

(define-c-parse-rule white-char-no-newline ()
  (|| #\Space #\tab))
(define-c-parse-rule whitespace-no-newline ()
  (postimes white-char-no-newline)
  nil)

(define-c-parse-rule directive ()
  (progn-v (times white-char-no-newline)
	   #\#
	   (stringy (times (progn (! #\Newline)
				  (v character))))))

(define-c-parse-rule thing ()
  (|| directive
      (progn
	(|| whitespace-no-newline
	    lex-yacc-multiline-comment
	    //comment
	    character)
	nil)))
;;FIXME:: non-consing esrap-liquid?
(defparameter *file2*
  (alexandria:read-file-into-string "/home/imac/install/src/emacs-mirror/emacs-master/src/lisp.h"))
(defparameter *acc* nil)
(defun get-directives (&optional (text *file2*))
  (catch 'out
    (let ((start 0))
      (loop (multiple-value-bind (directive place)
		(parse-with-garbage 'thing text :start start)
	      (when (eql 0 place)
		(throw 'out nil))
	      (when directive
		(per-iter directive)
		)
	      (incf start place)))))
  (values))
(defun per-iter (directive)
  (terpri)
  (princ directive)
  (push directive *acc*))
