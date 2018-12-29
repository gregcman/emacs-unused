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

(defun concatenate-string (&rest rest)
  (apply 'concatenate 'string rest))
;;be able to make a derived filename
(defparameter *path* "/home/imac/install/src/emacs-mirror/emacs-master/src/lisp.h")
(defun pathname-name-and-type (&optional (path *path*))
  (let ((name (pathname-name path))
	(type (pathname-type path)))
    (if (or name type)
	(concatenate-string
	 name
	 (if type
	     "."
	     nil)
	 type))))
(defun get-directory (&optional (path *path*))
  (make-pathname :directory (pathname-directory path)))
(defun add-file-extension (extension-fun &optional (path *path*))
  (let ((dir (get-directory path))
	(file (pathname-name-and-type path)))
    (merge-pathnames
     (concatenate-string (funcall extension-fun file))
     dir)))

;;(ADD-FILE-SUFFIX "~") lisp.h -> ~lisp.h
(defun add-file-suffix (suffix &optional (path *path*))
  (add-file-extension (lambda (x)
			(concatenate-string suffix x))
		      path))
;;(ADD-FILE-SUFFIX ".directive") lisp.h -> lisp.h.directive
(defun add-file-prefix (prefix &optional (path *path*))
  (add-file-extension (lambda (x)
			(concatenate-string x prefix))
		      path))
