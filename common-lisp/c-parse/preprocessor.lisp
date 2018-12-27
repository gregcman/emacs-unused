(in-package :c-parse)

(defun pathname-to-string-filename (path)
  (pathname-))

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

