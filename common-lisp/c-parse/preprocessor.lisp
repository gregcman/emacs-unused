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
	    (push (attach lines-acc line)
		  acc)
	    (setf lines-acc nil))))
    (when lines-acc
      (push (attach (butlast lines-acc)
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
	    (terpri output))
	  (write-string line output)
	  (incf count))))))
