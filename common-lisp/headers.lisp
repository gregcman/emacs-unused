(defpackage :emacs-unused
  (:use :cl))
(in-package :emacs-unused)
(defun alphabetical-sort (list)
  (sort list 'string<))

(defparameter *path* (asdf:system-source-directory :emacs-unused))
;;generated via grepping .h and .c files for "#include <"
(defparameter *includes* (merge-pathnames "includes.txt" *path*))

(defun find-included (string)
  (subseq string
	  (+ 1 (position #\< string))
	  (position #\> string)))

(defparameter *include-data*
  (alphabetical-sort
   (remove-duplicates 
    (mapcar #'find-included
	    (remove ""
		    (split-sequence:split-sequence #\Newline
						   (alexandria:read-file-into-string *includes*))
		    :test 'string=))
    :test 'string=)))

(defun print-list (&optional (data *include-data*))
  (dolist (item data)
    (terpri)
    (princ item)))

(defun test-output (&optional (include-name "stdbool.h"))
  ;;ripped from https://stackoverflow.com/questions/13079650/how-can-i-find-the-header-files-of-the-c-programming-language-in-linux
  (let ((command-string
	 (format nil "echo '#include <~A>' | cpp -H -o /dev/null 2>&1 | head -n1" include-name)))
    (chop-trailing
     (with-output-to-string (var)
       (uiop:run-program command-string :output var)))))
(defun chop-trailing (string)
  "bash returns output with extra newline. chop it off"
  (subseq string 0 (1- (length string))))

(defun test69 ()
  (mapc
   (lambda (x)
     (print (test-output x)))
   *include-data*)
  (values))

;;./configure --without-all --without-x CC=clang CFLAGS=-H ;;emacs by default has
;;;optimizations [-O2] turned on and debugging [-g and -g3] ;;-H means print the headers used

;;make &> compile.txt ;;the &> pipes all output including errors to compile.txt. > won't

(defparameter *includes-h* (merge-pathnames "compile.txt" *path*))
;;header files in *includes-h* start on a line which begins with #\.
(defun header-line-p (string)
  (let ((end? (position #\Space string)))
    (when (or (not end?)
	      (zerop end?))
      (return-from header-line-p nil))
    (dotimes (i end?)
      (unless (char= #\. (aref string i))
	(return-from header-line-p nil))))
  t)

;;.... /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
;;4, "/usr/include/x86_64-linux-gnu/bits/libc-header-start.h"
(defun chop2 (&optional (string ".... /usr/include/x86_64-linux-gnu/bits/libc-header-start.h"))
  (let ((spacepos
	 (position #\Space string)))
    (values (length (subseq string 0 spacepos))
	    (subseq string (+ 1 spacepos) (length string)))))
(defparameter *includes-h-hash* (make-hash-table :test 'equal))
(defparameter *total-scanned* 0)
(defun test56 ()
  (declare (optimize (debug 3)))
  (let ((eof (list "eof"))
	header-stack)
    (flet ((reset-header-stack ()
	     (setf header-stack (list (list 0 "root element")))))
      (reset-header-stack)
      (setf *total-scanned* 0)
      (block out
	(with-open-file (stream *includes-h*)
	  (loop
	     (let ((value 
		    (read-line stream nil eof)))
	       (cond ((eq eof value)
		      (return-from out))
		     (t
		      (cond ((header-line-p value)
			     (incf *total-scanned*)
			     (multiple-value-bind (depth path) (chop2 value)
			       (unless (gethash path *includes-h-hash*)
				 ;;push an empty list to start a header
				 (setf (gethash path *includes-h-hash*) nil))
			       (destructuring-bind (last-depth last-path) (first header-stack)
				 (cond ((> depth last-depth) ;;child of last element
					;;push it as a child
					(pushnew path (gethash last-path *includes-h-hash*)
						 :test 'string=
						 ))
				       (t ;;pop header stack until top is less than depth
					(block cya
					  (loop
					     (destructuring-bind (top-depth top-path)
						 (first header-stack)
					       (declare (ignorable top-path))
					       (if (> depth top-depth)
						   (return-from cya)
						   (pop header-stack))))))))
			       (push (list depth path) header-stack)))
			    (t
			     (reset-header-stack))))))))))))

(defparameter *includes-h-alist*
  (progn
    (test56)
    (alexandria:hash-table-alist *includes-h-hash*)))

;;headers that may be in $EMACS_SRC_ROOT/src or $EMACS_SRC_ROOT/lib-src
(defparameter *includes-h-in-emacs-source*
  (alphabetical-sort
   (mapcar #'first
	   (remove-if #'uiop:absolute-pathname-p *includes-h-alist* :key 'car))))
;;headers that are installed separately from emacs
(defparameter *includes-h-in-system-libs*
  (alphabetical-sort
   (mapcar #'first
	   (remove-if-not #'uiop:absolute-pathname-p *includes-h-alist* :key 'car))))

;;compile system libs into a shared library?

;;apparently many headers have the same file name, but not the same path. 
(defun check-header-name-conflict ()
  (= (print (length *includes-h-alist*))
     (print (length (remove-duplicates (mapcar (lambda (item)
						 (pathname-name (first item)))
					       *includes-h-alist*)
				       :test 'string=)))))

;;get vacietis to parse all these headers?

;;old dump by using grep, not clang -H
#|
X11/Intrinsic.h
X11/Xlib.h
X11/Xos.h
X11/Xresource.h
Xm/Xm.h
Xm/XmStrDefs.h
acl.h
alloca.h
allocator.h
alsa/asoundlib.h
arpa/inet.h
asoundlib.h
binary-io.h
bsdtty.h
byteswap.h
c-ctype.h
c-strcase.h
careadlinkat.h
close-stream.h
conf_post.h
config.h
count-leading-zeros.h
count-one-bits.h
count-trailing-zeros.h
dirent.h
dosname.h
epaths.h
errno.h
execinfo.h
fcntl.h
filemode.h
filevercmp.h
flexmember.h
float.h
fpending.h
fsusage.h
ftoastr.h
glib.h
gnutls/gnutls.h
gnutls/x509.h
gpm.h
grp.h
ifaddrs.h
ignore-value.h
intprops.h
inttypes.h
langinfo.h
lcms2.h
libxml/HTMLparser.h
libxml/parser.h
libxml/tree.h
limits.h
locale.h
machine/soundcard.h
math.h
mbstring.h
memory.h
mmsystem.h
net/if.h
net/if_dl.h
netdb.h
netinet/in.h
netio.h
otf.h
procfs.h
pthread.h
pty.h
pwd.h
sched.h
selinux/context.h
selinux/selinux.h
setjmp.h
share.h
sig2str.h
signal.h
soundcard.h
stat-time.h
stdalign.h
stdarg.h
stdbool.h
stddef.h
stdint.h
stdio.h
stdlib.h
strftime.h
string.h
sym.h
sys/acl.h
sys/attr.h
sys/bsdtty.h
sys/elf_mips.h
sys/fcntl.h
sys/file.h
sys/ioctl.h
sys/mman.h
sys/param.h
sys/prctl.h
sys/pty.h
sys/ptyio.h
sys/resource.h
sys/select.h
sys/socket.h
sys/soundcard.h
sys/stat.h
sys/sysctl.h
sys/sysinfo.h
sys/systeminfo.h
sys/time.h
sys/types.h
sys/un.h
sys/utsname.h
sys/wait.h
tempname.h
termios.h
time.h
timespec.h
unistd.h
util.h
utimens.h
utmp.h
valgrind/memcheck.h
valgrind/valgrind.h
verify.h
vla.h
w32term.h
wchar.h
windows.h
zlib.h
|#
