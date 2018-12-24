(defpackage :emacs-unused
  (:use :cl))
(in-package :emacs-unused)

(defparameter *path* (asdf:system-source-directory :emacs-unused))
(defparameter *includes* (merge-pathnames "includes.txt" *path*))

(defun find-included (string)
  (subseq string
	  (+ 1 (position #\< string))
	  (position #\> string)))

(defparameter *include-data*
  (sort 
   (remove-duplicates 
    (mapcar #'find-included
	    (remove ""
		    (split-sequence:split-sequence #\Newline
						   (alexandria:read-file-into-string *includes*))
		    :test 'string=))
    :test 'string=)
   'string<))

(defun print-includes ()
  (dolist (item *include-data*)
    (terpri)
    (princ item)))

(defun test-output (&optional (include-name "stdbool.h"))
  (let ((command-string
	 (format nil "echo '#include <~A>' | cpp -H -o /dev/null 2>&1 | head -n1" include-name)))
    (with-output-to-string (var)
      (uiop:run-program command-string :output var))))

(defun test69 ()
  (mapc
   (lambda (x)
     (print (test-output x)))
   *include-data*))

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
