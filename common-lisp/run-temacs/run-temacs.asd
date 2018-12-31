(asdf:defsystem #:run-temacs
  :author "terminal625"
  :license "MIT"
  :description "try to run emacs as shared library"
  :depends-on (#:cffi
	       #:utility)
  :serial t
  :components 
  ((:file "run")))
