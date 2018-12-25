(asdf:defsystem #:emacs-unused
  :author "terminal625"
  :license "MIT"
  :description "emacs unused"
  :depends-on (#:alexandria
	       #:split-sequence
	       #:uiop)
  :serial t
  :components 
  ((:file "headers")))
