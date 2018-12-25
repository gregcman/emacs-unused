(asdf:defsystem #:c-parse
  :author "terminal625"
  :license "MIT"
  :description "processed c parser"
  :depends-on (#:alexandria
	       #:split-sequence
	       #:uiop
	       #:esrap-liquid)
  :serial t
  :components 
  ((:file "c-parse")))
