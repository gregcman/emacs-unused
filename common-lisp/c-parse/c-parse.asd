(asdf:defsystem #:c-parse
  :author "terminal625"
  :license "MIT"
  :description "processed c parser"
  :depends-on (#:alexandria
	       #:split-sequence
	       #:uiop
	       #:esrap-liquid
	       #:utility
	       #:yacc
	       #:uncommon-lisp
	       #:trivia
	       #:fiveam)
  :serial t
  :components 
  ((:file "c-parse")
   (:file "c-parse-test")
   (:file "yacc")))
