(in-package :c-parse)

(define-c-parse-rule yacc-token-line ()
  (v "%token")
  (times (progn-v whitespace
		  lex-token-string)))
(define-c-parse-rule yacc-start-line ()
  (v "%start")
  (progn-v whitespace
	   lex-token-string))

(defun foobar65 ()
  "return (values #<list-of-tokens> start)"
  (values (alexandria:flatten
	   (mapcar (lambda (line)
		     (parse-with-garbage 'yacc-token-line line))
		   *yacc-tokens-lines*))
	  (first
	   (alexandria:flatten
	    (mapcar (lambda (line)
		      (parse-with-garbage 'yacc-start-line line))
		    *yacc-tokens-lines*)))))

(defparameter *yacc-token-strings* nil)
(defparameter *yacc-start-string* nil)
(defun foobar67 ()
  (setf (values *yacc-token-strings* *yacc-start-string*)
	(foobar65)))

;;FIXME::does not handle // comments
(define-c-parse-rule yacc-whitespace-or-comment ()
  (|| whitespace
      lex-yacc-multiline-comment))
(define-c-parse-rule yacc-whitespace-or-comments ()
  (postimes yacc-whitespace-or-comment))

(define-c-parse-rule yacc-grammar-one-expansion ()
  (v yacc-whitespace-or-comments)
  (|| lex-read-char
      lex-token-string))
(define-c-parse-rule yacc-rule-aux ()
  (times yacc-grammar-one-expansion))
(define-c-parse-rule yacc-rule ()
  (list* (prog1 (v lex-token-string)
	   (v yacc-whitespace-or-comments))
	 (prog1 (list*
		 (progn
		   (v #\:)
		   (v yacc-rule-aux))
		 (times (progn-v yacc-whitespace-or-comments
				 #\|
				 yacc-rule-aux)))
	   (v yacc-whitespace-or-comments)
	   (v #\;)
	   (v #\Newline))))
(define-c-parse-rule yacc-rules ()
  (times yacc-rule))

(defun foobar68 ()
  (parse-with-garbage 'yacc-rules *yacc-definitions*))

(defparameter *yacc-grammar* nil)
(defun foobar69 ()
  (setf *yacc-grammar* (foobar68)))

(defparameter *yacc-terminal-chars* nil) ;;because characters are terminals. FIXME:documentation
(defparameter *yacc-package* (make-package "YACC-SYMBOLS"))
(defun yacc-symbol (x)
  (if x
      (let ((string (etypecase x
		      (string (format nil "string%~a" x))
		      (character
		       (pushnew x *yacc-terminal-chars*)
		       (format nil "character%-~a" x))
		      (symbol (format nil "symbol%~a" x)))))
	(intern string *yacc-package*))
      x))

(progn
  (struct-to-clos:struct->class
   ;;represent a contiguous region of memory
   (defstruct character-section
     data
     start
     end))
  (defun print-character-section (stream object)
    (write (character-section-data object) :stream stream)
    (format stream "<~a,~a>"
	    (character-section-start object)
	    (character-section-end object)))
  (set-pprint-dispatch 'character-section 'print-character-section))
#+nil
"A  parser  consumes  the  output  of  a  lexer,  that  produces  a  stream  of  terminals.   CL-Yacc
expects the lexer to be a function of no arguments (a
thunk
) that returns two values:  the next
terminal symbol, and the value of the symbol, which will be passed to the action associated with
a production.  At the end of the input, the lexer should return
nil
."
(defun lex-for-cl-yacc (string &key (start 0) (end nil))
  (lambda ()
    (block out
      (tagbody try-again
	 (multiple-value-bind (result len)
	     (parse-with-garbage 'lexer-foo string :start start)
	   (let ((old-pos start)
		 (new-pos (+ start len)))
	     (setf start new-pos)
	     (when (or (and end (> new-pos end))
		       (zerop len))
	       (return-from out (values nil nil)))
	     (destructuring-bind (string-thing ignorable yacc-token-type) result
	       (declare (ignorable string-thing yacc-token-type ignorable))
	       ;;(write-char (char-code-object yacc-token-type) stream)
	       ;;(princ (stringy (car result)) stream)
	       
	       ;;to skip over whitespace
	       (when (not yacc-token-type)
		 (go try-again))
	       (return-from out
		 (values yacc-token-type
			 (make-character-section
			  :data (stringy string-thing)
			  :start old-pos
			  :end new-pos)
			 ))
	       
	       ))
	   )))))
(defparameter *yacc-start-symbol* (yacc-symbol *yacc-start-string*))
(defparameter *yacc-grammar-symbols* (tree-map 'yacc-symbol *yacc-grammar*))
(defparameter *yacc-token-symbols* (tree-map 'yacc-symbol
					     (append *yacc-token-strings*
						     *yacc-terminal-chars*)))

(defun tree-map (fn tree)
  "replace each list-element in tree with (funcall fn list-element)"
  ;;(tree-map (lambda (x) (* x x)) '(1 2 (2 3) . foo)) -> (1 4 (4 9) . FOO)
  (cond ((atom tree) (funcall fn tree))
	(t (cons (tree-map fn (first tree))
		 (let ((rest (rest tree)))
		   (if (and rest
			    (listp rest))
		       (tree-map fn rest)
		       rest))))))

(utility:etouq
  `(define-parser *c*
     (:start-symbol
      ;;don't 
      ;;,*yacc-start-symbol*
      ,(yacc-symbol "external_declaration")
      )
     (:terminals ,*yacc-token-symbols*)
     ,@*yacc-grammar-symbols*))
(defun parsefoobar (string &key (start 0) (end nil))
  (block out
    (tagbody try-again
       (handler-case
	   (return-from out
	     (values (yacc:parse-with-lexer (lex-for-cl-yacc string :start start :end end) *c*)
		     end))
	 (yacc-parse-error (c)
	   (when (eq nil (first (yacc-parse-error-expected-terminals c)))
	     (setf end (character-section-start (yacc-parse-error-value c)))
	     (go try-again)))))))

(defparameter *c-data-0*
  "typedef struct tagNode
{
    enum tagNode* entry;

    struct tagNode* next;
} Node;
")


