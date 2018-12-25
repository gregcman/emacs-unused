(in-package :c-parse)

(fiveam:in-suite* c-parse)

(defun run-tests ()
  (let ((results (fiveam:run 'c-parse)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(defun regex-character-class-to-esrap-liquid (regex)
  (lex-rule-dump (parse-with-garbage 'lex-rule-character-class regex)))
(fiveam:test what
  (fiveam:is (char= (c-parse-parse 'lex-char-or-escaped-char "\\\\")
		    #\\))
  (fiveam:is (char= (c-parse-parse 'lex-char-or-escaped-char "\\t")
		    #\Tab))
  (fiveam:is (string= (c-parse-parse 'lex-string "\"234\\t234\"")
		      "234	234"))
  (fiveam:is (string= (char-to-escaped-char #\Newline)
		      "\\n"))
  (fiveam:is (string= (char-to-escaped-char #\s)
		      "s"))
  (fiveam:is (char= (parse-with-garbage
		     (regex-character-class-to-esrap-liquid "[^a-zA-Z_0-9]")
		     "$"))
	     #\$)
  (fiveam:is (char= (parse-with-garbage
		     (regex-character-class-to-esrap-liquid "[a-zA-Z_0-9]")
		     "S"))
	     #\S))
