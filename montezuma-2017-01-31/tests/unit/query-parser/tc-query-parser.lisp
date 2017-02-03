(in-package #:montezuma)

(defclass test-query-parser (query-parser)
  ())

(defmethod $add-and-clause ((parser test-query-parser) clauses clause)
  (setf clauses (if (listp (car clauses)) clauses (list clauses)))
  (setf clauses (remove nil clauses))
  (when (= (length clauses) 1)
    (let ((last-clause (first clauses)))
      (let ((occur (second last-clause)))
	(when (not (eq occur :must-not-occur))
	  (setf (second last-clause) :must-occur)))))
  (if clause
      (progn
	(unless (eq (second clause) :must-not-occur)
	  (setf (second clause) :must-occur))
	(cons clause clauses))
      clauses))

(defmethod $add-or-clause :around ((parser test-query-parser) clauses clause)
  (if (listp (car clauses))
      (call-next-method)
      (call-next-method parser (list clauses) clause)))

(defmethod $get-fuzzy-query ((parser test-query-parser) field-value similarity)
  (setf similarity (if similarity (parse-float similarity) 0.5))
  (let ((field ($get-active-field parser)))
    (list :fuzzy-query field field-value similarity)))

(defmethod $get-match-all-query ((parser test-query-parser))
  (list :match-all-query))

(defmethod $get-term-query ((parser test-query-parser) word)
  (let* ((field ($get-active-field parser)))
    (list :term-query field word)))
#|
;;(term-text (car tokens))))
    ;;(tokens (all-tokens (slot-value parser 'analyzer) field word)))
    (cond ((= (length tokens) 0)
	   (list :term-query field ""))
	  ((= (length tokens) 1)
	   (list :term-query field (term-text (car tokens))))
	  (T
	   (list :phrase-query field (mapcar #'(lambda (token)
						 (term-text token)) ;(cons (term-text token) (token-increment token)))
					     tokens))))))
|#

(defmethod $get-field-range-query ((parser test-query-parser) lower upper)
  "Create a field range query from the active field (the field name last specified) and a lower and upper value.
The lower and upper values are provided with square or curly brackets for inclusive or exclusive range boundaries."
  (list :range-query
        ($get-active-field parser)
        (car lower)
        (cadr lower)
        (car upper)
        (cadr upper)))

(defmethod $get-boolean-clause ((parser test-query-parser) query occur)
  (list :boolean-clause (or occur (default-occur parser)) query))

(defmethod $get-boolean-query ((parser test-query-parser) clauses)
  (setf clauses (if (and (listp clauses) (listp (car clauses))) clauses (list clauses)))
  (list :boolean-query (reverse clauses)))

(defmethod $add-word-to-phrase ((parser test-query-parser) phrase word)
  (append (enlist phrase) (list word)))

(defmethod $get-phrase-query ((parser test-query-parser) words proximity)
  (setf words (enlist words))
  (if (length-1-list-p words)
      ($get-term-query parser (car words))
    (let ((field ($get-active-field parser)))
      (list :phrase-query field
            (mapcan
             #'(lambda (word)
                 (if (null word)
                     '()
                   (list word)))
                   ;;(let ((tokens (all-tokens (analyzer parser) field word)))
		   ;;  (mapcar #'(lambda (token) (term-text token)) tokens))))
             words)))))

(defmethod $get-wild-query ((parser test-query-parser) word)
  (list :wild-query ($get-active-field parser) word))

(defmethod get-bad-parse ((parser test-query-parser) query-string)
  (format t "~&Bad query: ~s!~%" query-string)
  ($set-query-field parser (default-field parser))
  (let* ((field ($get-active-field parser))
	 (tokens (all-tokens (make-instance 'standard-analyzer) field "english" query-string))
         (clauses ()))
    (cond ((zerop (length tokens))
           (push (list :term-clause field "") clauses))
          ((= (length tokens) 1)
           (push (list :term-clause field (term-text (elt tokens 0))) clauses))
          (T
           (mapcar
            #'(lambda (token)
                (push `(:term-clause :should-occur ,field ,(term-text token)) clauses))
            tokens)))
    ($get-boolean-query parser (reverse clauses))))

(defmethod $get-sorting-specification ((parser test-query-parser) sorting direction)
  (make-sort-definition parser (enlist sorting) (and direction (string= direction "-"))))

(defmethod make-sort-definition ((parser test-query-parser) sort-fields reverse)
  `(:sort-definition ,sort-fields ,@(if reverse (list :reverse-p reverse))))

(defmethod make-sorting ((parser test-query-parser) field reverse-p) ;; field-parser)
  `((:sort-field
     ,field
     ;;,@(if field-parser (list :sort-type (named field-parser)))
     ,@(if reverse-p (list :reverse-p reverse-p)))))

(defmethod make-query-parser-result ((parser test-query-parser) query &key
                                     sort-specification
                                     first-document
                                     number-of-documents
                                     chosen-fields)
  ;;(format t "make-query-parser-result query: ~s~%" query)
  (cond
   ((or (null query) (stringp query))
    (setf query ($get-boolean-query parser ($get-match-all-query parser))))
   ((not (eql (car query) :boolean-query))
    (setf query ($get-boolean-query parser query))))

  `(:query-parse-result
    :query ,query 
    :sort ,sort-specification
    ,@(if first-document (list :first-doc first-document))
    ,@(if number-of-documents (list :num-docs number-of-documents))
    ,@(if chosen-fields (list :chosen-fields (reverse chosen-fields)))))

(defun make-test-query-parser ()
  (make-instance 'test-query-parser :default-field "*" :field-definitions '(("f1") ("f2") ("field"))))

(defun test-parse-query (query-string)
  (let ((qp (make-test-query-parser)))
    (cadr (member :query (montezuma-parse qp query-string)))))

(defun test-parse-query-results (query-string)
  (montezuma-parse (make-test-query-parser) query-string))

(defun check-query-parse (query-string expected-parse-tree)
  (atest check-query-parse
	 (test-parse-query query-string)
	 expected-parse-tree
	 #'(lambda (a b) (tree-equal a b :test #'equal))
	 (format T "~&Query string was ~S" query-string))
  (atest check-real-query-parse
	 (query (montezuma-parse (make-instance 'query-parser
                                      :default-field "*"
                                      :field-definitions '(("blah") ("f1") ("f2") ("field"))) query-string))
	 'query
	 #'typep
	 (format T "~&Query string was ~S" query-string)))

(defun check-query-parse-results (query-string expected-result)
  (atest check-query-parse-results
	 (test-parse-query-results query-string)
	 expected-result
	 #'(lambda (a b) (tree-equal a b :test #'equal))
	 (format T "~&Query string was ~S" query-string))
  (atest check-real-query-parse
	 (montezuma-parse (make-instance 'query-parser :default-field "*" :field-definitions '(("f1") ("f2") ("field")))
                query-string)
         'query-parse-result
	 #'typep
	 (format T "~&Query string was ~S" query-string)))

(deftestfun query-parser
    (let ((tests
	   '(("abc"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc")))) )
	     ("abc def"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "def"))))) 
	     ("john's"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "john's")))))
	     ("john's email is jjwiseman@yahoo.com mail-to"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "john's"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "email"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "is"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "jjwiseman@yahoo.com"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "mail"))
                (:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:TERM-QUERY "*" "to")))))
	     ("+abc" (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :MUST-OCCUR (:TERM-QUERY "*" "abc")))))
	     ("abc +def ghi"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc"))
		(:BOOLEAN-CLAUSE :MUST-OCCUR (:TERM-QUERY "*" "def"))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "ghi"))))) 
	     ("foo*"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "foo*")))) )
	     ("foo?bar"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "foo?bar")))) )
	     ("*foo*"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "*foo*")))) )
	     ("*foo"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "*foo")))) )
	     ("*foo*bar"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "*foo*bar")))) )
	     ("+*foo"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :MUST-OCCUR (:WILD-QUERY "*" "*foo")))) )
	     ("!abc"
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:TERM-QUERY "*" "abc")))) )
	     ("abc !def"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc"))
		(:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:TERM-QUERY "*" "def")))))
	     ("\"abc\""
	      (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc")))))
	     ("\"abc def\""
              (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "*" ("abc" "def"))))))
	     ("abc \"def hij\""
              (:BOOLEAN-QUERY
               ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "abc"))
                (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "*" ("def" "hij"))))))
	     ("foo* \"bad dog\""
              (:BOOLEAN-QUERY
               ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "*" "foo*"))
                (:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "*" ("bad" "dog"))))))
	     ("field:value"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "field" "value")))))
	     ("field:foo*"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "field" "foo*")))))
	     ("field:*foo"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:WILD-QUERY "field" "*foo")))))
	     ("+field:abc"
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :MUST-OCCUR (:TERM-QUERY "field" "abc")))))
	     ("field:\"1 2 3\""
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "field" ("1" "2" "3"))))))
	     ("!\"ha ha\" !\"ha ha\" !\"ha ha\" \"ha ha\""
	      (:BOOLEAN-QUERY
	       ((:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:PHRASE-QUERY "*" ("ha" "ha")))
		(:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:PHRASE-QUERY "*" ("ha" "ha")))
		(:BOOLEAN-CLAUSE :MUST-NOT-OCCUR (:PHRASE-QUERY "*" ("ha" "ha")))
		(:BOOLEAN-CLAUSE :SHOULD-OCCUR (:PHRASE-QUERY "*" ("ha" "ha"))))))
             ("heavy~"
              (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:FUZZY-QUERY "*" "heavy" 0.5)))))
             ("heavy~0.8"
              (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:FUZZY-QUERY "*" "heavy" 0.8)))))
             ("[abacus to calculator]"
              (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:RANGE-QUERY "*" "abacus" :INCLUDE "calculator" :INCLUDE)))))
             ("{abacus to calculator}"
              (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:RANGE-QUERY "*" "abacus" :EXCLUDE "calculator" :EXCLUDE)))))
             ("{abacus to calculator]"
              (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:RANGE-QUERY "*" "abacus" :EXCLUDE "calculator" :INCLUDE)))))
             (""
              (:BOOLEAN-QUERY ((:MATCH-ALL-QUERY))))
	     ;; The point of this test is to trigger query-parser's
	     ;; get-bad-parse method by giving it a string that
	     ;; doesn't parse.
             #|
             ("blah::boom"
              (:BOOLEAN-QUERY ((:TERM-CLAUSE :SHOULD-OCCUR "*" "boom") (:TERM-CLAUSE :SHOULD-OCCUR "*" "blah"))))
             |#
             ))
          (results-tests
           '((""
              (:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:MATCH-ALL-QUERY))) :SORT (:SORT-DEFINITION (:DOCUMENT-ORDER))))
             ("/document"
              (:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:MATCH-ALL-QUERY))) :SORT (:SORT-DEFINITION (:DOCUMENT-ORDER))))
             ("/-document"
              (:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:MATCH-ALL-QUERY))) :SORT (:SORT-DEFINITION (:DOCUMENT-ORDER) :REVERSE-P T)))
             ("/score"
              (:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:MATCH-ALL-QUERY))) :SORT (:SORT-DEFINITION (:SCORE-ORDER))))
             ("/-score"
              (:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:MATCH-ALL-QUERY))) :SORT (:SORT-DEFINITION (:SCORE-ORDER) :REVERSE-P T)))
             ("/f1"
              (:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:MATCH-ALL-QUERY))) :SORT (:SORT-DEFINITION ((:SORT-FIELD "f1")))))
             ("/(f1 f2)"
              (:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:MATCH-ALL-QUERY))) :SORT (:SORT-DEFINITION ((:SORT-FIELD "f1") ((:SORT-FIELD "f2"))))))
             #|
             ("/f1 f2"
              ;; Invalid query raises an error condition
              (:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:TERM-CLAUSE :SHOULD-OCCUR "*" "f2")
                                                           (:TERM-CLAUSE :SHOULD-OCCUR "*" "f1")))
               :SORT (:SORT-DEFINITION (:DOCUMENT-ORDER))))
             |#
             ("/document:0,20"
              (:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:MATCH-ALL-QUERY))) :SORT (:SORT-DEFINITION (:DOCUMENT-ORDER)) :FIRST-DOC 0 :NUM-DOCS 20))
             ("/document:20,50"
              (:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:MATCH-ALL-QUERY))) :SORT (:SORT-DEFINITION (:DOCUMENT-ORDER)) :FIRST-DOC 20 :NUM-DOCS 50))
             ("boom/score[*]"
              (:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "boom"))))
               :SORT (:SORT-DEFINITION (:SCORE-ORDER)) :CHOSEN-FIELDS ("f1" "f2" "field")))
             ("boom/score[f1 f2]"
              (:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:BOOLEAN-CLAUSE :SHOULD-OCCUR (:TERM-QUERY "*" "boom"))))
               :SORT (:SORT-DEFINITION (:SCORE-ORDER)) :CHOSEN-FIELDS ("f1" "f2")))
              )))
      (dolist (test tests)
	(apply #'check-query-parse test))
      (dolist (test results-tests)
	(apply #'check-query-parse-results test))))

	    
#|
(run-test-function 'query-parser)

(:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:MATCH-ALL-QUERY))) :SORT :DOCUMENT-ORDER)
(:QUERY-PARSE-RESULT :QUERY (:BOOLEAN-QUERY ((:MATCH-ALL-QUERY))) :SORT (:DOCUMENT-ORDER))
(test-parse-query-results "boom/score[f1 f2]")
(test-parse-query-results "boom/score[*]")

|#
