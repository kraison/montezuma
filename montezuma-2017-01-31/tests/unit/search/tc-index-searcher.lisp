(in-package #:montezuma)

(defun get-docs (score-docs)
  (mapcar #'doc score-docs))

(defun check-hits (is query expected &optional top total-hits)
  ;;  (format T "~&query: ~S expected: ~S~%" query expected)
  (let* ((top-docs (search-index is query :num-docs 50))
         (test-docs (get-docs (score-docs top-docs))))
    ;;(format T "~&got: ~S~%" test-docs)
    (atest check-hits-0 (length test-docs) (length expected)
	   #'=
	   (format T "~&Query: ~S~&Expected docs: ~S~&Actual docs: ~S" query expected test-docs))
    (when top
      (atest check-hits-1 top (doc (elt (score-docs top-docs) 0))))
    (if total-hits
	(atest check-hits-2 (total-hits top-docs) total-hits)
	(atest check-hits-3 (total-hits top-docs) (length expected)))
    (dosequence (score-doc (score-docs top-docs))
      (atest check-hits-4 (member (doc score-doc) expected) T #'bool=
	     (format T "~&Resulting doc ~S was not expected." (doc score-doc)))
      (atest check-hits-5
             (value (explain-score is (query query) (doc score-doc) (score score-doc)))
             (score score-doc)))))

(defun check-docs (is query expected &rest options)
  (let* ((top-docs (apply #'search-index is query options))
	 (docs (score-docs top-docs)))
    (atest check-docs-1 (length docs) (length expected))
    (dotimes (i (length docs))
      (atest check-docs-2 (doc (elt docs i)) (elt expected i)))))

(defun make-actual-query-parser ()
  (make-instance
   'query-parser
   ;;:analyzer (make-instance 'standard-analyzer)
   :default-field "*"
   :field-definitions '(("cat") ("date") ("field"))
   :default-occur 'default-occur))

(defun check-document-annotation (is qp query-string expected-annotations)
  (let* ((query-result (montezuma-parse qp query-string))
         (query (query query-result)))
    (rewrite query is)
    (let* ((apparatus (annotation-expressions query))
           (top-docs (search-index is (rewritten query) :num-docs 50))
           (doc-scores (mapcar #'(lambda(sd) (list (doc sd) (score sd))) (score-docs top-docs))))
      (when (if expected-annotations
                (null doc-scores)
              doc-scores)
        (format t "~&parsed query ~s~%" query)
        (format t "~&rewritten ~s~%" (rewritten query))
        (format t "~&apparatus ~s~%" apparatus))

      (loop
       for (docnum score) in doc-scores
       as document = (get-document is docnum)
       as docres = (make-document-result-from-document document docnum score)
       do (annotate
           docres
           apparatus
           :hit-function
           #'(lambda(stream field x required) (declare (ignore required)) (format stream "<~a>~a</~a>" field x field))
           :processor
           #'(lambda(stream field x) (declare (ignore field)) (format stream "<pre>~a</pre>" x))
           :start-sentence
           #'(lambda(stream field x)
               (declare (ignore field))
               (if x
                   (format stream "<sentence num=~a>" x)
                 (format stream "<doc>")))
           :end-sentence
           #'(lambda(stream field x)
               (declare (ignore field))
               (if x
                   (format stream "</sentence>")
                 (format stream "</doc>"))))
       collect (list docnum (annotations docres)) into annotated-documents
       finally
       (cond
        ((null expected-annotations)
         (if annotated-documents
             (format t "~&Query ~s~%Apparatus: ~s~%Results ~s" query-string apparatus  annotated-documents)
           (atest check-document-annotation-0 (null doc-scores) (null expected-annotations)
                  #'bool=
                  (format T "~&Query: ~S~&Expected docs: ~S~&Actual docs: ~S"
                          query-string
                          (mapcar #'first expected-annotations)
                          (mapcar #'first doc-scores)))))
        (expected-annotations
         (atest check-document-annotation-0 (length doc-scores) (length expected-annotations)
                #'=
                (format T "~&Query: ~S~&Expected docs: ~S~&Actual docs: ~S"
                        query-string
                        (mapcar #'first expected-annotations)
                        (mapcar #'first doc-scores)))
         (loop
          for (docnum annotations) in annotated-documents
          as expected = (cadr (assoc docnum expected-annotations)) do

          (atest check-document-annotation-1 expected T #'bool=
                 (format T "~&Resulting doc ~S was not expected." docnum))

          (atest check-document-annotation-2 (length annotations) (length expected) #'=
                 (format T "~&Resulting annotated number of fields ~S was not expected." (length annotations) (length expected)))
          (loop
           for (field field-annotation) in annotations
           for expected-field-annotation = (cadr (assoc field expected :test #'string-equal)) do
           (atest check-document-annotation-3 field-annotation expected-field-annotation #'equal
                  (format T "~&Field-REGULAR-EXPRESSION ~s Resulting annotations ~S was not expected."
                          (field-expression-regexp (field-expression apparatus field))
                          field-annotation)))))
        (t
         (format t "check-document-annotation ~s annotated ~s" query-string annotated-documents)))))))

(defun check-pastes-annotation (is qp query-string expected-annotations)
  (let* ((apparatus (annotation-expressions (query (montezuma-parse qp query-string))))
         (top-docs (search-index is (query apparatus) :num-docs 5))
         (doc-scores (mapcar #'(lambda(sd) (list (doc sd) (score sd))) (score-docs top-docs))))

    (when (if expected-annotations
              (null doc-scores)
            doc-scores)
      (format t "~&parsed query ~s~%" (query apparatus))
      (format t "~&rewritten ~s~%" (rewritten (query apparatus)))
      (format t "~&apparatus ~s~%" apparatus))
    ;;(break "check-pastes-annotation query ~s rewritten ~s" query rewritten)
    (loop
     for (docnum score) in doc-scores
     as document = (get-document is docnum)
     as docres = (make-document-result-from-document document docnum score)
     as hits = nil
     do (annotate
         docres
         apparatus
         :hit-function
         #'(lambda(stream field x required) (declare (ignore stream required))
             (let ((y (assoc field hits :test #'equal)))
               (when (or (null y)
                         (not (find x (cdr y) :test #'string-equal)))
                 (unless y (setf y (list field)) (push y hits))
                 (setf (cdr y) (append (cdr y) (list x))))))
         ;;(push (list field x) hits))
         :processor
         #'(lambda(stream field x) (declare (ignore stream field x)) )
         :start-sentence
         #'(lambda(stream field x) (declare (ignore stream field x)) )
         :end-sentence
         #'(lambda(stream field x) (declare (ignore field stream x)))
         :context 
         #'(lambda(stream field x) (declare (ignore stream field x)))
         )
     
     collect (list docnum (reverse hits)) into annotated-documents
     finally
     ;;(setf annotated-documents (gather-hits annotated-documents))
     (cond
      ((null expected-annotations)
       (if annotated-documents
           (format t "~&Query ~s~%Results ~s" query-string annotated-documents)
         (atest check-document-annotation-0 (null doc-scores) (null expected-annotations)
                #'bool=
                (format T "~&Query: ~S~&Expected docs: ~S~&Actual docs: ~S"
                        query-string
                        (mapcar #'first expected-annotations)
                        (mapcar #'first doc-scores)))))
      (expected-annotations
       (atest check-document-annotation-0 (length doc-scores) (length expected-annotations)
              #'=
              (format T "~&Query: ~S~&Expected docs: ~S~&Actual docs: ~S"
                      query-string
                      (mapcar #'first expected-annotations)
                      (mapcar #'first doc-scores)))
       (loop
        for (docnum annotations) in annotated-documents
        as expected = (cadr (assoc docnum expected-annotations)) do

        (atest check-document-annotation-1 expected T #'bool=
               (format T "~&Resulting doc ~S was not expected." docnum))

        (atest check-document-annotation-2 (length annotations) (length expected) #'=
               (format T "~&Resulting annotated number of fields ~S was not expected." (length annotations) (length expected)))
        (loop
         for (field field-annotation) in annotations
         for expected-field-annotation = (cadr (assoc field expected :test #'string-equal)) do
         (atest check-document-annotation-3 field-annotation expected-field-annotation #'equal
                (format T "~&Resulting annotations ~S was not expected." field-annotation)))))
      (t
       (format t "check-document-annotation ~s annotated ~s" query-string annotated-documents))))))

(deftestfixture index-searcher-test
  (:setup
   (setf (fixture-var 'dir) (make-instance 'ram-directory))
   (let ((iw (make-instance 'index-writer
			    :directory (fixture-var 'dir)
			    :analyzer (make-instance 'whitespace-analyzer)
			    :create-p T)))
     (setf (fixture-var 'documents) (index-test-helper-prepare-search-docs))
     (dosequence (doc (fixture-var 'documents))
       (add-document-to-index-writer iw doc))
     (close-down iw)
     (setf (fixture-var 'is) (make-instance 'index-searcher
					    :directory (fixture-var 'dir)))))
  (:teardown
   (close-down (fixture-var 'is))
   (close-down (fixture-var 'dir)))
  (:testfun test-index-searcher-get-doc
   (let ((is (fixture-var 'is)))
     (test index-searcher-get-doc-1 (max-doc is) 18)
     (test index-searcher-get-doc-2
	   (document-values (get-document is 0) :|date|)
	   "20050930"
	   #'string=)
     (test index-searcher-get-doc-3
	   (document-values (get-document is 4) :|cat|)
	   "cat1/sub2/subsub2"
	   #'string=)))
  (:testfun test-term-query
   (flet ((term-query (field value) (make-instance 'term-query :term (make-term field value)))
	  (check-hits (query expected)
	    (check-hits (fixture-var 'is) query expected)))
     (let ((tq (term-query "field" "word2")))
       (setf (boost tq) 100)
       (check-hits tq '(1 4 8)))
     (check-hits (term-query "field" "2342") '())
     (check-hits (term-query "field" "") '())
     (let* ((tq (term-query "field" "word1"))
            (top-docs (search-index (fixture-var 'is) tq))) ;(make-parser-result tq))))
       (test term-query-1 (total-hits top-docs) (length (fixture-var 'documents)))
       (test term-query-2 (length (score-docs top-docs)) 10)
       (let ((top-docs (search-index (fixture-var 'is) tq :num-docs 20)))
         (test term-query-3 (length (score-docs top-docs)) (length (fixture-var 'documents)))))))
  (:testfun test-first-doc
   (let* ((tq (make-instance 'term-query :term (make-term "field" "word1")))
          (is (fixture-var 'is)))
     (setf (boost tq) 100)
     (let* ((top-docs (search-index is tq :num-docs 100))
	    (expected (mapcar #' doc (score-docs top-docs))))
       (condition-test first-doc-1
		       (search-index is tq :first-doc -1)
		       'error)
       (condition-test first-doc-2
		       (search-index is tq :num-docs 0)
		       'error)
       (condition-test first-doc-3
		       (search-index is tq :num-docs -1)
		       'error)
       (check-docs is tq (subseq expected 0 8) :num-docs 8 :first-doc 0)
       (check-docs is tq (subseq expected 1 4) :num-docs 3 :first-doc 1)
       (check-docs is tq (subseq expected 2 8) :num-docs 6 :first-doc 2)
       (check-docs is tq '() :num-docs 2 :first-doc (length expected))
       (check-docs is tq '() :num-docs 2 :first-doc (+ (length expected) 100)))))
  (:testfun test-boolean-query
   (let ((bq (make-instance 'boolean-query))
	 (tq1 (make-instance 'term-query :term (make-term "field" "word1")))
	 (tq2 (make-instance 'term-query :term (make-term "field" "word3"))))
     (add-query bq tq1 :must-occur)
     (add-query bq tq2 :must-occur)
     (check-hits (fixture-var 'is) bq '(2 3 6 8 11 14) 14)
     (let ((tq3 (make-instance 'term-query :term (make-term "field" "word2"))))
       (add-query bq tq3 :should-occur)
       (check-hits (fixture-var 'is) bq '(2 3 6 8 11 14) 8)
       (let ((bq (make-instance 'boolean-query)))
	 (add-query bq tq2 :must-occur)
	 (add-query bq tq3 :must-not-occur)
	 (check-hits (fixture-var 'is) bq '(2 3 6 11 14)))
       (let ((bq (make-instance 'boolean-query)))
	 (add-query bq tq2 :must-not-occur)
	 (check-hits (fixture-var 'is) bq '()))
       (let ((bq (make-instance 'boolean-query)))
	 (add-query bq tq2 :should-occur)
	 (add-query bq tq3 :should-occur)
	 (check-hits (fixture-var 'is) bq '(1 2 3 4 6 8 11 14)))
       (let ((bq (make-instance 'boolean-query)))
	 (add-query bq tq1 :must-occur)
	 (add-query bq tq2 :should-occur)
	 (add-query bq tq3 :should-occur)
	 (check-hits (fixture-var 'is) bq '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))))))

  (:testfun test-boolean-query-with-must-not-and-phrase
   ;; Trying to tickle ticket:3
   (let ((query (montezuma-parse (make-actual-query-parser) "field:word1 -field:word2 field:\"word1 word3\"")))
     ;; REA: this test was supposed to include a must-not-occur but as it was, no documents were returned.
     ;; I have changed the query and expected results. It still has a :must-not query though.
     (check-hits (fixture-var 'is) query '(0 2 3 5 6 7 9 10 11 12 13 14 15 16 17)))) ; was: (0 1 2 3 4 5 6 7 8 9 10 11 12 13))))

  (:testfun test-phrase-query
   (let ((qp (make-actual-query-parser)))
     (check-hits (fixture-var 'is) (montezuma-parse qp "field:\"quick\"") '(1 11 14 16 17))
     (check-hits (fixture-var 'is) (montezuma-parse qp "field:\"quick brown\"") '(1))
     (check-hits (fixture-var 'is) (montezuma-parse qp "field:\"quick brown fox\"") '(1))
     (check-hits (fixture-var 'is) (montezuma-parse qp "field:\"quick . fox\"") '(1 11 14))
     (check-hits (fixture-var 'is) (montezuma-parse qp "field:\"quick . fox\"~1") '(1 11 14 16))
     (check-hits (fixture-var 'is) (montezuma-parse qp "field:\"quick . fox\"~4") '(1 11 14 16 17))
     (check-hits (fixture-var 'is) (montezuma-parse qp "field:\"the quick . fox\"") '(1 11 14))
     (check-hits (fixture-var 'is) (montezuma-parse qp "field:\"the quick . fox\"~4") '(1 11 14 16 17))
     (check-hits (fixture-var 'is) (montezuma-parse qp "field:\"the QUICK . FOX\"~4") '(1 11 14 16 17)))
   
   (let ((pq (make-instance 'phrase-query))
         (t1 (make-term "field" "quick"))
         (t2 (make-term "field" "brown"))
         (t3 (make-term "field" "fox")))
     (add-term-to-query pq t1)
     ;;(format t "pq1: ~s~%" pq)

     (check-hits (fixture-var 'is) pq '(1 11 14 16 17))
     (add-term-to-query pq t2)
     (check-hits (fixture-var 'is) pq '(1))

     (add-term-to-query pq t3)
     (check-hits (fixture-var 'is) pq '(1))
     
     (setf pq (make-instance 'phrase-query))
     (add-term-to-query pq t1)
     (add-term-to-query pq t3 2)
     (check-hits (fixture-var 'is) pq '(1 11 14))
     (setf (slop pq) 1)

     (check-hits (fixture-var 'is) pq '(1 11 14 16))
     (setf (slop pq) 4)
     (check-hits (fixture-var 'is) pq '(1 11 14 16 17)))

   (let ((pq (make-instance 'phrase-query))
	 (t1 (make-term "field" "blaaaargh"))
	 (t2 (make-term "field" "blaaaaaaaargh"))
	 (t3 (make-term "field" "blaaaaaaaaaarrgh!")))
     (add-term-to-query pq t1)
     (add-term-to-query pq t2)
     (add-term-to-query pq t3)
     (check-hits (fixture-var 'is) pq '())))

  (:testfun test-phrase-boolean-query
   (let ((pq (montezuma-parse (make-actual-query-parser) "-field:\"brown fox\" field:word1")))
     (check-hits (fixture-var 'is) pq '(0 2 3 4 5 6 7 9 10 11 12 13 14 15 16))))

  (:testfun test-range-query
   (let ((qp (make-actual-query-parser)))
     (check-hits (fixture-var 'is) (montezuma-parse qp "date:[20051006 to 20051010]") '(6 7 8 9 10))
     (check-hits (fixture-var 'is) (montezuma-parse qp "date:{20051006 to 20051010]") '(7 8 9 10))
     (check-hits (fixture-var 'is) (montezuma-parse qp "date:[20051006 to 20051010}") '(6 7 8 9))
     (check-hits (fixture-var 'is) (montezuma-parse qp "date:{20051006 to 20051010}") '(7 8 9))
     (check-hits (fixture-var 'is) (montezuma-parse qp "date:[. to 20051003]") '(0 1 2 3))
     (check-hits (fixture-var 'is) (montezuma-parse qp "date:[. to 20051003}") '(0 1 2))))

  (:testfun test-wildcard-query
   (let ((qp (make-actual-query-parser)))
     (check-hits (fixture-var 'is) (montezuma-parse qp "cat:cat1*") '(0 1 2 3 4 13 14 15 16 17))
     ;;(check-hits (fixture-var 'is) (montezuma-parse qp "cat1*/su??ub2") '(4 16)) ;  this test fails because "/" is a query operator 
     (check-hits (fixture-var 'is) (montezuma-parse qp "cat:cat1*?su??ub2") '(4 16))
     (check-hits (fixture-var 'is) (montezuma-parse qp "cat:cat1*&slash;su??ub2") '(4 16))))

  (:testfun test-multi-phrase-query
   (let ((mpq (montezuma-parse (make-actual-query-parser) "field:\"quick fast\"~4 field:\"brown red hairy\"~4 field:fox")))
     (check-hits (fixture-var 'is) mpq '(1 8 11 14 16 17))))
  
  (:testfun test-annotation
   (let* ((is (fixture-var 'is))
          (qp (make-actual-query-parser)))

     (check-document-annotation
      is qp "field:\"the quick brown fox\""
      '((1 (("field" "<doc><sentence num=1><pre>word1 word2 </pre><field>the quick brown fox</field></sentence></doc>")))))

     (check-document-annotation
      is qp "field:\"the . brown fox\""
      '((8 (("field" "<doc><sentence num=1><pre>word1 word2 word3 </pre><field>the fast brown fox</field></sentence></doc>")))
        (1 (("field" "<doc><sentence num=1><pre>word1 word2 </pre><field>the quick brown fox</field></sentence></doc>")))))

     (check-document-annotation
      is qp "field:(+\"the . brown fox\" +word1)"
      '((8 (("field" "<doc><sentence num=1><field>word1</field><pre> word2 word3 </pre><field>the fast brown fox</field></sentence></doc>")))
        (1 (("field" "<doc><sentence num=1><field>word1</field><pre> word2 </pre><field>the quick brown fox</field></sentence></doc>")))))

     (check-document-annotation
      is qp "field:(+\"the . brown fox\" word1)"
      '((8 (("field" "<doc><sentence num=1><field>word1</field><pre> word2 word3 </pre><field>the fast brown fox</field></sentence></doc>")))
        (1 (("field" "<doc><sentence num=1><field>word1</field><pre> word2 </pre><field>the quick brown fox</field></sentence></doc>")))))

     #|
      BEWARE the following searches fail because we don't have an index reader available!!!

     (check-document-annotation
      is qp "field:word*"
      '())

     (check-document-annotation
      is qp "field:(+\"the . brown fox\" word*)"
      '())

     |#

     (check-document-annotation is qp "field:\".\"" '())

     (check-document-annotation
      is qp "field:quick"
      '((14 (("field" "<doc><sentence num=1><pre>word1 word3 the </pre><field>quick</field><pre> hairy fox</pre></sentence></doc>")))
        (17 (("field" "<doc><sentence num=1><pre>word1 the brown fox is </pre><field>quick</field><pre> and red</pre></sentence></doc>")))
        (11 (("field" "<doc><sentence num=1><pre>word1 word3 the </pre><field>quick</field><pre> red fox</pre></sentence></doc>")))
        (16 (("field" "<doc><sentence num=1><pre>word1 the </pre><field>quick</field><pre> fox is brown and hairy and a little red</pre></sentence></doc>")))
        (1 (("field" "<doc><sentence num=1><pre>word1 word2 the </pre><field>quick</field><pre> brown fox</pre></sentence></doc>")))))

     (check-document-annotation
      is qp "field:(quick fox)"
      '((14 (("field" "<doc><sentence num=1><pre>word1 word3 the </pre><field>quick</field><pre> hairy </pre><field>fox</field></sentence></doc>")))
        (17 (("field" "<doc><sentence num=1><pre>word1 the brown </pre><field>fox</field><pre> is </pre><field>quick</field><pre> and red</pre></sentence></doc>")))
        (11 (("field" "<doc><sentence num=1><pre>word1 word3 the </pre><field>quick</field><pre> red </pre><field>fox</field></sentence></doc>")))
        (16 (("field" "<doc><sentence num=1><pre>word1 the </pre><field>quick</field><pre> </pre><field>fox</field><pre> is brown and hairy and a little red</pre></sentence></doc>")))
        (8 (("field" "<doc><sentence num=1><pre>word1 word2 word3 the fast brown </pre><field>fox</field></sentence></doc>")))
        (1 (("field" "<doc><sentence num=1><pre>word1 word2 the </pre><field>quick</field><pre> brown </pre><field>fox</field></sentence></doc>")))))
     ))
  
  (:testfun test-pastes-annotation
   (let* ((pastes ;setf (fixture-var 'pastes)
         (make-instance
          'mtz:index
          :path (merge-pathnames "contrib/pastes-1000/pasteindex" cl-user::*montezuma-root*)
          :document-root (merge-pathnames "contrib/pastes-1000/pastes.sexp" cl-user::*montezuma-root*)
          :title "1000 documents submitted to lisppastes"
          :document-key "number"
          :create-p NIL
          :create-if-missing-p NIL
          :index-key "PASTES"
          :name "Lisp Pastes"
          ;; Unless otherwise specified, queries will search all fields simultaneously.
          :default-field "*"
          :fields '("date" "displaydate" "number" "user" "channel" "title" "contents")
          :default-number-retrieved 1
          :field-definitions
          '(("date" :stored nil :index :untokenized)
            ("displaydate" :stored t :index :untokenized :type :date
             :form ((:year 4)-(:month 2)-(:day 2)" "(:hour 2)":"(:min 2)":"(:sec 2)))
            ("number" :stored t :index :untokenized :type :integer)
            ("user" :stored t :index :tokenized)
            ("channel" :stored t :index :tokenized)
            ("title" :stored t :index :tokenized)
            ("contents" :stored t :index :tokenized :height 20 :width 80 :blockquote t :linebreaks t :preformatted t))))
         (qp (make-instance 'query-parser :fields (mapcar #'first (field-definitions pastes)) :default-field "contents" :default-occur 'default-occur)))

     (check-pastes-annotation
      pastes qp "read*"
      '((32 (("contents" "readDec" "read" "readBin" "readOct" "readHex" "readInt" "ReadS")))
        (830 (("contents" "readSort" "read" "readInt")))
        (734 (("contents" "read_term" "read" "reader" "read-prolog1" "read_clause" "read_prompt" "read_code_tail" 
               "read_comma_list" "read_number" "read_atom" "read_qatom" "read_string" "read_atomic" "readto" "read_list"
               "read_code_cl" "readmacro")))
        (838 (("contents" "readline5" "README")))
        (449 (("contents" "READ_PROC_BODY" "READ_PROC" "READ_DECLARATIONS")))))

     (check-pastes-annotation
      pastes qp "*ated"
      '((550 (("contents" "concatenated")))
        (143 (("contents" "DG_DISABLE_DEPRECATED" "DGDK_PIXBUF_DISABLE_DEPRECATED" "DGDK_DISABLE_DEPRECATED")))
        (167 (("contents" "generated"))) (917 (("contents" "p_created")))
        (959 (("contents" "Rotated" "Translated")))))

     (check-pastes-annotation
      pastes qp "defun word*"
      '((407 (("contents" "defun" "word1" "word2")))
        (855 (("contents" "word" "words")))
        (701 (("contents" "word")))
        (702 (("contents" "word")))
        (790 (("contents" "defun" "words")))))

     )))


#|
(montezuma-parse (make-actual-query-parser) "field:word1 -field:word2 field:\"word1 word3\"")
(run-test-named 'index-searcher-test)
|#