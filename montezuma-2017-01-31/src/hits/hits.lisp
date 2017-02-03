#|

Copyright 2015-2016 Roy Anderson <reanz1959@gmail.com>

9/5/2015

Montezuma Hits extends searching by post-processing document results using queries parsed by Montezuma.
In addition to the document number and score, field values are fetched and where they match
all or part of the query terms, they are submitted for mark-up, for example: marking up matches.

|#

(in-package #:montezuma)

(defclass sentence-hits ()
  ((sentence :initform "" :initarg :sentence :type string :accessor sentence-hits-sentence)
   (indices :initform () :initarg :indices :type list :accessor sentence-hits-indices)))

(defmethod print-object ((self sentence-hits) stream)
  (print-unreadable-object (self stream :type T :identity nil)
    (with-slots (sentence indices) self
      (format stream "~%sentence: ~s indices: ~s~%" sentence indices))))

(defmethod size ((self field-expressions))
  (length (slot-value self 'fields)))

(defmethod empty ((self field-expressions))
  (zerop (size self)))

(defclass field-hits ()
  ;; sentence hits in named field
  ((name :initform "" :type string :accessor field-hits-name :initarg :name)
   (sentences :initform () :type list :accessor field-hits-sentences :initarg :sentences)))

(defmethod print-object ((self field-hits) stream)
  (print-unreadable-object (self stream :type T :identity nil)
    (with-slots (name sentences) self
      (format stream "field: ~s" name)
      (format stream " sentences ~s" sentences))))


(defun marshall-field (name fields)
  "Either return an existing marshalled term or create a new one and return it."
  (or (find name fields :test 'string-equal :key 'field-name)
      (let ((mf (make-instance 'field-expression :field name)))
        (vector-push-extend mf fields)
        mf)))

(defmethod field-manifest ((manifest field-expressions) field)
  (marshall-field field (fields manifest)))

;; FIXME: locks
(defmethod make-apparatus ((self index) query)
  (if query
      (let* ((parsed-result (process-query self query))
             (parsed-query (query parsed-result)))
        (rewrite parsed-query (reader self))
        (values (annotation-expressions parsed-query) parsed-result))))
#|
(defun sentence-delimiterp (ch)
  (find ch '(#\. #\? #\!)))
|#

(defun split-sentences (text striker &optional (delimiters '(#\. #\? #\!)))
  "iterate text sentences calling striker until it signals enough by returning a non-null value"
  (loop
   with start = 0
   while start
   for x = (position-if #'(lambda (ch) (find ch delimiters)) text :start start) do
   (if x (incf x)) ; include the trailing punctuation
   (funcall striker (subseq text start x))
   (unless (and x (< x (length text))) (return))
   (let ((y (position-if-not #'whitespace-char-p text :start x)))
     (if (and y (> y x)) (funcall striker (subseq text x y)))
     (setf x y))
   (setf start x)))

(defmethod precis ((docres document-result)
                   (apparatus field-expressions)
                   (field string)
                   &key processor hitfunc)
  (let* ((fex (field-manifest apparatus field))
         (scanner (scanner fex)))
    (unless scanner (return-from precis))
    (split-sentences
     (field-value docres field)
     #'(lambda(sentence)
         (when (satisfies-conditions fex sentence)
           (let ((matches (all-matches scanner sentence)))
             (when matches
               (loop
                with offset = 0
                for (starts ends) in matches
                while (and starts ends) do
                (if (> starts offset)
                    (funcall processor (subseq sentence offset starts)))
                (let ((word (subseq sentence starts ends)))
                  (funcall hitfunc word (find word (conditions fex) :test #'string-equal)))
                (setf offset ends)
                finally
                (if (< offset (length sentence))
                    (funcall processor (subseq sentence offset)))
                (return-from precis))))))
     ".;!?,;")))

(defmethod annotate ((docres document-result)
                     (apparatus field-expressions)
                     &key
                     hit-function
                     processor context
                     start-sentence
                     end-sentence)
  ;;(break "annotate ~s" apparatus)
  (loop
   for fex across (fields apparatus)
   as field = (field-name fex) do
   (add-annotations docres field
                    (get-text-annotations
                     fex
                     (field-value docres field)
                     :hitfunc hit-function
                     :processor processor
                     :context context
                     :start-sentence-match-func start-sentence
                     :end-sentence-match-func end-sentence
                     )))
  docres)

(defun all-matches (scanner text &optional (start 0))
  (let ((matches ()))
    (loop
     (multiple-value-bind (starting ending starts ends)
         (cl-ppcre:scan scanner text :start start)
       (unless (and starting ending) (return))
       (if (zerop (length starts))
           (push (list starting ending) matches)
         (push (list (aref starts 0) (aref ends 0)) matches))
       (setf start (1- ending))))
    (nreverse matches)))

#|
(defmethod indices ((hit field-hits))
  (if (field-hits-sentences hit)
      (sentence-hits-indices (car (field-hits-sentences hit)))))
|#

#|
(defmethod contents ((hit field-hits))
  (sentence-hits-sentence (car (field-hits-sentences hit))))
|#

(defmethod field-equal (scanner text)
  (multiple-value-bind (start end) (cl-ppcre:scan scanner text)
    (and start end (zerop start) (= (length text) end))))

(defmethod satisfies-conditions ((fex field-expression) sentence)
  (or (null (conditions-scanner fex))
      (cl-ppcre:scan (conditions-scanner fex) sentence)))

(defmethod process-sentence ((fex field-expression) stream sentence sentence-count last-sentence &key hitfunc processor context startfunc endfunc)
  (with-slots (scanner field) fex
    (when scanner
      (cond
       ((satisfies-conditions fex sentence)
        (let ((offset 0)
              (matches (all-matches scanner sentence)))
          (cond
           (matches
            (funcall startfunc stream field sentence-count)
            (loop
             for (starts ends) in matches
             while (and starts ends) do
             (if (> starts offset)
                 (funcall processor stream field (subseq sentence offset starts)))
             (let ((word (subseq sentence starts ends)))
               (funcall hitfunc stream field word (find word (conditions fex) :test #'string-equal)))
             (setf offset ends)
             finally
             (if (< offset (length sentence))
                 (funcall processor stream field (subseq sentence offset)))
             (setf last-sentence sentence-count)
             (funcall endfunc stream field sentence-count)))
           (t
            (if context
                (funcall context stream field sentence)
              (princ sentence stream))))))
       (t
        (princ sentence stream))))))

(defmethod get-text-annotations ((fex field-expression)
                                 text
                                 &key
                                 hitfunc processor context
                                 start-sentence-match-func
                                 end-sentence-match-func)
  (with-slots (field scanner) fex
    (let ((vector (make-vector))
          (sentence-count 0)
          (last-sentence -1))
      (setf text (string-trim  '(#\space) text))
      (with-output-to-string (stream vector)
        (when (field-equal scanner text)
          (funcall start-sentence-match-func stream field t) ; signal that the whole field is matched
          (funcall processor stream field text)
          (funcall end-sentence-match-func stream field t)
          (return-from get-text-annotations vector))

        (funcall start-sentence-match-func stream field nil)
        (split-sentences
         text
         #'(lambda(sentence)
             (incf sentence-count)
             (process-sentence
              fex
              stream
              sentence
              sentence-count
              last-sentence
              :hitfunc hitfunc
              :processor processor
              :context context
              :startfunc  start-sentence-match-func
              :endfunc end-sentence-match-func)))
        (funcall end-sentence-match-func stream field nil))
      (if (string= "" vector) text vector)
      vector)))

(defun test-search (index query)
  ;;  (let ((parsed-result (process-query index query)))
  (multiple-value-bind (apparatus parsed-query) (make-apparatus index query)
    (format t "parsed query ~s~%" (query parsed-query))
    (format t "rewritten ~s~%" (rewritten (query parsed-query)))
    (format t "apparatus ~s~%" apparatus)
    (search-each
     index
     (query parsed-query)
     #'(lambda (docnum score) (format t "docnum ~d score ~d~%" docnum score))
     `(:num-docs ,(or (mtz:number-of-documents parsed-query) (mtz:default-number-retrieved index))
       :first-doc ,(or (mtz:first-document parsed-query) 0)
       :sort ,(mtz:sort-specification parsed-query)))))

(defun test-annotate (index query)
  (multiple-value-bind (apparatus parsed-result) (make-apparatus index query)
    (search-each
     index
     (query parsed-result)
     #'(lambda (docnum score)
         (format t "docnum ~d score ~d~%" docnum score)
         (let ((docres (make-document-result index docnum score)))
           (annotate
            docres apparatus
            :hit-function #'(lambda(stream field x reqd) (declare (ignore stream)) (format t "<~a>~a</~a>" field (if reqd (string-upcase x) x) field))
            :processor #'(lambda(stream field x)  (declare (ignore stream field x))) ;(format stream "<intern>~a</intern>~%" x))
            :start-sentence #'(lambda(stream field x) (declare (ignore stream field x))) ; (format stream "<document>~a~%" x))
            :end-sentence #'(lambda(stream field x) (declare (ignore stream field x))) ; (format stream "</document>~%" x)))
            )))
     `(:num-docs ,(or (mtz:number-of-documents parsed-result) (mtz:default-number-retrieved index ))
       :first-doc ,(or (mtz:first-document parsed-result) 0)
       :sort ,(mtz:sort-specification parsed-result)))))

#|
(mtz::test-search library "human rights")
(mtz::test-search library "\"human rights\"")
(mtz::test-search library "\"freedom from fear\"")
(test-annotate library "\"freedom from fear\"")

(montezuma::annotated-search-apparatus oanc-corpus "chemical")
(montezuma::annotated-document oanc-corpus 0 "chemical" #'(lambda(name)"xxx") #'(lambda(name) "yyy") #'identity))
(annotated-search-results oanc-corpus "<span class='hit'>" "</span>" #'identity "chemical" 0 1000)
(montezuma::annotated-search-each montezuma::pastes-index "'mother'")
(montezuma::annotated-search-each montezuma::pastes-index "contents:\"expects type\"")
(montezuma::annotated-search-results montezuma::pastes-index "<<<<<<" ">>>>>>>>>>" #'identity "contents:\"expects type\"" 0 10)
(montezuma::annotated-search-each (get-corpus "RVBC")  "chemical")
(montezuma::annotated-search-each (get-corpus "RVBC")  "book:rom +grace" 0 10)
(montezuma::annotated-search-each library  "key:analects" 0 10)
(fields (car (annotated-search-results library #'identity #'identity #'identity (montezuma-parse (make-parser-with-index library) "key:cusa"))))
|#
