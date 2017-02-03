(in-package #:montezuma)

(defclass index-searcher ()
  ((similarity :accessor similarity :initarg :similarity)
   (reader :accessor reader)
   (search-directory :accessor search-directory))
  (:default-initargs
    :similarity (make-default-similarity)))

(defmethod initialize-instance :after ((self index-searcher) &key
                                       directory reader)
  (setf (slot-value self 'reader)
        (initialize-reader self (or directory reader))))

(defgeneric initialize-reader (index-searcher reader))

(defmethod initialize-reader ((self index-searcher) (reader index-reader))
  (values reader))

(defmethod initialize-reader ((self index-searcher) (directory virtual-directory))
  (setf (search-directory self) directory)
  (open-index-reader directory :close-directory-p nil))

(defmethod initialize-reader ((self index-searcher) (reader string))
  (setf (search-directory self) (make-fs-directory reader))
  (open-index-reader (search-directory self) :close-directory-p t))

(defmethod close-down ((self index-searcher))
  ;; delegate
  (close-down (reader self)))

(defmethod term-doc-freq ((self index-searcher) (term term))
  ;; delegate
  (term-doc-freq (reader self) term))

(defgeneric term-doc-freqs (index-searcher terms))

(defmethod term-doc-freqs ((self index-searcher) (terms sequence))
  (let ((result (make-array (length terms))))
    (dosequence (i terms)
      (setf (aref result i)
            (term-doc-freq self (aref terms i))))
    (values result)))

(defmethod get-document ((self index-searcher) index)
  ;; delegate
  (get-document (reader self) index))

(defmethod max-doc ((self index-searcher))
  ;; delegate
  (max-doc (reader self)))

(defmethod create-weight ((self index-searcher) query)
  (weight query self))

(defgeneric get-sort-specification (sort reverse-p))

(defmethod get-sort-specification ((sort symbol) reverse-p)
  (case sort
    (:document-order (make-document-order-sort-field reverse-p))
    (:score-order (make-score-order-sort-field reverse-p))
    (t (error "Unrecognised sort specification: ~a" sort))))

(defmethod get-sort-specification ((sort sort-field) reverse-p)
  (case (id sort)
    (:document-order sort)
    (:score-order sort)
    (t (error "Unrecognised sort specification: ~a" sort))))

(defmethod get-sort-specification ((sort sort-definition) reverse-p)
    sort)

(defmethod get-sort-specification ((sort vector) reverse-p)
  (if (zerop (length sort))
      (error "One or more field sort specifications was expected"))
  (map 'vector #'(lambda (field)
                   (if (typep field 'sort-field)
                       field
                     (make-instance 'sort-field
                                    :name (string field)
                                    :sort-type *auto-sorter*
                                    :reverse-p reverse-p)))
       sort))

(defmethod get-sort-specification ((sort list) reverse-p)
  (get-sort-specification (coerce sort 'vector) reverse-p))

(defmethod get-sort-specification ((sort null) reverse-p)
  (make-score-order-sort-field reverse-p))

(defun make-hit-queue (sorter reader max-size reverse-p)
  (let ((sort (get-sort-specification sorter reverse-p)))
    ;;(format t "make-hit-queue sort: ~s reverse-p ~s~%" sort reverse-p)
    (typecase sort
      (vector
       (make-instance
        'field-sorted-hit-queue
        :max-size max-size
        :reader reader
        :fields sort
        :reverse-p reverse-p))
      (sort-definition
       (if (and (= (length (fields sort)) 1)
                (sort-document-order (aref (fields sort) 0)))
           (make-instance
            'trivial-hit-queue
            :max-size max-size
            :reader reader
            :reverse-p reverse-p)
         (if (and (= (length (fields sort)) 1)
                  (sort-score-order (aref (fields sort) 0)))
             (make-instance
              'hit-queue
              :max-size max-size
              :predicate (predicate sort)
              :reverse-p reverse-p)
           (make-instance
            'field-sorted-hit-queue
            :max-size max-size
            :reader reader
            :fields (fields sort)
            :reverse-p reverse-p))))
      (t
       (make-instance
        'hit-queue
        :max-size max-size
        :predicate (predicate sort)
        :reverse-p reverse-p)))))

;; FIXME: locks
(defmethod search-index ((self index-searcher) query &rest options)
  (let* ((filter (getf options :filter))
	 (first-doc (or (getf options :first-doc) 0))
	 (num-docs (or (getf options :num-docs) 10))
	 (max-size (+ first-doc num-docs))
         (pq (if (typep query 'query-parse-result) (query query) query))
	 (sort (getf options :sort))
         (reverse-p (if sort (reverse-p sort) (getf options :reverse))))
    (when (<= num-docs 0)
      (error ":num-docs must be greater than zero to run a search."))
    (when (< first-doc 0)
      (error "first-doc must be greater than or equal to zero to run a search."))
    (let ((scorer (scorer (weight pq self) (reader self))))
      ;;(break "search-index query: ~s, sort ~s reverse ~s scorer ~s~%" query sort reverse-p scorer)
      (if (null scorer)
	  (make-instance 'top-docs :total-hits 0 :score-docs '())
        (let ((bits (unless (null filter)
                      (bits filter (reader self))))
              (hq (make-hit-queue sort (reader self) max-size reverse-p)))
          (let ((total-hits 0)
                (minimum-score 0.0))
            (each-hit scorer
                      #'(lambda (doc score)
                          (when (and (> score 0.0) (or (null bits) (bit-set-p bits doc)))
                            (incf total-hits)
                            (when (or (< (size hq) max-size) (>= score minimum-score))
                              (queue-insert hq (make-instance 'score-doc :doc doc :score score))
                              (setf minimum-score (score (queue-top hq)))))))
            (let ((score-docs '()))
              (when (> (size hq) first-doc)
                (when (< (- (size hq) first-doc) num-docs)
                  (setf num-docs (- (size hq) first-doc)))
                (dotimes (i num-docs)
                  (declare (ignorable i))
                  (push (queue-pop hq) score-docs)))

              ;;?? why bother...
              (queue-clear hq)
              (make-instance 'top-docs :total-hits total-hits :score-docs score-docs))))))))

(defmethod search-each ((self index-searcher) query-result fn &optional (filter nil))
  ;;(format t "search-each query: ~s, filter ~s~%" query-result filter)
  (let* ((query (query query-result))
         (scorer (scorer (weight query self) (reader self))))
    (when (null scorer)
      (return-from search-each nil))
    (let ((bits (if (null filter) nil (bits filter (reader self)))))
      (each-hit scorer
		#'(lambda (doc score)
		    (when (and (> score 0.0)
			       (or (null bits) (bit-set-p bits doc)))
		      (funcall fn doc score)))))))

(defmethod rewrite-searcher ((self index-searcher) original)
  (let* ((query original)
         (rewritten-query (rewrite query (reader self))))
    (while (not (equal query rewritten-query))
      (setf query rewritten-query
            rewritten-query (rewrite query (reader self))))
    (values query)))

(defmethod explain-score ((self index-searcher) query doc-num doc-score)
  (explain-score (weight query self) (reader self) doc-num doc-score))
