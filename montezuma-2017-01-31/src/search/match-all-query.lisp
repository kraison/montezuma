(in-package #:montezuma)

(defclass match-all-query (query)
  ())

(defclass match-all-scorer (scorer)
  ())

(defmethod initialize-instance :after ((self match-all-scorer) &key)
  (with-slots (max-doc reader) self
    (setf max-doc (max-doc reader))))

(defmethod extract-terms ((self match-all-query) terms)
  (declare (ignore terms))
  t)

(defmethod prohibited? ((self match-all-query))
  nil)

(defmethod explain ((self match-all-scorer))
  (make-instance 'explanation :value 1.0 :description "match-all-query"))

(defmethod satisfactory ((self query) reader count)
  "Return True if the document (docnum count) satisfies the query"
  (declare (ignore reader count))
  t)

(defclass match-all-weight (weight)
  ((query :initarg :query :reader query)
   (searcher :initarg :searcher)))

(defmethod value ((self match-all-weight))
  1.0)

(defmethod sum-of-squared-weights ((self match-all-weight))
  1.0)

(defmethod normalize-weight ((self match-all-weight) query-norm)
  (declare (ignore query-norm))
  )

(defmethod scorer ((self match-all-weight) reader)
  (with-slots (query searcher) self
    (make-instance 'match-all-scorer
		   :reader reader
		   :similarity (similarity-implementation query searcher)
                   :query query)))

(defmethod create-weight ((self match-all-query) searcher)
  (make-instance 'match-all-weight :query self :searcher searcher))
