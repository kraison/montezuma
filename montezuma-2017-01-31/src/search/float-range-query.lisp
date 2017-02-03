(in-package #:montezuma)

(defclass float-range-query (typed-range-query)
  ())

(defmethod initialize-instance :after ((self float-range-query) &key)
  (with-slots (field-type) self
    (unless field-type (setf (field-type self) 'float))))

(defclass float-range-scorer (typed-range-scorer)
  ())

(defclass float-range-weight (match-all-weight)
  ())

(defmethod parse-data ((self float-range-query) data)
  (if data (parse-float data) 0.0))

(defmethod create-weight ((self float-range-query) searcher)
  (make-instance
   'float-range-weight
   :query self
   :searcher searcher))

(defmethod scorer ((self float-range-weight) reader)
  (with-slots (query searcher) self
    (make-instance 'float-range-scorer
		   :reader reader
                   :query query
		   :similarity (similarity-implementation query searcher))))

