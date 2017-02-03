(in-package #:montezuma)

(defclass date-range-query (typed-range-query)
  ())

(defmethod parse-data ((self date-range-query) data)
  (if data
      (parse-date (registers self) (scanner self) data)))

(defmethod initialize-instance :after ((self date-range-query) &key)
  (with-slots (field-type lower upper) self
    (unless field-type (setf (field-type self) 'date))
    (unless (or (lower-function self) (null lower))
      (setf (lower-function self) (if (include-lower-p self) 'local-time:timestamp>= 'local-time:timestamp>)))
    (unless (or (upper-function self) (null upper))
      (setf (upper-function self) (if (include-upper-p self) 'local-time:timestamp<= 'local-time:timestamp<)))))

(defclass date-range-scorer (typed-range-scorer)
  ())

(defclass date-range-weight (typed-range-weight)
  ())

(defmethod create-weight ((self date-range-query) searcher)
  (make-instance
   'date-range-weight
   :query self
   :searcher searcher))

(defmethod scorer ((self date-range-weight) reader)
  (with-slots (query searcher) self
    (make-instance 'date-range-scorer
		   :reader reader
                   :query query
		   :similarity (similarity-implementation query searcher))))

