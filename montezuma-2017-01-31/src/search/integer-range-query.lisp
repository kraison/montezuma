(in-package #:montezuma)

(defclass integer-range-query (typed-range-query)
  ())

(defmethod initialize-instance :after ((self integer-range-query) &key)
  (with-slots (lower include-lower-p upper include-upper-p lower-function upper-function) self
    (unless (field-type self) (setf (field-type self) 'integer))
    (unless lower-function
      (setf (lower-function self) (if include-lower-p '>= '>)))
    (unless upper-function 
      (setf (upper-function self) (if include-upper-p '<= '<)))  
    (if (and lower (stringp lower))
        (setf (lower self) (parse-integer lower)))
    (if (and upper (stringp upper))
        (setf (upper self) (parse-integer upper)))))

(defmethod parse-data ((self integer-range-query) data)
  (if data
      (if (integerp data)
          data
        (multiple-value-bind (match substrings)
            (cl-ppcre:scan-to-strings (scanner self) data :sharedp t)
          ;;(break "integer-range-query scan ~s start ~d substrings ~s" data match substrings)
          (if match
              (parse-integer (aref substrings 0)))))))

(defclass integer-range-scorer (typed-range-scorer)
  ())

(defclass integer-range-weight (typed-range-weight)
  ())

(defmethod create-weight ((self integer-range-query) searcher)
  (make-instance 'integer-range-weight :query self :searcher searcher))

(defmethod scorer ((self integer-range-weight) reader)
  (with-slots (query searcher) self
    (make-instance 'integer-range-scorer
		   :reader reader
                   ;;:field (field query)
                   :query query
		   :similarity (similarity-implementation query searcher))))



