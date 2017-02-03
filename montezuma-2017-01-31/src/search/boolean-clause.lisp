(in-package #:montezuma)

;;?? equality testing in ruby (cf. eql?)
;;?? alias :== eql?

(defclass boolean-clause (query)
  ((query :initform nil :accessor query :initarg :query
          ;;:type 'vector
          :documentation "The query whose matching documents are combined by the boolean query.")
   (required :initform nil :reader required? :writer required
             :documentation "If true, documents which _do not_ match this sub-query will _not_ match the boolean query.")
   (prohibited :initform nil :reader prohibited? :writer prohibited
               :documentation "If true, documents which _do_ match this sub-query will _not_ match the boolean query.")
   (occur :initform :should-occur :accessor occur :initarg :occur
          :documentation "See BooleanQuery::Occur for values for this attribute.")))

(defmethod empty-clauses ((bc montezuma:boolean-clause))
  (let ((bq (montezuma:query bc)))
    (and (typep bq 'montezuma:boolean-query)
         (zerop (fill-pointer (montezuma:clauses bq))))))

(defmethod rewrite ((self boolean-clause) reader)
  (rewrite (query self) reader))

(defmethod initialize-instance :after ((self boolean-clause) &key)
  (set-fields self (occur self)))

(defmethod print-object ((self boolean-clause) stream)
  (print-unreadable-object (self stream)
    (format stream "~(~a~) :occur ~(~s~) :query ~a" (type-of self) (occur self) (query self))))

(defmethod (setf occur) :after (value (self boolean-clause))
  (set-fields self value))

#||
(defmethod eql? ((self boolean-clause) (other t))
  (and (typep other 'boolean-clause)
       (and (equal (query self) (query other))
            (equal (required? self) (required? other))
            (equal (prohibited? self) (prohibited? other)))))
||#

(defgeneric set-fields (boolean-clause occur))

(defmethod set-fields ((self boolean-clause) occur)
  (ecase occur
    (:must-occur
     (required t self)
     (prohibited nil self))
    (:must-not-occur
     (required nil self) 
     (prohibited t self))
    (:should-occur
     (required nil self)
     (prohibited nil self))))

(defmethod extract-terms ((clause boolean-clause) terms)
  (unless (prohibited? clause)
    (extract-terms (query clause) terms)))
