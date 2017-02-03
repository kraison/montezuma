(in-package #:montezuma)

(defclass query ()
  ((boost :accessor boost :initform 1.0 :initarg :boost)
   (prohibited? :accessor prohibited? :initform nil)
   (occur :accessor occur :initform :should-occur)
   (rewritten :accessor rewritten :initform nil :initarg :rewritten)))

(defmethod print-object ((self query) stream)
  (print-unreadable-object (self stream :type T)
    (unless (equal (boost self) 1.0) (format stream "Boost: ~s~%" (boost self)))))

(defmethod weight ((self query) searcher)
  (let* ((query (rewrite-searcher searcher self))
	 (weight (create-weight query searcher))
	 (sum (sum-of-squared-weights weight))
	 (norm (query-norm (similarity searcher) sum)))
    (normalize-weight weight norm)
    weight))

(defmethod empty-clauses ((cq montezuma:query))
  nil)

(defmethod non-empty-clauses ((self montezuma:query))
  self)

(defmethod rewrite :around ((self query) reader)
  ;; Cache rewrites so we don't have to reevaluate the query every time we use it.
  (if (rewritten self)
      (rewritten self)
    (let ((result (call-next-method)))
      (setf (rewritten self) result))))

(defmethod rewrite ((self query) reader)
  "Override in subclasses to execute as a rewritten query."
  (declare (ignore reader))
  self)

(defmethod similarity-implementation ((self query) searcher)
  (similarity searcher))

(defmethod first-document ((self query))
  "defer putting a value on first document to be returned"
  nil)
        
(defmethod number-of-documents ((self query))
  nil)

(defmethod query ((self query))
  self)

(defmethod sort-specification ((self query))
  nil)

(defmethod extract-terms ((self query) terms)
  t)

#|

(defmethod extract-query-terms ((self query))
  (let ((terms (make-array 0 :fill-pointer 0 :adjustable t)))
    (extract-terms self terms)
    terms))
|#