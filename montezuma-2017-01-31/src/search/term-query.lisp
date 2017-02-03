(in-package #:montezuma)

(defclass term-query (query)
  ((term :reader term :initarg :term :type term)
   (definition :accessor definition :initarg :definition :initform nil)))

(defmethod print-object ((self term-query) stream)
  (let ((term (term self)))
      (print-unreadable-object (self stream) ; :type T :identity nil)
        (format stream "~(~a~) ~s ~s" (type-of self) (term-field term) (term-text term))
        (unless (= 1.0 (boost self)) (format stream "^~S" (boost self))))))

(defmethod rewrite ((self term-query) reader)
  (declare (ignore reader))
  self)

(defclass term-weight (weight)
  ((value :accessor value :initform 0)
   (query :reader query :initarg :query)
   (query-weight :accessor query-weight)
   (similarity :accessor similarity)
   (term-idf :accessor term-idf)
   (term-query-norm :accessor term-query-norm)))

(defmethod print-object ((self term-weight) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (format stream "query: ~S" (query self))))

(defmethod initialize-instance :after ((self term-weight) &key query searcher)
  (setf (similarity self) (similarity-implementation query searcher)
        (term-idf self) (idf (similarity self)
                             (term-doc-freq searcher (term query))
                             (max-doc searcher))))

(defmethod sum-of-squared-weights ((self term-weight))
  (setf (query-weight self) (* (term-idf self) (boost (query self))))
  (values (* (query-weight self) (query-weight self))))

(defmethod normalize-weight ((self term-weight) query-norm)
  (setf (term-query-norm self) query-norm
        (query-weight self) (* (query-weight self) query-norm)
        (value self) (* (query-weight self) (term-idf self))))

(defmethod scorer ((self term-weight) reader)
  (let ((term-docs (term-docs-for reader (term (query self)))))
    (if term-docs
	(make-instance 'term-scorer
		       :weight self
		       :term-docs term-docs
		       :similarity (similarity self)
		       :norms (get-norms reader (term-field (term (query self)))))
      nil)))

(defmethod explain-score ((self term-weight) reader doc-num doc-score)
  (with-slots (query) self ; query-weight term-idf) self
    (let* (;(term (term query))
           (explanation (make-instance
                         'explanation
                         :description (format nil "Weight(~A in ~A)" query doc-num)
                         :value doc-score))
#|
           (idf-explanation (make-instance
                             'explanation
                             :description (format nil "idf(doc-freq=~A) ~S"
                                                  (term-doc-freq reader term)
                                                  term)
                             :value term-idf))
           (query-explanation (make-instance
                               'explanation
                               :description (format nil "query-weight(~A)" query)
                               :value query-weight))
           (boost-explanation (make-instance 'explanation :description "boost" :value (boost query)))
|#
           )
;      (format t "~a: product of: ~a~%" query-explanation (if (= (boost query) 1.0) boost-explanation idf-explanation))
      explanation)))

(defmethod create-weight ((self term-query) searcher)
  (make-instance 'term-weight :searcher searcher :query self))

(defmethod extract-terms ((self term-query) terms)
  (vector-push-extend (term self) terms))

(defmethod field ((self term-query))
  (term-field (term (query self))))

(defmethod term-text ((self term-query))
  (term-text (term (query self))))

#|
(defmethod extract-query-terms ((self term-query))
  (let ((terms (make-array 0 :fill-pointer 0 :adjustable t)))
    (extract-terms self terms)
    terms))
|#
;;?? to-s
;;?? eql?
