(in-package #:montezuma)

;;?? <<
;;?? create-weight
;;?? to-s
;;?? eql?

(defparameter +default-max-clause-count+ (expt 2 20))

(defclass boolean-query (query)
  ((clauses :initform (make-array 10 :adjustable T :fill-pointer 0) :accessor clauses)
   (coord-disabled :initform T :initarg :coord-disabled :reader coord-disabled?)
   (max-clause-count :initarg :max-clause-count :accessor max-clause-count :allocation :class))
  (:default-initargs 
    :max-clause-count +default-max-clause-count+))

(defmethod size ((query boolean-query))
  (fill-pointer (clauses query)))
#|
(defmethod empty ((query boolean-query))
  (zerop (size query)))
|#

(defmethod print-object ((self boolean-query) stream)
  (print-unreadable-object (self stream :type T)
     (format stream " ~d clauses:~%" (size self))
     (loop
      for clause across (clauses self) do
      (format stream "~& ~a" clause))
     (unless (= 1.0 (boost self)) (format stream " Boost: ~s" (boost self)))))

(define-condition too-many-clauses-error (error)
  ()
  (:documentation "Thrown when an attempt is made to add more than #max_clause_count() clauses.
 This typically happens if a PrefixQuery, FuzzyQuery, WildcardQuery, or RangeQuery is expanded to many terms during search."))


#|
Constructs an empty boolean query.

Similarity#coord(int,int) may be disabled in scoring, as appropriate. For example, this score factor does not make sense for most automatically generated queries, like WildcardQuery and FuzzyQuery. 

coord_disabled:: disables Similarity#coord(int,int) in scoring.
|#

(defun coord-disabled-function (similarity overlap max-overlap)
  (declare (ignore similarity overlap max-overlap))
  (values 1.0))

(defmethod similarity-implementation ((self boolean-query) searcher)
  (declare (ignore searcher))
  ;;?? should this be a copy of sim?
  (let ((sim (call-next-method)))
    (when (coord-disabled? self)
      (setf (slot-value sim 'coord-function) 'coord-disabled-function))
    sim))

(defgeneric add-query (boolean-query query occur)
  (:documentation ""))

(defmethod add-query ((self boolean-query) (query query) occur)
  (add-clause self (make-instance 'boolean-clause :query query :occur occur)))

(defmethod add-term-query ((self boolean-query) (term term) boost occur)
  (when (> (size self) (max-clause-count self))
    (error 'too-many-clauses-error))
  (let ((term-clause (make-instance 'term-clause :term term :boost boost :occur occur)))
    (vector-push-extend term-clause (clauses self))))

(defgeneric add-clause  (boolean-query clause))

(defmethod add-clause  ((self boolean-query) clause)
  (when (> (size self) (max-clause-count self))
    (error 'too-many-clauses-error))
  (vector-push-extend clause (clauses self)))

(defmethod create-weight ((self boolean-query) searcher)
  (make-instance 'boolean-weight :query self :searcher searcher))

(defmethod rewrite ((self list) reader)
  (mapcar #'(lambda(q) (rewrite (query q) reader)) self))

(defmethod rewrite ((self vector) reader)
  (loop
   for q across self do
   (if (typep q 'query)
       (rewrite (query q) reader))))

(defmethod non-empty-clauses ((self montezuma:boolean-query))
  (loop for clause across (clauses self)
        unless (empty-clauses clause) collect clause))

(defmethod non-redundant-clauses ((self boolean-query))
  (loop for clause across (clauses self)
        unless (empty-clauses clause) collect clause))

(defmethod rewrite :after ((self montezuma:boolean-query) reader)
  (with-slots (clauses) self
    (let ((non-empty (non-redundant-clauses self)))
      (when (\= (length non-empty) (length clauses))
        (setf (fill-pointer clauses) 0)
        (loop
         for clause in non-empty do
         (vector-push-extend clause clauses))))))

(defmethod rewrite ((self boolean-query) reader)
  (when (= (length (clauses self)) 1)
    (let ((clause (aref (clauses self) 0)))
      (unless (prohibited? clause)
        (let ((query (rewrite (query clause) reader)))
          (when (/= (boost self) 1.0)
            ;; if rewrite was a no-op, make a clone
            (when (eq query (query clause))
              (setf query (clone query)))
            (setf (boost query) (* (boost self) (boost query))))
          
          ;; early returns
          (return-from rewrite query)))))
  
  (let ((clone nil))
    (dosequence (clause (clauses self) :index i)
      (let ((query (rewrite (query clause) reader)))
        (when (not (eq query (query clause)))
          (setf clone (or clone (clone self)))
          (setf (aref (clauses clone) i)
                (make-instance 'boolean-clause :query query :occur (occur clause))))))
    ;; if we did some re-writing, return changed clone else unchanged query
    (or clone self)))

(defmethod extract-terms ((self boolean-query) terms)
  (dosequence (clause (clauses self))
    (if (and clause (query clause))
        (let ((query (query clause)))
          (if query
              (extract-terms query terms))))))

(defgeneric combine (boolean-query queries))

(defmethod initialize-copy :after ((copy boolean-query) original)
  (setf (clauses copy) (clone (clauses original))))

;;; ---------------------------------------------------------------------------
;;; boolean-weight
;;; ---------------------------------------------------------------------------

(defclass boolean-weight (weight)
  ((similarity :accessor similarity)
   (weights :accessor weights :initform '())
   (query :initarg :query :reader query)
   (searcher :initarg :searcher :reader searcher)))

(defmethod initialize-instance :after ((self boolean-weight) &key)
  (setf (similarity self) (similarity-implementation (query self) (searcher self)))
  (let ((weights '())
	(searcher (searcher self)))
    (dosequence (clause (clauses (query self)))
      (let ((query (if clause (query clause))))
        (when query
          (push (create-weight query searcher) weights))))
    (setf (weights self) (reverse weights))))

(defmethod value ((self boolean-weight))
  (values (boost (query self))))

(defmethod sum-of-squared-weights ((self boolean-weight))
  (let ((sum 0)
	(query (query self)))
    (dosequence (weight (weights self) :index i)
      (let ((clause (elt (clauses query) i)))
	(if clause
            (unless (prohibited? clause)
              (incf sum (sum-of-squared-weights weight))))))
    (setf sum (* sum (boost query) (boost query)))))

(defmethod normalize-weight ((self boolean-weight) norm)
  (let ((query (query self)))
    (setf norm (* norm (boost query)))
    (dosequence (weight (weights self) :index i)
      (let ((clause (elt (clauses query) i)))
        (if clause
            (unless (prohibited? clause)
              (normalize-weight weight norm)))))))

(defmethod scorer ((self boolean-weight) reader)
  (let ((result (make-instance 'boolean-scorer :similarity (similarity self)))
	(query (query self)))
    (dosequence (weight (weights self) :index i)
      (let* ((clause (elt (clauses query) i))
	     (sub-scorer (scorer weight reader)))
        (if clause
            (if sub-scorer
                (add-scorer result sub-scorer (occur clause))
              (when (required? clause)
                (return-from scorer nil))))))
    result))
	
(defmethod explain-score ((self boolean-weight) reader doc-num doc-score)
  (with-slots (query) self ; weights) self
    (make-explanation (format nil "Weight(~A in ~A)" query doc-num) doc-score)))
