(in-package #:montezuma)

;; A phrase-query is a query that matches documents containing a
;; particular sequence of terms.

(defclass phrase-query (query)
  ((slop :type integer :initform 0 :accessor slop :initarg :slop)
   (terms :initform (make-array 3 :adjustable T :fill-pointer 0) :accessor terms)
   (positions :initform (make-array 3 :adjustable T :fill-pointer 0) :reader positions)
   (field :initform nil :initarg field :accessor field)))

(defmethod phrase-term-positions ((self query))
  (loop for term across (terms self)
        for position across (positions self)
        collect (list (term-text term) position)))

(defmethod phrase-term ((self phrase-query) offset)
  (let ((term (aref (terms self) offset)))
    (values (term-field term) (term-text term) (aref (positions self) offset))))

(defmethod print-object ((self phrase-query) stream)
  (print-unreadable-object (self stream :type T)
    (pprint-newline :linear stream)
    (pprint-logical-block (stream '(1 2))
      (format stream "~a: " (field self))
      (format stream "~{~a ~}" (phrase-term-positions self))
      (unless (zerop (slop self)) (format stream "~% proximity: ~d" (slop self)))
      (unless (= 1.0 (boost self)) (format stream "~% boost: ^~a" (boost self))))))

(defgeneric add-term-to-query (phrase-query term &optional position pos-inc))

(defmethod add-term-to-query ((self phrase-query) term &optional position (pos-inc 1))
  (with-slots (positions terms field) self
    (when (null position)
      (setf position (if (> (length positions) 0)
			 (+ (aref positions (- (length positions) 1)) pos-inc)
			 0)))
    (if (= (length terms) 0)
	(setf field (term-field term))
      (when (not (string= (term-field term) field))
        (error "All phrase terms must be in the same field: ~S" term)))
    (vector-push-extend term terms)
    (vector-push-extend position positions)
    self))

(defclass phrase-weight (weight)
  ((query :initarg :query :reader query)
   (similarity)
   (idf)
   (query-weight)
   (query-norm)
   (value :initform nil :reader value)))

(defmethod print-object ((self phrase-weight) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (format stream "query: ~S" (query self))))

(defmethod initialize-instance :after ((self phrase-weight) &key searcher)
  (with-slots (similarity idf query) self
    (setf similarity (similarity-implementation query searcher))
    (setf idf (idf-phrase similarity (terms query) searcher))))

(defmethod sum-of-squared-weights ((self phrase-weight))
  (with-slots (query-weight idf query) self
    (let ((w (* idf (boost query))))
      (setf query-weight w)
      (* w w))))

(defmethod explain-score ((self phrase-weight) reader doc-num doc-score)
  (with-slots (query) self ; query-weight idf) self
    ;;(break "explain-score ~s phrase-weight ~s" query self)
    (let* (;;(phrase (terms query))
           (explanation (make-instance
                         'explanation
                         :description (format nil "Weight(~A in ~A)" query doc-num)
                         :value doc-score))
#|
           (idf-explanation (make-instance
                             'explanation
                             :description (format nil "idf(doc-freq=~A) ~S"
                                                  (term-doc-freq reader phrase)
                                                  phrase)
                             :value idf))

           (query-explanation (make-instance
                               'explanation
                               :description (format nil "query-weight(~A)" query)
                               :value query-weight))
           (boost-explanation (make-instance 'explanation :description "boost" :value (boost query)))
|#
           )
      ;;(format t "~a: product of: ~a~%" query-explanation (if (= (boost query) 1.0) boost-explanation idf-explanation))
      explanation)))


(defmethod normalize-weight ((self phrase-weight) query-norm)
  (with-slots (query-weight value idf) self
    (setf (slot-value self 'query-norm) query-norm)
    (setf query-weight (* query-weight query-norm))
    (setf value (* query-weight idf))))

(defmethod scorer ((self phrase-weight) reader)
  (with-slots (query similarity) self
    (if (= (length (terms query)) 0)
	nil
	(let ((tps (loop for term across (terms query)
			collecting
			(let ((tp (term-positions-for reader term)))
			  (if (null tp)
			      (return-from scorer nil)
			      tp)))))
	  (if (= (slop query) 0)
	      (make-instance 'exact-phrase-scorer
			     :weight self
			     :term-positions tps
			     :positions (positions query)
			     :similarity similarity
			     :norms (get-norms reader (field query)))
	      (make-instance 'sloppy-phrase-scorer
			     :weight self
			     :term-positions tps
			     :positions (positions query)
			     :similarity similarity
			     :slop (slop query)
			     :norms (get-norms reader (field query))))))))


(defmethod create-weight ((self phrase-query) searcher)
  (with-slots (terms) self
    (if (= (length terms) 1)
	(let* ((term (aref terms 0))
	       (tq (make-instance 'term-query
				  :term term)))
	  (setf (boost tq) (boost self))
	  (create-weight tq searcher))
	(make-instance 'phrase-weight
		       :query self
		       :searcher searcher))))
#|
(defmethod extract-terms ((self phrase-query) query-terms)
  ;;(break "slop ~a terms ~s" (slop self) query-terms)
  (with-slots (terms slop) self
    (if (zerop slop)
        (vector-push-extend (coerce terms 'list) query-terms)
      (let ((phrase-list ())
            (gap (make-term (field self) nil)))
        (loop
         for term across terms do
         (push term phrase-list)
         (push gap phrase-list)) ; use a gap to signify unlimited words between phrase elements
        (if (eql gap (first phrase-list))
            (pop phrase-list))
        (vector-push-extend (nreverse phrase-list) query-terms)))))
|#

(defmethod extract-terms ((self phrase-query) query-terms)
  ;;(break "slop ~a terms ~s" (slop self) query-terms)
  ;(with-slots (terms slop) self
    ;(if (zerop slop)
    ;    (vector-push-extend (coerce terms 'list) query-terms)
      (vector-push-extend self query-terms))
