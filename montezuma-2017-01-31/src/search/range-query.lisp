(in-package #:montezuma)

(defclass range-query (match-all-query) ;(query)
  ((field :initarg :field :accessor field)
   (definition :initarg :definition :accessor definition :initform nil)
   (defined-type :initarg :defined-type :accessor defined-type :initform "string")
   (form :initarg :form :accessor form :initform nil)
   (lower-term :initform nil :initarg :lower-term :accessor lower-term)
   (upper-term :initform nil :initarg :upper-term :accessor upper-term)
   (include-upper-p :initarg :include-upper-p :accessor include-upper-p)
   (include-lower-p :initarg :include-lower-p :accessor include-lower-p)))

(defmethod initialize-instance :after ((self range-query) &key)
  (with-slots (lower-term upper-term include-lower-p include-upper-p) self
    (cond ((and (null lower-term) (null upper-term))
	   (error "At least one of :upper-term and :lower-term must be specified and non-null."))
	  ((and include-lower-p (null lower-term))
	   (error "The lower term bound must be non-null to be inclusive."))
	  ((and include-upper-p (null upper-term))
	   (error "The upper term bound must be non-null to be inclusive."))
	  ;;((and upper-term lower-term (string< upper-term lower-term))
          ;;(error "The lower term bound must be less than the upper term bound.")))))
          )))

(defmethod print-object ((self range-query) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (if (defined-type self)
        (format stream "~a " (defined-type self)))
    (format stream "~s " (field self))
    (if (lower-term self)
        (format stream "~a ~s"
                (if (include-lower-p self) ">=" ">")
                (lower-term self)))
     (if (and (lower-term self) (upper-term self))
        (format stream " and "))
    (if  (upper-term self)
        (format stream "~a ~s"
                (if (include-upper-p self) "<=" "<")
                (upper-term self)))))

(defmethod field-type ((self range-query))
  (intern (string (defined-type self))))

(defmethod make-typed-range ((self range-query) range-type)
  (with-slots (field form lower-term upper-term include-lower-p include-upper-p) self
    (make-instance range-type
                   :field field
                   :form form
                   :definition (definition self)
                   :lower lower-term
                   :upper upper-term
                   :include-upper-p include-upper-p
                   :include-lower-p include-lower-p)))

(defmethod rewrite ((self range-query) reader)
  (case (field-type self)
    (int (make-typed-range self 'integer-range-query))
    (float (make-typed-range self 'float-range-query))
    (date (make-typed-range self 'date-range-query))
    (t (expand-range self reader))))

(defmethod expand-range ((self range-query) reader)
  ;; default range query is satisfied by any index terms in the range of the query
  (with-slots (field lower-term upper-term include-lower-p include-upper-p) self
    (let ((bq (make-instance 'boolean-query))
          (term-enum (terms-from reader (make-term field (or lower-term ""))))
          (check-lower-p (not include-lower-p)))
      (unwind-protect
          (loop
           for term = (term term-enum) do
           (unless (and term (string= (term-field term) field)) (return))
           (when (or (not check-lower-p)
                     (null lower-term)
                     (string> (term-text term) lower-term))
             (setf check-lower-p NIL)
             (when upper-term
               (let ((compare (string-compare upper-term (term-text term))))
                 (when (or (< compare 0) (and (not include-upper-p) (= compare 0)))
                   (return))))
             (add-term-query bq term (boost self) :should-occur))
           while (next? term-enum))
        (close-down term-enum))
      bq)))

#|
(rewrite (query (process-query oanc-corpus "title:[advent to bar]")) (reader oanc-corpus))
|#
