(in-package #:montezuma)

(defclass term-query (query)
  ((term :reader term :initarg :term :type term)
   (definition :accessor definition :initarg :definition :initform nil)
   (index :accessor index :initarg :index :initform nil)))

(defclass key-term-query (term-query)
  ())

(defmethod print-object ((self term-query) stream)
  (with-slots (term index definition) self
    (print-unreadable-object (self stream) ; :type T :identity nil)
      (format stream "~(~a~) ~s ~s" (type-of self) (term-field term) (term-text term))
      (when index (format stream " ~a index" (index-key index)))
      (when definition (format stream " ~{~s ~}" (cdr definition)))
      (unless (= 1.0 (boost self)) (format stream "^~S" (boost self))))))

(defmethod rewrite ((self term-query) reader)
  (with-slots (term  definition index) self
    (if (and index
             (metadata index)
             (string-equal (document-key index)
                           (field self)))
        (make-instance 'key-term-query :term term :definition definition :index index)
      self)))

(defmethod rewrite ((self key-term-query) reader)
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

(defmethod term-doc-frequency ((searcher t) (query key-term-query))
  (with-slots (index term) query
    (if (and index (metadata index))
        (key-term-frequency index (term-text term))
      (term-doc-freq searcher (term query)))))

(defmethod term-doc-frequency ((searcher t) (query term-query))
  (term-doc-freq searcher (term query)))

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
  (let* ((term-docs (term-docs-for reader (term (query self))))
         (index (index (query self))))
    (if term-docs
	(make-instance (if (cached index (field (query self)))
                           'key-term-scorer
                         'term-scorer)
		       :weight self
		       :term-docs term-docs
		       :similarity (similarity self)
		       :norms (get-norms reader (term-field (term (query self)))))
      nil)))

(defmethod explain-score ((self term-weight) reader doc-num doc-score)
  (with-slots (query) self
    (make-instance
     'explanation
     :description (format nil "Weight(~A in ~A)" query doc-num)
     :value doc-score)))

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

#|
> (make-instance 'mtz::key-term-query :term (mtz:make-term "name" "ca01") :index brown-corpus)

> (mtz::term-doc-frequency (slot-value brown-corpus 'mtz::searcher) (make-instance 'mtz::key-term-query :term (mtz:make-term "name" "ca01") :index brown-corpus))
1

> (mtz::term-doc-freq (mtz:reader brown-corpus) (mtz:make-term "name" "ca01"))
1

> (mtz::term-docs-for (mtz:reader brown-corpus) (mtz::make-term "name" "ca01"))
#<MONTEZUMA::MULTI-TERM-DOC-ENUM "name":"ca01">

> (mtz::term-docs (slot-value brown-corpus 'mtz::reader))
#<MONTEZUMA::MULTI-TERM-DOC-ENUM >

> (setq term (mtz:make-term "name" "ca01"))

> (setq searcher (slot-value brown-corpus 'mtz::searcher))

> (mtz::create-weight (make-instance 'mtz::key-term-query :term term :index brown-corpus) searcher)

> (setq keyquery (make-instance 'mtz::key-term-query :term (mtz:make-term "name" "ca01") :index brown-corpus))

> (setq weight (mtz::create-weight keyquery (slot-value brown-corpus 'mtz::searcher)))

> (mtz::scorer weight (slot-value brown-corpus 'mtz::reader))

> (setq scorer (mtz::scorer weight (slot-value brown-corpus 'mtz::reader)))

> (setq term-scorer (mtz::scorer weight (slot-value brown-corpus 'mtz::reader)))

> (mtz:each-hit term-scorer #'(lambda(docnum score) (format t "~d ~d" docnum score)))
|#
