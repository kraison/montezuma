(in-package #:montezuma)

(defclass typed-range-query (match-all-query)
  ((field :initarg :field :accessor field)
   (field-type :initarg :field-type :accessor field-type :initform nil)
   (form :initarg :form :accessor form :initform nil)
   ;(scanner :initarg :scanner :initform nil :accessor scanner)
   ;(registers :initarg :registers :initform nil :accessor registers)
   (definition :accessor definition :initarg :definition :initform nil) 
   (query :initform nil :initarg :query :accessor query)
   (lower :initform nil :initarg :lower :accessor lower)
   (upper :initform nil :initarg :upper :accessor upper)
   (lower-function :initarg :lower-function :initform nil :accessor lower-function)
   (upper-function :initarg :upper-function :initform nil :accessor upper-function)
   (include-upper-p :initarg :include-upper-p :accessor include-upper-p)
   (include-lower-p :initarg :include-lower-p :accessor include-lower-p)))

(defmethod scanner ((self typed-range-query))
  (definition-aspect (definition self) :scanner))

(defmethod registers ((self typed-range-query))
  (definition-aspect (definition self) :registers))

(defmethod parse-data ((self typed-range-query) data)
  ;; override this method in subclasses for type specific range queries
  data)

(defmethod print-object ((self typed-range-query) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (format stream "~a: " (field self))
    (format stream "~{~s ~}"
           `(,@(if (definition self) `(:definition ,(definition self)))
             ,@(if (lower self) `(:lower ,(lower-function self) ,(lower self)))
             ,@(if (and (lower self) (upper self)) '(" and "))
             ,@(if (upper self) `(:upper ,(upper-function self) ,(upper self)))))))

(defmethod extract-terms ((self typed-range-query) terms)
  (vector-push-extend self terms))

#|
(defmethod extract-query-terms ((self typed-range-query))
  (let ((terms (make-array 0 :fill-pointer 0 :adjustable t)))
    (extract-terms self terms)
    terms))
|#

(defclass typed-range-scorer (match-all-scorer)
  ((query :initarg :query :accessor query)))

(defclass typed-range-weight (match-all-weight)
  ())

(defmethod create-weight ((self typed-range-query) searcher)
  (make-instance
   'typed-range-weight
   :query self
   :searcher searcher))

(defmethod scorer ((self typed-range-weight) reader)
  (with-slots (query searcher) self
    (make-instance 'typed-range-scorer
		   :reader reader
                   :query query
		   :similarity (similarity-implementation query searcher))))

(defmethod scanned ((self typed-range-query) text &key (start 0) (end nil))
  (declare (ignore end))
  (multiple-value-bind (starting ending)
      (cl-ppcre:scan (scanner self) text :start start)
    (values (and starting ending) starting ending)))

#|
(defmethod field-range-data ((self typed-range-scorer))
  (with-slots (reader count) self
    ;;(field-data (document-field (get-document reader counter) (field self)))))
    (document-value (get-document reader count) (field self))))
|#

(defmethod satisfactory ((self typed-range-query) reader count)
  "Return True if the document (docnum count) satisfies the query"
  (let* ((document (get-document reader count))
         (data (document-value document (field self))))
    (satisfied-query self data)))

(defmethod satisfied-query ((self typed-range-query) data &key (start 0) (end nil))
  (declare (ignore start end))
  (with-slots (lower upper lower-function upper-function) self
    (let* ((value (parse-data self data)))
      (and value
           (or (null lower) (funcall lower-function value lower))
           (or (null upper) (funcall upper-function value upper))))))

(defmethod satisfied ((self typed-range-scorer) &key (start 0) (end nil))
  (satisfied-query (query self) (field-range-data self) start end))
