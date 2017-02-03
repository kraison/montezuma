(in-package #:montezuma)

(defclass sort-field ()
  ((id :accessor id :initarg :id)
   (name :initform nil :initarg :name :accessor named)
   (sort-type :initarg :sort-type)
   (parser :initarg :parser)
   (comparator :reader comparator :initarg :comparator)
   (predicate :accessor predicate :initarg :predicate)
   (reverse-p :initform nil :accessor reverse-p :initarg :reverse-p)
   )
  (:default-initargs
   :id nil
   :parser #'string
   :sort-type *auto-sorter*))

(defmethod print-object ((self sort-field) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (with-slots (name id sort-type reverse-p) self
      (when id (format stream "~s" id))
      (when name (format stream "~s" name))
      (when sort-type (format stream " :sort-type ~a" sort-type)) ;(sort-name sort-type)))
      (when reverse-p (format stream " Reversed")))))

(defclass sort-type ()
  ((name :initform nil :initarg :name :accessor sort-name)
   (parser :initarg :parser)
   (comparator :initform nil :reader comparator :initarg :comparator)
   (reverse-p :initform nil :initarg :reverse-p :reader reverse-p))
  (:default-initargs
   :parser #'string))

(defvar *sort-types* ())

(defun make-sort-type (name &optional (parser nil))
  (let ((sort-type (make-instance 'sort-type :name name :parser parser)))
    (push sort-type *sort-types*)
    sort-type))

(defun get-sorter (name)
  (find name *sort-types* :key #'sort-name :test #'string=))

(defmethod print-object ((self sort-type) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (with-slots (name reverse-p) self
      (when name (format stream "~s" name))
      ;;(when parser (format stream " :parser ~s" parser))
      ;;(when comparator (format stream " ~s" comparator))
      (when reverse-p (format stream " :reversed")))))

(defparameter *score-sorter* (make-sort-type "score"))
(defparameter *doc-sorter* (make-sort-type "doc"))
(defparameter *auto-sorter* (make-sort-type "auto"))
(defparameter *string-sorter* (make-sort-type "string"))
(defparameter *int-sorter* (make-sort-type "int" #'parse-integer))
(defparameter *float-sorter* (make-sort-type "float" #'parse-float))
;;(defparameter *date-sorter* (make-sort-type "date" #'???

(defmethod initialize-instance :after ((self sort-field) &key)
  (with-slots (name sort-type) self
    (when name (setf name (string name))) ; don't stringify NIL
    (unless (slot-boundp self 'comparator)
      (setf (slot-value self 'comparator) (comparator sort-type)))
    (when (and (null name)
	       (not (sort-document-order self)) ;eq id :document-order))
	       (not (sort-score-order self))) ; (eq id :score-order)))
      (error "You must supply a field name for your sort field."))))

(defmethod sort-document-order ((self sort-field))
  ;;(with-slots (id) self
  (eq (id self) :score-order))

(defmethod sort-score-order ((self sort-field))
  ;;(with-slots (id) self
  (eq (id self) :document-order))

(defun make-score-order-sort-field (reverse)
  (make-instance
   'sort-field
   :id :score-order
   :sort-type *score-sorter*
   :reverse-p reverse
   :predicate 
   #'(lambda(a b)
       (if (= (score a) (score b))
           (> (doc a) (doc b))
         (< (score a) (score b))))))

(defun make-document-order-sort-field (reverse) ; sort by docnum
  (make-instance
   'sort-field
   :id :document-order
   :sort-type *doc-sorter*
   :reverse-p reverse
   :predicate #'(lambda(a b) (> (doc a) (doc b)))))


#|
(defparameter *field-score* ; sort by score then docnum
  (make-instance 'sort-field
                 :id :score-order
                 :sort-type *score-sorter*
                 :predicate 
                 #'(lambda(a b)
                     (if (= (score a) (score b))
                         (> (doc a) (doc b))
                       (< (score a) (score b))))))

(defparameter *field-doc* ; sort by docnum
  (make-instance 'sort-field
                 :id :document-order
                 :sort-type *doc-sorter*
                 :predicate
                 #'(lambda(a b) (> (doc a) (doc b)))))
|#

