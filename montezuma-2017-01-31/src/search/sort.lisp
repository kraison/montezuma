(in-package #:montezuma)

(defclass sort-definition ()
  ((fields :accessor fields :initarg :fields :initform nil :documentation "a vector of one or more sort-fields")
   (reverse-p :initarg :reverse-p :accessor reverse-p :initform nil))
  (:default-initargs
   ))

(defmethod print-object ((self sort-definition) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (with-slots (fields reverse-p) self
      (when fields (format stream "~s" fields))
      (when reverse-p (format stream " Reverse")))))

(defun make-sort-field (field reverse-p field-parser)
  (if (typep field 'sort-field)
      field
    (make-instance
     'sort-field
     :name (string field)
     :sort-type (or field-parser *auto-sorter*)
     :reverse-p reverse-p)))

(defmethod initialize-instance :after ((self sort-definition) &key)
  (with-slots (fields reverse-p) self
     ;; natural order was: *field-score* *field-doc*)
    (unless fields (setf fields (make-score-order-sort-field reverse-p)))
    (setf fields (map 'vector
                      #'(lambda(f) (if (typep f 'sort-field)
                                       f
                                     (make-sort-field f reverse-p *auto-sorter*)))
                      (enlist fields)))
    ;(when (= (length fields) 1) (setf fields (concatenate 'vector fields (vector *field-doc*))))))
    ))

(defmethod document-sort-field ((self sort-definition))
  (and (= 1 (length (fields self)))
       (document-sort-field (aref (fields self) 0))))

(defmethod score-sort-field ((self sort-definition))
  (and (= 1 (length (fields self)))
       (score-sort-field (aref (fields self) 0))))

;(defparameter *relevance* (make-instance 'sort))

;(defparameter *index-order* (make-instance 'sort-definition :fields (vector *field-doc*)))

;(defparameter *author-title-order* (make-instance 'sort-definition :fields '("author" "title")))
