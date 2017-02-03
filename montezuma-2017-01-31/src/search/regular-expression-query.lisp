(in-package #:montezuma)

(defclass regular-expression-query (multi-term-query)
  ())

(defclass regular-expression-term-enum (pattern-term-enum)
  ())

(defmethod initialize-instance :after ((self regular-expression-term-enum) &key reader)
  (with-slots (field search-term prefix pattern) self
    (let ((term-text (term-text search-term)))
      (setf field (term-field search-term))
      (setf prefix (cl-ppcre:scan-to-strings "^[\\w\\s-]*" term-text))
      (setf pattern (cl-ppcre:create-scanner (subseq term-text (if prefix (length prefix) 0))))
      (setf (enum self) (terms-from reader (make-term field prefix))))))

(defmethod render ((self regular-expression-query))
  (with-slots (term) self
    (format nil "~a:'~a'" (term-field term) (term-text term))))
 
(defmethod get-term-enum ((self regular-expression-query) reader)
  (make-instance 'regular-expression-term-enum
		 :reader reader
		 :search-term (slot-value self 'term)))

(defmethod extract-terms ((self regular-expression-query) terms)
  (let ((rewritten (rewritten self)))
    (extract-terms rewritten terms)))
