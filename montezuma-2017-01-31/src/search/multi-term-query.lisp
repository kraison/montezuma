(in-package #:montezuma)

(defclass multi-term-query (query)
  ((term :initarg :term :reader term)))

(defmethod render ((self multi-term-query))
  (with-slots (term) self
    (format nil "~a:~a" (term-field term) (term-text term))))
 
(defmethod field ((self multi-term-query))
  (term-field (term self)))

(defmethod print-object ((self multi-term-query) stream)
  (print-unreadable-object (self stream) ; :type T)
    (if (slot-boundp self 'term)
        (let ((trm (term self)))
          (format stream "~(~a~) ~S:~S" (type-of self) (term-field trm) (term-text trm)))
      (format stream "[no term]"))))

(defmethod rewrite ((self multi-term-query) reader)
  (let ((enumerator (get-term-enum self reader))
	(bq (make-instance 'boolean-query)))
    (unwind-protect
        (loop
         for term = (term enumerator)
         when term do
         (add-term-query bq term (* (boost self) (difference enumerator)) :should-occur)
         while (and term (next? enumerator)))
      (close-down enumerator))
    bq))

(defgeneric get-term-enum (query reader))

#|
(rewrite (query (process-query oanc-corpus "title:to*")) (reader oanc-corpus))
|#
