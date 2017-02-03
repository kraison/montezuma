
(in-package #:montezuma)

(defclass integer-term-query (term-query)
  ())

#|
(defmethod initialize-instance :after ((self integer-term-query) &key)
  )
|#

(defmethod print-object ((self integer-term-query) stream)
  (let ((term (term self)))
      (print-unreadable-object (self stream :type T :identity nil)
        (princ `(:field ,(term-field term)
                  :definition ,(definition self)
                  :term ,(term-text term)
                  ,@(unless (= 1.0 (boost self))
                      (list :boost (boost self))))
                stream))))

(defmethod rewrite ((self integer-term-query) reader)
  "I'm using an integer range to normalize actual field values."
  (declare (ignore reader))
  (with-slots (term definition) self
    (make-instance 'integer-range-query
                   :field (term-field term)
                   :lower (term-text term)
                   :upper (term-text term)
                   :lower-function #'>=
                   :upper-function #'<=
                   :include-lower-p t
                   :include-upper-p t
                   :definition definition)))
