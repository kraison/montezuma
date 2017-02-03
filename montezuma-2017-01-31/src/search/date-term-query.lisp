(in-package #:montezuma)

(defclass date-term-query (term-query)
  ())

(defmethod print-object ((self date-term-query) stream)
  (let ((term (term self)))
    (print-unreadable-object (self stream :type T :identity T)
      (princ
       `(:field ,(term-field term)
         :definition ,(definition self)
         :term ,(term-text term)
         ,@(unless (= 1.0 (boost self)) (list :boost (boost self))))
       stream))))

(defmethod rewrite ((self date-term-query) reader)
  "Convert any partial date into an equivalent datetime interval."
  (declare (ignore reader))
  (let ((datetime (term-text (term self))))
    (unless datetime (return-from rewrite nil))
    (multiple-value-bind (lower-timestamp upper-timestamp)
        (make-timestamps datetime)
      (make-instance 'date-range-query
                     :field (term-field (term self))
                     :lower lower-timestamp
                     :upper upper-timestamp
                     :lower-function #'local-time:timestamp>=
                     :upper-function #'local-time:timestamp<
                     :include-lower-p t
                     :include-upper-p nil
                     :definition (definition self)))))
