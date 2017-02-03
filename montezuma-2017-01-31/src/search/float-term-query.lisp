
(in-package #:montezuma)

(defclass float-term-query (term-query)
  ((float-term :accessor float-term :initarg :float-term :initform nil)))

(defmethod initialize-instance :after ((self float-term-query) &key)
  (unless (float-term self)
    (setf (float-term self) (parse-float (term-text (term self))))))

(defmethod print-object ((self float-term-query) stream)
  (let ((term (term self)))
      (print-unreadable-object (self stream :type T :identity nil)
        (princ `(:field ,(term-field term)
                 :definition ,(definition self)
                 :float-term ,(float-term self)
                 ,@(unless (= 1.0 (boost self)) (list :boost (boost self))))
               stream))))

(defmethod rewrite ((self float-term-query) reader)
  "I'm using an float range to normalize actual field values."
  (declare (ignore reader))
  (let ((float-term (float-term self)))
    (make-instance 'float-range-query
                   :field (term-field (term self))
                   :lower float-term
                   :upper float-term
                   :lower-function #'>=
                   :upper-function #'<=
                   :include-lower-p t
                   :include-upper-p t
                   :definition (definition self))))

