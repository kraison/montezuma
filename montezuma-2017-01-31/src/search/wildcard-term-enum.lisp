(in-package #:montezuma)

(defclass wildcard-term-enum (pattern-term-enum)
  ())

(defmethod initialize-instance :after ((self wildcard-term-enum) &key reader query)
  (with-slots (field search-term prefix pattern) self
    (setf search-term (term query))
    (setf field (term-field (term query)))
    (setf prefix (wildcard-prefix query))
    (setf pattern (wildcard-pattern query))
    (setf (enum self) (terms-from reader (make-term field prefix)))))
