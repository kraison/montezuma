(in-package #:montezuma)

(defclass term-clause (boolean-clause term-query)
  ())

(defmethod query ((self term-clause))
  self)

(defmethod print-object ((self term-clause) stream)
  (let ((term (term self)))
    (print-unreadable-object (self stream) ; :type T :identity T)
      (format stream "~(~a~) ~s: ~S~a ~(~s~)"
              (type-of self)
              (term-field term)
              (term-text term)
              (if (= 1.0 (boost self)) "" (format nil "^~d" (boost self)))
              (occur self)))))

(defmethod rewrite ((self term-clause) reader)
  (declare (ignore reader))
  self)

(defmethod extract-terms ((self term-clause) terms)
  (vector-push-extend (term self) terms))


