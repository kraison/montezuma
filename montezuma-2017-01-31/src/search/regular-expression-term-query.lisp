(in-package #:montezuma)

(defclass regular-expression-term-query (multi-term-query)
  ())

(defmethod get-term-enum ((self regular-expression-term-query) reader)
  (make-instance 'regular-expression-term-enum
		 :reader reader
		 :search-term (term self)))

(defmethod extract-terms ((self regular-expression-term-query) terms)
  (extract-terms (rewritten self) terms))
