(in-package #:montezuma)

(defclass query-parse-result ()
  ((query :accessor query :initarg :query :initform nil)
   (sort-specification :accessor sort-specification :initarg :sort-specification :initform ())
   (first-document :accessor first-document :initarg :first-document :type fixnum :initform 0)
   (number-of-documents :accessor number-of-documents :initarg :number-of-documents :type fixnum :initform 10)
   (chosen-fields :accessor chosen-fields :initarg :chosen-fields :initform ())))

(defmethod print-object ((result query-parse-result) stream)
  ;;(format stream "~a~%" (type-of result))
  (print-unreadable-object (result stream :type T :identity nil)
    (with-slots (query first-document number-of-documents chosen-fields sort-specification) result
      (if query (format stream "Query: ~s~%" query)) ;format stream "  Query: ~a~%" query))
      (if first-document (format stream "~&First-document: ~d" first-document))
      (if number-of-documents (format stream "~&Number-of-documents: ~d" number-of-documents))
      (if sort-specification (format stream "~&Sort: ~a" sort-specification))
      (if chosen-fields (format stream "~&Fields: ~a~%" chosen-fields)))))
