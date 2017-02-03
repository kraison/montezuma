(in-package #:montezuma)

;;?? depth
;;?? <<

(defclass explanation ()
  ((value :accessor value :initarg :value :initform nil)
   (description :accessor description :initarg :description :initform nil)
   (details :accessor details :initarg :details :initform (make-array 0 :adjustable T :fill-pointer 0))))

(defmethod print-object ((self explanation) stream)
  (print-unreadable-object (self stream :type nil :identity t)
    (format stream "~a => ~a" (value self) (description self))))


(defun make-explanation (description value)
  (make-instance 'explanation :description description :value value))

(defmethod add-detail ((explanation explanation) detail)
  (vector-push-extend detail (details explanation)))

