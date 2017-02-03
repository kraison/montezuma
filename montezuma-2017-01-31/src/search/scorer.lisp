(in-package #:montezuma)

(defparameter +max-docs+ #x7FFFFFF)

(defclass scorer ()
  ((similarity :initarg :similarity :reader similarity)
   (query :initarg :query :initform nil :accessor query)
   (reader :initarg :reader :accessor reader)
   (count :initform -1 :accessor counter)
   (max-doc :accessor max-doc)
   ))

(defmethod field ((self scorer))
  (field (query self)))

(defmethod field-range-data ((self scorer))
  (with-slots (reader count) self
    (document-value (get-document reader count) (field self))))

(defmethod each-hit ((self scorer) fn)
  (loop while (next? self) do (funcall fn (document self) (score self))))

(defmethod each-hit-up-to ((self scorer) max fn)
  (loop while (and (next? self) (< (document self) max))
       do (funcall fn (document self) (score self)))
  (< (document self) max))

(defmethod document ((self scorer))
  (counter self))

(defmethod next? ((self scorer))
  "Return T if there is an undeleted document ahead"
  (with-slots (count max-doc reader query) self
    (loop
     while (< count (- max-doc 1)) do
     (incf count)
     (when (and (not (deleted-p reader count))
                (satisfactory query reader count))
	   (return-from next? t)))
    nil))

(defmethod score ((self scorer))
  1.0)

(defmethod skip-to ((self scorer) target)
  (setf (slot-value self 'count) (- target 1))
  (next? self))


#|

(defmethod last-document-number ((self scorer))
  (with-slots (count reader) self
    (loop
     (incf count)
     (when (and (not (deleted-p reader count)))
       (return-from last-document-number t)))
    nil))
  (document self))


(last-document-number (make-instance 'scorer :reader (reader oanc-corpus)))

|#
