(in-package #:montezuma)


(defclass filtered-term-enum (term-enum)
  ((term :initform nil :reader term)
   (enum :initform nil)
   (reader :initform nil)))


(defgeneric (setf enum) (enum filtered-term-enum))

(defmethod (setf enum) (enum (self filtered-term-enum))
  (setf (slot-value self 'enum) enum)
  (let ((term (term enum)))
    (if (and term (term-compare self term))
	(setf (slot-value self 'term) term)
	(next? self))))

(defmethod doc-freq ((self filtered-term-enum))
  (with-slots (enum) self
    (if (null enum)
	-1
	(doc-freq enum))))

(defmethod next? ((self filtered-term-enum))
  (with-slots (enum) self
    (if (null enum)
	NIL
	(progn
	  (setf (slot-value self 'term) nil)
	  (loop while (null (slot-value self 'term))
	       do
	       (when (or (end-enum self) (not (next? enum)))
		 (return-from next? NIL))
	       (let ((term (term enum)))
		 (when (term-compare self term)
		   (setf (slot-value self 'term) term)
		   (return-from next? term))))
	  (setf (slot-value self 'term) nil)
	  NIL))))

(defmethod close-down ((self filtered-term-enum))
  (with-slots (enum term) self
    (close-down enum)
    (setf term nil)
    (setf enum nil)))

(defgeneric difference (enum))

(defmethod difference ((self filtered-term-enum))
  1.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pattern-term-enum (filtered-term-enum)
  ((end-enum :initform NIL :reader end-enum)
   (search-term :type 'term :initarg :search-term)
   (field :type 'string)
   (prefix :type 'string)
   (pattern)))

(defmethod term-compare ((self pattern-term-enum) term)
  "If term is in the enum field, it matches the prefix, and the pattern, return T. When we exhaust field tokens return NIL."
  (with-slots (field prefix pattern end-enum) self
    (when (string= field (term-field term))
      (return-from term-compare (scans pattern prefix (term-text term))))
    (setf end-enum t)
    nil))

(defmethod close-down :after ((self pattern-term-enum))
  (with-slots (pattern field) self
    (setf pattern nil
	  field nil)))
