(in-package #:montezuma)

(defclass field-sorted-hit-queue (priority-queue)
  ((reader :type t :initarg :reader :accessor reader)
   (fields :type vector :initarg :fields :accessor fields)))

(defun get-document-field-data (document name)
  (let ((field (document-field document name)))
    (if field (field-data field))))

(defmethod initialize-instance :after ((queue field-sorted-hit-queue) &rest args &key &allow-other-keys)
  (with-slots (reader fields predicate) queue
    (if (or (null reader) (not (typep reader 'index-reader)))
        (error "The slot READER must be bound to a function in the object ~s" queue))
    (if (or (null fields) (not (vectorp fields)))
        (error "The slot FIELDS must be bound to a vector of field names in the object ~s" queue))
    (setf predicate
          #'(lambda(a b)
              ;(break "field-sorted-less-than")
              (let ((doca (get-document (reader queue) (doc a)))
                    (docb (get-document (reader queue) (doc b)))
                    (comparison nil))
                (loop
                 for sort-field across (fields queue)
                 for fda = (get-document-field-data doca (named sort-field))
                 for fdb = (get-document-field-data docb (named sort-field)) do
                 (if (reverse-p sort-field)
                     (let ((fdx fda))
                       (setf fda fdb fdb fdx)))
                 (setf comparison (cond
                                   ((and (null fda) (null fdb)) 0)
                                   ((null fda) -1)
                                   ((null fdb) 1)
                                   (t (string-compare fdb fda))))
                 (unless (zerop comparison) (return)))
                (< comparison 0))))))
#|
(defvar fshq)
(setf fshq (make-instance 'montezuma::field-sorted-hit-queue
                          :max-size 10
                          :reader (reader oanc-corpus)
                          :fields #("title" "author")))
|#