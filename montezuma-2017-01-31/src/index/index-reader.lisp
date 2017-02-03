(in-package #:montezuma)

(defparameter *index-reader-filename-extensions*
  '("cfs"
    "fnm"
    "fdx"
    "fdt"
    "tii"
    "tis"
    "frq"
    "prx"
    "del"
    "tvx"
    "tvd"
    "tvf"
    "tvp"))

(defvar *reader-id* 0)
(defvar *reader-id-lock* (bordeaux-threads:make-recursive-lock))

(defun next-reader-id ()
  (bordeaux-threads:with-recursive-lock-held (*reader-id-lock*)
    (incf *reader-id*)))

(defclass index-reader ()
  ((index-directory :initarg :directory :reader index-directory)
   (close-directory-p :initarg :close-directory-p :initform NIL)
   (segment-infos :initarg :segment-infos :initform nil)
   (directory-owner :initarg :directory-owner :initform NIL)
   (has-changes-p :initform NIL)
   (stale :initform NIL)
   (id :initform 0 :accessor id :initarg :id)))

(defmethod initialize-instance :after ((self index-reader) &key)
  ;;(care-when-finalized self)
  )

(defmethod release-reader ((self index) (reader index-reader))
  (bordeaux-threads:with-recursive-lock-held ((readers-lock self))
    (close-down reader)
    (setf (in-use-readers self)
          (remove reader (in-use-readers self)))))

;; FIXME: locks
;; We assume that anyone calling this already has the rw-lock
(defun open-index-reader (directory &key (close-directory-p T) (infos nil) id)
  (if (null directory)
      (setf directory (make-instance 'ram-directory))
      (when (stringp directory)
	(setf directory (make-fs-directory directory :create-p NIL))))
  (when (null infos)
    (setf infos (make-instance 'segment-infos))
    (read-segment-infos infos directory))
  (if (= (size infos) 1)
      (get-segment-reader (segment-info infos 0)
                          :infos infos
                          :id id
                          :close-directory-p close-directory-p)
      (let ((readers (make-array (size infos))))
	(dotimes (i (size infos))
	  (setf (aref readers i) (get-segment-reader (segment-info infos i) :id id)))
	(make-instance 'multi-reader
		       :sub-readers readers
		       :directory directory
		       :segment-infos infos
		       :close-directory-p close-directory-p))))

(defgeneric get-current-version (index-reader directory))

(defmethod get-current-version ((self index-reader) directory)
  (segment-infos-read-current-version directory))

(defgeneric get-term-vectors (index-reader doc-number))

(defgeneric get-term-vector (index-reader doc-number field))

(defgeneric index-reader-index-exists (directory))
(defmethod index-reader-index-exists (directory)
  (file-exists-p directory "segments"))

(defgeneric num-docs (index-reader))

(defgeneric max-doc (index-reader))

(defgeneric get-document-with-term (index-reader term))

(defmethod get-document-with-term ((self index-reader) term)
  (let ((docs (term-docs-for self term)))
    (if (null docs)
	nil
	(unwind-protect
	     (if (next? docs)
		 (get-document self (doc docs))
		 nil)
	  (close-down docs)))))

(defgeneric document-deleted-p (index-reader n))

(defgeneric has-norms-p (index-reader field))

(defmethod has-norms-p ((self index-reader) field)
  (get-norms self field))

(defgeneric get-norms (index-reader field))

(defgeneric get-norms-info (index-reader field bytes offset))

(defgeneric set-norm (index-reader doc field value))

(defmethod set-norm ((self index-reader) doc field value)
  (when (floatp value)
    (setf value (similarity-encode-norm value)))
  (do-set-norm self doc field value)
  (setf (slot-value self 'has-changes-p) T))

(defgeneric do-set-norm (index-reader doc field value))

(defgeneric term-doc-freq (index-reader term))

(defgeneric term-docs-for (index-reader term))

(defmethod term-docs-for ((self index-reader) term)
  (let ((term-docs (term-docs self)))
    (seek term-docs term)
    term-docs))

(defgeneric term-docs (index-reader))

(defgeneric term-positions-for (index-reader term))

(defmethod term-positions-for ((self index-reader) term)
  (let ((term-positions (term-positions self)))
    (seek term-positions term)
    term-positions))

(defgeneric term-positions (index-reader))

(defgeneric latest-p (index-reader))

(defmethod latest-p ((self index-reader))
  (with-slots (index-directory segment-infos) self
    (eql (segment-infos-read-current-version index-directory)
	 (version segment-infos))))

(defgeneric delete-document (index-reader doc-number))

(defmethod delete-document ((self index-reader) doc-num)
  (do-delete self doc-num)
  (setf (slot-value self 'has-changes-p) T))

(defgeneric delete-docs-with-term (index-reader term))

(defmethod delete-docs-with-term ((self index-reader) term)
  (let ((docs (term-docs-for self term)))
    (if (null docs)
	0
	(let ((n 0))
	  (unwind-protect
	       (while (next? docs)
		 (delete-document self (doc docs))
		 (incf n))
	    (close-down docs))
	  n))))

(defgeneric undelete-all (index-reader))

(defmethod undelete-all ((self index-reader))
  (do-undelete-all self)
  (setf (slot-value self 'has-changes-p) T))

(defgeneric commit (index-reader))

;; We assume anyone who calls this is holding the write lock
(defmethod commit ((self index-reader))
  (with-slots (has-changes-p directory-owner index-directory segment-infos) self
    (when has-changes-p
      (if directory-owner
	  (progn
	    (do-commit self)
	    (write-segment-infos segment-infos index-directory))
	  (do-commit self)))
    (setf has-changes-p NIL)))

;;(defmethod when-finalized ((self index-reader))
;;  (close-down self))

(defmethod close-down ((self index-reader))
;  (ignore-finalization self)
  (commit self)
  (do-close self)
  (with-slots (index-directory close-directory-p) self
    (when close-directory-p
      (close-down index-directory))))
