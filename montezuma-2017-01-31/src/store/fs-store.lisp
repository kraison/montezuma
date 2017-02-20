(in-package #:montezuma)


(defvar *directory-cache*
  (make-hash-table :test #'equal))

(defun make-fs-directory (path &key create-p)
  (setf path (cl-fad:pathname-as-directory path))
  (when create-p
    (ensure-directories-exist path))
  (let ((truename (truename path)))
    (format t "make-fs-directory ~s create-p ~a~%" truename create-p)
    (let ((dir (gethash truename *directory-cache*)))
      (unless dir
	(setf dir (make-instance 'fs-directory :path path))
	(setf (gethash truename *directory-cache*) dir))
      (when create-p
	(refresh dir))
      dir)))

(defun clear-fs-directory-cache ()
  (setf *directory-cache* (make-hash-table :test #'equal)))


(defclass fs-directory (virtual-directory)
  ((path :initarg :path :accessor path)))

(defmethod print-object ((self fs-directory) stream)
  (print-unreadable-object (self stream :type T)
    (if (slot-boundp self 'path)
	(format stream "~S" (slot-value self 'path))
      (format stream "[path unbound]"))))

(defmethod initialize-instance :after ((self fs-directory) &key)
  (with-slots (path) self
    (unless (cl-fad:directory-exists-p path)
      (error "The directory ~S does not exist." path))
    ;; FIXME: do the locking
    ))


(defgeneric full-path-for-file (fs-directory file))

(defmethod full-path-for-file ((self fs-directory) file)
  (with-slots (path) self
    (merge-pathnames (pathname file) path)))

(defgeneric refresh (fs-directory))

(defmethod refresh ((self fs-directory))
  ;; Delete all files
  (with-slots (path) self
    (fad:delete-directory-and-files path))
  ;; Remove all locks
  )

(defmethod files ((self fs-directory))
  (with-slots (path) self  
    (mapcar #'(lambda (file-path)
		(enough-namestring file-path path))
	    (cl-fad:list-directory path))))

(defmethod file-exists-p ((self fs-directory) file)
  (probe-file (full-path-for-file self file)))

(defmethod touch ((self fs-directory) file)
  (let ((file-path (full-path-for-file self file)))
    (ensure-directories-exist file-path)
    (if (probe-file file-path)
	;; FIXME: Set file-write-date.
	nil
      (with-open-file (f file-path :direction :output :if-does-not-exist :create)
        (declare (ignorable f))))))

(defmethod delete-directory-file ((self fs-directory) file)
  (cl:delete-file (full-path-for-file self file)))

(defmethod rename-directory-file ((self fs-directory) from to)
  (let ((from-path (full-path-for-file self from))
        (to-path (full-path-for-file self to)))
    ;; RENAME-FILE uses the from-path to provide defaults for the to-path.
    (when (null (pathname-type to-path))
      (setf to-path (make-pathname :type :unspecific :defaults to-path)))
    ;; cl:rename-file may throw an error if the to-path already exists
    ;; -- we want to just overwrite it.
    (when (probe-file to-path)
      (cl:delete-file to-path))
    (cl:rename-file from-path to-path)))


#|
(defmethod rename-directory-file ((self fs-directory) from to)
  (let ((from-path (full-path-for-file self from))
	(to-path (full-path-for-file self to)))
    ;; Uses the from-path to provide defaults for the to-path.
    (when (null (pathname-type to-path))
      (setf to-path (make-pathname :type :unspecific :defaults to-path)))
    (cl-fad:copy-file from-path to-path :overwrite t)
    (cl:delete-file from-path)
    ))

(defmethod rename-directory-file ((self fs-directory) from to)
  (let ((from-path (full-path-for-file self from))
	(to-path (full-path-for-file self to)))
    ;; Use the from-path to provide defaults for the to-path.
    (when (null (pathname-type to-path))
      (setf to-path (make-pathname :type :unspecific :defaults to-path)))
    ;; cl:rename-file may throw an error if the to-path already exists--we want to just overwrite it.
    (when (probe-file to-path)
      (cl:delete-file to-path))
    (cl:rename-file from-path to-path)))

      (loop
       for try from 0 to 5 do
       (unless (zerop try) (format t "Retry ~d delete-file ~a to ~a~%" try to-path))
       (catch 'file-error
         (cl:delete-file to-path))
       (return)))

    (loop
     for try from 0 to 5 do
     (unless (zerop try) (format t "Retry ~d rename ~a to ~a~%" try from-path to-path))
     (catch 'file-error ; operation-error
       (if (cl:rename-file from-path to-path)
           (return))))))
|#

#|
(defmethod rename-directory-file ((self fs-directory) from to)
  (let ((from-path (full-path-for-file self from))
	(to-path (full-path-for-file self to)))
    ;; RENAME-FILE uses the from-path to provide defaults for the
    ;; to-path.  This is terrible, just terrible:
    (when (null (pathname-type to-path))
      (setf to-path (make-pathname :type :unspecific :defaults to-path)))
    ;; cl:rename-file is allowed to throw an error if the to-path
    ;; already exists--we want to just overwrite it.
    (when (probe-file to-path)
      (cl:delete-file to-path)) ; REA: try fixing contention with segment files
    ;;(with-open-file (f to-path :direction :io :if-exists :overwrite :if-does-not-exist nil)
    ;;  (declare (ignorable f)))
    (cl:rename-file from-path to-path)))
|#

(defmethod modified-time ((self fs-directory) file)
  (file-write-date (full-path-for-file self file)))

(defmethod file-size ((self fs-directory) file)
  (with-open-file (f (full-path-for-file self file)
		     :direction :input)
    (cl:file-length f)))

(defmethod create-output ((self fs-directory) file)
  (make-instance 'fs-index-output :path (full-path-for-file self file)))


(defmethod open-input ((self fs-directory) file)
  (make-instance 'fs-index-input :path (full-path-for-file self file)))

(defmethod close-down ((self fs-directory))
  ;; FIXME: ugh.
  ())


(defclass fs-index-output (buffered-index-output)
  ((file))
  (:default-initargs
      :buffer-size 4096))

(defmethod initialize-instance :after ((self fs-index-output) &key path)
  (with-slots (file) self
    (ensure-directories-exist path)
    (setf file (open path
		     :direction :output
		     :element-type '(unsigned-byte 8)
		     :if-exists :supersede))))

(defmethod close-down :after ((self fs-index-output))
  (with-slots (file) self
    (cl:close file)))

(defmethod seek :after ((self fs-index-output) pos)
  (with-slots (file) self
    (file-position file pos)))

(defmethod flush-buffer ((self fs-index-output) b size)
  (with-slots (file) self
    (write-sequence b file :start 0 :end size)))


(defclass fs-index-input (buffered-index-input)
  ((file)
   (is-clone-p :initform NIL)
   (size :reader size)))

(defmethod initialize-instance :after ((self fs-index-input) &key path)
  (when path
    (with-slots (file size) self
      (setf file (open path :direction :input :element-type '(unsigned-byte 8) :if-does-not-exist :create))
      (setf size (cl:file-length file)))))

(defmethod close-down ((self fs-index-input))
  (with-slots (file is-clone-p) self
    (unless is-clone-p
      (cl:close file))))

(defmethod initialize-copy :after ((self fs-index-input) o)
  (declare (ignore o))
  (with-slots (is-clone-p) self
    (setf is-clone-p T)))

(defmethod read-internal ((self fs-index-input) b offset length)
  (with-slots (file) self
    (let ((position (pos self)))
      (when (not (= position (file-position file)))
	(file-position file position)))
    (let ((num-bytes-read (read-sequence b file :start offset :end (+ offset length))))
      (unless (= num-bytes-read length)
	(error "End of file error while reading ~S" file))))
  (values))

(defmethod seek-internal ((self fs-index-input) pos)
  (with-slots (file) self
    (file-position file pos)))



