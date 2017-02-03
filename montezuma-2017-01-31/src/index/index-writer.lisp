(in-package #:montezuma)

(defparameter *index-writer-write-lock-timeout* 1)
(defparameter *index-writer-commit-lock-timeout* 10)
(defparameter *index-writer-write-lock-name* "write.lock")
(defparameter *index-writer-commit-lock-name* "commit.lock")
(defparameter *index-writer-default-merge-factor* 10)
(defparameter *index-writer-default-min-merge-docs* 10)
(defparameter *index-writer-default-max-merge-docs* #x7fffffff)
(defparameter *index-writer-default-max-field-length* 10000)
(defparameter *index-writer-default-term-index-interval* 128)

(defclass index-writer ()
  ((segment-directory :initarg :directory)
   (close-dir-p :initarg :close-dir-p)
   (use-compound-file-p :initarg :use-compound-file-p)
   (analyzer :initarg :analyzer :reader analyzer)
   (merge-factor :initarg :merge-factor :accessor merge-factor)
   (min-merge-docs :initarg :min-merge-docs :accessor min-merge-docs)
   (max-merge-docs :initarg :max-merge-docs :accessor max-merge-docs)
   (max-field-length :initarg :max-field-length)
   (term-index-interval :initarg :term-index-interval)
   (similarity :initform (make-default-similarity))
   (segment-infos :initform (make-instance 'segment-infos))
   (ram-directory)
   (info-stream :initform nil :initarg :info-stream :accessor info-stream))
  (:default-initargs
   :close-dir-p          NIL
    :use-compound-file-p T
    :analyzer            (make-instance 'standard-analyzer) ; :language "english")
    :merge-factor        *index-writer-default-merge-factor*
    :min-merge-docs      *index-writer-default-min-merge-docs*
    :max-merge-docs      *index-writer-default-max-merge-docs*
    :max-field-length    *index-writer-default-max-field-length*
    :term-index-interval *index-writer-default-term-index-interval*))

(defmethod initialize-instance :after ((self index-writer) &key (create-p NIL) (create-if-missing-p NIL) &allow-other-keys)
  (with-slots (segment-directory ram-directory segment-infos) self
    (if (null segment-directory)
	(setf segment-directory (make-instance 'ram-directory))
	(when (not (typep segment-directory 'virtual-directory))
	  (setf segment-directory (make-fs-directory segment-directory :create-p create-p))))
    (if create-p
	(write-segment-infos segment-infos segment-directory)
	;; FIXME: This really isn't the best way of doing this.
	(handler-case (read-segment-infos segment-infos segment-directory)
	  (error (e)
	    (if create-if-missing-p
		(write-segment-infos segment-infos segment-directory)
		(error e)))))
    (setf ram-directory (make-instance 'ram-directory))))

(defmethod close-down ((self index-writer))
  (flush-ram-segments self)
  (with-slots (ram-directory close-dir-p segment-directory) self
    (close-down ram-directory)
    (when close-dir-p
      (close-down segment-directory))))

(defgeneric document-count (index-writer))

(defmethod document-count ((self index-writer))
  (with-slots (segment-infos) self
    (let ((count 0))
      (dotimes (i (size segment-infos) count)
	(incf count (doc-count (segment-info segment-infos i)))))))

(defgeneric add-document-to-index-writer (index-writer document &optional analyzer))

(defmethod add-document-to-index-writer ((self index-writer) document &optional (analyzer nil analyzer-supplied-p))
  (with-slots (ram-directory similarity max-field-length term-index-interval info-stream
			     segment-infos) self
    (unless analyzer-supplied-p
      (setf analyzer (slot-value self 'analyzer)))
    (let ((dw (make-instance 'document-writer
			     :directory ram-directory
			     :analyzer analyzer
			     :similarity similarity
			     :max-field-length max-field-length
			     :term-index-interval term-index-interval))
	  (segment-name (new-segment-name self)))
      (setf (info-stream dw) info-stream)
      (add-document-to-writer dw segment-name document)
      ;; FIXME synchronize
      (add-segment-info segment-infos
			(make-instance 'segment-info
				       :name segment-name
				       :doc-count 1
				       :directory ram-directory))
      (maybe-merge-segments self))))


(defmethod optimize-index ((self index-writer))
  (flush-ram-segments self)
  (with-slots (segment-infos merge-factor segment-directory use-compound-file-p) self
    ;; FIXME: Insane.
    (while (or (> (size segment-infos) 1)
	       (and (= (size segment-infos) 1)
		    (let ((si (segment-info segment-infos 0)))
		      (or (has-deletions-p si)
			  (or (not (eq segment-directory (segment-directory si)))
			      (and use-compound-file-p
				   (or (not (uses-compound-file-p si))
				       (has-separate-norms-p si))))))))
	(let ((min-segment (- (size segment-infos) merge-factor)))
	  (merge-index self (max 0 min-segment))))))

(defmethod add-indexes ((self index-writer) &rest dirs)
  (optimize-index self)
  (with-slots (segment-infos merge-factor) self
    (let ((start (size segment-infos)))
      (dolist (dir dirs)
	(let ((sis (make-instance 'segment-infos)))
	  (read-segment-infos sis dir)
	  (dotimes (i (size sis))
	    (add-segment-info segment-infos (segment-info sis i)))))
      (while (> (size segment-infos) (+ start merge-factor))
	(do-range (base (+ start 1) (size segment-infos))
	  (let ((last (min (size segment-infos) (+ base merge-factor))))
	    (when (> (- last base) 1)
	      (merge-index self base last)))))
      (optimize-index self))))

(defmethod add-indexes-readers ((self index-writer) readers)
  (let ((segments-to-delete '())
	(merged-name (new-segment-name self)))
    (optimize-index self)
    (with-slots (segment-directory term-index-interval segment-infos use-compound-file-p) self
      (let ((merger (make-instance 'segment-merger
				   :directory segment-directory
				   :name merged-name
				   :term-index-interval term-index-interval)))
      (when (= (size segment-infos) 1)
	(let ((s-reader (get-segment-reader (segment-info segment-infos 0))))
	  (add-reader merger s-reader)
	  (push segments-to-delete s-reader)))
      (dosequence (reader readers)
	(add-reader merger reader))
      (let ((doc-count (merge-segment merger)))
	(clear segment-infos)
	(add-segment-info segment-infos (make-instance 'segment-info
						       :name merged-name
						       :doc-count doc-count
						       :directory segment-directory))
	(write-segment-infos segment-infos segment-directory)
	(delete-segments self segments-to-delete)
	(when use-compound-file-p
	  (let ((files-to-delete (create-compound-file merger (format nil "~A.tmp" merged-name))))
	    (rename-directory-file
             segment-directory
             (add-file-extension merged-name "tmp")
             (add-file-extension merged-name "cfs"))
	    (delete-files-and-write-undeletable self files-to-delete))))))))


(defgeneric new-segment-name (index-writer))

(defmethod new-segment-name ((self index-writer))
  (with-slots (segment-infos) self
    (let ((seg-name (string-downcase (format nil "_~36R" (counter segment-infos)))))
      (incf (counter segment-infos))
      seg-name)))

(defgeneric flush-ram-segments (index-writer))

(defmethod flush-ram-segments ((self index-writer))
  (with-slots (segment-infos ram-directory merge-factor) self
    (let ((min-segment (- (size segment-infos) 1))
	  (doc-count 0))
      (while (and (>= min-segment 0)
		  (eq (segment-directory (segment-info segment-infos min-segment)) ram-directory))
	(incf doc-count (doc-count (segment-info segment-infos min-segment)))
	(decf min-segment))
      (when (or (< min-segment 0)
		(> (+ doc-count (doc-count (segment-info segment-infos min-segment))) merge-factor)
		(not (eq (segment-directory (segment-info segment-infos (- (size segment-infos) 1)))
			 ram-directory)))
	(incf min-segment))
      (when (< min-segment (size segment-infos))
	(merge-index self min-segment)))))

(defgeneric maybe-merge-segments (index-writer))

(defmethod maybe-merge-segments ((self index-writer))
  (with-slots (min-merge-docs max-merge-docs segment-infos merge-factor) self
    (let ((target-merge-docs min-merge-docs))
      (while (<= target-merge-docs max-merge-docs)
	(let ((min-segment (- (size segment-infos) 1))
	      (merge-docs 0))
	  (while (>= min-segment 0)
	    (let ((si (segment-info segment-infos min-segment)))
	      (when (>= (doc-count si) target-merge-docs)
		(return))
	      (incf merge-docs (doc-count si))
	      (decf min-segment)))
	  (if (>= merge-docs target-merge-docs)
	      (merge-index self (+ min-segment 1))
	      (return))
	  (setf target-merge-docs (* target-merge-docs merge-factor)))))))

(defgeneric merge-index (index-writer min-segment &optional max-segment))

(defmethod merge-index ((self index-writer) min-segment &optional (max-segment nil max-segment-supplied-p))
  (with-slots (segment-infos info-stream term-index-interval segment-directory ram-directory use-compound-file-p) self
    (unless max-segment-supplied-p
      (setf max-segment (size segment-infos)))
    (let ((segments-to-delete '())
	  (merged-name (new-segment-name self)))
      (when info-stream
	(format info-stream "~&Merging segments from ~S to ~S"
		min-segment (- max-segment 1)))
      (let ((merger (make-instance 'segment-merger
				   :directory segment-directory
				   :name merged-name
				   :term-index-interval term-index-interval)))
	(do-range (i min-segment max-segment)
	  (let ((si (segment-info segment-infos i)))
	    (when info-stream
	      (format info-stream "~&~S (~S docs)"
		      (segment-info-name si)
		      (doc-count si)))
	    (let ((reader (make-instance 'segment-reader
					 :directory (segment-directory si)
					 :info si
					 :segment-infos nil
					 :close-directory-p NIL
					 :directory-owner NIL)))
	      (add-reader merger reader)
	      (when (or (eq (index-directory reader) segment-directory)
			(eq (index-directory reader) ram-directory))
		(push reader segments-to-delete)))))
	(let ((merged-doc-count (merge-segment merger)))
	  (when info-stream
	    (format info-stream "~& into ~S (~S docs)"
		    merged-name merged-doc-count ))
	  (loop for i from (- max-segment 1) downto min-segment
	       do (delete-at segment-infos i))
	  (add-segment-info segment-infos (make-instance 'segment-info
							 :name merged-name
							 :doc-count merged-doc-count
							 :directory segment-directory))
	  (close-readers merger)
	  (write-segment-infos segment-infos segment-directory)
	  (delete-segments self (reverse segments-to-delete))
	  (when use-compound-file-p
	    (let ((files-to-delete (create-compound-file merger (add-file-extension merged-name "tmp"))))
	      (rename-directory-file
               segment-directory
               (add-file-extension merged-name "tmp")
               (add-file-extension merged-name "cfs"))
	      (delete-files-and-write-undeletable self files-to-delete))))))))

(defgeneric delete-segments (index-writer segment-readers))
(defmethod delete-segments ((self index-writer) segment-readers)
  (with-slots (segment-directory) self
    (let ((deletable (try-to-delete-files self (read-deletable-files self))))
      (dolist (segment-reader segment-readers)
        ;;REA: should we see a match?
        ;;(break "delete-segments reader: ~s segment-directory ~s" (index-directory segment-reader) segment-directory)
	(if (eql (index-directory segment-reader) segment-directory)
	    (setf deletable (append deletable (try-to-delete-files self (file-names segment-reader))))
	    (delete-files self (file-names segment-reader) (index-directory segment-reader))))
      (write-deletable-files self deletable))))

(defgeneric delete-files-and-write-undeletable (index-writer files))

(defmethod delete-files-and-write-undeletable ((self index-writer) files)
  (let ((deletable '()))
    (setf deletable (try-to-delete-files self (read-deletable-files self)))
    (setf deletable (append deletable (try-to-delete-files self files)))
    (write-deletable-files self deletable)))

(defgeneric delete-files (index-writer filenames dir))

(defmethod delete-files ((self index-writer) filenames dir)
  (dolist (filename filenames)
    (delete-directory-file dir filename)))

(defgeneric try-to-delete-files (index-writer filenames))

(defmethod try-to-delete-files ((self index-writer) filenames)
  (with-slots (segment-directory) self
    (let ((deletions-to-retry '()))
      (dolist (filename filenames)
	(handler-case (delete-directory-file segment-directory filename)
	  (error (e)
	    (declare (ignore e))
	    ;; FIXME: Can we do no better than blindly trying again on
	    ;; any error?
	    (push filename deletions-to-retry))))
      (reverse deletions-to-retry))))

(defgeneric read-deletable-files (index-writer))

(defmethod read-deletable-files ((self index-writer))
  (with-slots (segment-directory) self
    (if (not (file-exists-p segment-directory "deletable"))
	'()
	(let ((filenames '())
	      (input (open-input segment-directory "deletable")))
	  (unwind-protect
	       (let ((file-count (read-int input)))
		 (dotimes (i file-count)
		   (push (read-string input) filenames)))
	    (close-down input))
	  (reverse filenames)))))

(defgeneric write-deletable-files (index-writer filenames))

(defmethod write-deletable-files ((self index-writer) filenames)
  (with-slots (segment-directory) self
    (let ((output (create-output segment-directory "deleteable.new")))
      (unwind-protect
	   (progn
	     (write-int output (length filenames))
	     (dolist (filename filenames)
	       (write-string-to-buffer output filename)))
	(close-down output)))
    (rename-directory-file segment-directory "deleteable.new" "deletable")))
