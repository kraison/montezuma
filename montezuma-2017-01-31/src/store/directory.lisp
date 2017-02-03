(in-package #:montezuma)

(defclass virtual-directory ()
  ((lock-prefix :initarg :lock-prefix))
  (:default-initargs
   :lock-prefix "montezuma-"))

(defmethod file-count ((self virtual-directory))
  (length (files self)))

(defclass lock ()
  ((max-attempts :initarg :max-attempts))
  (:default-initargs
   :max-attempts 5))

(defmacro with-lock ((lock) &body body)
  (let ((lock-var (gensym "LOCK")))
    `(let ((,lock-var ,lock))
       (obtain ,lock-var)
       (unwind-protect
	    (progn ,@body)
	 (release ,lock-var)))))

(defmacro do-files ((file-var virtual-directory) &body body)
  `(dolist (,file-var (files ,virtual-directory))
     ,@body))
