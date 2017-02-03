(in-package #:montezuma)

(defgeneric flush (index-output))

(defgeneric flush-buffer (buffered-index-output buffer length))

(defgeneric close-down (index-output))

(defgeneric pos (index-output))

(defgeneric seek (index-output pos))

(defgeneric files (directory))

(defgeneric file-exists-p (directory file))

(defgeneric modified-time (directory file))

(defgeneric touch (directory file))

(defgeneric delete-directory-file (directory file)) ; was delete-file

(defgeneric rename-directory-file (directory from to))

(defgeneric file-size (directory file))

(defgeneric create-output (directory filename))

(defgeneric open-input (directory filename))

(defgeneric make-lock (directory lock-name))

(defgeneric obtain (lock &optional timeout))

(defgeneric release (lock))

(defgeneric locked-p (lock))

(defgeneric read-byte-from-buffer (index-input))

(defgeneric read-bytes-from-buffer (index-input buffer offset length))

(defgeneric read-int (input))
(defgeneric read-long (input))
(defgeneric read-uint (input))
(defgeneric read-ulong (input))
(defgeneric read-vint (input))
(defgeneric read-vlong (input))
(defgeneric read-string (input))
(defgeneric read-chars (input buffer start length))

(defgeneric write-byte-to-buffer (index-output byte))

(defgeneric write-bytes-to-buffer (index-output buffer length))

(defgeneric write-int (index-output int))
(defgeneric write-uint (index-output uint))
(defgeneric write-vint (index-output vint))
(defgeneric write-vlong (index-output vlong))
(defgeneric write-long (index-output long))
(defgeneric write-ulong (index-output ulong))
(defgeneric write-string-to-buffer (index-output string))
(defgeneric write-chars (index-output buffer start length))

(defgeneric read-internal (buffered-index-input buffer offset length))

(defgeneric seek-internal (buffered-index-input pos))

(defgeneric file-count (directory))



