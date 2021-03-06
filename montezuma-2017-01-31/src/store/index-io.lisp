(in-package #:montezuma)

(defclass index-input ()
  ())

(defmethod read-int ((self index-input))
  (let ((i1 (read-byte-from-buffer self))
	(i2 (read-byte-from-buffer self))
	(i3 (read-byte-from-buffer self))
	(i4 (read-byte-from-buffer self)))
    (+ (if (zerop (logand i1 #x80)) 0 (- #x100000000))
       (ash i1 24) (ash i2 16) (ash i3 8) i4)))

(defmethod read-long ((self index-input))
  (+ (ash (read-int self) 32)
     (logand (read-int self) #xFFFFFFFF)))

(defmethod read-uint ((self index-input))
  (logior (ash (read-byte-from-buffer self) 24)
	  (ash (read-byte-from-buffer self) 16)
	  (ash (read-byte-from-buffer self) 8)
	  (read-byte-from-buffer self)))

(defmethod read-ulong ((self index-input))
  (logior (ash (read-uint self) 32)
	  (logand (read-uint self) #xFFFFFFFF)))


(defmethod read-vint ((self index-input))
  (let* ((b (read-byte-from-buffer self))
	 (i (logand b #x7f))
	 (shift 7))
    (loop while (not (zerop (logand b #x80)))
	 do
	 (setf b (read-byte-from-buffer self))
	 (setf i (logior i (ash (logand b #x7f) shift)))
	 (incf shift 7))
    i))

(defmethod read-vlong ((self index-input))
  (read-vint self))


(defmethod read-string ((self index-input))
  (let* ((length (read-vint self))
	 (chars (make-array (list length))))
    (read-chars self chars 0 length)
    (bytes-to-string chars)))

(defmethod read-chars ((self index-input) buffer start length)
  ;; FIXME: the ferret code extends the array here if required, but do
  ;; we really want to have to force callers to use the return value
  ;; (in the case where adjust-array can't return the same array?)
  ;; Let's try forcing callers to call us with a sufficiently-sized
  ;; array in the first place.
;;  (when (< (array-dimension buffer 0) (+ start length))
;;    (adjust-array buffer (+ start length) :fill-pointer T))
  (dotimes (i length)
    (setf (elt buffer (+ i start)) (read-byte-from-buffer self))))

(defclass index-output ()
  ())

(defmethod write-int ((self index-output) i)
  (flet ((sa (i count) (logand (ash i count) #xff)))
    (write-byte-to-buffer self (sa i -24))
    (write-byte-to-buffer self (sa i -16))
    (write-byte-to-buffer self (sa i -8))
    (write-byte-to-buffer self (sa i 0))))

(defmethod write-uint ((self index-output) i)
  (write-int self i))

(defmethod write-vint ((self index-output) int)
  (loop for i = int then (ash i -7)
        while (> i 127) do (write-byte-to-buffer self (logior (logand i #x7f) #x80))
        finally (write-byte-to-buffer self i)))

(defmethod write-vlong ((self index-output) i)
  (write-vint self i))

(defmethod write-long ((self index-output) i)
  (write-int self (ash i -32))
  (write-int self i))

(defmethod write-ulong ((self index-output) i)
  (write-long self i))

(defmethod write-string-to-buffer ((self index-output) s)
  (let ((chars (string-to-bytes s)))
    (write-vint self (length chars))
    (write-chars self chars 0 (length chars))))

(defmethod write-chars ((self index-output) buffer start length)
  (dotimes (i length length)
    (write-byte-to-buffer self (aref buffer (+ i start)))))


