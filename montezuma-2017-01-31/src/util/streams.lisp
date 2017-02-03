(in-package #:montezuma)

#|
(defun stream-contents-failure-on-non-base-char (stream)
  "Returns a string with the entire contents of the specified stream."
  (with-output-to-string (contents)
    (let* ((buffer-size 4096)
	   (buffer (make-string buffer-size)))
      (loop for size = (read-sequence buffer stream)
	 do (cl:write-string buffer contents :start 0 :end size)
	 while (= size buffer-size)))))
|#

(defun stream-contents (stream &optional (buffer-size 4096))
  "Returns a string with the entire contents of the specified stream."
  (with-output-to-string (contents nil :element-type 'character)
    (let ((buffer (make-string buffer-size :element-type 'character)))
      (loop for size = (read-sequence buffer stream)
            do (cl:write-string buffer contents :start 0 :end size)
            while (and (> size 0) (= size buffer-size))))))


(defun file-contents (path)
  "Returns a string with the entire contents of the specified file."
  (with-open-file (stream path
                          :direction :input
                          ;;:external-format :utf-8
                          :element-type 'character)
    (stream-contents stream (cl:file-length stream))))
