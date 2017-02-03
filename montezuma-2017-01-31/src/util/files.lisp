(in-package #:montezuma)

(defmacro add-file-extension (file extension)
  `(concatenate 'string ,file "." ,extension))

(defun device (path)
  (let* ((filespec (princ-to-string path))
         (colon (position #\: filespec)))
    (if colon (subseq filespec 0 (1+ colon)))))

(defun relative-path (root path)
  (if (null path) (return-from relative-path root))
  (let ((type (pathname-type path))
        (name (pathname-name path))
        (directory (pathname-directory path))
        (root-directory (pathname-directory root))
        (device1 (device root))
        (device2 (device path)))
    ;;(format t "relative-path type: ~s name ~s directory ~s root-directory ~s~%root device ~s~%path device ~s"
    ;;        type name directory root-directory device1 device2)
    (cond
     ((and device1 device2 (string-not-equal device1 device2))
      path)
     (type
      (format () 
              "~{~a/~}~a~:[~;.~a~]" ; path directory names followed by file name, type if not keyword
              (nthcdr (length root-directory) directory)
              (pathname-name path)
              (not (keywordp type))  ; no type so probably a directory, but may also be an untyped file
              type))
     (name
      (if (and (equal (car root-directory) (car directory))
               (equal (cadr root-directory) (cadr directory)))
          (format () "~{~a/~}~a" (nthcdr (length root-directory) directory) name)
        (if device2
            (subseq (princ-to-string path) 2) ; drop the device
          path)))
     (t
      (format () "~{~a/~}" (nthcdr (length root-directory) directory))))))


(defun file-size-kb (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (round (file-length stream) 1024)))

#|
(defvar *stream-buffer-size* 8192)

(defun copy-stream (from to &optional (checkp t))
  "Copies into TO \(a stream) from FROM \(also a stream) until the end
of FROM is reached, in blocks of *stream-buffer-size*.  The streams
should have the same element type.  If CHECKP is true, the streams are
checked for compatibility of their types."
  (when checkp
    (unless (subtypep (stream-element-type to) (stream-element-type from))
      (error "Incompatible streams ~A and ~A." from to)))
  (let ((buf (make-array *stream-buffer-size* :element-type (stream-element-type from))))
    (loop
       (let ((pos (read-sequence buf from)))
         (when (zerop pos) (return))
         (write-sequence buf to :end pos))))
  (values))

(defun copy-file (from to &key overwrite)
  "Copies the file designated by the non-wild pathname designator FROM
to the file designated by the non-wild pathname designator TO.  If
OVERWRITE is true overwrites the file designtated by TO if it exists."
  (let ((element-type '(unsigned-byte 8)))
    (with-open-file (in from :element-type element-type)
      (with-open-file (out to :element-type element-type
                           :direction :output
                           :if-exists (if overwrite :supersede :error))
        (copy-stream in out))))
  (values))
|#

(defun search-file (path target)
  (let ((contents (file-contents path))
        (offset 0))
    (loop
     (multiple-value-bind (start end) (cl-ppcre:scan target contents :start offset)
       (unless (and start end) (return))
       (if (zerop offset) (format t "~s~%" path))
       (format t " > ~a~%" (subseq contents start end ))
       (setf offset end)))
    (if (> offset 0) (terpri))))

(defun search-files (target &optional (root cl-user::*working-directory*) (types '("lisp")))
  (let* ((regexp (concatenate 'string "^.*" target ".*$"))
         (scanner (cl-ppcre:create-scanner regexp :case-insensitive-mode t :multi-line-mode t)))
    (fad:walk-directory 
     root #'(lambda(path) (search-file path scanner))
     :test #'(lambda(path) (or (not types) (member (pathname-type path) types :test 'string=)))
     :directories :depth-first)))

(defun search-montezuma (target)
  (search-files target :root  cl-user::*MONTEZUMA-ROOT*))

(defun search-dependencies (target)
  (search-files target :root  cl-user::*DEPENDENCIES*))

(defun list-files (root &optional types)
  (let ((files ()))
    (fad:walk-directory
     root
     #'(lambda(p) (push p files))
     :test #'(lambda(f) (or (null types)
                            (let ((type (pathname-type f)))
                              (if (listp types)
                                  (member type types)
                                (string-equal types type))))))
    (nreverse files)))

(defun line-count (path)
  (when (probe-file path)
    (with-open-file (stream path :direction :input)
      (loop while (read-line stream nil nil) counting t))))


#|
(search-files "file")
(search-files "field.*reader")
|#
