(in-package :cl-user)

(defparameter downloads (make-array 0 :fill-pointer t :adjustable t :element-type 'list))
(defparameter project-directory (merge-pathnames "ProjectGutenberg/" *working-directory*))
(defparameter zipped-files (merge-pathnames "zipped/" project-directory))
(defparameter max-zips-download 100) ;800)

(defun http-request (url)
  ;;(format t "~a~%" url)
  (multiple-value-bind (body-or-stream status-code)
      (drakma:http-request url)
    (if (= status-code 200)
        body-or-stream)))

(defun downloadPage(offset)
  (http-request (format nil "http://www.gutenberg.org/robot/harvest?offset=~d" offset)))

(defun getNextOffset (string &optional (retryCount 0))
  (multiple-value-bind (start end starts ends) (cl-ppcre:scan "href=\"harvest\\?offset=([0-9]+)\"" string)
    (cond
     ((and start end (> (length starts) 0))
      (read-from-string string nil nil :start (aref starts 0) :end (aref ends 0)))
     ((> retryCount 3)
      nil)
     (t
      (getNextOffset string (incf retryCount))))))

(defun slice (scanner match)
  (multiple-value-bind (start end starts ends) (cl-ppcre:scan scanner match)
    (if (and start end)
        (subseq match (aref starts 0) (aref ends 0)))))

(defun getZips(string)
  (let ((scanner (cl-ppcre:create-scanner " href=\"http://www\.gutenberg\.lib\.md\.us/(.+.zip)\""
                                          :case-insensitive-mode t
                                          :multi-line-mode t)))
    (loop
     for match in (cl-ppcre:all-matches-as-strings scanner string)
     collect (slice scanner match))))

(defun buildDownloadList (offset)
  (let ((string (downloadPage offset)))
    (loop
     for item in (getZips string) do
     (let ((filename (getFilename item)))
       (unless (probe-file filename)
         (format t "buildDownloadList page ~a item ~s~%" offset item)
         (if (>= (vector-push-extend (list offset item) downloads)
                 (1- max-zips-download))
             (return-from buildDownloadList)))))
    (unless (>= (fill-pointer downloads) max-zips-download)
      (let ((next (getNextOffset string)))
        (format t "Next page is ~s~%" next)
        (when next
          (buildDownloadList next))))))

(defun downloadToFile (file_url filename)
  ;;(break "downloadToFile ~s filename ~s" file_url filename)
  (let ((file (http-request (format nil "http://www.gutenberg.lib.md.us/~a" file_url))))
    (ensure-directories-exist filename :verbose t)
    (with-open-file (stream filename :direction :output :element-type '(unsigned-byte 8)) (write-sequence file stream))
    (zip:with-zipfile (zip filename)
      (zip:do-zipfile-entries (name entry zip)
        (addGutenbergTextFile project-directory name entry file_url)))))

(defun getFilename (url)
  (if (stringp url)
      (let ((pos (position #\/ url :from-end t)))
        (if pos (format nil "~a~a" zipped-files (subseq url (1+ pos)))))))

(defun downloadFile (url)
  (let ((fileName (getFilename url)))
    (if (and filename (not (probe-file filename)))
        (downloadToFile url fileName))))

(defun crawl (&optional (maxzips 100))
  (setf max-zips-download maxzips)
  (format t "Gutenberg index size: ~d~%" (index-size))

  (setf (fill-pointer downloads) 0)
  (format t "Building download list~%")
  (buildDownloadList 0)
  (format t "DownloadList complete: ~d zip files~%" (fill-pointer downloads))

  (format t "Downloading ~d zipped files~%" (length downloads))
  (loop
   with n = 0
   for (offset file) across downloads do
   (format t "zip file ~d of ~d: ~a ~a~%" (incf n) (fill-pointer downloads) offset file)
   (downloadFile file))
  (format t "Download complete...~%")

  (format t "Gutenberg index size: ~d~%" (index-size))
  
  (values))
