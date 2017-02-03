(in-package :cl-user)

(defun http-request (url)
  (multiple-value-bind (body-or-stream status-code)
      (drakma:http-request url)
    (if (= status-code 200)
        body-or-stream)))

(defun downloadPage (offset)
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

(defun buildZipList (offset stream)
  (let ((string (downloadPage offset)))
    (loop for item in (getZips string) do (format stream "~a~%" item))
    (let ((next (getNextOffset string)))
      (format t "Next page is ~a~%" next)
      (when next (buildZipList next stream)))))

(defun line-count (path)
  (when (probe-file path)
    (with-open-file (stream path :direction :input)
      (loop while (read-line stream nil nil) counting t))))

(defun list-zip-files (path)
  ;; create a filtered list of Gutenberg zip files
  (with-open-file (stream path :direction :output :if-exists :supersede :element-type 'character)
    (buildZipList 0 stream))
  (format t "~d zip files~%" (line-count path))
  path)

(defun process-zip-file (filename)
  (let ((text-files 0))
    (zip:with-zipfile (zip filename)
      (zip:do-zipfile-entries (name entry zip)
        (if (and (> (length name) 4) (string-equal name ".txt" :start1 (- (length name) 4)))
            (let ((textname (merge-pathnames name project-directory)))
              (incf text-files)
              (unless (and (probe-file textname)
                           (> (file-size textname) 0))
                (format t "Found ~a~%" textname)))
#|
              (unless (and (probe-file textname)
                           (> (file-size textname) 0))
                (format t "Unzipping ~a~%" (montezuma:relative-path project-directory textname))
                (ensure-directories-exist textname)
                (with-open-file (stream textname :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
                  (zip:zipfile-entry-contents entry stream)))
              ;;(format t "Loading ~a into Gutenberg index~%" name)
              ;;(add-Gutenberg-text (mtz:relative-path project-directory filename) textname))))))
|#
              )))
    (when (zerop text-files)
      (let ((excluded-filename (merge-pathnames "../excluded/" filename)))
        (format t "Moving ~a to ~a~%" filename excluded-filename)
        (rename-file filename excluded-filename)
        ))))

(defun replace-spaces (str)
  (let ((vector (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character)))
    (with-output-to-string (stream vector)
      (loop
       for ch across str
       if (char= #\space ch) do (princ "%20" stream)
       else do (princ ch stream)))
    vector))

(defun downloadToFile (filename url)
  ;;(let ((pos (position #\/ url :from-end t)))
  ;;http://www.gutenberg.lib.md.us/4/8/7/6/48762/files/Volume%201.zip
  (setf url ;;(if pos
        (format nil "http://www.gutenberg.lib.md.us/~a" (replace-spaces url))
        ;;subseq url 0 (1+ pos)) (drakma:url-encode (subseq url (1+ pos)) :latin1))
        ;;(format nil "http://www.gutenberg.lib.md.us/~a" (drakma:url-encode url :latin1))))
        )
  ;;(break "downloadToFile ~s filename ~s" url filename)
  ;;(setf url (format nil "http://www.gutenberg.lib.md.us/~a" (drakma:url-encode url :latin1)))
  (ensure-directories-exist filename :verbose t)
  (let ((contents (http-request url)))
    (with-open-file (stream filename :direction :output :element-type '(unsigned-byte 8))
      (format t "Downloaded ~s: ~d bytes~%" filename (length contents))
      (write-sequence contents stream))
    (process-zip-file filename)))

(defparameter excluded-zip-files 
  '(("tex.zip" 6)
    ;;("Variations_Chromatiques_de_concert_mp3.zip" 1)
    ;;("Variations_Chromatiques_de_concert.zip" 1)
    ;;("trinity.zip" 1)
    ;;("baker.zip" 1)
    ;;("atomic.zip" 1)
    ;;("all.zip" 2)
    ;;("able.zip" 1)
    ("mpg.zip" 3)
    ;;("Descriptions.zip" 1)
    ;;("Contents.zip" 1) 
    ;;("AnnexI.zip" 1)
    ("ps.zip" 1)
    ;;("text.zip" 1)
    ("32bit.zip" 8)
    ("16bit.zip" 8)
    ("rst.zip" 622)
    ("spx.zip" 208)
    ("ogg.zip" 208)
    ("m4b.zip" 208)
    ;;("final.zip" 10)
    ("tmx.zip" 1)
    ("mac.zip" 1)
    ("mp3.zip" 575)
    ("tei.zip" 395)
    ("h.htm.zip" 2)
    ;;("d.zip" 1)
    ;;("inames.zip" 1)
    ("doc.zip" 54)
    ;;("utf8.zip" 1)
    ;;("utf16.zip" 1)
    ("rtf.zip" 3)
    ("images.zip" 343)
    ("xml.zip" 4)
    ;;("08.zip" 1)
    ;;("08_files.zip" 1)
    ("mus.zip" 5)
    ;;("readme.zip" 2)
    ;;("index.zip" 3)
    ;;("pos.zip" 2)
    ;;("body.zip" 4)
    ("t.zip" 139)
    ("pdf.zip" 691)
    ("mid.zip" 7)
    ;;("ly.zip" 2)
    ("m.zip" 44)
    ("r.zip" 160)
    ;;("p.zip" 1)
    ("x.zip" 29)
    ("sib.zip" 6)
    ;;("0.zip" 12217)
    ("h.zip" 42754)
    ;;("8.zip" 33598))
    ))

(defun included (zipfile zipfiles &key verbose)
  ;; Return T if we want this zipfile
  (let* ((pos (position #\- zipfile :from-end t))
         (name (pathname-name zipfile))
         (zipfile8 (concatenate 'string name "-8"))
         (zipfile0 (concatenate 'string name "-0")))
    (block nil
      (cond
       (pos
        (let ((tail (subseq zipfile (1+ pos)))
              (head (subseq zipfile 0 pos)))
          (cond
           ((string= tail "8.zip")
            (let ((utf8 (concatenate 'string head "0")))
              (when (gethash utf8 zipfiles)
                (if verbose (format t ";; ~a is preferred to ~a~%" utf8 zipfile))
                (return nil)))
            (return t))
           ((assoc tail excluded-zip-files :test #'string=)
            (if verbose (format t ";; ~a is amongst excluded extensions~%" zipfile))
            (return nil))
           (t
            (return t)))))
       ((gethash zipfile8 zipfiles)
        (if verbose (format t ";; ~a is preferred to ~a~%" zipfile8 zipfile))
        (return nil))
       ((gethash zipfile0 zipfiles)
        (if verbose (format t ";; ~a is preferred to ~a~%" zipfile0 zipfile))
        nil)
       (t
        (return t))))))

(defun make-zip-files-table ()
  (let ((zipfiles (make-hash-table :test #'equal)))
    (dolist (file (directory zipped-directory))
      (setf (gethash (pathname-name file) zipfiles) :zipped))
    (dolist (file (directory (merge-pathnames "../excluded/" zipped-directory)))
      (setf (gethash (pathname-name file) zipfiles) :excluded))
    zipfiles))

(defun move-redundant-zip-files (&key (actually-move nil))
  ;; We don't want to delete zip files that have been downloaded but let's put them to one side till later.
  (let* ((path zipped-files-list)
         (excluded 0)
         (moved 0)
         (zipfiles (make-zip-files-table)))
    (with-open-file (stream path :direction :input :element-type 'character)
      (loop
       with included = 0
       for zipfile = (read-line stream nil nil)
       while zipfile
       unless (eq :excluded (gethash (pathname-name zipfile) zipfiles))
       do
       (cond
        ((included zipfile zipfiles :verbose t)
         (incf included))
        (t
         (incf excluded)
         (let ((filename (getFilename zipfile)))
           (when (probe-file filename)
             (format t "moving ~a to ../excluded~%" filename)
             (incf moved)
             (if actually-move (rename-file filename (merge-pathnames "../excluded/" filename)))
             ))))
       finally
       (format t "Total zip files: ~d~%" (line-count path))
       (format t "Included: ~d~%" included)
       (format t "Moved: ~d~%" moved)
       (format t "Excluded: ~d~%" excluded)))))

(defun delete-extraneous-files (&key (actually-delete nil))
  ;; identify any zip files in the zipped directory that were not downloaded from the gutenberg site
  ;; only delete them if ACTUALLY-DELETE is true 
  (let* ((path zipped-files-list)
         (zipfiles (make-hash-table :test #'equal)))

    ;; make a table of all valid zip file names
    (with-open-file (stream path :direction :input)
      (loop
       for file = (read-line stream  nil nil)
       while file do
       (setf (gethash (pathname-name file) zipfiles) t)))

    ;; now loop though the zipped files and identify any zip files that are not in the download set
    (loop
     for zipfile in (directory zipped-directory)
     when (and (not (gethash (pathname-name zipfile) zipfiles))
               (string= "zip" (pathname-type zipfile))) do
     (format t "deleting extraneous zip file: ~a~%" zipfile)
     (if actually-delete (delete-file zipfile)))))

(defun survey-zipfiles ()
  ;; collect a list of zip file suffixes with counts
  (let ((path zipped-files-list)
        (types ()))
    (with-open-file (stream path :direction :input :element-type 'character)
      (loop
       for zipfile = (read-line stream nil nil)
       as pos = (position #\- zipfile :from-end t)
       while zipfile
       if pos do
       (let* ((tail (subseq zipfile (1+ pos)))
              (count (assoc tail types :test #'string=)))
         (if count
             (incf (cadr count))
           (push (list tail 1) types)))))
    types))

(defun getFilename (url)
  (if (stringp url)
      (let ((pos (position #\/ url :from-end t)))
        (if pos (format nil "~a~a" zipped-directory (subseq url (1+ pos)))))))

(defun file-size (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun survey (&optional (root *working-directory*) (types '("txt")))
  (let ((file-count 0)
        (total-size 0))
    (fad:walk-directory 
     root
     #'(lambda(path)
         (incf file-count)
         (let ((size (file-size path)))
           (if (zerop size)
               (format t "zero length file: ~s~%" path))
           (incf total-size size)))
     :test #'(lambda(path) (or (null types) (member (pathname-type path) types :test 'string=)))
     :directories :depth-first)
    (format t "Number of files of type/s ~a: ~d~%" types file-count)
    (format t "Total size of all files of type ~s: ~d~%" types total-size)
    (format t "Average file size: ~d~%" (floor total-size file-count))))

(defun survey-project-files (&optional (types '("zip")))
  (survey project-directory types))

(defun crawl (&optional (max-downloads 100))
  (let ((path zipped-files-list)
        (downloads 0)
        (already-downloaded 0)
        (zipfiles (make-zip-files-table)))
    (unless (probe-file path)
      (list-zip-files path))

    (with-open-file (stream path :direction :input :element-type 'character)
      (loop
       for zipfile = (read-line stream nil nil)
       while zipfile do
       (when (included zipfile zipfiles)
         ;;(incf included)
         (let* ((filename (getFilename zipfile))
                (name (pathname-name filename)))
           (format t "Considering ~a~%" name)
           (cond
            ((gethash name zipfiles)
             (format t "Skipping ~a~%" zipfile)
             (incf already-downloaded))
            ((< downloads max-downloads)
             (incf downloads)
             (format t "Downloading ~d of ~d ~a as ~a~%" downloads max-downloads zipfile filename)
             (downloadToFile filename zipfile)
             (setf (gethash (pathname-name filename) zipfiles) t)))))
       finally
       (format t "Included: ~d~%"
               (with-open-file (stream path :direction :input)
                 (loop for file = (read-line stream nil nil)
                       while file
                       count (included file zipfiles))))
       (format t "Already downloaded ~d~%" already-downloaded)
       (format t "Zip files downloaded: ~d~%" downloads)
       (format t "Total zip files downloaded: ~d~%" (+ downloads already-downloaded))
       (format t "Total zip files: ~d~%" (line-count path))))))

