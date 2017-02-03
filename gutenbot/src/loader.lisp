(in-package :cl-user)

(defparameter gutenberg
  (make-instance
   'montezuma:index
   :document-root (merge-pathnames "gutenberg" *corpus-root*)
   :path (merge-pathnames "gutenberg/index" *corpus-root*)
   :index-key "Gutenberg"
   :document-key "key"
   :title "Texts from Project Gutenberg"
   :name "Gutenberg"
   :field-definitions 
   '(("path" :stored t :index :untokenized)
     ("name" :stored t :index :untokenized)
     ("key" :stored t :index :tokenized)
     ("title" :stored t :index :tokenized)
     ("language" :stored t :index :tokenized)
     ("size" :stored t :index :tokenized)
     ("source" :stored t :index :tokenized)
     ("url" :stored t :index :tokenized)
     ("author" :stored t :index :tokenized)
     ("header" :stored t :index :untokenized :height 5 :width 80) ; Project Gutenberg terms and conditions
     ("text" :stored :compress :index :tokenized :height 20 :width 80 :query "justice")
     ("footer" :stored t :index :untokenized :height 5 :width 80) ; Project Gutenberg full license
     )
   :retrieved-fields '("key" "name" "author" "title" "size" "source" "url" "header" "text" "footer")
   :default-number-retrieved 1
   :default-search-field "text"
   :language "english"))

(defun getTextFollowing (text label)
  (let ((pos1 (search label text)))
    (if pos1
        (let* ((pos2 (+ pos1 (length label)))
               (eol (position #\newline text :start pos2)))
          (string-trim '(#\space #\tab #\return #\newline) (subseq text pos2 eol))))))

(defun external-format (path)
  (let ((name (pathname-name path)))
    (cond
     ((string= name "-8" :start1 (- (length name) 2))
      :latin-1)
     ((string= name "-0" :start1 (- (length name) 2))
      :utf-8)
     (t
      :ascii))))

(defun file-contents (path)
  "Returns a string with the entire contents of the specified file."
  (with-open-file (stream
                   path
                   :direction :input
                   :external-format (external-format path)
                   :element-type 'character)
    (mtz::stream-contents stream (cl:file-length stream))))

(defun test-external-format (&optional (path "24763-0.zip"))
  (loop
   with contents = (file-contents (merge-pathnames path project-directory))
   for ch across contents
   when (> (char-code ch) 127) do (format t "~a~%" ch)))

(defconstant division1  "*** START OF THIS PROJECT GUTENBERG EBOOK")
(defconstant division2  "*** END OF THIS PROJECT GUTENBERG EBOOK ")

(defun chop (text)
  ;; chop TEXT into three sections: Gutenberg header, text, and footer so we can get better index performance
  (let* ((end-of-header (search division1 text))
         (end-of-body (if end-of-header (search division2 text :start end-of-header))))
    (if end-of-header
        (setf end-of-header (position #\newline text :start end-of-header)))
    (values (subseq text 0 end-of-header)
            (subseq text end-header end-of-body)
            (subseq text end-of-body))))

(defun add-Gutenberg-text (url path)
  (let* ((index (montezuma:get-corpus "gutenberg"))
         (relative-path (mtz:relative-path project-directory path))
         (text (file-contents path))
         (definitions (montezuma:field-definitions index))
         (key (remove-if-not #'alphanumericp (pathname-name path)))
         (doc (make-instance 'montezuma:document))
         (title (getTextFollowing text "Title: "))
         (author (getTextFollowing text "Author: "))
         (language (getTextFollowing text "Language: "))
         (header)
         (body)
         (footer))
    (multiple-value-setq (header body footer) (chop text))
    (unless title
      (let* ((scanner (cl-ppcre:create-scanner "^.+$"  :multi-line-mode t))
             (metadata-scanner  (cl-ppcre:create-scanner "The Project Gutenberg Etext of ([^,]+) by (.+)$" ))
             (line1 (remove #\* (cl-ppcre:scan-to-strings scanner text))))
        (multiple-value-bind (match substrings) (cl-ppcre:scan-to-strings metadata-scanner line1)
          (when match
            (setf title (aref substrings 0))
            (setf author (aref substrings 1))
            (format t "title ~a~%author ~a~%" title author)))))
         
    (flet ((add-field (name value)
             (let ((field-definition (assoc name definitions :test #'string-equal)))
               (montezuma:add-field doc (montezuma:make-field name value
                                                  :stored (cadr (member :stored field-definition))
                                                  :index (cadr (member :index field-definition)))))))
      (add-field "path" relative-path)
      (add-field "name" (pathname-name path))
      (add-field "key" key)
      (add-field "title" title)
      (add-field "author" author)
      (add-field "language" language)
      (add-field "size" (princ-to-string (length text)))
      (add-field "source" "Project Gutenberg")
      (add-field "url" url)
      (add-field "text" text)
      (montezuma:add-document-to-index index doc :uniqueness t :overwrite t)
      (format t "key: ~a~%title: ~s~%author: ~s~%" key title author))))

(defun index-size ()
  (mtz:size (mtz:get-corpus "gutenberg")))

#|
(defun file-size-probe (pathname)
  (with-open-file (f pathname :element-type '(unsigned-byte 8))
    (file-length f)))
|#

(defun file-size (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun survey (&optional (root cl-user::*working-directory*) (types '("txt")))
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

(defun zip-seek (textname &optional(root cl-user::*working-directory*))
  (let ((file-count 0))
    (fad:walk-directory 
     root
     #'(lambda(path)
         (incf file-count)
         (zip:with-zipfile (zip path)
           (zip:do-zipfile-entries (name entry zip)
             (if (search textname name)
                 (format t "zip file: ~s~%" path)))))
     :test #'(lambda(path) (string-equal (pathname-type path) "zip"))
     :directories :depth-first)))

(defun audit-document (key)
  (let ((index (mtz:get-corpus "gutenberg"))
        (stream t))
    (mtz:search-each
     index
     (mtz:make-key-field-query index key)
     #'(lambda (docnum score)
         (let ((docres (mtz:make-document-result index docnum score)))
           (format stream "(")
           (dolist (field (mtz:fields docres))
             (unless (string-equal (car field) "text")
               (format stream "(~s ~s)~% " (car field) (cadr field))))
           (let ((path (mtz:field-value docres "path")))
              (unless (probe-file (merge-pathnames path project-directory))
               (format stream "^~a" path)))
           (format stream ")~%")))
     `(:num-docs ,(mtz:size index)
       :first-doc 0))))

(defun audit-paths ()
  ;; check document file paths exist
  (let ((index (mtz:get-corpus "gutenberg"))
        (stream t))
    (mtz:search-each
     index
     (make-instance 'mtz:match-all-query)
     #'(lambda (docnum score)
         (let* ((docres (mtz:make-document-result index docnum score))
                (path (mtz:field-value docres "path"))
                (absolute-path (merge-pathnames path project-directory))
                (key (mtz:field-value docres "key")))
           (if (probe-file absolute-path)
               (if (> (file-size absolute-path) 0)
                   (princ "." *standard-output*)
                 (format stream "~%document [~a] has a zero length file: ~a~%" key absolute-path))
             (format stream "~%document [~a] has a non-existent file: ~a~%" key absolute-path))))
     `(:num-docs ,(mtz:size index)
       :first-doc 0))))
 
(defun fetch (key)
  (let ((index (mtz:get-corpus "gutenberg"))
        (stream t))
    (mtz:search-each
     index
     (mtz:make-key-field-query index key)
     #'(lambda (docnum score)
         (let ((docres (mtz:make-document-result index docnum score)))
           (format stream "(")
           (dolist (field (mtz:fields docres))
             (cond
              ((string= (car field) "path")
               (let ((path (cadr field)))
                 (format stream "(~s ~s)~% " (car field) (cadr field))
                 (unless (probe-file (merge-pathnames path project-directory))
                   (format stream ";; file not found~%"))))
              (t
               (format stream "(~s ~s)~% " (car field) (cadr field)))))
           (format stream ")~%")))
     `(:num-docs ,(mtz:size index)
       :first-doc 0))))
    
(defun audit ()
  (let ((index (mtz:get-corpus "gutenberg"))
        (untitled 0)
        (total 0)) 
    (with-open-file (stream (merge-pathnames "metadata.lisp" project-directory) :direction :output :if-exists :supersede)
      (mtz:search-each
       index
       (make-instance 'mtz:match-all-query)
       #'(lambda (docnum score)
           (incf total)
           (let ((docres (mtz:make-document-result index docnum score)))
             (cond
              ((string= "" (or (mtz::field-value docres "title") ""))
               (princ "!" *standard-output*)
               (incf untitled)
               (format stream ";; docnum\" ~d~%" docnum)
               (let ((text (mtz:field-value docres "text")))
                 (if (zerop (length text))
                     ;;(format stream ";; no text?~%")
                     (format stream "~%;; no text ~a~%" (mtz:field-value docres "key"))
                   (with-input-from-string (in text)
                     (loop
                      for line = (read-line in nil nil)
                      as line-count from 0 upto 10
                      unless (or (null line) (zerop (length line))) do
                      (format stream ";; ~a~%" line)))))
               (format stream "(")
               (dolist (field (mtz:fields docres))
                 (unless (string-equal (car field) "text")
                   (format stream "(~s ~s)~% " (car field) (cadr field))))
               (format stream ")~%"))
              (t
               (princ "." *standard-output*)
               ))))
     `(:num-docs ,(mtz:size index)
       :first-doc 0)))
    (format t "~d of ~d~%" untitled total)))

(defun update ()
  (let ((count 0)
        (resolved 0))
    (with-open-file (stream (merge-pathnames "metadata.lisp" project-directory) :direction :input :if-does-not-exist :create)
      (loop
       for metadata = (read stream nil nil)
       while metadata do
       (if (update-Gutenberg-document metadata)
           (incf resolved))
       (incf count)))
    (format t "Resolved ~d of ~d documents~%" resolved count)))

(defun delete-document (index key)
  (let ((query (mtz:make-key-field-query index key)))
    (mtz:query-delete index query)))

(defun delete-document-number (index docnum)
  (mtz:delete-document (mtz:reader index) docnum))

(defun cull ()
  (let ((index (mtz:get-corpus "gutenberg"))
        (stream t))
    (mtz:search-each
     index
     (make-instance 'mtz:match-all-query)
     #'(lambda (docnum score)
         (let* ((docres (mtz:make-document-result index docnum score))
                (key (mtz:field-value docres "key")))
           (when (find-if-not #'alphanumericp key)
             (dolist (field (mtz:fields docres))
               (unless (string-equal (car field) "text")
                 (format stream "(~s ~s)~% " (car field) (cadr field))))
             ;;(delete-document index docnum))
             )))
     `(:num-docs ,(mtz:size index)
       :first-doc 0))))

  ;; Project Gutenberg's Etext of Shorter Prose Pieces by Oscar Wilde
  ;; The Project Gutenberg Etext of
  ;; "The Project Gutenberg Etext of The Road to Oz, by L. Frank Baum"
  ;; "The Project Gutenberg Etext of The Ethics, by Benedict de Spinoza"
  ;; "Project Gutenberg Etext of Contributions to:  All The Year Round "
  ;; "Project Gutenberg Etext Miscellaneous Papers by Charles Dickens"
(defmethod update-Gutenberg-document (values)
  (let* ((scanner (cl-ppcre:create-scanner "^.+$"  :multi-line-mode t))
         (metadata-scanner1  (cl-ppcre:create-scanner "Project Gutenberg Etext of ([^,]+),? by (.+)$" ))
         (metadata-scanner2  (cl-ppcre:create-scanner "Project Gutenberg's Etext of ([^,]+),? by (.+)$" ))
         (metadata-scanner3  (cl-ppcre:create-scanner "Project Gutenberg Etext ([^,]+),? by (.+)$" ))
         (metadata-scanner4  (cl-ppcre:create-scanner "Project Gutenberg Etext of (.+)"))
         (text (mtz:file-contents (merge-pathnames (cadr (assoc  "path" values :test #'equal)) project-directory)))
         (key (cadr (assoc "key" values :test #'equal)))
         (line1 (remove #\* (cl-ppcre:scan-to-strings scanner text)))
         (match)
         (substrings)
         (title (cadr (assoc "title" values :test #'string=)))
         (author (cadr (assoc "author" values :test #'string=))))
    (format t "Line 1 ~s~%" line1)
    (unless (and title (string= title ""))
      (multiple-value-setq (match substrings) (cl-ppcre:scan-to-strings metadata-scanner1 line1))
      (unless match (multiple-value-setq (match substrings) (cl-ppcre:scan-to-strings metadata-scanner2 line1)))
      (unless match (multiple-value-setq (match substrings) (cl-ppcre:scan-to-strings metadata-scanner3 line1)))
      (unless match (multiple-value-setq (match substrings) (cl-ppcre:scan-to-strings metadata-scanner4 line1)))
      (when match
        (setf title (aref substrings 0))
        (setf (cadr (assoc "title" values :test #'equal)) title)
        (when (> (length substrings) 1)
          (setf author (aref substrings 1))
          (setf (cadr (assoc "author" values :test #'equal)) author))))

    (format t "key ~a~%title ~a~%author ~a~%" key title author)
    (let* ((doc (make-instance 'mtz:document))
           (index (montezuma:get-corpus "gutenberg"))
           (definitions (montezuma:field-definitions index)))
      (flet ((add (name value)
               (let ((field-definition (assoc name definitions :test #'string-equal)))
                 (montezuma:add-field doc (montezuma:make-field name value
                                                                :stored (cadr (member :stored field-definition))
                                                                :index (cadr (member :index field-definition)))))))
        (dolist (definition definitions)
          (unless (string= "text" (first definition))
            (add (first definition) (cadr (assoc (first definition) values :test #'equal)))))
        (add "text" text) ;(mtz:file-contents (merge-pathnames (cadr (assoc  "path" values :test #'equal)) project-directory)))
        (montezuma:add-document-to-index index doc :uniqueness t :overwrite t))
      t)))

(defun process-zip-file (filename)
  (zip:with-zipfile (zip filename)
    (zip:do-zipfile-entries (name entry zip)
      (if (and (> (length name) 4) (string-equal name ".txt" :start1 (- (length name) 4)))
          (let ((textname (merge-pathnames name project-directory)))
            (unless (and (probe-file textname)
                         (> (file-size textname) 0))
              (format t "Unzipping ~a~%" (montezuma:relative-path project-directory textname))
              (ensure-directories-exist textname)
              (with-open-file (stream textname :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
                (zip:zipfile-entry-contents entry stream)))
            (format t "Loading ~a into Gutenberg index~%" name)
            (add-Gutenberg-text (mtz:relative-path project-directory filename) textname))))))

(defun reload (&optional start)
  ;; load all the texts in all the zip files downloaded from Project Gutenberg
  (let ((file-count 0)
        (total-size 0))
    (fad:walk-directory 
     zipped-files
     #'(lambda(path)
         (incf file-count)
         (when (or (null start) (>= file-count start))
           (let ((size (file-size path)))
             (cond
              ((zerop size)
               (format t "zero length file: ~s~%" path))
              (t
               (incf total-size size)
               (format t "loading zip ~d ~a~%" file-count path)
               (process-zip-file path))))))
     :test #'(lambda(path) (equal (pathname-type path) "zip"))
     :directories :depth-first)
    (format t "Number of zip files: ~d~%" file-count)
    (format t "Total size of all zip files: ~d~%" total-size)
    (format t "Average file size: ~d~%" (floor total-size file-count))))

(defun zipdir (filename)
  (zip:with-zipfile (zip filename)
    (zip:do-zipfile-entries (name entry zip)
      (format t "~a~%" name))))

