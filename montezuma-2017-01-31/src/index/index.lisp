(in-package #:montezuma)

(defparameter *cache-enabled* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *valid-index-options*
    '(:path
      :create-if-missing-p
      :create-p
      :default-field
      :id-field
      :default-search-field
      :default-number-retrieved
      :analyzer
      :directory
      :close-directory-p
      :occur-default
      :fields
      :default-slop
      :index-key
      :document-key
      :use-compound-file-p
      :handle-parse-errors-p
      :auto-flush-p
      :merge-factor
      :min-merge-docs
      :max-merge-docs
      :info-stream
      :document-root
      :field-definitions
      :retrieved-fields
      :overwrite
      :uniqueness
      :title
      :url
      :name
      :language
      :document-title)))

(defun index-options-list-p (list)
  (do ((options list (cddr options)))
      ((endp options) T)
    (when (not (member (car options) *valid-index-options*))
      (error "~s is not a valid option for creating an instance of INDEX" (car options))
      (return-from index-options-list-p NIL))))

(deftype index-option () `(member ,@*valid-index-options*))
(deftype index-options-list () '(satisfies index-options-list-p))

(defun get-index-option (options option &optional default)
  (check-type option index-option)
  (getf options option default))

(define-setf-expander get-index-option (place option &environment env)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (declare (ignore writer-form))
    (let ((goption (gensym "OPTION"))
	  (gstore (if store-vars (car store-vars) (gensym "STORE"))))
      (values
       (list* goption vars)
       (list* option vals)
       (list gstore)
       `(progn
	  (check-type ,goption index-option)
	  (setf (getf ,reader-form ,goption) ,(car store-vars)))
       `(getf ,goption ,reader-form)))))

(defclass index ()
  ((index-key :initform "" :accessor index-key :documentation "identity of index")
   (document-key :initform nil :accessor document-key :documentation "a field name that uniquely identifies documents")
   (dir :initform nil :accessor dir :documentation "index directory. Not necessarily document root.")
   (has-writes-p :initform NIL)
   (reader :initform nil)
   (writer)
   (close-dir-p)
   (auto-flush-p)
   (default-search-field :initform "*" :accessor default-search-field)
   (document-title :initform nil :initarg :document-title :accessor document-title)
   (default-field)
   (analyzer :accessor analyzer)
   (searcher :initform nil)
   (open-p :initform T)
   (options)
   (qp :initform nil)
   (document-root :initform nil :accessor document-root :documentation "used if needed to resolve a document source path")
   (default-number-retrieved :initform 10 :accessor default-number-retrieved :initarg :default-number-retrieved
                             :documentation "the default upper limit of documents retrieved from the index")
   (retrieved-fields :initform () :accessor retrieved-fields :documentation "a subset of fields that are retrieved by default, or nil")
   (title :initform "" :accessor title :documentation "verbose name of index")
   (url :initform "" :accessor url :documentation "web address where documents can be obtained")
   (name :initform "":accessor index-name :documentation "text suitable for use as the index name")
   (language :initform :english :accessor language :documentation "an indicator of the document language")
   (field-definitions :initform nil :accessor field-definitions :documentation "association list of field definitions")
   (metadata-cache :initform nil :accessor metadata-cache)
   (field-values-cache :initform (trivial-garbage:make-weak-hash-table :test #'equal :weakness :value) :accessor field-values-cache)
   (overwrite :initform nil :initarg :overwrite :accessor overwrite :documentation "when adding a document, delete any existing documents with the same document key valuue")
   (uniqueness :initform t :initarg :overwrite :accessor uniqueness :documentation "only one document is allowed with same document key valuue")))

(defmethod index-directory ((self ram-directory))
  "RAM")

(defmethod index-directory ((self fs-directory))
  (path self))

(defmethod print-object ((self index) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (format stream "~%index-key: ~S:~%" (index-key self))
    (format stream "dir: ~S~%" (if (slot-boundp self 'dir) (index-directory (dir self))));(if (and (dir self) (path (dir self))) (princ-to-string (path (dir self)))))
    (format stream "documentroot: ~s~%" (document-root self))
    ;;(format stream "abridgement-threshold: ~s~%" (abridgement-threshold self))
    (format stream "field-definitions:~%")            
    (dolist (fd (field-definitions self))
      (format stream "  ~{~s ~}~%" fd))
    (format stream "document-key: ~S" (document-key self))
    (format stream "~%default-search-field: ~s" (default-search-field self))
    (format stream "~%analyzer: ~s" (type-of (analyzer self)))
    (format stream "~%Name: ~s" (index-name self))
    (format stream "~%title: ~s" (title self))
    (format stream "~%url: ~s~%" (url self))))

(defvar *montezuma-indexes* ())

(defun get-index (key)
  (or (find key *montezuma-indexes* :key #'index-key :test #'string-equal)
      (find key *montezuma-indexes* :key #'index-name :test #'string-equal)
      (first *montezuma-indexes*)))

(defun get-corpus (key)
  (get-index key))

(defmethod initialize-instance :after ((self index) &rest args &key &allow-other-keys)
  (with-slots (options) self
    (check-type args index-options-list)
    (setf options (copy-list args))
    (setf (get-index-option options :default-field)
	  (if (get-index-option options :default-field)
	      (string (get-index-option options :default-field))
	      ""))
    (setf (get-index-option options :default-search-field)
	  (or (get-index-option options :default-search-field)
	      (get-index-option options :default-field)
	      "*"))
    (setf (get-index-option options :create-if-missing-p)
	  (get-index-option options :create-if-missing-p T))
    ;; FIXME: I don't flatten the :key option, I'm not sure why Ferret does.
    (with-slots (index-key document-key dir options close-dir-p auto-flush-p analyzer writer
                           default-search-field default-field document-root
                     field-definitions ;; abridgement-threshold
                     retrieved-fields title url name language) self
      (setf index-key (get-index-option options :index-key))
      (setf document-key (get-index-option options :document-key))

      (cond ((get-index-option options :path)
	     (handler-case
		 (setf dir (make-fs-directory (get-index-option options :path)
					      :create-p (get-index-option options :create-p)))
	       (error () (setf dir (make-fs-directory (get-index-option options :path)
						      :create-p (get-index-option options :create-if-missing-p)))))
	     (setf (get-index-option options :close-directory-p) T))
	    ((get-index-option options :directory)
	     (setf dir (get-index-option options :directory)))
	    (T
	     (setf (get-index-option options :create-p) T)
	     (setf dir (make-instance 'ram-directory))))

      ;; Create the index if need be
      (setf writer (apply #'make-instance 'index-writer
			  :directory dir
			  options))
      (setf (get-index-option options :analyzer) (setf analyzer (analyzer writer)))
      (close-down writer)
      (setf writer nil)
      ;; Only want to create the first time, if at all.
      (setf (get-index-option options :create-p) NIL)
      (setf close-dir-p (get-index-option options :close-directory-p))
      (setf (get-index-option options :close-directory-p) NIL)
      (setf auto-flush-p (get-index-option options :auto-flush-p))
      (setf default-search-field (or (get-index-option options :default-search-field)
				     (get-index-option options :default-field)
				     "*"))
      (setf default-field (or (get-index-option options :default-field) ""))
      (setf document-root (get-index-option options :document-root))
      (setf field-definitions (get-index-option options :field-definitions))
      (dolist (fd field-definitions)
        (let ((form (definition-aspect fd :form))
              (regexp (definition-aspect fd :regexp))
              (scanner (definition-aspect fd :scanner))
              (type (intern (string (definition-aspect fd :type))))
              (registers (definition-aspect fd :registers)))
;;          (break "field ~s" fd)
          (case type ;(field-defined-type self (first fd))
            (int
             ;; create a regular expression that matches a field possibly with leading white space
             (unless regexp
               (setf regexp "^\\D*(\\d+)")
               (rplacd (last fd) (list :regexp regexp)))
             (unless scanner
               (setf scanner (cl-ppcre:create-scanner regexp))
               (rplacd (last fd) (list :scanner scanner))))

            (date
             (unless form
               (setf form +iso-8601-date-format+) ;; default date format "yyyy-mm-dd"
               (setf (cdr fd) `(,@(cdr fd) :form ,form)))
             (unless regexp
               (setf regexp (regular-expression form))
               (rplacd (last fd) (list :regexp regexp)))
             (unless (and scanner registers)
               (let ((cl-ppcre:*allow-named-registers* t))
                 (multiple-value-bind (scanner registers) (cl-ppcre:create-scanner regexp)
                   (when (or (null registers) (zerop (length registers)))
                     (error "Date fields expected in regular expression: ~s" regexp))
                   ;;(setf (cdr fd) `(,@(cdr fd) :scanner ,scanner :registers ,registers)))))))))
                   (rplacd (last fd) (list :scanner scanner :registers registers)))))))))
      ;;(setf abridgement-threshold (get-index-option options :abridgement-threshold))
      (setf retrieved-fields (or (get-index-option options :retrieved-fields) (mapcar #'car field-definitions)))
      (setf title (get-index-option options :title))
      (setf url (get-index-option options :url))
      (setf name (get-index-option options :name))
      (setf language (get-index-option options :language))

      (when (not (get-index-option options :handle-parse-errors-p))
	(setf (get-index-option options :handle-parse-errors-p) T)))

    (with-slots (index-key) self
      (when index-key
        (delete (find index-key *montezuma-indexes* :key #'index-key :test #'string-equal) *montezuma-indexes*)
        (setf *montezuma-indexes* `(,@*montezuma-indexes* ,self))))))

(defmethod close-down ((self index))
  (with-slots (open-p reader writer dir) self
    (when (not open-p)
      (error "Tried to close an already closed directory."))
    (when reader (close-down reader))
    (when writer (close-down writer))
    (close-down dir)
    (setf open-p NIL)))

(defgeneric reopen (index))

(defmethod reopen ((self index))
  (apply #'make-instance 'index (slot-value self 'reader)))

(defgeneric reader (index))

(defmethod reader ((self index))
  (ensure-reader-open self)
  (slot-value self 'reader))
              
(defgeneric field-definition (index name))

(defmethod field-is-defined ((self index) name)
  (assoc name (field-definitions self) :test #'string-equal))

(defmethod field-definition ((self index) name)
  (cdr (assoc name (field-definitions self) :test #'string-equal)))

(defgeneric field-definition-aspect (index name aspect))
              
(defmethod field-definition-aspect ((self index) name aspect)
  (cadr (member aspect (field-definition self name))))

(defmethod field-height ((self index) name)
  (or (field-definition-aspect self name :height)
      1))

(defmethod field-width ((self index) name)
  (or (field-definition-aspect self name :width)
      80))

(defmethod definition-aspect ((self list) aspect)
  (cadr (member aspect self)))

(defgeneric field-defined-stored (index name))

(defmethod field-defined-stored ((self index) name)
  (field-definition-aspect self name :stored))

(defgeneric field-abridged (index name))
              
(defmethod field-abridged ((self index) name)
  (field-definition-aspect self name :abridged))

#|
(defmethod field-excluded-from-metadata ((self index) name)
  (field-definition-aspect self name :excluded-from-metadata))
|#

(defgeneric field-path-field (index name))
              
(defmethod field-path-field ((self index) name)
  (field-definition-aspect self name :path))

(defgeneric field-path (index doc name))

(defmethod field-path ((self index) doc name)
  "Derive the path to the field data from the index root and the field path value."
  (let* ((altname (field-path-field self name))
         (path (if altname (document-value doc altname))))
    (if path (fad:merge-pathnames-as-file (document-root self) path))))

(defgeneric field-defined-type (index name))
              
(defmethod field-defined-type ((self index) name)
  (let ((type (field-definition-aspect self name :type)))
    (if type
        (intern (string type))
      t)))

(defgeneric field-defined-form (index name))
              
(defmethod field-defined-form ((self index) name)
  (field-definition-aspect self name :form))

(defgeneric field-defined-tokenized (index name))

(defmethod field-defined-tokenized ((self index) name)
  (eq :tokenized (field-definition-aspect self name :index)))

;(defmethod field-defined-indexed ((self index) name)
;  (field-definition-aspect self name :indexed)) ; all fields are indexed

(defgeneric field-defined-query (index name))

(defmethod field-defined-query ((self index) name)
  (field-definition-aspect self name :query))

(defmethod field-defined-preformatted ((self index) name)
  (field-definition-aspect self name :preformatted))

(defmethod field-defined-blockquote ((self index) name)
  (field-definition-aspect self name :blockquote))

(defgeneric fields-defined (index))

(defmethod fields-defined ((self index))
  (mapcar #'car (field-definitions self)))

(defgeneric searcher (index))

(defmethod searcher ((self index))
  (ensure-searcher-open self)
  (slot-value self 'searcher))

(defgeneric writer (index))

(defmethod writer ((self index))
  (ensure-writer-open self)
  (slot-value self 'writer))

(defmethod exists-p ((self index) query)
  (search-each (searcher self) query
               #'(lambda (doc score)
                   (declare (ignore score))
                   (return-from exists-p doc)))
  nil)

(defgeneric add-document-to-index (index doc &key overwrite analyzer))

(defmethod add-document-to-index ((self index) doc &key (overwrite t) (uniqueness nil) analyzer)
  "Add DOC to index. If overwrite, delete any documents with the same key value. 
Unless uniqueness, allow duplicate keys. When uniqueness but not overwrite, don't add DOC."
  (let ((fdoc nil)
	(default-field (slot-value self 'default-field)))
    (when (listp doc)
      ;; Turn association lists into something we can treat like any
      ;; other table (including hash tables).
      (setf doc (convert-alist-to-table doc)))
    (cond ((stringp doc)
	   (setf fdoc (make-instance 'document))
	   (add-field fdoc (make-field default-field doc
				       :stored T :index :tokenized)))
	  ((typep doc 'array)
	   (setf fdoc (make-instance 'document))
	   (dosequence (field doc)
	     (add-field fdoc (make-field default-field field
					 :stored T :index :tokenized))))
	  ((table-like-p doc)
	   (setf fdoc (make-instance 'document))
	   (dolist (field (table-keys doc))
	     (let ((text (table-value doc field)))
	       (add-field fdoc (make-field (string field) (stringify text)
					  :stored T :index :tokenized)))))
	  ((typep doc 'document)
	   (setf fdoc doc))
	  (T
	   (error "Unknown document type ~S" doc)))

    ;; Delete existing documents with the same key, if overwrite.
    (when uniqueness
      (let ((key (slot-value self 'document-key)))
        (when key
          (let* ((key-value (get-document-field-data fdoc key))
                 (query (make-key-field-query self key-value)))
            (if overwrite
                (progn
                  (delete-from-cache self key-value)
                  (query-delete self query))
              (if (exists-p self query)
                  (return-from add-document-to-index nil)))))))

    (let ((writer (writer self)))
      (setf (slot-value self 'has-writes-p) T)
      (add-document-to-index-writer (slot-value self 'writer) fdoc
				    (if analyzer analyzer (analyzer writer)))
      (let* ((key (slot-value self 'document-key))
             (keyvalue (if key (get-document-field-data fdoc key)))
             (table (metadata self)))
        (when (and table keyvalue)
          (loop
           for (docnum) in (search-for-keyvalue self keyvalue) do
           (add-to-cache self table docnum))))

      (when (slot-value self 'auto-flush-p)
	(flush self)))
    t))

(defmethod search-document-and-scores ((index index) query)
  (let ((results ()))
    (search-each
     (searcher index) query
     #'(lambda(docnum score) (push (list docnum score) results)))
    results))

(defmethod search-for-keyvalue ((index index) (keyvalue string))
  (search-document-and-scores index (make-key-field-query index keyvalue)))

;; The main search method for the index. You need to create a query to
;; pass to this method. You can also pass a hash with one or more of
;; the following; {filter, num_docs, first_doc, sort}
;;
;; query::      The query to run on the index
;; filter::     Filters docs from the search result
;; first_doc::  The index in the results of the first doc retrieved.
;;              Default is 0
;; num_docs::   The number of results returned. Default is 10
;; sort::       An array of SortFields describing how to sort the results.

(defgeneric search-index (index query &rest options))

(defmethod search-index ((self index) query &rest options)
  (do-search self query options))

(defgeneric search-each (index query fn &optional options))

(defmethod search-each ((self index) query fn &optional options)
  (let ((hits (do-search self query options)))
    (dosequence (score-doc (score-docs hits))
      (funcall fn (doc score-doc) (score score-doc)))
    (if hits (total-hits hits))))

(defgeneric search-each-if (index query fn &optional options))

(defmethod search-each-if ((self index) query fn &optional options)
  (let ((hits (do-search self query options)))
    (dosequence (score-doc (score-docs hits))
      (if (not (funcall fn (doc score-doc) (score score-doc)))
          (return)))
    (total-hits hits)))

(defmethod get-document ((self index) id)
  (let ((reader (reader self)))
    (typecase id
      (string
       (get-document-with-term reader (make-term "id" id)))
      (term
       (get-document-with-term reader id))
      (T
       (get-document reader id)))))

(defun make-field-value-query (field value)
  (let ((query (make-instance 'boolean-query)))
    (add-query query
               (make-instance 'term-query :term (make-term field (string-downcase value)))
               :must-occur)
    query))

(defmethod add-field-query ((index index) (field string) (value string) (query string))
  (let ((result (compiled-query index query)))
    (add-term-query (query result) (make-term field value) 1.0 :must-occur)
    result))

(defmethod make-key-field-query ((index index) keyvalue)
  (make-field-value-query (document-key index) keyvalue))

#|
(defmethod delete-document ((self index) id)
  (let ((reader (reader self)))
    (let ((count (cond ((stringp id)
                        (let ((term (make-term "id" id)))
                          (delete-docs-with-term reader term)))
		       ((typep id 'term)
			(delete-docs-with-term reader id))
		       ((integerp id)
			(delete-document reader id))
		       (T
			(error "Can't delete for id ~S" id)))))
      (when (slot-value self 'auto-flush-p)
	(flush self))
      count)))
|#

(defmethod delete-document ((self index) id)
  (let ((reader (reader self)))
    (let ((count (typecase id
                   (string
                    (let ((term (make-term "id" id)))
                      (delete-from-cache self term)
                      (delete-docs-with-term reader term)))
                   (term
                    (delete-from-cache self id)
                    (delete-docs-with-term reader id))
                   (integer
                    (delete-from-cache self id)
                    (delete-document reader id))
                   (T
                    (error "Can't delete for id ~S" id)))))
      (when (slot-value self 'auto-flush-p)
	(flush self))
      count)))

(defgeneric query-delete (index query))

(defmethod query-delete ((self index) query)
  (let ((reader (reader self))
	(searcher (searcher self))
        (query (process-query self query)))
    (search-each searcher query
		 #'(lambda (doc score)
		     (declare (ignore score))
		     (delete-document reader doc)))
    (when (slot-value self 'auto-flush-p)
      (flush self))))

(defmethod deleted-p ((self index) n)
  (deleted-p (reader self) n))

(defgeneric update (index id new-val))

(defmethod update ((self index) id new-val)
  (with-slots (options) self
    (cond ((stringp id)
	   ;; FIXME: how about using a pre-parsed form of query?
	   (query-update self (format nil "id:~A" id) new-val))
	  ((typep id 'term)
	   (query-update self
			 (make-instance 'term-query
					:term id)
			 new-val))
	  ((integerp id)
	   (let ((reader (reader self))
		 (document (get-document self id)))
	     (when (listp new-val)
	       (setf new-val (convert-alist-to-table new-val)))
	     (cond ((table-like-p new-val)
		    (dolist (name (table-keys new-val))
		      (let ((content (table-value new-val name)))
			(setf (document-values document name) (string content)))))
		   ((typep new-val 'document)
		    (setf document new-val))
		   (T
		    (setf (document-values document (get-index-option options :default-field))
			  (string new-val))))
	     (delete-document reader id)
	     (let ((writer (writer self)))
	       (add-document-to-index-writer writer document))))
	  (T
	   (error "Cannot update for id ~S" id)))
    (when (slot-value self 'auto-flush-p)
      (flush self))))

(defgeneric query-update (index query new-val))

(defmethod query-update ((self index) query new-val)
  (let ((searcher (searcher self))
	(reader (reader self))
	(docs-to-add '())
	;;(query (parsed-query (process-query self query)))) ; don't need sort
	(query (process-query self query))) ; don't need sort
    (search-each searcher query
		 #'(lambda (id score)
		     (declare (ignore score))
		     (let ((document (get-document self id)))
		       (when (listp new-val)
			 (setf new-val (convert-alist-to-table new-val)))
		       (cond ((table-like-p new-val)
			      (dolist (name (table-keys new-val))
				(let ((content (table-value new-val name)))
				  (setf (document-values document name) (string content)))))
			     ((typep new-val 'document)
			      (setf document new-val))
			     (T
			      (setf (document-values document (get-index-option (slot-value self 'options) :default-field))
				    (string new-val))))
		       (push document docs-to-add)
		       (delete-document reader id))))
    (let ((writer (writer self)))
      (dolist (doc (reverse docs-to-add))
	(add-document-to-index-writer writer doc))
      (when (slot-value self 'auto-flush-p)
	(flush self)))))

(defmethod has-deletions-p ((self index))
  (has-deletions-p (reader self)))

(defgeneric has-writes (index))

(defmethod has-writes ((self index))
  (slot-value self 'has-writes))

(defmethod flush ((self index))
  (with-slots (reader writer searcher) self
    (when reader (close-down reader))
    (when writer (close-down writer))
    (setf reader nil
	  writer nil
	  searcher nil)))

(defmethod optimize-index ((self index))
  (optimize-index (writer self))
  (flush self))

(defmethod size ((self index))
  (num-docs (reader self)))

(defmethod add-indexes ((self index) &rest indexes)
  (when (> (length indexes) 0)
    (when (typep (elt indexes 0) 'index)
      (setf indexes (map 'vector #'reader indexes)))
    (cond ((typep (elt indexes 0) 'index-reader)
	   (let ((reader (reader self)))
	     (setf indexes (remove reader indexes)))
	   (add-indexes-readers (writer self) indexes))
	  ((typep (elt indexes 0) 'virtual-directory)
	   (setf indexes (remove (slot-value self 'dir) indexes))
	   (apply #'add-indexes (writer self) indexes))
	  (T
	   (error "Unknown index type ~S when trying to merge indexes." (elt indexes 0))))))

(defgeneric persist (index directory &key create-p))

(defmethod persist ((self index) directory &key (create-p T))
  (flush self)
  (with-slots (dir options) self
    (let ((old-dir dir))
      (etypecase directory
	((or string pathname)
	 (setf dir (make-fs-directory directory :create-p create-p))
	 (setf (get-index-option options :close-directory-p) T))
	(virtual-directory
	 (setf dir directory)))
      (ensure-writer-open self)
      (add-indexes (writer self) old-dir))))

(defgeneric ensure-writer-open (index))

(defmethod ensure-writer-open ((self index))
  (with-slots (open-p writer reader dir options searcher) self
    (unless open-p
      (error "Tried to use a closed index."))
    (unless writer
      (when reader
	(close-down reader)
	(setf reader nil
	      searcher nil))
      (setf writer (apply #'make-instance 'index-writer
				  :directory dir
				  options)))))

(defgeneric ensure-reader-open (index))

(defmethod ensure-reader-open ((self index))
  (with-slots (open-p writer reader dir) self
    (unless open-p
      (error "Tried to use a closed index."))
    (if reader
	(if (not (latest-p reader))
	  (setf reader (open-index-reader dir :close-directory-p NIL))
	  NIL)
	(progn
	  (when writer
	    (close-down writer)
	    (setf writer nil))
	  (setf reader (open-index-reader dir :close-directory-p NIL))))))

(defgeneric index-path (index))

(defmethod index-path ((self index))
  (and (slot-boundp self 'dir)
       (slot-value self 'dir)
       (slot-value (slot-value self 'dir) 'path)))
 
(defun index-write-date (index)
  (let ((path (index-path index)))
    (if path 
        (file-write-date (merge-pathnames path "segments")))))

(defmethod index-keys-metadata-path ((index index))
  (let ((path (index-path index)))
    (if path
        (merge-pathnames "metadata-keys.dat" path))))

(defun metadata-up-to-date (index)
  (let* ((metadata-path (index-keys-metadata-path index))
         (timestamp (and metadata-path (file-write-date metadata-path))))
    (and metadata-path
         timestamp
         (> (size index) 0)
         (if (probe-file metadata-path)
             (latest-p (reader index))
             (<= (index-write-date index) timestamp)))))

(defmethod is-ram-directory ((dir ram-directory))
  t)

(defmethod is-ram-directory ((dir t))
  nil)

(defmethod is-ram-directory ((index index))
  (is-ram-directory (slot-value index 'dir)))

(defmethod metadata-table ((index index))
  (unless (is-ram-directory index)
    (let ((metadata (metadata index)))
      (unless metadata
        (with-slots (metadata-cache) index
          (let ((maxdocs (max-doc (searcher index))))
            (setf metadata (make-hash-table :test #'equal))
            (format t "~%;; Building metadata cache" *standard-output*)
            (loop
             for docnum from 0 upto (1- maxdocs)
             unless (deleted-p index docnum) do
             ;;(format t "metadata-table docnum ~d~%" docnum)
             (add-to-cache index metadata docnum))
            (setf metadata-cache (trivial-garbage:make-weak-pointer metadata)))))
      (values metadata (hash-table-count metadata)))))

(defun metadata-vector (index)
  (unless (is-ram-directory index)
    (let ((table (metadata-table index))
          (records (make-array (size index) :element-type 'list :fill-pointer 0 :adjustable t)))
      (loop for value being the hash-value of table do
            (vector-push-extend value records))
      (sort records #'string-lessp :key #'cadr)))) ; sort by key value

(defmethod ensure-searcher-open ((self index))
  (with-slots (open-p searcher reader) self
    (unless open-p
      (error "Tried to use a closed index."))
    (when (or (ensure-reader-open self) (not searcher))
      (setf searcher (make-instance 'index-searcher :reader reader)))))

(defgeneric do-search (index query options))

(defmethod do-search ((self index) query options)
  (let ((pqr (process-query self query)))
    (apply #'search-index (searcher self) pqr options))) ; this method is in the search/index-searcher

(defgeneric fetch-field-value (index doc name))

(defmethod fetch-field-value ((self index) doc (name string))
  "one of three possibilities: use the stored field value, or read a file using either relative or absolute path"
  (if (integerp doc)
      (setf doc (get-document self doc)))
  (let ((field (document-field doc name)))
    (if (and field (field-stored-p field))
        (field-data field)
      (let ((path (field-path self doc name)))
        (if path (file-contents path) "")))))

(defgeneric field-values (index doc))

(defmethod field-values ((self index) doc)
  "Return an association list of field names and data."
  (if (integerp doc)
      (setf doc (get-document self doc)))
  (loop for name in (fields-defined self)
        collect (list name (fetch-field-value self doc name))))

(defun list-index-documents (index &optional types)
  (list-files (document-root index) types))

(defun stringify (x)
  (etypecase x
    (string x)
    ((or character symbol) (string x))
    (integer (format nil "~A" x))))

(defmethod get-absolute-path ((self index) docnum &optional (field-name "path"))
  (let* ((doc (get-document self docnum))
         (path (document-value doc field-name)))
    (merge-pathnames path (document-root self))))

(defmethod make-parser-with-index ((index index))
  (with-slots (default-search-field language) index
    (make-instance 'query-parser
                   :analyzer (analyzer index)
                   :allow-other-keys T
                   :language language
                   :default-field default-search-field
                   :index index
                   :field-definitions (field-definitions index))))

(defgeneric process-query (index query))

(defmethod process-query ((self index) query)
  (if (stringp query)
      (with-slots (qp) self
        (setf qp (make-parser-with-index self))
        (montezuma-parse qp query))
    query))

(defun compiled-query (index input)
  (montezuma-parse (make-parser-with-index index) input))

(defmethod rewrite-query ((index index) query)
  (rewrite (query (compiled-query index query)) (reader index)))

(defmethod normalised-field-name ((index index) name)
  (find name (fields-defined index) :test #'string-equal))

(defmethod wildcard-query ((index index) field wildcard)
  (let ((query (make-instance 'boolean-query)))
    (add-query query
               (if (string= wildcard "*")
                   (make-instance 'match-all-query)
                 (make-instance 'wildcard-query :term (make-term (normalised-field-name index field) wildcard)))
               :must-occur)
    query))

(defun get-document-field (index docnum field)
  (handler-case
      (unless (deleted-p index docnum)
        (let ((doc (get-document index docnum)))
          (if doc (field-data (document-field doc field)))))
    (t nil)))

(defmethod all-field-values ((index index) field)
  (when (and field (setf field (normalised-field-name index field)))
    (or (gethash field (field-values-cache index))
        (let* ((maxdocs (max-doc (searcher index)))
               (results
                (loop
                 for docnum from 0 upto (1- maxdocs)
                 unless (deleted-p index docnum)
                 collect (field-data (document-field (get-document index docnum) field)))))
          (setf results (sort results #'string-lessp))
          (setf (gethash field (field-values-cache index)) results)
          results))))
#|
(defclass document-metadata ()
  ((docnum :type integer :initarg :docnum :accessor docnum)
   (key :type string :initarg :key :accessor key)
   (title :type string :initarg :title :accessor title :initform "")
   (author :type string :initarg :author :accessor author :initform "")
   (language :type string :initarg :language :accessor language :initform "")
   ))
|#

#|
(defmethod print-object ((self document-metadata) stream)
  (print-unreadable-object (self stream :type T :identity T)
    (format stream " docnum: ~d" (docnum self))
    (format stream " key: ~s" (key self))
    (format stream " title: ~s" (title self))
    (format stream " author: ~s" (author self))
    (format stream " language: ~s" (language self))))
|#

#|
(defmethod tip ((record document-metadata))
  (with-slots (title author language) record
    (with-output-to-string (stream)
      (if title (princ title stream))
      (when author (format stream " by ~a" author))
      (when language (format stream " [~a]" language)))))
|#

#|
(defun make-document-metadata (docnum key title author &optional language) 
  (make-instance 'document-metadata :docnum docnum :key key :title title :author author :language language))
|#
(defmethod cached ((index t) (field string))
  (and *cache-enabled*
       index
       (not (is-ram-directory index))
       (string-equal (document-key index) field)))

(defmethod delete-from-cache ((index index) (term string))
  (let ((table (metadata-table index)))
    (if table
        (remhash term table))))

(defmethod delete-from-cache ((index index) (docnum integer))
  (let ((table (metadata-table index)))
    (if table
        (let* ((doc (get-document index docnum))
               (field (document-key index))
               (keyvalue (if field (field-data (document-field doc field)))))
          (if keyvalue
              (remhash (string-downcase keyvalue) table))))))

(defmethod delete-from-cache ((index index) (term term))
  (let ((table (metadata-table index)))
    (if table
        (let ((query (make-instance 'term-query :term term :index index)))
          (dolist (docnum (get-document-numbers index query))
            (delete-from-cache index docnum))))))

(defmethod add-to-cache ((index index) table docnum)
  (let* ((doc (get-document index docnum))
         (field-names (all-field-names doc)))
    (flet ((extract (name)
             (if (find name field-names :test #'string-equal)
                 (field-data (document-field doc name)))))
      (let* ((key (extract (document-key index)))
             (entry-key (string-downcase key)))
        (setf (gethash entry-key table)
              `(,@(gethash entry-key table)
                ,(list docnum key (extract "title") (extract "author") (extract "language"))))))))

(defmethod metadata ((index t))
  (break "index is nil")
  nil)

(defmethod metadata ((index index))
  "Are we caching metadata?"
  (if *cache-enabled*
      (let ((metadata (metadata-cache index)))
        (and metadata
             (trivial-garbage:weak-pointer-p metadata)
             (trivial-garbage:weak-pointer-value metadata)))))
         
(defmethod key-term-frequency ((index index) (text string))  
  (let ((cache (metadata index)))
    (if cache (length (gethash (string-downcase text) cache)))))

(defmethod key-term-documents ((index index) (text string))
  (let ((cache (metadata index)))
    (if cache (mapcar #'first (gethash (string-downcase text) cache)))))

(defun get-document-number (index query)
  (search-each 
   index query
   #'(lambda (docnum score)
       (declare (ignore score))
       (return-from get-document-number docnum)))
  -1)

(defun get-document-numbers (index query)
  (let ((docs ()))
    (search-each 
     index query
     #'(lambda (docnum score)
         (declare (ignore score))
         (push docnum docs)))
    docs))
  
(defun get-document-number-from-query (index keyvalue)
  (let ((query (make-field-value-query (document-key index) (or keyvalue ""))))
    (search-each 
     index query
     #'(lambda (docnum score)
         (declare (ignore score))
         (return-from get-document-number-from-query docnum)))
    -1))

#|
(defparameter pastes
  (make-instance
   'index
   :path (merge-pathnames "contrib/pastes-1000/pasteindex" cl-user::*montezuma-root*)
   :document-root (merge-pathnames "contrib/pastes-1000/pastes.sexp" cl-user::*montezuma-root*)
   :title "1000 documents submitted to lisppastes"
   :document-key "number"
   :create-p NIL
   :create-if-missing-p NIL
   :index-key "PASTES"
   :name "Lisp Pastes"
   ;; Unless otherwise specified, queries will search all fields simultaneously.
   :default-field "*"
   :fields '("date" "displaydate" "number" "user" "channel" "title" "contents")
   :default-number-retrieved 1
   :field-definitions
   '(("date" :stored nil :index :untokenized)
     ("displaydate" :stored t :index :untokenized :type :date
      :form ((:year 4)-(:month 2)-(:day 2)" "(:hour 2)":"(:min 2)":"(:sec 2)))
     ("number" :stored t :index :untokenized :type :integer)
     ("user" :stored t :index :tokenized)
     ("channel" :stored t :index :tokenized)
     ("title" :stored t :index :tokenized)
     ("contents" :stored t :index :tokenized :height 20 :width 80 :blockquote t :linebreaks t :preformatted t))))
|#

(defun stress (index &key cache)
  (let ((keys ())
        (key (slot-value index 'document-key))
        (table (metadata-table index)))
    (dotimes (n 1000)
      (let* ((docnum (random (1- (size index))))
             (doc (get-document index docnum)))
        (push (get-document-field-data doc key) keys)))
    (setf *cache-enabled* cache)
    (time
     (dolist (keyvalue keys)
       ;;(format t "~s~%" (search-for-keyvalue index keyvalue))))))
       (exists-p index (make-instance 'key-term-query :term (make-term key keyvalue) :index index))))
    table))
 