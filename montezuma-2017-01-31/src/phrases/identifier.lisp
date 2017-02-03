(in-package #:montezuma)

(defclass phrases ()
  ((terms :initarg :terms :initform (make-array 0 :adjustable t :fill-pointer 0) :accessor terms)
   (table :initform (make-hash-table :test #'equal) :accessor table)
   (text :initform "" :initarg :text :accessor text)
   (language :accessor language :initarg :language)
   (analyzer :accessor analyzer :initarg :analyzer)
   (sequences :initform (make-hash-table :test #'equal) :accessor sequences)
   (phrase-list :initform () :accessor phrase-list)
   (stage :initform "Initializing" :accessor stage)
   (progress :initform 0 :accessor progress)
   (progress-function :initarg :progress-function :initform nil :accessor progress-function)
   (progress-stage-function :initarg :progress-stage-function :initform nil :accessor progress-stage-function)))

(defstruct (document-token
            (:constructor make-document-token (token symbol position))
            (:print-object print-document-token))
  token
  symbol
  position
  already-considered
  redundant
#|
  image
  start
  end
  (increment 1)
  (type :word)
  |#
  )

(defun show-operation (phrases text)
  (with-slots (progress-stage-function) phrases
    (if progress-stage-function
        (funcall progress-stage-function text)
      (format t "~%~a~%" text))))

(defun show-progress (phrases new-progress)
  (with-slots (progress progress-function) phrases
    (when (/= new-progress progress)
      (if progress-function
          (funcall progress-function new-progress)
        (princ #\.)))
    (setf progress new-progress)))

(defun print-document-token (object stream)
  (format stream "#<DOCUMENT-TOKEN ~a ~s POSITION:~d CONSIDERED: ~s>" 
          (key object)
          (type-of-token object)
          (document-token-position object)
          (document-token-already-considered object)))

(defmethod text ((token document-token))
  (token-image (document-token-token token)))

(defmethod type-of-token ((token document-token))
  (token-type (document-token-token token)))

(defmethod key ((token document-token))
  (document-token-symbol token))

(defmethod token-usage ((phrases phrases) doctok)
  (gethash (key doctok) (table phrases)))

(defmethod investigate ((phrases phrases) (use document-token) tokens)
  ;; is the sequence of document-tokens starting at USE a match with TOKENS?
  (with-slots (position docnum) use
    (loop
     for token in tokens
     for i from position
     as next-token = (aref (terms phrases) i) do
     (unless (and next-token
                  (eq :word (type-of-token next-token)) ; trial this condition
                  (eq (key token) (key next-token)))
       (return-from investigate nil)))
    t))

(defmethod count-tokens ((phrases phrases) tokens)
  ;; how many times does the sequence of TOKENS of type document-tokens occur in the field values of the index?
  (count-if #'(lambda(use) (investigate phrases use tokens))
            (token-usage phrases (car tokens))))

(defmethod collect-adjacency ((phrases phrases) use)
  (with-slots (sequences phrase-list terms) phrases
    (loop
     with tokens = (list use)
     with first-key = (key use)
     for token-position from (1+ (document-token-position use)) upto (length terms)
     as next-token = (aref terms token-position) do
     (if (or (null next-token)
             (not (find (type-of-token next-token) '(:word :stopword)))
             (eq first-key (key next-token)) ; reject repeating tokens!
             )
         (return-from collect-adjacency))
     (setf tokens `(,@tokens ,next-token))
     (let ((repetitions (count-tokens phrases tokens)))
       (when (> repetitions 1)
         (push tokens (gethash (mapcar #'key tokens) sequences)))))))

(defmethod collect-adjacencies ((phrases phrases) usage tokens)
  (loop
   for use across usage do
   (collect-adjacency phrases use)
   (setf (document-token-already-considered use) t)))

(defmethod save ((phrases phrases) doctoken)
  (let ((entry (token-usage phrases doctoken)))
    (with-slots (terms table) phrases
      (unless entry
        (setf entry (make-array 0 :adjustable t :fill-pointer 0))
        (setf (gethash (key doctoken) table) entry))
      (vector-push-extend doctoken entry)
      (vector-push-extend doctoken terms))))

(defun percentage-completed (count total)
  (floor (* 100 count) total))

(defun compile-document-phrases (phrases)
  (with-slots (text terms analyzer language) phrases
    (dolist (token (all-tokens analyzer nil language text))
      (unless (eq :space (token-type token)) ; (eq :stopword (token-type token)))
        (save phrases (make-document-token token (intern (token-image token)) (fill-pointer terms))))))
  phrases)

(defmethod discover ((phrases phrases))
  (with-slots (terms) phrases
    (show-progress phrases 0)
    (loop
     with number-of-terms = (length terms)
     for count from 0 to number-of-terms
     for doctok across terms do
     (show-progress phrases (percentage-completed count number-of-terms))
     (unless (or (document-token-already-considered doctok)
                 (not (eq (type-of-token doctok) :word)))
       (let ((usage (token-usage phrases doctok)))
         (when (> (length usage) 1)
           (collect-adjacencies phrases usage (list doctok))))))))

(defmethod identify-phrases ((phrases phrases) sequences)
  (show-progress phrases 0)
  (loop
   with size = (hash-table-count sequences)
   for occurs being the hash-value in sequences
   for count from 0 do
   (show-progress phrases (percentage-completed count size))
   ;;(format t "~s~%" (make-phrase-query field occurs))
   collect (list (format nil "~{~a~^ ~}" (mapcar #'text (first occurs)))
                 (length occurs) ; number of times phrase is found
                 ;;1 ; number of documents in which phrase occurs
                 nil ; assume phrase is not redundant
                 )))

(defun redundant-phrase (phrase next-phrase)
  (when next-phrase
    (let* ((str1 (car phrase))
           (freq1 (cadr phrase))
           (str2 (car next-phrase))
           (freq2 (cadr next-phrase)))
      (if (< (length str2) (length str1))
          (return-from redundant-phrase nil))
      (if (string-equal str1 str2 :end2 (length str1))
          (if (= freq1 freq2)
              (return-from redundant-phrase t)))))
  nil)

(defun remove-redundant-phrases (phrase-list)
  (mapl
   #'(lambda(list) (setf (third (car list)) (redundant-phrase (car list) (cadr list))))
   phrase-list)
  (remove-if #'fourth phrase-list))

(defmethod discover-phrases ((phrases phrases))
  (show-operation phrases "Building phrase index")
  (compile-document-phrases phrases)
  (show-operation phrases "Discovering adjacent words")
  (discover phrases)
  (show-operation phrases "Identifying and collating phrases")
  (setf (phrase-list phrases) (sort (identify-phrases phrases (sequences phrases)) #'string-lessp :key #'car))
  (show-operation phrases "Removing redundant phrases")
  (let ((size (length (phrase-list phrases))))
    (setf (phrase-list phrases) (remove-redundant-phrases (phrase-list phrases)))
    (show-operation phrases (format nil "Completed phrase discovery (removed ~d redundant phrases)" (- size (length (phrase-list phrases))))))
  (mapcar #'(lambda(p) (subseq p 0 2)) (phrase-list phrases)))

(defmethod text-phrases (text language &optional progress-function progress-stage-function)
  (discover-phrases
   (make-instance 'phrases
    ;;:index index
    ;;:field (or field (default-search-field index))
    ;;:document document
    :language language
    :analyzer (make-instance 'text-analyzer)
    :text text
    :progress-function progress-function
    :progress-stage-function progress-stage-function)))
#|
(defmethod phrases ((index index) (document document) field &optional progress-function progress-stage-function)
  (discover-phrases
   (make-instance
    'phrases
    :text (fetch-field-value index document field)
    ;;:index index
    ;;:field (or field (default-search-field index))
    ;;:document document
    :progress-function progress-function
    :progress-stage-function progress-stage-function)))

(defmethod phrases ((index index) (docnum integer) field &optional progress-function progress-stage-function)
  (unless (deleted-p index docnum)
    (phrases
     index
     (get-document (reader index) docnum)
     field
     progress-function
     progress-stage-function)))
|#

#|
(phrases (get-corpus "library") '(3) "text"))
(discover-phrases (make-phrases (get-corpus "PASTES") "contents" nil nil))
|#
