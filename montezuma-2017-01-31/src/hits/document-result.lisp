(in-package :montezuma)

(defclass document-result ()
  ;;a search result document with the expected docnum and score with the addition of a set of field hits and annotations
  ((docnum :initarg :docnum :accessor docnum :initform -1 :type fixnum)
   (score :initform 0.0 :type real :initarg :score :accessor score)
   (field-hits :initform () :type list :initarg :field-hits :accessor field-hits)
   (fields :initform () :type list :initarg :fields :accessor fields)
   (annotations :initform () :type list :initarg :annotations :accessor annotations)
   ;;(document :initform nil :type document :initarg :document :accessor document)
   ))

(defmethod print-object ((self document-result) stream)
  (print-unreadable-object (self stream :type T :identity nil)
    (with-slots (docnum score fields) self
      (format stream "docnum: ~d " docnum)
      (when (/= score 1.0) (format stream "score: [~d]~%" score))
      (dolist (field fields)
        (let ((name (car field)))
          (format stream "~a: ~s~%" name (annotated-field-value self name))))
      (terpri stream))))

(defmethod wrap ((docresult document-result) width)
  (dolist (field (fields docresult))
    (setf (cadr field) (wrap (remove #\return (cadr field)) 80)))
  docresult)

(defmethod wrap ((string string) width)
  (format nil "~{~<~%~1,80:;~A~> ~}" (split-sequence:split-sequence '(#\space) string)))

(defmethod initialize-instance :after ((self document-result) &rest args)
  (wrap self 80))

(defmethod make-document-result ((index index) (docnum integer) (score real) &optional doc)
  (unless (minusp docnum)
    (make-instance 'document-result
                   :docnum docnum
                   :score score
                   :fields (field-values index (or doc (get-document index docnum))))))


(defmethod make-document-result-from-document ((document document) (docnum integer) (score real))
  ;;(break "make-document-result-from-document ~s" (all-fields document))
  (make-instance 'document-result
                 :docnum docnum
                 :score score
                 :fields (mapcar #'(lambda(field)
                                     (with-slots (name data) field ;(cadr field)
                                       (list name data)))
                                 (all-fields document))))

(defun assoc-data (key a-list)
  (cadr (assoc key a-list :test 'string-equal)))

(defmethod field-value ((docres document-result) field-name)
  (assoc-data field-name (fields docres)))

(defmethod annotated-field-value ((docres document-result) field)
  ;; return the annotated FIELD value, or else the plain FIELD text,
  ;; and T if the field was not annotated
  (let ((annotated (assoc-data field (annotations docres))))
    (values (or annotated
                (assoc-data field (fields docres)))
            (null annotated))))

(defmethod add-annotations ((docres document-result) field annotations)
  (push (list field annotations) (annotations docres))
  docres)

(defmethod document-digest ((index index) (docres document-result))
  (let (;;(doc (get-document index docnum))
        (key-field (document-key index))
        (title-field "title")
        (author-field "author")
        (language-field "language"))
    (list (docnum docres)
          (annotated-field-value docres key-field)
          (annotated-field-value docres title-field)
          (annotated-field-value docres author-field)
          (annotated-field-value docres language-field))))

(defmethod document-digest ((index index) (docnum integer))
  (let ((docres (make-document-result index docnum 0)))
    (document-digest index docres)))
#|
  (let ((doc (get-document index docnum))
        (key-field (document-key index))
        (title-field "title")
        (author-field "author")
        (language-field "language"))
    (list docnum
          (fetch-field-value index doc key-field)
          (fetch-field-value index doc title-field)
          (fetch-field-value index doc author-field)
          (fetch-field-value index doc language-field))))
|#
