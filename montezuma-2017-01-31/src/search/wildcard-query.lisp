(in-package #:montezuma)

(defclass wildcard-query (multi-term-query)
  ((wildcard-prefix :initform "" :initarg :wildcard-prefix :type string :accessor wildcard-prefix)
   (wildcard-pattern :initform "" :initarg :wildcard-pattern :type string :accessor wildcard-pattern)
   (scanner :initform nil :initarg :scanner :accessor scanner)
   (wildcard-regular-expression :initform "" :initarg :wildcard-regular-expression :type string :accessor wildcard-regular-expression)
   (full-text-regular-expression :initform "" :initarg :full-text-regular-expression :type string :accessor wildcard-full-text-expression)))

(defmethod initialize-instance :after ((self wildcard-query) &key)
  (with-slots (term scanner wildcard-prefix wildcard-pattern wildcard-regular-expression full-text-regular-expression) self
    (multiple-value-setq (wildcard-prefix wildcard-pattern scanner)
        (regular-expression-from-wildcard-string (term-text term)))
    (setf wildcard-regular-expression (translate-to-regular-expression (term-text term)))
    (setf full-text-regular-expression (full-text-regular-expression (term-text term)))))

(defun regular-expression-from-wildcard-string (text)
  (let* ((idx (wildcard-position text))
         (regexp (resolve-entities (translate-to-regular-expression (if (< idx (length text)) (subseq text idx) "")))))
    (values (resolve-entities (subseq text 0 idx)) ; i.e. prefix
            regexp
            (cl-ppcre:create-scanner regexp))))

(defmethod get-term-enum ((self wildcard-query) reader)
  (make-instance 'wildcard-term-enum :reader reader :query self))

(defmethod extract-terms ((self wildcard-query) terms)
  (extract-terms (rewritten self) terms))
