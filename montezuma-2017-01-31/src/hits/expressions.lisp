#|

Copyright 2017 Roy Anderson <reanz1959@gmail.com>

Montezuma expressions provides a framework for building annotations to document fields.

|#

(in-package #:montezuma)

(defclass field-expression ()
  ;; Compilation of a regular expression based on group of query terms to apply to FIELD
  ((field :accessor field-name :initform "" :initarg :field :type string)
   (conditions :accessor conditions :initform () :initarg :conditions :type list) ; match criteria
   (conditions-scanner  :accessor conditions-scanner :initform ()) ; required match criteria scanner
   (regexps :initarg :regexps :initform () :accessor field-expression-regexps)
   (regexp :initarg :regexp :initform () :accessor field-expression-regexp)
   (scanner :initarg :scanner :initform nil :accessor scanner)))

(defmethod print-object ((self field-expression) stream)
  (print-unreadable-object (self stream :type nil :identity nil)
    (format stream "~(~a~) ~s " (type-of self) (field-name self))
    (if (conditions self) (format stream "conditions: ~{~s ~}" (conditions self)))
    (format stream "regexp: ~s" (slot-value self 'regexp))
    ))

(defclass field-expressions ()
  ((fields :accessor fields :initform (make-array 0 :element-type 'field-expression :fill-pointer 0 :adjustable t))
   (query :accessor query :initform nil :initarg :query)
   ))

(defmethod field-expression ((fexs field-expressions) field)
  (loop
   for fex across (fields fexs)
   if (string-equal (field-name fex) field)
   return fex))

(defmethod print-object ((self field-expressions) stream)
  (print-unreadable-object (self stream :type nil :identity nil)
    (with-slots (fields) self
      (format stream "~(~a~) ~d expression~:p~%" (type-of self) (length fields))
      (princ fields stream))))

(defmethod add-condition ((manifest field-expressions) condition field)
  (push condition (conditions (field-manifest manifest field))))

(defmethod make-regular-expression ((term null) proximity)
  (format nil "(\\w+)"))

(defmethod extract-annotation-terms ((fexs field-expressions) (self typed-range-query))
  ;;(vector-push-extend self terms)
  )

(defmethod extract-annotation-terms ((fexs field-expressions) (self term-clause))
  (add-regular-expression fexs (field self) (term-text self)))

(defmethod extract-annotation-terms ((fexs field-expressions) (self regular-expression-term-query))
  (let ((rewritten (rewritten self)))
    (add-regular-expression fexs (field self) (term-text rewritten))))

(defmethod extract-annotation-terms ((fexs field-expressions) (self regular-expression-query))
  (let ((rewritten (rewritten self)))
    (add-regular-expression fexs (field self) (term-text rewritten))))

(defmethod extract-annotation-terms ((fexs field-expressions) (self query))
  ;;(break "shouldn't be here query ~s" self)
  )

(defmethod extract-annotation-terms ((fexs field-expressions) (self term-query))
  (with-slots (term) self
    (add-regular-expression fexs (field term) (format nil "\\b~a\\b" (term-text term)))))

(defmethod extract-annotation-terms ((fexs field-expressions) (clause boolean-clause))
  ;;(break "extract-annotation-terms clause ~s" clause)
  (unless (prohibited? clause)
    (let ((query (query clause)))
      (extract-annotation-terms fexs query))))

(defmethod extract-annotation-terms ((fexs field-expressions) (self boolean-query))
  ;;(break "extract-annotation-terms boolean-query ~s" self)
  (dosequence (clause (clauses self))
    (if (and clause (query clause))
        (let ((query (query clause)))
          (when query
            (extract-annotation-terms fexs query)

            (if (typep query 'term-query)
                (when (required? clause) ;eq (occur clause) :must-occur)
                  ;; we need the occurs value to process the term query
                  (let* ((term (term query))
                         (field (term-field term))
                         (text (term-text term)))
                    ;;(break "must-occur extract-annotation-terms~%query ~s~%field ~a~%text ~s" query field text)
                    (add-condition fexs text field)))))))))

(defun  annotation-terms (query)
  (let ((ta (make-array 0 :fill-pointer 0 :adjustable t)))
    (extract-annotation-terms query ta)
    ta))

(defmethod extract-annotation-terms ((self match-all-query) terms)
  )

(defmethod term-expression ((term term))
  (with-slots (type text) term
    (cond
     ((or (eq :anyword type) (string= "." text))
       "\\w+")
     (t
      text))))

(defmethod extract-annotation-terms ((fexs field-expressions) (self array))
  ;;(break "array")
  (loop for trm across self do (extract-annotation-terms fexs trm)))

(defmethod extract-annotation-terms ((fexs field-expressions) (self phrase-query))
  (loop
   with offset = 0
   with texs = ()
   for term across (terms self)
   for position across (positions self)
   do
   (loop for n from (1+ offset) upto (1- position) do (push "\\w+" texs))
   (setf offset position)
   (push (term-expression term) texs)
   finally 
   (setf texs (reverse texs))
   ;;(break "extract-annotation-terms phrase-query ~s texs ~s" self texs)
   (let ((proximity (slop self)))
     (if (zerop proximity)
         ;;(add-regular-expression fexs (field self) (format nil "\\b(~{~a~^\\W+~})\\b" texs))
         (add-regular-expression fexs (field self) (format nil "~{~a~^\\W+~}" texs))
       (let ((regexp (make-vector)))
         (with-output-to-string (stream regexp)
           (let ((gap (format nil "(\\w+\\W)+{0,~d}" proximity)))
             (loop
              with prev = nil
              for trm in texs do
              (if prev (format stream gap))
              (setf prev trm)
              (if trm (format stream "~a" trm)))))
         ;;(field-expression-terms (field-manifest fexs (field self))
         (add-regular-expression fexs (field self) regexp))))))

(defmethod add-regular-expression ((fexs field-expressions) (field string) (regexp string))
  (pushnew regexp (field-expression-regexps (field-manifest fexs field)) :test #'string-equal))

(defmethod extract-annotation-terms ((fexs field-expressions) (self wildcard-query))
  (add-regular-expression fexs (field self) (wildcard-full-text-expression self)))

(defun annotation-expressions (query)
  (let ((fexs (make-instance 'field-expressions :query query)))
    (extract-annotation-terms fexs query)
    (loop
     for fex across (fields fexs) do
     (setf (field-expression-regexp fex) (format nil "~{~a~^|~}" (field-expression-regexps fex)))
     (setf (scanner fex) (cl-ppcre:create-scanner (field-expression-regexp fex) :case-insensitive-mode t :multi-line-mode t))
     (if (conditions fex)
         (setf (conditions-scanner fex) ;  (format nil "\\b~a\\b" text) field)))))))))
               (cl-ppcre:create-scanner (format nil "\\b~{~a~^|~}\\b" (conditions fex)) :case-insensitive-mode t :multi-line-mode t))))
    fexs))

(defun longestCommonPrefix (strs)
    (if (zerop (length strs)) (return-from longestCommonPrefix ""))
    (let*((minStr (aref strs 0))
          (end (length minStr)))
      (loop
       for str across strs
       if (< (length str) (length minStr))
       do (setf minStr str)) 
      (loop
       for str across strs do
       (loop
        for j from 0 upto (1- end)
        unless (char-equal (char minStr j)
                           (char str j)) do
          (when (< j end)
            (setf end j)
            (return))))
 
      (subseq minStr 0 end)))

(defmethod redundantp ((fex field-expression))
  (with-slots (conditions regexp) fex
    (and (null conditions)
         (null regexp))))
