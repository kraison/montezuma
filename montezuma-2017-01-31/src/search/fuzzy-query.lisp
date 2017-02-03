2(in-package #:montezuma)

(defclass fuzzy-query (query)
  ((reader :initarg :reader)
   (count :initform -1)
   (max-doc)
   (field :initarg :field :initform () :accessor field)
   (word :initarg :word :initform () :accessor word)
   (similarity :initarg :similarity :initform 0.5 :accessor similarity)))

(defmethod initialize-instance :after ((self fuzzy-query) &key)
  (with-slots (field word similarity) self
    (cond ((null field)
	   (error "A field is required for a fuzzy query."))
	  ((null word)
	   (error "A word is required for a fuzzy query."))
	  ((or (null similarity) (< similarity 0) (> similarity 1))
	   (error "Similarity is required and must be greater than or equal to 0 and less than or equal to 1.")))))

(defmethod print-object ((self fuzzy-query) stream)
  (print-unreadable-object (self stream :type T)
    (format stream "Field: ~s Word: ~s Similarity ~s~%" (field self) (word self) (similarity self))))

;levenshtein-distance

(defmethod rewrite ((self fuzzy-query) reader)
  "Translate a fuzzy word query consisting of a field, a word and similarty factor 0 - 1
 into a boolean query of similar words in the index based on the levenshtein-distance."
  (with-slots (field word similarity) self
    (let ((bq (make-instance 'boolean-query))
	  (term-enum (terms-from reader (make-term field word))))
      (unwind-protect
          (loop
           for term = (term term-enum) do
           (unless (and term (string= (term-field term) field)) (return))           
           (let ((factor (- 1.0 (fuzzy-factor (term-text term) word)))) ; 1.0 means an exact match
             (when (>= factor similarity)
               (add-term-query bq term (boost self) :should-occur)))
           while (next? term-enum))
        (close-down term-enum))
      bq)))
