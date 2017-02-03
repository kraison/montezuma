(in-package :montezuma)

(defun split-sentence-tokens (tokens striker)
  "iterate sentence tokens calling striker"
  (loop
   with start = 0
   for x = (position-if #'sentence-terminatorp tokens :start start) do
   (funcall striker (subseq tokens start (if x (incf x))))
   (unless x (funcall striker (subseq tokens start)) (return))
   (setf start x)))

(defun make-token-list (table)
  (loop
   for k being the hash-keys in table using (hash-value v)
   collect (make-instance 'orbit
                          :word (string-downcase k)
                          :key (intern (string-downcase k))
                          :word-type '?
                          :frequency (length v)
                          :sentence-numbers v)))

(defun gather-tokens (sentences)
  (let ((table (make-hash-table :test #'equal)))
    (loop
     for (sentence-number contents) in sentences do
     (dolist (token contents)
       (when (and (eq (token-type token) :word)
                  (> (length (token-image token)) 1))
         ;;(break "gather-tokens token ~s" token)
         (let ((key (intern (string-downcase (token-image token)))))
           (setf (gethash key table) `(,@(gethash key table) ,sentence-number))))))
    (make-token-list table)))

(defun select-abstract-tokens (token-records)
  (cond
   ((<= (length token-records) 12)
    (remove-if #'zerop token-records :key #'overlap))
   (t
    (let ((guess (overlap (nth 8 token-records)))
          (best ()))
      (dolist (record token-records)
        (when (or (corep record) (and (> (overlap record) 0) (>= (overlap record) guess)))
          (push record best)
          (if (>= (length best) 12) (return))))
      (reverse best)))))

(defun calculate-saturation (node core)
  (let ((core-sentences (sentence-numbers core)))
    (cond
     ((eq node core)
      (setf (overlap node) (length core-sentences))
      (setf (saturation node) 1.0))
     (t
      (setf (overlap node) (count-if #'(lambda(x) (find x core-sentences)) (sentence-numbers node)))
      (setf (saturation node) (float (/ (overlap node) (length (sentence-numbers node)))))
      ))))

(defclass orbit ()
  ((word :initarg :word :type string :accessor word)
   (key :initarg :key :type symbol :accessor key)
   (word-type :initarg :word-type :type symbol :accessor word-type)
   (corep :initarg :corep :initform nil :accessor corep)
   (parent :initarg :parent :initform nil :accessor parent)
   (saturation :initarg :saturation :type float :initform 0 :accessor saturation)
   (sentence-numbers :initarg :sentence-numbers :type list :initform () :accessor sentence-numbers)
   (overlap :initarg :overlap :type integer :initform 0 :accessor overlap)
   (frequency :initarg :frequency :type integer :initform 0 :accessor frequency)))

(defmethod print-object ((self orbit) stream)
  (print-unreadable-object (self stream :type T :identity nil)
    (with-slots (word corep saturation overlap frequency) self
      (format stream "~s" word) 
      (if corep (format stream " :corep ~a" corep))
      (format stream " :saturation ~d" saturation) 
      (format stream " :overlap ~a" overlap) 
      (format stream " :frequency ~a" frequency)
      ;;(format stream " :sentence-numbers ~a" sentence-numbers)))
      )))

(defun assimilate (selected core depth)
  (when selected
    (let ((core-record (find core selected :test #'string= :key #'word)))
      (when core-record
        (if (zerop depth) (setf (corep core-record) t))
        (dolist (x selected) (calculate-saturation x core-record))
        (let ((orbits (remove-if #'(lambda(x) (or (eq core-record x) (parent x))) selected)))
          (setf orbits (sort orbits #'> :key #'overlap))
          (push core-record orbits)
          (let ((result (select-abstract-tokens orbits)))
            (dolist (o result) (setf (parent o) core))
            ;;(format t "assimilate ~{~a~^, ~}~2%" (mapcar #'(lambda(x) (format nil "~a/~a[~d]" (word x) (parent x) (saturation x))) result))
            result))))))

(defun get-sentences (text &optional (language "english"))
  (let ((sentence-number 0)
        (sentences ()))
    (split-sentence-tokens
     (all-tokens (make-instance 'text-analyzer) nil language text)
     #'(lambda(sentence) (push (list (incf sentence-number) sentence) sentences)))
    (nreverse sentences)))

(defun get-sentences2 (text core &optional (language "english"))
  (let ((sentence-number 0)
        (sentences ())
        (scanner (cl-ppcre:create-scanner (format nil "\\b~a\\b" core) :case-insensitive-mode t :multi-line-mode t))
        (analyzer (make-instance 'text-analyzer)))
    (loop
     with start = 0
     with text-length = (length text)
     for x = (position-if #'(lambda(c) (find c ".?!")) text :start start) do
     (if x (incf x) (setf x text-length))
     (incf sentence-number)
     (let ((sentence (subseq text start x)))
       (if (cl-ppcre:scan scanner sentence) 
           (push (list sentence-number (all-tokens analyzer nil language sentence)) sentences))
       (setf start x)
       (if (= start text-length)
           (return))))
     (nreverse sentences)))

(defmethod abstract-document (text language core)
  (let ((orbits (gather-tokens (get-sentences text language))))
    (assimilate orbits core 0)))

(defmethod compute-wordmap1 (text language core)
  (let* ((sentences (get-sentences text language))
         (table (make-hash-table :test #'equal))
         (orbitals (gather-tokens sentences))
         (inner-orbitals (assimilate orbitals core 0))
         (wordmap ()))
    (dolist (orbital inner-orbitals)
      (setf (gethash (key orbital) table) orbital))
    (push (first inner-orbitals) wordmap)
    (dolist (orbital (cdr inner-orbitals))
      (let ((outer-orbitals (assimilate orbitals (word orbital) 1))
            (suborbitals))
        (dolist (xo outer-orbitals)
          (unless (gethash (key xo) table)
            (push xo suborbitals)))
        (push (cons orbital suborbitals) wordmap)))
    (setf wordmap (reverse wordmap))
    #|
    (format t "(~s~%" (word (car wordmap)))
    (mapcar #'(lambda(x) (format t "(~{~s~^ ~})~%"  (mapcar #'word x))) (cdr wordmap))
    (format t ")~2%")
    |#
    wordmap))

(defmethod compute-wordmap (text language core)
  (let* ((sentences (get-sentences2 text core language))
         (table (make-hash-table :test #'equal))
         (orbitals (gather-tokens sentences))
         (inner-orbitals (assimilate orbitals core 0))
         (wordmap ()))
    (dolist (orbital inner-orbitals)
      (setf (gethash (key orbital) table) orbital))
    (push (first inner-orbitals) wordmap)
    (dolist (orbital (cdr inner-orbitals))
      (let ((outer-orbitals (assimilate orbitals (word orbital) 1))
            (suborbitals))
        (dolist (xo outer-orbitals)
          (unless (gethash (key xo) table)
            (push xo suborbitals)))
        (push (cons orbital suborbitals) wordmap)))
    (setf wordmap (reverse wordmap))
    #|
    (format t "(~s~%" (word (car wordmap)))
    (mapcar #'(lambda(x) (format t "(~{~s~^ ~})~%"  (mapcar #'word x))) (cdr wordmap))
    (format t ")~2%")
    |#
    wordmap))

(defun token-frequencies (text &optional (language "english"))
  ;; count the number of times each word occurs in TEXT
  (let ((analyzer (make-instance 'text-analyzer))
        (table (make-hash-table :test #'equal)))
    (dolist (token (all-tokens analyzer nil language text))
      (when (eq :word (token-type token))
        (let* ((key (intern (string-downcase (token-image token))))
               (entry (gethash key table)))
          (setf (gethash key table) (if entry (1+ entry) 1)))))
    (let ((entries ()))
      (maphash #'(lambda(key val) (push (list (string-downcase key) val) entries)) table)
      entries)))

(defmethod orbit-color ((x orbit))
  (if (corep x)
      "#ff0202"
    (let* ((saturation (saturation x))
           (shade (truncate (* 255 saturation)))
           (g (format nil "~2,'0x" shade)))
      (format nil "#~a~a~a" g g g))))

(defmethod orbit-background ((x orbit))
  (if (or (corep x) (< (saturation x) 0.75))
      "ivory"
    "black"))
          
#|
(abstract-documents (get-corpus "RVBC") "book:EPH") ; doesn't work because book field is not tokenized: need to reload with book tokenized
(abstract-documents (get-corpus "RVBC") "heading:ephesians" "verses" "grace")
(abstract-field-terms (get-corpus "RVBC") "book:eph" "verses")
(abstract-documents library "+against" "text" "against" nil 0 10)
(compute-wordmap2 (get-corpus "brown") 
(COMPUTE-SUMMARY (get-corpus "brown") "ca01" "be")
|#

  
