(in-package #:montezuma)

(defparameter +character-class+ '("digit" "word" "space" "upper" "lower")) ; "d" "w" "s" "u" "l"))

(defun first-letter-or-word (cs words)
  (let ((cl (length cs)))
    (find cs
          words
          :test #'(lambda(x y)
                    (if (= 1 cl)
                        (char-equal (char x 0) (char y 0))
                      (string-equal x y))))))

(defun character-class (cc) 
  (let* ((cs (string cc))
         (c (first-letter-or-word cs +character-class+)))
    (if c
        (format nil "\\~a" (char-downcase (char c 0)))
      (if (and (char= (char cs 0) #\^)
               (setf c (first-letter-or-word (subseq cs 1) +character-class+)))
          (format nil "\\~a" (char-upcase (char c 0)))))))

(defun compose-subexpression (element stream)
  (let ((name (pop element))
        (type (pop element))
        (min (pop element))
        max)

    (if (and type (or (numberp type) (listp type)))
        (setf min type
              type "d"))

    (if (and type (listp min))
        (setf max (cadr min)
              min (car min)))

    (if (and min (listp min) (= 2 (length min)))
        (setf max (cadr min)
              min (car min)))

    (let ((cc (character-class (string type))))
      (if name
          (format stream "(?<~a>" name))
      (format stream "~a" (or cc type "d"))
      (if (and min (integerp min) max (integerp max))
          (format stream "{~d,~d}" min max)
        (if (and min (integerp min) (eq max t))
            (format stream "{~d,}" min) ; minimum and unlimited repetition
          (format stream "{~d}" min)))
      (if name
          (princ ")" stream)))))

(defun make-regular-expression-from-form (form regexp)
  "Make a regular expression from a list of variable pairs and literals."
  (with-output-to-string (stream regexp)
    (let ((element))
      (do ()
          ((null form))
        (setf element (pop form))
        (cond
         ((atom element)
          (let ((cc (character-class (string element))))
            (format stream "~a" (or cc element))))

         ((and element
               (listp element)
               (find (first element) '(?)))
          ;;(format t "list element ~s~%" element)
          (let ((op (pop element)))
            (princ "(" stream)
            (make-regular-expression-from-form element regexp)
            (princ ")" stream)
            (princ op stream)))

         ((and element
               (listp element)
               (find (first element) '(* + *? +? ?? *+ ++ ?+)))
          ;;(format t "list element ~s~%" element)
          (let ((op (pop element)))
            (princ "(" stream)
            (make-regular-expression-from-form element regexp)
            (princ op stream)
            (princ ")" stream)))

         ((or (eq '^ element) (eq '$ element))
          (princ element stream))

         ((listp element)
          (compose-subexpression element stream))

         (t
          (format stream "~a" element))))))
  regexp)

(defun regular-expression (form)
  "Make a regular expression from a list of variable pairs and literals."
  (let ((regexp (make-vector)))
    (make-regular-expression-from-form  (enlist form) regexp)
    regexp))

(defun make-scanner (format)
  (let ((regexp (regular-expression format)))
    (let ((cl-ppcre:*allow-named-registers* t))
      (multiple-value-bind (scanner registers) (cl-ppcre:create-scanner regexp)
        (values scanner registers regexp)))))

(defun get-register-number (starts ends register data)
  (if register
      (let ((start (aref starts register))
            (end (aref ends register)))
        (if (and start end)
            (subseq data start end)))))

(defun get-register (registers starts ends name data)
  (let ((register (position name registers :test #'string-equal)))
    (if register
        (let ((start (aref starts register))
              (end (aref ends register)))
          (if (and start end)
              (return-from get-register (subseq data start end)))))))

(defun get-scanned-register (registers substrings name)
  (let ((register (position name registers :test #'string-equal)))
    (if register
        (aref substrings register))))

(defun get-integer-register (registers starts ends name data &optional (default 1))
  (let ((value (get-register registers starts ends name data)))
    (if value 
        (parse-integer value)
      default)))

(defun get-scanned-integer (registers substrings name &optional (default 1))
  (let ((value (get-scanned-register registers substrings name)))
    (if value 
        (parse-integer value)
      default)))

(defun get-scanned-float (registers substrings name)
  (let ((value (get-scanned-register registers substrings name)))
    (when value 
      (if (not (position #\. value))
          (setf value (format nil "0.~a" value)))
      (return-from get-scanned-float (parse-float value))))
  0.0)

(defun replace-all (regex replacement target)
  (cl-ppcre:regex-replace-all regex target replacement))

(defun full-text-regular-expression (expression)
  "Translate a wildcard expression into an equivalent regular expression.
Wildcard operators are '?' for any single character and '*' for zero or more characters."
  (let ((regex (format
                nil "\\b~A\\b" ; enclose expression with starting and ending operators
                (replace-all
                 "\\\\([*])" ; replace '*' with .*
                 "\\w\\1"
                 (replace-all
                  "\\\\([?])" ; replace '?' with '.'
                  "\\w" ;"."
                  (cl-ppcre:quote-meta-chars expression))))))
    (replace-all "\\\\([&;])" "\\1" regex))) ; strip off the quotes in front of "&" or ";"

(defun translate-to-regular-expression (expression)
  "Translate a wildcard expression into an equivalent regular expression.
Wildcard operators are '?' for any single character and '*' for zero or more characters."
  (let ((regex (format
                nil "^~A$" ; enclose expression with starting and ending operators
                (replace-all
                 "\\\\([*])" ; replace '*' with .*
                 ".\\1"
                 (replace-all
                  "\\\\([?])" ; replace '?' with '.'
                  "."
                  (cl-ppcre:quote-meta-chars expression))))))
    (replace-all "\\\\([&;])" "\\1" regex))) ; strip off the quotes in front of "&" or ";"

(defun collect-named-substrings (registers substrings)
  "Assemble an association list of register names and values."
  (unless substrings (return-from collect-named-substrings nil))
  (unless (= (length registers) (length substrings))
    (error "Number of register names must be equal to number of substrings: ~d compared to ~d."
           (length registers)
           (length substrings)))
  (loop
   for substring across substrings
   for register in registers
   collect (list register substring)))

(defun get-named-substring (name alist)
  (cadr (assoc name alist :test #'string-equal)))

(defun get-named-integer (name alist)
  (parse-integer (or (get-named-substring name alist)
                     (return-from get-named-integer nil))))

(defparameter +wildcard-string+ #\*)
(defparameter +wildcard-char+ #\?)

(defun wildcard-position (text)
  (or (position-if
       #'(lambda(ch) (or (char= ch +wildcard-string+) (char= ch +wildcard-char+)))
       text)
      (length text)))

(defun scans (pattern prefix text)
  (when (or (null prefix)
            (and (>= (length text) (length prefix))
                 (string= prefix text :start2 0 :end2 (length prefix))))
    (or (eq pattern t)
        (cl-ppcre:scan pattern text
                       :start (if prefix (length prefix) 0)
                       :end (length text)))))

#|
(regular-expression '(^ (:year 4) (? (- (:month 2) (? (- (:day 2)))))))
(regular-expression '((day (1 2)) / (month (1 2)) / (year 4) (? " " (hour 2) #\: (min 2))))
(regular-expression '((:year 4) - (:month 2) - (:day 2)))
(regular-expression '(^ (* space) (day digit (1 2)) / (month (1 2)) / (year 4) (? (" " (hour 2) #\: (2)))))
(regular-expression '(^ (* space) $))
(regular-expression '((day digit (1 2)) / (month (1 2)) / (year 4) (? " " (hour 2) #\: (min 2))))
|#
