(in-package #:montezuma)


(defconstant brief-explanation-of-query-syntax
"
This description of [Montezuma Query Syntax] is based on the implementation
of the Montezuma Query Parser grammar used to process search queries. 
It is intended to be a guide for the construction of Montezuma queries, rather
than to be a rigourous BNF definition of Montezuma query syntax.

Queries:

A Montezuma Query generally consists of one or more clauses, but not necessarily!

A query without clauses is equivalent to a wild-card search for all documents 
regardless of document content but it is almost certainly more efficient than a 
search using the match-all wild-card ('*'). 

The full range of sort orders may be used to express built-in document number
and score value sorts in addition to document fields in the index for sorting
in either ascending or descending order.

Boundary values can used to specify the first document and number of documents
to be retrieved.

Finally, the fields extracted from documents in search results can be used to
select specified document fields.

Clauses can be combined in a list or with logical operators ('and', 'or', 'not').

In addition, clauses may be prefixed by a field specifier, optionally using a
wild-card field specifier.

Queries may be given a boost factor to tune the weight of some clauses relative
to the others.

Range Queries consist of one or two limiting values with operators for inclusive
or exclusive limits.

Fuzzy Queries consist of a word value followed by a '~' and optionally a fuzzy 
factor between 0 and 1.0 which are satisfied by field words with an closely 
matching editing difference.

Support has been added for typed terms and range queries for: integer, float and dates.

Dates can be specified in a form based on ISO 8601: YYYYMMDDTHHmmSSssssss. A minimum
datetime value is a four digit year. Datetimes may be followed by a timezone offset:
Z, or a + or - followed by hhmm. For example, 2016 or 20151225. Note: dates with or 
without times and timezone offsets must be given without either spaces or non-alphabetic
punctuatiom. Alternatively, datetime values may be specified in a quoted datetime phrase
which makes them easier to enter and read, e.g. \"2015 12 25\".

Similarly, fields me be defined in a Montezuma Index as :type int or float in which case the
Query Parser generates typed queries for these fields.

Execution of typed terms and ranges is coordinated with field definitions in the
Montezuma Index.

Grammar:

<montezuma-query> == <clause> <sort-order> <boundaries> <field-list>
<boundaries> == ':' <first-document> ',' <number-of-documents> | ''
<first-document> == integer
<number-of-documents> == integer
<sort-order> == '/' <direction> <sorting> | ''
<direction> == '+' | '-' | ''
<sorting> == 'document' | 'score' | <word> | '(' <sort-field-names> ')'
<sort-field-names> == <sort-field-name> | <sort-field-name> <sort-field-names>
<sort-field-name> == <direction> <word>
<field-list> == '[' <field-names> ']' | ''
<field-names> == <field-name> | <field-name> <field-names>
<field-name> == <wild-word> | <word>
<logical-operator> == 'not' | 'and' | 'or' | ''
<field-specifier> == <field-name> ':' | ''
<simple-query> == <phrase-query> | <wild-query> | <parenthesised-query> | <field-range-query> | <fuzzy-query> | <term-query>
<clause> == <logical-operator> <field-specifier> <simple-query> <boost> | ''
<boost> == '^' <number> | ''
<field-range-query> == <lower-bound> <word> 'to' <word> <upper-bound>
<lower-bound> == '[' | '{'
<upper-bound> == ']' | '}'
<parenthesised-query> == '(' <clause> ')'
<phrase> == '\"' <word-list> '\"' <proximity>
<word-list> == <word> | <word> <word-list>
<proximity> == '~' integer | ''
<term-query> == <word>
<fuzzy-query> == <word> '~' <fuzzy-factor>
<fuzzy-factor> == <number> | ''
<wild-query> == <wild-word>
<number> == integer | real
<letter> ==  <non-wild-letter> | <character-entity>
<word> ==  <letter> | <letter> <word>
<character-entity> == <decimal-entity> | <hexadecimal-entity> | <named-entity>
<decimal-entity> == '&#' integer ';'
<hexdecimal-entity> == '&#x' hexadecimal-number ';'
<named-entity> == '&' <word> ';'
<wild-word-constituent> == <any-letter> | <wild-letter> | <character-entity>
<wild-word> == <non-wild-letter>* <wild-letter> <wild-word-constituent>*
<wild-letter> == '*' | '?'
<any-letter> == 'A-Z0-9-'
")

(defclass query-parser ()
  ((query
    :initform nil :accessor query
    :documentation "the query submitted to the query parser")
   (field
    :initform nil :accessor field
    :documentation "the name of the field in the last specifier e.g. title:(as a fox)")
   (default-field
    :initarg :default-field :accessor default-field
    :documentation "any query without a specifier is referred to the default field")
   (analyzer
    :initarg :analyzer :accessor analyzer
    :documentation "Analyzer should be the same tokenizer used to add documents to the index.")
   (field-definitions
    :initarg :field-definitions :initform nil :reader field-definitions
    :documentation "used to resolve wildcard field specifications")
   (fields
    :initarg :fields :accessor fields
    :documentation "used to resolve wildcard field specifications and check fild specifiers")
   (chosen-fields
    :initarg :chosen-fields :accessor chosen-fields :initform ()
    :documentation "A set of fields that may be specified in a query to be fetched from documents in the result set.")
   (default-occur :initarg :default-occur :accessor default-occur
   :documentation "how the query should be treated (required, prohibited, or should)")
   (default-slop 
    :initarg :default-slop :accessor default-slop
    :documentation "how cloose is required of phrase words")
   (field-stack :initform () :accessor field-stack)
   (language :accessor language :initform "english")
   )
  (:default-initargs
   :default-field "*" ; all fields
   :fields '()
   :field-definitions ()
   :default-occur :should-occur ; desirable to have query satisified
   :default-slop 0 ; all phrase word in sequence
   ))

(defmethod initialize-instance :after ((self query-parser) &key)
  ;;(break "language?")
  (with-slots (field fields field-definitions default-field default-occur analyzer) self
    (unless field
      (setf field default-field))
    (if (eq default-occur 'default-occur)
        (setf default-occur :should-occur))
    (unless fields
      (if field-definitions
          (setf fields (mapcar #'car (field-definitions self)))))
    (unless field-definitions
      (if (fields self)
          (setf field-definitions (mapcar #'list fields))))
    ;;(setf analyzer (make-instance 'standard-analyzer)))) ; :language language))))
    (setf analyzer (make-instance 'query-analyzer)))) ; :language language))))

(define-condition invalid-query (error)
  ((query :initarg :query :reader query)
   (offset :initarg :offset :reader offset)
   (message :initarg :message :reader message)
   (fields :initarg :fields :reader fields :initform nil)
   (field :initarg :field :reader field :initform nil)))

(define-condition invalid-integer (invalid-query) ())
  
(define-condition invalid-field (invalid-query)
  ())

(define-condition no-matching-fields (invalid-field)
  ())

(defmethod initialize-instance :after ((self invalid-query) &key)
  (setf (slot-value self 'offset) 0))

(defmethod field-definition ((self query-parser) name)
  (assoc name (field-definitions self) :test #'string-equal))
          
(defmethod field-definition-feature ((self query-parser) name feature)
  (cadr (member feature (field-definition self name))))

(defmethod field-definition-type ((parser query-parser) field)
  (intern (string (or (field-definition-feature parser field :type)
                      (return-from field-definition-type nil)))))

(defmethod field-definition-form ((parser query-parser) field)
  (field-definition-feature parser field :form))

(defgeneric $add-and-clause (parser clauses clause))
(defgeneric $add-or-clause (parser clauses clause))
(defgeneric $add-not-clause (parser clauses clause))
(defgeneric $add-clause (parser clauses clause occur))
(defgeneric $get-term-query (parser word))
(defgeneric $get-boolean-clause (parser query occur))
(defgeneric $get-boolean-query (parser clauses))
(defgeneric $add-word-to-phrase (parser phrase word))
(defgeneric $get-phrase-query (parser words proximity))
(defgeneric $get-wild-query (parser word))
(defgeneric $set-query-field (parser field))
(defgeneric $get-field-range-query (parser lower upper))
(defgeneric $get-fuzzy-query (parser field-value similarity))
(defgeneric $get-logical-query (parser query1 operator query2))
(defgeneric $get-active-field (parser))
(defgeneric compute-multiple-fields (parser field-spec))
(defgeneric $set-query-boost-factor (parser query number))
(defgeneric montezuma-parse (parser query-string))

(defconstant punctuation-character '(#\( #\) #\" #\* #\? #\: #\^ #\[ #\] #\{ #\} #\- #\+ #\! #\~ #\/ #\, #\!))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((query-wildcards (cl-ppcre:create-scanner "(\\*|\\?)")))
    (defun wildcard-word-p (word)
      ;; see if there are any unescaped wild characters
      (cl-ppcre:scan query-wildcards word))))

(defun placeholder (element)
  (cond
   ((null element)
    nil)
   ((and (stringp element)
         (string= "." element))
    nil)
   (t element)))

(defmethod print-object ((self invalid-query) stream)
  (with-slots (query offset message) self
    (princ message stream)
    (format stream "~%~a~%"
            (if offset
                (concatenate 'string (subseq query 0 offset) " <> " (subseq query offset))
              query))))

(defmethod $get-field-range-query ((parser query-parser) lower upper)
  "Create a field range query from the active field (the field name last specified) and a lower and upper value.
The lower and upper values are provided with square or curly brackets for inclusive or exclusive range boundaries."
  (let ((low (placeholder (car lower)))
        (upr (placeholder (car upper))))
    (when (and (null low) (null upr))
      (error 'invalid-query :parser parser "Expected at least a lower or upper value."))
    (let ((name ($get-active-field parser)))
      (make-instance 'range-query
                     :field name
                     :defined-type (field-definition-type parser name)
                     :form (field-definition-form parser name)
                     :definition (field-definition parser name)
                     :lower-term low
                     :include-lower-p (and low (eq :include (cadr lower)))
                     :upper-term upr
                     :include-upper-p (and upr (eq :include (cadr upper)))))))

(defun combine-multiple-fields (queries)
  ;; When one query, return it. Otherwise make a boolean query including all the given queries.
  (if (length-1-list-p queries)
      (first queries)
    (let ((bq (make-instance 'boolean-query)))
      (dolist (q queries)
        (if q (add-clause bq q)))
      bq)))

(defmacro do-multiple-fields ((field-var parser) &body body)
  "Macro that computes the list of field names from the active field and calls the body for each field."
  `(combine-multiple-fields
    (map 'list 
         #'(lambda (,field-var) ,@body)
         (compute-multiple-fields ,parser ($get-active-field ,parser)))))

(defmethod compute-multiple-fields ((parser query-parser) field-spec)
  "A field specifier may be either a literal field name, or a wildcard expression with one or more of '*' or '?'."
  (cond
   ((string= field-spec "*")
    ;; interpret field-spec as a wildcard expression matching all the fields in the parser fields slot
    (coerce (fields parser) 'list))

   ((not (wildcard-word-p field-spec))
    ;; interpret field-spec as a literal field name
    (list (resolve-entities field-spec)))

   ((fields parser)
    ;; filter the parser's fields using a regular expression from field-spec
    (let* ((regexp (resolve-entities (translate-to-regular-expression field-spec)))
           (scanner (cl-ppcre:create-scanner regexp))
           (matching-names ()))
      (dosequence (field (fields parser))
        (if (cl-ppcre:scan scanner field)
            (push field matching-names)))
      (unless matching-names
        (error 'no-matching-fields
               :query (query parser) :message "No fields matching" :field field-spec :fields (field-definitions parser)))))
   (T
    (error 'invalid-query
           :query (query parser)
           :message "Insufficient index information to parse this query: provide at least a list of field definitions."))))

(defun set-occur (clause occur)
  (unless (prohibited? clause) ; don't change the occurence value if it's a 'prohibition clause.
    (setf (occur clause) occur)))

(defmethod $add-and-clause ((parser query-parser) clauses clause)
  "Combine clause with clauses and make the occurence of clauses required for documents."
  (setf clauses (remove nil (enlist clauses)))
  (when (length-1-list-p clauses)
    (set-occur (first clauses) :must-occur))
  (cond
   (clause
    (set-occur clause :must-occur)
    (cons clause clauses))
   (t 
    clauses)))

(defmethod $add-or-clause ((parser query-parser) clauses clause)
  (cons clause (enlist clauses)))

(defmethod $add-not-clause ((parser query-parser) clauses clause)
  (setf clauses (remove nil (enlist clauses)))
  (cond
   (clause
    (set-occur clause :must-not-occur)
    (cons clause clauses))
   (t clauses)))

(defmethod $add-clause ((parser query-parser) clauses clause occur)
  (case occur
    (:must-occur ($add-and-clause parser clauses clause))
    (:should-occur ($add-or-clause parser clauses clause))
    (:must-not-occur ($add-not-clause parser clauses clause))))

(defmethod $get-term-query ((parser query-parser) word)
  (do-multiple-fields (field parser)
    (let ((field-type (field-definition-type parser field))
          (field-definition (field-definition parser field)))
      ;;(break "$get-term-query ~s parser ~s" word parser)
      (case field-type
#|
        (integer
         (let ((integer (parse-fixed-point word)))
           (if (and integer (integerp integer))
               (make-instance 'integer-term-query
                              :term (make-term field integer)
                              :definition field-definition))))
        ;;(make-instance 'term-query :term (make-term field word)))))
|#
        (float
         (make-instance 'float-term-query :term (make-term field word) :definition field-definition))
        (date
         ;; we have a field defined to be a date so now let's parse our term and make a query
         (let ((date (parse-query-date word)))
           (if date
               (make-instance 'date-term-query :term (make-term field date) :definition field-definition))))
        (t
         (let ((tokens (all-tokens (analyzer parser) field (language parser) (resolve-entities word))))
           (break "$get-term-query field ~s tokens: ~s" field tokens)
           (cond
            ((zerop (length tokens))
             (make-instance 'term-query :term (make-term field "")))
            
            ((= (length tokens) 1)
             (make-instance 'term-query :term (make-term field (term-text (car tokens)))))
            
            (t
             (let ((pq (make-instance 'phrase-query)))
               (dolist (token tokens)
                 (add-term-to-query pq (make-term field (term-text token)) nil (token-increment token)))
               pq)))))))))


(defmethod $get-wild-query ((parser query-parser) word)
  ;; we don't really want to resolve entities yet: (setf word (resolve-entities word))
  (do-multiple-fields (field parser)
    ;;(break "$get-wild-query word ~s" word)
    (make-instance 'wildcard-query :term (make-term field word))))

(defmethod $get-regular-expression-term-query ((parser query-parser) field word)
  ;(do-multiple-fields (field parser)
    (make-instance 'regular-expression-query :term (make-term field word)))

(defmethod $get-literal-query ((parser query-parser) word)
  (do-multiple-fields (field parser)
    (let ((field-type (or (field-definition-type parser field) t)))
      ;;(break "$get-literal-query field-type: ~s" field-type)
      (case field-type
        #|
        ((:int int)
         (break "$get-literal-query word ~s" word)
         (make-instance 'integer-term-query
                        :term (make-term field word)
                        :definition (field-definition parser field)))
        |#

        (float
         (make-instance 'float-term-query
                        :term (make-term field word)
                        :definition (field-definition parser field)))
        (date
         ;; we have a field defined to be a date so now let's parse our term
         (make-instance 'date-term-query
                        :term (make-term field (parse-query-date word))
                        :definition (field-definition parser field)))
        (t
         ;;(let ((stored (field-definition-feature parser field :stored)))
           ;(break "$get-literal-query: ~s field: ~s definition ~s" word field (field-definition parser field))
           ;(if stored
           ;    (make-instance 'regular-expression-query :regexp (resolve-entities word) :field field)
           ($get-regular-expression-term-query parser field (resolve-entities word)))))))

(defmethod $get-boolean-clause ((parser query-parser) query occur)
  (unless (typep query 'query)
    (error "Query expected! Got ~s." query))
  (make-instance 'boolean-clause :query query :occur (or occur (default-occur parser))))

(defmethod $get-boolean-query ((parser query-parser) clauses)
  (let ((q  (make-instance 'boolean-query)))
    (dolist (clause (reverse (enlist clauses)))
      (add-clause q clause))
    q))

(defmethod $add-word-to-phrase ((parser query-parser) phrase word)
  (append (enlist phrase) (list word)))


(defun parse-fixed-point (x)
  (if (integerp x)
      x
    (with-standard-io-syntax
      (read-from-string x))))

(defmethod $get-phrase-query ((parser query-parser) words proximity)
  (setf words (mapcar #'resolve-entities (enlist words)))
  (if (length-1-list-p words)
      (return-from $get-phrase-query ($get-term-query parser (car words))))
  ;;(setf proximity 
  (let ((pqs ())
        (prox (if proximity (parse-fixed-point proximity) (default-slop parser)))
        (pos-inc 0))
    (unless (integerp prox)
      (error 'invalid-query
             :query (query parser)
             :message (format nil "Invalid Phrase proximity value. Got ~a. Wanted an integer from 0." proximity)))
    (do-multiple-fields (field parser)
      (let (;;(field-type (field-definition-type parser field))
            (pq (make-instance 'phrase-query :slop prox)))
        (dolist (word words)
          (if (or (null word) (string= "." word))
              (incf pos-inc)
            (let ((tokens (all-tokens (analyzer parser) field (language parser) word)))
              (dolist (token tokens)
                (add-term-to-query pq (make-term field (term-text token)) nil (+ pos-inc (token-increment token)))
                (setf pos-inc 0)))))
        (push pq pqs)))
    (if (length-1-list-p pqs)
        (car pqs)
      ($get-boolean-query parser pqs))))

(defmethod $set-query-field ((parser query-parser) field)
  (setf (field parser) field))

(defmethod $get-active-field ((parser query-parser))
  (or (field parser)
      (default-field parser)
      (error 'invalid-query :query (query parser)
             :message "Insufficient information about document fields to parse this query!")))

(defun parse-floating (p)
  (if (floatp p)
      p
    (with-standard-io-syntax
      (read-from-string p))))

(defmethod $set-query-boost-factor ((parser query-parser) query number)
  ;;(format t "$set-query-boost-factor ~s ~s~%" query number)
  (if number
      (setf (boost query) (parse-number number)))
  query)

(defmethod $get-fuzzy-query ((parser query-parser) field-value similarity)
  (let ((x (if similarity (parse-floating similarity) 0.5)))
    (unless (floatp x)
      (error 'invalid-query
             :query (query parser)
             :message (format nil "Invalid Levenstein similarity factor from ~s! Got: ~a. Wanted a number between 0.0 and 1.0" field-value similarity)))
    (make-instance 'fuzzy-query :field ($get-active-field parser) :word field-value :similarity x)))
    
(defmethod $get-match-all-query ((parser query-parser))
  (make-instance 'match-all-query))

(defmethod $get-occur ((parser query-parser) operator)
  (let ((op (or operator (default-occur parser))))
    (if (not (keywordp op)) (setf op :should-occur))
    op))

(defmethod $get-logical-query ((parser query-parser) query1 operator query2)
  (if (null query2)
      query1
    ($add-clause parser query1 query2 ($get-occur parser operator))))

#|
(defmethod make-sort-definition (sort-fields &optional (reverse nil))
  (make-instance 'sort-definition :fields (enlist sort-fields) :reverse-p reverse))
|#

(defmethod make-sort-definition ((parser query-parser) sort-fields reverse)
  (make-instance 'sort-definition :fields sort-fields :reverse-p reverse))

(defmethod $get-sorting-specification ((parser query-parser) sorting reverse)
  (setf reverse (string= "-" reverse))
  ;;(format t "$get-sorting-specification sorting ~s reverse ~s~%" sorting reverse)
  (case sorting
    (:document-order (setf sorting (make-document-order-sort-field reverse)))
    (:score-order (setf sorting (make-score-order-sort-field reverse)))
    (T 
     (make-sort-definition parser sorting reverse))))

(defmethod make-query-parser-result ((parser query-parser) query
                                     &key sort-specification
                                     first-document
                                     number-of-documents
                                     chosen-fields)
  (cond
   ((or (null query) (stringp query))
    (setf query ($get-match-all-query parser)))
   (t
    (unless (typep query 'boolean-query) (setf query ($get-boolean-query parser query)))))
  (make-instance 
   'query-parse-result
   :query query
   :sort-specification sort-specification
   :first-document first-document
   :number-of-documents number-of-documents
   :chosen-fields (reverse chosen-fields)))

(defmethod make-sorting ((parser query-parser) field reverse-p) ; field-parser)
  (let* ((type (field-definition-type parser field))
         (scanner (field-definition-feature parser field :scanner))
         (registers (field-definition-feature parser field :registers)))
    ;;(break "make-sorting :field ~s type: ~s :registers ~s" field type registers)
    (case type
      (null)
      (:int)
      (:float)
      (:date
       (when (and registers scanner)
         (let ((date-parser #'(lambda(data) (parse-date registers scanner data))))
           (make-instance
            'sort-field
            :name field
            :reverse-p reverse-p
            :parser date-parser
            :sort-type (make-instance
                        'sort-type
                        :name field
                        :parser date-parser
                        :comparator #'local-time:timestamp<=
                        :reverse-p reverse-p))))))))

(defmethod validate-field-name ((parser query-parser) field-name)
  (let ((fields (field-definitions parser)))
    (cond
     (fields 
      ;(break "validate-field-name ~a fields ~s" field-name fields);(assoc field-name fields :test #'string-equal))
      (if (assoc field-name fields :test #'string-equal)
          field-name
        (error 'invalid-field :query (query parser) :field field-name :fields fields :message "Unrecognized field.")))
        ;(error "Invalid-field :query ~s field ~s fields ~s" (query parser) field-name fields)))
     (t field-name))))

;; Note: the following grammar rules must be used in a parser created with
;; a parselet form, with a lexical context that includes a "parser" binding.

(defprod top-query ()
  ;; start state for query parser: a query optionally followed by a sort specification preceeded by a '/'
  (^ ((? bool-query)
      (? sort-order)
      (? boundaries)
      (? field-list))
     (make-query-parser-result 
      parser
      bool-query
      :sort-specification sort-order
      :first-document (car boundaries)
      :number-of-documents (cadr boundaries)
      :chosen-fields (chosen-fields parser))))

(defprod boundaries ()
  ;; boundaries extends a query by allowing the first document and the number of documents to be specified
  (^ (":" (* white-space) integer (? number-of-documents))
     (list (parse-integer integer) (parse-integer (or number-of-documents "10")))))

(defprod number-of-documents ()
  ;; number of documents can be used to specify the maximum number of documents return by a query
  (^ ("," (* white-space) integer)
     integer))

(defprod sort-type ()
  (^ (":" (* white-space) word)
     (or (get-sorter word)
         (error 'invalid-query
                :query (query parser)
                :message (format nil "Unrecognised sort type: ~a" word)))))

(defprod sort-field ()
  (^ ((? direction) word)
     (if (validate-field-name parser word)
         (make-sorting parser word (and direction (string= "-" direction))))))

(defprod sort-fields ()
  ("("
   (^ sort-field)                     ; first word in list
   (* (^ ((* white-space) sort-field) ; collect the words, zero or more additional word
         (append (enlist sort-fields) (list sort-field))))
   ")"))                       ; terminating sort field bracket

(defprod sorting ()
  (/ (^ "document" :document-order)
     (^ "docnum" :document-order)
     (^ "score" :score-order)
     (^ ((? direction) word)
        (make-sorting parser word (string= "-" direction)))
     (^ sort-fields)))

(defprod sort-order ()
  (^ ("/" (? direction) sorting)
     ($get-sorting-specification parser sorting direction)))

(defprod direction ()
  (/ (^ "+")
     (^ "-")))

(defprod field-list ()
  (#\[
   (^ field-name)
   (* (^ ((+ white-space) field-name)))
   #\]))                       ; terminating sort field bracket

(defprod field-name ()
  ;; a field-name is a word or wild word
  (/ (^ wild-word
        (let ((fields (compute-multiple-fields parser wild-word)))
          (unless fields
            (error 'no-matching-fields :query (query parser)
                   :field wild-word :fields (field-definitions parser)
                   :message  "No matching field names"))
          ;;(bad-query parser (format nil "No matching field names for: ~a~%Query: ~a" wild-word (query parser))))
          (dolist (name fields)
            (pushnew name (chosen-fields parser) :test #'string=))))
     (^ word
        (pushnew (validate-field-name parser word) (chosen-fields parser) :test #'string=))))

#|
(defprod logical-operator () 
  ;; if any of the logical operators are found, interpret them as occurrence value
  (/ (^ "not" :must-not-occur)
     (^ "and" :must-occur)
     (^ "or" :should-occur)
     (^ "NOT" :must-not-occur)
     (^ "AND" :must-occur)
     (^ "OR" :should-occur)
     (^ "!" :must-not-occur)
     (^ "+" :must-occur)
     (^ "-" :must-not-occur)))
|#

(defprod logical-operator () 
  ;; if any of the logical operators are found, interpret them as occurrence value
  (/ (^ ("not" (+ white-space)) :must-not-occur)
     (^ ("and" (+ white-space)) :must-occur)
     (^ ("or" (+ white-space)) :should-occur)
     (^ ("NOT" (+ white-space)) :must-not-occur)
     (^ ("AND" (+ white-space)) :must-occur)
     (^ ("OR" (+ white-space)) :should-occur)
     (^ "!" :must-not-occur)
     (^ "+" :must-occur)
     (^ "-" :must-not-occur)))

(defprod field-specifier ()
  ;; a field name is a word or wild word followed by a colon making a field specifier
  (/ (^ (word ":")
        (validate-field-name parser word))
     (^ (wild-word ":")
        ($set-query-field parser wild-word))))

(defprod bool-query ()
  ;; a query clause may be followed by a logical operator which is applied to the next clause
  (* (* white-space)
     (^ bool-clause
        ($add-clause parser bool-query bool-clause :should-occur))))

(defprod simple-query () 
  ;; any of the following queries may be followed by a boost factor
  (/ (^ phrase-query)
     (^ wild-query)
     (^ parenthesised-query)
     (^ relational-query)
     (^ field-range-query)
     (^ fuzzy-query)
     (^ quoted-literal)
     (^ term-query)))

(defprod bool-clause ()
  ;; a query clause may be followed by a logical operator which is applied to the next clause
  (^ ((* white-space)
      (? logical-operator)
      (* white-space)
      (? field-specifier)
      simple-query
      (? ("^" number)) ; optional boost factor
      (* white-space))
     (prog1
         ($get-boolean-clause parser simple-query logical-operator)
       ($set-query-boost-factor parser simple-query number)
       (unless (field-stack parser) ($set-query-field parser nil))))) ; reset active field)))

(defprod lower-field-range ()
  ;; a range search consists of a lower and upper value each proceeded by either an inclusive or exclusive indicator
  (/ (^ ("{" word)
        (list word :exclude))
     (^ ("[" word)
        (list word :include))))

(defprod upper-field-range ()
   ;; a value followed by an inclusive indicator
   (/ (^ (word "]")
         (list word :include))
      (^ (word "}")
         (list word :exclude))))

(defprod field-range-query ()
  ;; range of field values, inclusive or exclusive
  (^ (lower-field-range (+ white-space) (/ "to" "TO") (+ white-space) upper-field-range)
     ($get-field-range-query parser lower-field-range upper-field-range)))

(defprod relational-operator () 
  (/ (^ ">" :gt)
     (^ ">=" :ge)
     (^ ">" "=" :ge)
     (^ "<" :lt)
     (^ "<=" :le)
     (^ "<" "=" :le)
     ))

(defprod relational-query ()
  (^ (relational-operator word)
     (case relational-operator
       (:gt ($get-field-range-query parser (list word :exclude) nil))
       (:ge ($get-field-range-query parser (list word :include) nil))
       (:lt ($get-field-range-query parser nil (list word :exclude)))
       (:le ($get-field-range-query parser nil (list word :include))))))

(defprod open-parenthesis ()
  (^ "("
     (push (field parser) (field-stack parser))))

(defprod close-parenthesis ()
  (^ ")"
     (pop (field-stack parser))))

(defprod parenthesised-query ()
  ;; Group queries which may be prefixed by a field name applying to all the enclosed query clauses.
  ;; Enclose a boolean query in brackets, return the query within the brackets.
  (^ (open-parenthesis bool-query close-parenthesis)
     ;; returns the enclosed query as the production rule's value
     ($get-boolean-query parser bool-query)))

(defprod phrase () 
  ;; A phrase consists of one or more words within double quotes.
  ("\""
   (^ word)                                         ; first value of a phrase
   (* (^ ((* white-space) word)                     ; phrase collects the words, zero or more words in addition to initial value 
         ($add-word-to-phrase parser phrase word)))
   "\""))                                           ; final quote marking end phrase

(defprod phrase-query () 
  ;; A phrase query is a sequence of words optionally followed by proximity number.
  (^ (phrase (? ("~" integer))) ;integer)))
     ($get-phrase-query parser phrase integer))) ;word)));integer)))
  
(defprod term-query ()
  ;; If a value is found, generate a term query.
  (^ word
     ($get-term-query parser word)))

(defprod fuzzy-query ()
  ;; A fuzzy query uses the levenshtein-distance function to measure the edit differnce between two strings
  (^ (word "~" (? number))
     ;; a fuzzy query must be value followed by a tilde and which may be followed by a difference factor
     ($get-fuzzy-query parser word number)))

(defprod wild-query ()
  ;; a wild query is executed by finding matching terms in the index and being rewriitten as a query consisting of the matched terms.
  (^ wild-word
     (if (char= #\' (char wild-word 0))
         ($get-regular-expression-term-query parser ($get-active-field parser) (subseq wild-word 1 (1- (length wild-word))))
       ($get-wild-query parser wild-word))))

(defprod number ()
  ;; Parse a number that may be an integer or an integer followed be a decimal point and an integer
  (^ (integer (? "." integer))))

(defun literal-character-p (char) (not (char= #\' char)))

(defchartype literal-character ()
  `(satisfies literal-character-p))

(defprod literal ()
  (* literal-character))

(defprod quoted-literal ()
  (^ (#\' literal #\')
     ($get-literal-query parser literal)))

(defprod integer () (+ digit)) ; one or more digits

(defprod word ()
  (+ (/ non-wild-letter
        character-entity)))

(defprod character-entity ()
  (/ (^ ("&#" integer ";"))
     (^ ("&#x" (+ hexadecimal-digit) ";"))
     (^ ("&" word ";"))))

(defprod wild-word-constituent ()
  (/ (^ any-letter)
     (^ wild-letter)
     (^ character-entity)))

(defprod wild-word ()
  ((* non-wild-letter)
   wild-letter
   (* wild-word-constituent)))

(defchartype reserved-punctuation () '(satisfies reserved-punctuation-p))

(defchartype any-character '(satisfies any-character-p))

(defun any-character-p (char) (characterp char))

(defun whitespace-char-p (char)
  (find char '(#\space #\tab #\newline #\return)))

(defun any-letter-p (char)
  "Predicate for characters that can be used in field specifiers and query values."
  (and (graphic-char-p char) ; common lisp test for characters that are visible glyphs (excludes #\newline)
       (not (whitespace-char-p char))
       (not (reserved-punctuation-p char))))

(defun wildcard-char-p (char)
  "Predicate for wildcard characters: '*' for zero or more characters and '?' for any single character."
  (member char '(#\* #\?)))

(defun not-wildcard-char-p (char) (not (wildcard-char-p char)))

(defun numeric-char-p (char) (or (digit-char-p char) (char= char #\.)))

(defun reserved-punctuation-p (char) (member char punctuation-character))

(defchartype white-space ()
  ;; match a whitespace character
  '(satisfies whitespace-char-p))

(defchartype any-letter '(satisfies any-letter-p))

(defchartype non-wild-letter '(satisfies letter-p))

(defun letter-p (char)
  (and (any-letter-p char)
       (not-wildcard-char-p char)))

(defchartype wild-letter '(satisfies wildcard-char-p))

(defchartype digit () '(satisfies numeric-char-p))

(defun hexadecimal-char-p (char) (digit-char-p char 16))

(defchartype hexadecimal-digit () '(satisfies hexadecimal-char-p))

;(eval-when (:execute)

(defun make-query-parser (parser query-string)
  (parselet
   ((query-parser (^ top-query)))
   (multiple-value-bind (success-p parser-result)
       (query-parser query-string)
     (if success-p
         parser-result
       (error 'invalid-query
         :query (query parser)
         :message "An error condition was raised while parsing this query.")))))

(defmethod montezuma-parse ((parser query-parser) query-string)
  (setf (query parser) query-string)
  (if (string= "" (string-trim "\" " query-string))
      (make-query-parser-result
       parser
       ($get-match-all-query parser)
       :sort-specification ($get-sorting-specification parser :document-order "+"))
    (make-query-parser parser query-string)))
;)

#|
(montezuma-parse (make-instance 'test-query-parser :fields '("f1" "f2")) "john's email is jjwiseman@yahoo.com mail-to")
(montezuma-parse (make-instance 'query-parser :default-field "f1" :fields '("f1" "f2")) "+f1:abc")
(montezuma-parse (make-instance 'test-query-parser :fields '("f1 " "f2")) "\"abc def\"")
(montezuma-parse (make-instance 'test-query-parser :fields '("f1" "f2")) "\"abc . def\"")
(montezuma-parse (make-instance 'query-parser :fields '("f1" "f2")) "+f1:abc")
(montezuma-parse (make-instance 'query-parser :fields '("f1" "f2")) "f1:abc or xxx")
(montezuma-parse (make-instance 'query-parser :fields '("f1" "f2")) "f1:abc or xxx^31")
(montezuma-parse (make-instance 'query-parser :fields '("f1" "f2")) "abc")
(montezuma-parse (make-instance 'query-parser :fields '("f1" "f2")) " blah::blah ")
(annotated-search-results1 etec-corpus "/document")
(annotated-search-results1 etec-corpus "shock horror")
(quick-search oanc-corpus "chemical weapons/score:100" '("author" "title"))
(quick-search oanc-corpus "chemical/-score:0,50[author title]))
(quick-search oanc-corpus "chemic&ast;/-score:0,50[author title]")
(montezuma-parse (make-instance 'query-parser :fields '("f1" "f2")) "*:*foo*")
(montezuma-parse (make-instance 'query-parser :fields '("f1" "f2")) "blah::blah")
(montezuma-parse (make-instance 'query-parser :fields '("f1" "f2")) "blah::boom")
(montezuma-parse (make-instance 'query-parser :analyzer (make-instance 'escape-analyzer) :fields '("f1" "f2")) "blah::boom")
(montezuma-parse (make-instance 'query-parser :analyzer (make-instance 'escape-analyzer) :fields '("f1" "f2")) "f*::boom")
(montezuma-parse (make-instance 'query-parser :analyzer (make-instance 'escape-analyzer) :fields '("f1" "f2")) "f*:boom/f1")
(test-parse-query "blah::bloom")
(montezuma-parse (make-instance 'query-parser :field-definitions '(("f1" :type :date) ("f2"))) "f*:boom/f1[f*]")
(montezuma-parse (make-instance 'query-parser :field-definitions '(("f1" :type :date) ("f2"))) "cat1*\\/su??ub2")
(montezuma-parse (make-instance 'query-parser :field-definitions '(("f1" :type :date) ("f2"))) "cat1\\*suub2")
(montezuma-parse (make-instance 'query-parser :field-definitions '(("f1" :type :date) ("f2"))) "cat1\\/suub2")
(montezuma-parse (make-instance 'query-parser :field-definitions '(("f1" :type :date) ("f2"))) "cat1suub2&#47;&#x27;")
(montezuma-parse (make-instance 'query-parser :field-definitions '(("f1" :type :date) ("f2"))) "cat1suub2&sol;&#x27;")
(montezuma-parse (make-instance 'query-parser :field-definitions '(("f1" :type :date) ("f2"))) "f2:\"boom . bang\"")
(montezuma-parse (make-instance 'query-parser :field-definitions '(("f1" :type :date) ("f2"))) "f1:\"2015\"")
(montezuma-parse (make-instance 'query-parser :field-definitions '(("f1" :type :date) ("f2"))) "f1:\"2015-12\"")
(montezuma-parse (make-instance 'query-parser :field-definitions '(("f1" :type :date) ("f2"))) "f1:\"2015-12-25\"")
(montezuma-parse (make-instance 'query-parser :field-definitions '(("f1" :type :date) ("f2"))) "f1:'2015-12-25'")
(montezuma-parse (make-instance 'query-parser :field-definitions '(("f1" :type :date) ("f2"))) "f1:'2015-12-25'")

(montezuma-parse (make-instance 'query-parser :analyzer (make-instance 'text-analyzer) :fields '("f1" "f2")) "f*:boom/f1")

(rewrite-query pastes-index "heading:*") ; should raise an invalid-query condition
|#

