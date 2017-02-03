(in-package #:montezuma)

(defclass analyzer ()
  ())

(defgeneric token-stream (analyzer field language string-or-stream))

(defmethod token-stream ((self analyzer) field language string-or-stream)
  (declare (ignore field language))
  (make-instance 'lowercase-tokenizer :input string-or-stream))

(defgeneric position-increment-gap (analyzer field-name))

(defmethod position-increment-gap ((self analyzer) field-name)
  (declare (ignore field-name))
  0)

(defgeneric all-tokens (analyzer field language string))

(defmethod all-tokens ((self analyzer) field language data)
  (when (not (stringp data)) (return-from all-tokens data))
  (let ((token-stream (token-stream self field language data)))
    (do ((token (next-token token-stream) (next-token token-stream))
	 (tokens '() (cons token tokens)))
	((null token) (reverse tokens)))))

(defclass whitespace-analyzer (analyzer)
  ())

(defmethod token-stream ((self whitespace-analyzer) field language string-or-stream)
  (declare (ignore field language))
  (make-instance 'whitespace-tokenizer :input string-or-stream))
#|
(defparameter *english-stop-words*
  '("a" "about" "also" "am" "an" "and" "any" "are" "as" "at" "be" "but" "by" "do" "does" "for" "either"
    "from" "has" "hath" "have" "he" "her" "him" "his"
    "himself" "i" "if" "in" "into" "is" "it" "let" "may" "me" "my" "no" "not" "of" "on" "or" "our"
    "s" "shall" "so" "she" "should" "such" "t" "than" "that" "the" "their" "then" "there" "them" "these"
    "thee" "they" "this" "thou" "through" "to" "together" "under" "unto" "up" "upon" "us"
    "we" "was" "were" "what" "when" "which" "who" "will" "with" "without" "would" "ye" "you"))
|#

#|
(defun english-stop-words ()
  (let ((stop-words-text (file-contents (merge-pathnames cl-user::*montezuma-root* "stop-words/english.txt"))))
    (cl-ppcre:split "\\s+" stop-words-text)))
|#

(defclass stop-analyzer (analyzer)
  ((stop-words :initarg :stop-words :initform () :accessor stop-words)
   (language :initarg language :initform nil :accessor language))
  ;;(:default-initargs 
  ;; :stop-words (english-stop-words))) ; *english-stop-words*))
  )

(defmethod initialise-instance :after ((self stop-analyzer) &rest args &key language stop-words &allow-other-keys)
  (unless (or stop-words language)
    (setf (language self) "english"))
    ;;(break "Either a stop words set or a Language is required to create a stop-analyzer"))
  )

(defmethod token-stream ((self stop-analyzer) field language string-or-stream)
  (declare (ignore field))
  (make-instance 'stop-filter
                 :input (make-instance 'lowercase-tokenizer :input string-or-stream)
                 :stop-set (stop-words self)
                 :language language))

(defclass text-analyzer (analyzer)
  ())

(defmethod token-stream ((self text-analyzer) field language string-or-stream)
  (declare (ignore field))
  (make-instance 'text-filter
                 :input (make-instance 'text-tokenizer :input string-or-stream)
                 :language language))

(defclass query-analyzer (analyzer)
  ())

(defmethod token-stream ((self query-analyzer) field language string-or-stream)
  (declare (ignore field))
  (make-instance 'query-filter
                 :input (make-instance 'text-tokenizer :input string-or-stream)
                 :language language))

(defclass standard-analyzer (stop-analyzer)
  ())

(defmethod token-stream ((self standard-analyzer) field language string-or-stream)
  (declare (ignore field))
  (make-instance 'stop-filter
		 :input (make-instance 'lowercase-filter
				       :input (make-instance 'standard-tokenizer
							     :input string-or-stream))
                 :language language))


(defclass per-field-analyzer-wrapper (analyzer)
  ((default-analyzer :initarg :default-analyzer)
   (analyzers :initform (make-hash-table :test #'equal))))

(defmethod token-stream ((self per-field-analyzer-wrapper) field language string-or-stream)
  (with-slots (analyzers default-analyzer) self
    (let ((analyzer (gethash field analyzers default-analyzer)))
      (token-stream analyzer field language string-or-stream))))

(defclass operators-analyzer (analyzer)
  ((operators :initarg :operators)))

(defmethod token-stream ((self operators-analyzer) field language string-or-stream)
  (declare (ignore field))
  (with-slots (operators) self
    (make-instance 'operators-filter
		   :input (make-instance 'lowercase-tokenizer :input string-or-stream)
		   :operators operators)))

(defclass porter-analyzer (standard-analyzer)
  ())

(defmethod token-stream ((self porter-analyzer) field language string-or-stream)
  (declare (ignore field))
  (make-instance 'porter-stem-filter :input (make-instance 'word-tokenizer :input string-or-stream)))

#|
(all-tokens (make-instance 'text-analyzer) "test" "english" "skelton and john")
(all-tokens (make-instance 'text-analyzer) "test" "english" "Skelton AND John")
(#<TOKEN "skelton" [0-7] :WORD> #<TOKEN " " [7-8] :SPACE> #<TOKEN "and" [8-11] :STOPWORD> #<TOKEN " " [11-12] :SPACE> #<TOKEN "john" [12-16] :WORD>)

(all-tokens (make-instance 'operators-analyzer :operators '("and" "or" "not" "to")) "test" "skelton and john")
 > (#S(TOKEN :IMAGE "skelton" :START 0 :END 7 :INCREMENT 1 :TYPE :WORD) #S(TOKEN :IMAGE "and" :START 8 :END 11 :INCREMENT 1 :TYPE :OPERATOR) #S(TOKEN :IMAGE "john" :START 12 :END 16 :INCREMENT 1 :TYPE :WORD))

(montezuma-parse (make-instance 'query-parser :default-field "text" :analyzer (make-instance 'operators-analyzer :operators '("and" "or" "not" "to"))) "skelton and john")
#<BOOLEAN-QUERY with 3 clauses: #<BOOLEAN-CLAUSE
                                :SHOULD-OCCUR
                                #<TERM-QUERY "text":"skelton"^1.0 2009DE23>> #<BOOLEAN-CLAUSE
                                :SHOULD-OCCUR
                                #<TERM-QUERY "text":"and"^1.0 2009B863>> #<BOOLEAN-CLAUSE
                                :SHOULD-OCCUR
                                #<TERM-QUERY "text":"john"^1.0 2009929B>>>

(all-tokens (make-instance 'standard-analyzer) "field" "john's email is jjwiseman@yahoo.com mail-to")
(all-tokens (make-instance 'escape-analyzer) "field" "john's email is jjwiseman@yahoo.com mail-to")

(all-tokens (make-instance 'standard-analyzer) "field" "*foo*")
(all-tokens (make-instance 'escape-analyzer) "field" "*foo*")

(all-tokens (make-instance 'porter-analyzer) "f1" "breath Breathes BreatHed BREATHING")

(all-tokens (make-instance 'operators-analyzer :operators '("and" "or" "not" "to")) "test" "skelton and john")
(#S(TOKEN :IMAGE "skelton" :START 0 :END 7 :INCREMENT 1 :TYPE :WORD) #S(TOKEN :IMAGE "and" :START 8 :END 11 :INCREMENT 1 :TYPE :OPERATOR) #S(TOKEN :IMAGE "john" :START 12 :END 16 :INCREMENT 1 :TYPE :WORD))
|#

