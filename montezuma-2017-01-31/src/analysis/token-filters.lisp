(in-package #:montezuma)

(defvar *stop-words-directory* nil)

(defclass token-filter (token-stream)
  ((input :initarg :input)))

(defmethod close-down ((self token-filter))
  (with-slots (input) self
    (close-down input)))

(defclass lowercase-filter (token-filter)
  ())

(defmethod next-token ((self lowercase-filter))
  (with-slots (input) self
    (let ((token (next-token input)))
      (when token
	(setf (token-image token) (nstring-downcase (token-image token))))
      token)))

(defclass stop-filter (token-filter)
  ((stop-set :initarg :stop-set :initform '())
   (stop-words-table :initform (make-hash-table :test #'equal))))

(defmethod initialize-instance :after ((self stop-filter) &key file language)
  (with-slots (stop-set stop-words-table) self
    (unless stop-set
      (setf stop-set
            (cond
             (file
              (load-word-list file))
             (t
              (unless language
                (break "Language is required to create a stop-filter!"))
              (load-language-stop-words language)))))
    (dolist (w stop-set)
      (setf (gethash (string-downcase w) stop-words-table) t))))

(defun load-word-list (path)
  (cl-ppcre:split "\\s+" (file-contents path)))

(defun load-language-stop-words (language)
  (cond ((not *stop-words-directory*)
         (warn "*STOP-WORDS-DIRECTORY* is NIL, no words loaded"))
        (t
         (let ((stop-words-file (make-pathname :name language
                                               :type "txt"
                                               :defaults *stop-words-directory*)))
           (if (probe-file stop-words-file)
               (load-word-list stop-words-file)
               (warn "Stop words file ~A does not exist, no words loaded"
                     stop-words-file))))))

(defclass text-filter (stop-filter)
  ())

(defclass query-filter (stop-filter)
  ())

(defmethod next-token ((self query-filter))
  "identify token type"
  (with-slots (input) self
    (let ((token (next-token input)))
      ;;(format t "next-token ~s~%" token)
      (if token
          (with-slots (image type) token
            (let ((punc (punctuation-character image)))

              (setf type
                    (cond
                     ((eq punc :space)
                      (return-from next-token (next-token self)))
                     ((is-stop-word self (string-downcase image))
                      :stopword)
                     ((lettersp image)
                      :word)
                     ((numericp image)
                      :number)
                     (t
                      punc)))

              (unless punc (setf image (string-downcase image))))))
      token)))

(defmethod is-stop-word ((self stop-filter) (image string))
  (with-slots (stop-words-table) self
    (gethash image stop-words-table)))

(defun numericp (str)
  (and (stringp str)
       (> (length str) 0) 
       (not (find-if-not #'digit-char-p str))))

(defun lettersp (str)
  (and (stringp str)
       (string/= "" str);;> (length str) 0) 
       (not (find-if-not #'alpha-char-p str))))

(defun punctuation-character (str)
  (and (stringp str)
       (= (length str) 1)
       (punctuation-type (char str 0))))

(defmethod sentence-terminatorp ((token token))
  (find (token-type token) '(:period :question-mark :exclamation-mark :semicolon)))

(defmethod is-white-space ((token token))
  (find (token-type token) '(:newline :space)))

(defmethod next-token ((self text-filter))
  "identify token type"
  (with-slots (input) self
    (let ((token (next-token input)))
      (if token
          (with-slots (image type) token
            (setf type
                  (cond
                   ((is-stop-word self (string-downcase image))
                    :stopword)
                   ((lettersp image)
                    :word)
                   ((numericp image)
                    :number)
                   (t
                    (punctuation-character image))))))
      token)))

(defmethod next-token ((self stop-filter))
  "skip any of stop words"
  (with-slots (input) self
    (let ((token (next-token input)))
      (if (or (null token)
	      (not (is-stop-word self (token-image token)))) ; (member (token-image token) stop-set :test #'string=)))
	  token
        (next-token self)))))

(defclass operators-filter (token-filter)
  ((operators :initarg :operators :initform '())))

(defmethod next-token ((self operators-filter))
  (with-slots (input operators) self
    (let ((token (next-token input)))
      (when token
	(if (member (token-image token) operators :test #'string=)
            (setf (token-type token) :operator)))
      token)))

(defclass porter-stem-filter (token-filter)
  ())

(defmethod next-token ((self porter-stem-filter))
  (with-slots (input) self
    (let ((token (next-token input)))
      (when token
	(setf (token-image token) (stem (nstring-downcase (token-image token)))))
      token)))

#|
(all-tokens (make-instance 'text-analyzer) "field" "John's email is 43 jjwiseman@yahoo.com mail-to
killer on#\tab
the run")
|#
