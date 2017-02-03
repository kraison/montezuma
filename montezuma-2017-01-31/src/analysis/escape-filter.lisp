(in-package #:montezuma)

(defclass escape-filter (lowercase-filter)
  ())

(alexandria:define-constant punctuation
  '((#\. :period)
    (#\: :colon)
    (#\; :semicolon)
    (#\- :hyphen)
    (#\, :comma)
    (#\/ :slash)
    (#\\ :backslash)
    (#\# :hash)
    (#\& :ampersand)
    (#\! :exclamation-mark)
    (#\> :gt)
    (#\< :lt)
    (#\[ :open-square-bracket)
    (#\] :closing-square-bracket)
    (#\{ :open-curly-bracket)
    (#\} :closing-curly-bracket)
    (#\( :open-parenthesis)
    (#\) :closing-parenthesis)
    (#\% :percentage)
    (#\| :vertical-bar)
    (#\& :ampersand)
    (#\~ :tilde)
    (#\@ :at-sign)
    (#\$ :dollar)
    (#\^ :circumflex)
    (#\* :asterisk)
    (#\? :question-mark)
    (#\_ :underscore)
    (#\= :equal)
    (#\+ :plus)
    (#\' :single-quote)
    (#\" :double-quote)
    (#\` :grave)
    (#\space :space)
    (#\newline :newline)
    )
  :test 'equalp)

;;(defun is-punctuation-p (char) (cadr (find char punctuation :key #'car :test #'char=)))
(defun punctuation-type (ch) (cadr (assoc ch punctuation)))

(deftype punctuation-character () '(satisfies punctuation-type))

(defmethod next-token ((self escape-filter))
  (let ((token (call-next-method)))
    (when token
      (let ((image (token-image token)))
        (setf (token-type token)
              (cond
               ((cl-ppcre:scan "^\\W$" image) ; single non-word character
                (or (punctuation-type (char image 0)) :punctuation))
               ((cl-ppcre:scan "(^|[^\\\\])(\\*|\\?)" image) :wildcard-word) ; REA: hard to read, but it does the job: no escaped wildcards
               (t :word)))))
    token))

(defclass escape-analyzer (analyzer)
  ())

(defmethod token-stream ((self escape-analyzer) field language string-or-stream)
  (declare (ignore field language))
  (make-instance 'escape-filter :input (make-instance 'escape-tokenizer :input string-or-stream))) ; was lowercase-tokenizer

(defclass escape-tokenizer (regexp-tokenizer)
  (string-scanner))

(defmethod initialize-instance :after ((self escape-tokenizer) &key)
  (with-slots (string-scanner input) self
    (let ((input-string (if (streamp input) (stream-contents input) input)))
      (setf string-scanner (string-scanner (token-regexp self) input-string)))))

(defmethod next-token ((self escape-tokenizer))
  (multiple-value-bind (term start end)
      (funcall (slot-value self 'string-scanner))
    (if term
	(make-token (normalize self term) start end)
      nil)))

(defmethod token-regexp ((self escape-tokenizer))
  "One or more word characters including non-letter word characters or any escaped character (preceded by '\')."
;  (cl-ppcre:create-scanner "(([\\w'*?@.-])|(\\\\.)+)+|[^\\w ]" :multi-line-mode T))
  (cl-ppcre:create-scanner "(\\w+)|[\\W]" :multi-line-mode T))

#|
(all-tokens (make-instance 'escape-analyzer) t "\\(SKELTON\\) \\ AND JOHN")
(#<TOKEN "\\(skelton\\)" [0-11] :WORD> #<TOKEN "\\ and" [12-17] :WORD> #<TOKEN "john" [18-22] :WORD>)


(all-tokens (make-instance 'standard-analyzer) "field" "john's email is jjwiseman@yahoo.com mail-to")
(#<TOKEN "john" [0-6] :WORD> #<TOKEN "email" [7-12] :WORD> #<TOKEN "is" [13-15] :WORD> #<TOKEN "jjwiseman@yahoo.com" [16-35] :WORD> #<TOKEN "mail" [36-40] :WORD> #<TOKEN "to" [41-43] :WORD>)

(all-tokens (make-instance 'escape-analyzer) "field" "john's email is jjwiseman@yahoo.com mail-to")
(#<TOKEN "john's" [0-6] :WORD> #<TOKEN "email" [7-12] :WORD> #<TOKEN "is" [13-15] :WORD> #<TOKEN "jjwiseman@yahoo.com" [16-35] :WORD> #<TOKEN "mail-to" [36-43] :WORD>)
|#
