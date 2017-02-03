(in-package #:montezuma)

(defstruct (token
	     (:constructor make-token (image start end &key (increment 1) (type :word)))
             (:print-object print-token))
  image
  start
  end
  (increment 1)
  (type :word))

(defun print-token (object stream)
  (format stream "#<TOKEN ~s [~d-~d] ~s>" 
          (token-image object) (token-start object) (token-end object) (token-type object)))

(defun token-compare (t1 t2)
  (let ((r (- (token-start t2) (token-start t1))))
    (if (not (= r 0))
	r
	(let ((r (- (token-end t2) (token-end t1))))
	  (if (not (= r 0))
	      r
	      (string-compare (token-image t1) (token-image t2)))))))

(defun token= (t1 t2)
  (= 0 (token-compare t1 t2)))


(defgeneric term-text (token))

(defmethod term-text ((token token))
  (token-image token))
