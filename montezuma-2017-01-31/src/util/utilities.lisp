(in-package #:montezuma)

(declaim (inline length-1-list-p))
(defun length-1-list-p (x) 
  "Is x a list of length 1? Note that this is better than the naive \(= \(length x\) 1\) because we don't need to traverse the entire list..."
  (and (consp x) (null (cdr x))))

;;; ---------------------------------------------------------------------------
;;; each method
;;; ---------------------------------------------------------------------------

(defgeneric each (sequence function))

(defmethod each ((self list) block)
  (mapc block self))

(defun make-vector ()
  (make-array 0
              :element-type 'character
              :fill-pointer 0
              :adjustable t))

(defun parse-float (p)
  (if (floatp p)
      p
    (with-standard-io-syntax
      (let ((f (read-from-string p)))
        (unless (floatp f)
          (error "~s is not a FLOAT" p))
        f))))

(defun parse-fixnum (x)
  (if (integerp x)
      x
    (with-standard-io-syntax
      (let ((i (read-from-string x)))
        (unless (integerp i)
          (error "~s is not an INTEGER" x))
        i))))

(defun parse-number (x)
  (if (numberp x)
      x
    (with-standard-io-syntax
      (let ((y (read-from-string x)))
        (unless (numberp y)
          (error "~s is not a NUMBER" x))
        y))))

(defun starts-with (word prefix)
  (if (>= (length word) (length prefix))
      (string-equal word prefix :start1 0 :start2 0 :end1 (length prefix))))

#|
(defmacro enlist (x)
  "Return X if it is a list, otherwise wrap x in a new list."
  `(if (listp ,x) ,x (list ,x)))
|#

(defun enlist (x)
  "Return X if it is a list, otherwise wrap x in a new list."
  (if (listp x) x (list x)))


(defun binary-search (a value &key (key nil) (nearest t))
  (let ((low 0)
        (mid 0)
        (high (1- (length a))))
  ;  (format t "low ~A, high ~a~%" low high)
    (loop while (<= low high) do
          (setf mid (truncate (+ low high) 2))
          (let* ((rec (aref a mid))
                 (str (if key (funcall key rec) rec)))
   ;         (format t "low ~A, high ~a mid ~a ~a~%" low high mid str)
            (if (string-greaterp str value)
                (setf high (1- mid))
              (if (string-lessp str value)
                  (setf low (1+ mid))
                (return mid)))))
    ;;(format t "mid: ~a~%" mid)
    (cond
     (nearest
      (loop for rec = (aref a mid) 
            while (string-lessp (if key (funcall key rec) rec) value) do
            (incf mid)
        ;  (format t "~a ~a~%" mid (aref a mid))
            )
      (values (aref a mid) mid))
     (t 
      (let ((str (aref a mid)))
        (if (string-equal str value)
            (values str mid)))))))
