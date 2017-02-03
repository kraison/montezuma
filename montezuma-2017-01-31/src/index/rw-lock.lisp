(in-package #:montezuma)

;; The following queueing code was borrowed and adapted from Russell &
;; Norvig's "Introduction to AI"
(defstruct (queue
             (:print-function
              (lambda (q stream depth)
                (declare (ignore depth))
                (format stream "<QUEUE: ~a>" (queue-elements q)))))
  (key #'identity)
  (last nil)
  (elements nil))

(defun make-empty-queue () (make-queue))

(defun empty-queue-p (q)
  (= (length (queue-elements q)) 0))

(defun queue-front (q)
  (elt (queue-elements q) 0))

(defun dequeue (q)
  (when (listp (queue-elements q))
    (pop (queue-elements q))))

(defun enqueue-front (q &rest items)
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-elements q) (nconc items (queue-elements q))
               (queue-last q) (last (queue-elements q))))
        (t (setf (queue-elements q) (nconc items (queue-elements q))))))

(defun enqueue (q &rest items)
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-last q) (last items)
               (queue-elements q) (nconc (queue-elements q) items)))
        (t (setf (cdr (queue-last q)) items
                 (queue-last q) (last items)))))

(defun queue-length (q)
  (length (queue-elements q)))
;; End of adapted code

(defstruct (rw-lock
	     (:conc-name lock-)
	     (:print-function
              (lambda (lock stream depth)
                (declare (ignore depth))
                (print-unreadable-object (lock stream :type t :identity t)
                  (format stream "W: ~A, R: ~A"
                          (lock-writer lock) (lock-readers lock)))))
	     (:predicate rw-lock-p))
  (lock (bordeaux-threads:make-recursive-lock))
  (readers 0 :type integer)
  (semaphore (bt-semaphore:make-semaphore))
  (writer-queue (make-empty-queue) :type queue)
  (writer nil)
  #+sbcl
  (waitqueue (sb-thread:make-waitqueue) :type sb-thread:waitqueue)
  #+lispworks
  (waitqueue (mp:make-condition-variable)))

(defun next-in-queue-p (rw-lock thread)
  (bordeaux-threads:with-recursive-lock-held ((lock-lock rw-lock))
    (and (not (empty-queue-p (lock-writer-queue rw-lock)))
	 (eq thread (queue-front (lock-writer-queue rw-lock))))))

(defun lock-unused-p (rw-lock)
  (bordeaux-threads:with-recursive-lock-held ((lock-lock rw-lock))
    (and (= 0 (lock-readers rw-lock))
	 (= 0 (bt-semaphore:semaphore-count (lock-semaphore rw-lock)))
	 (null (lock-writer rw-lock))
	 (empty-queue-p (lock-writer-queue rw-lock)))))

(defun release-read-lock (rw-lock)
  (bordeaux-threads:with-recursive-lock-held ((lock-lock rw-lock))
    (assert (not (eql 0 (lock-readers rw-lock))))
    (when (eql 0 (decf (lock-readers rw-lock)))
      (when (lock-writer rw-lock)
        (bt-semaphore:signal-semaphore (lock-semaphore rw-lock))))))

(defun acquire-read-lock (rw-lock &key (max-tries 1000))
  (loop for tries from 0 to max-tries do
       (bordeaux-threads:with-recursive-lock-held ((lock-lock rw-lock))
         (if (lock-writer rw-lock)
             #+sbcl
             (sb-thread:condition-wait (lock-waitqueue rw-lock)
                                       (lock-lock rw-lock))
             #+lispworks
             (mp:condition-variable-wait (lock-waitqueue rw-lock)
                                         (lock-lock rw-lock))
             (progn
               (incf (lock-readers rw-lock))
               (return-from acquire-read-lock rw-lock))))))

(defmacro with-read-lock ((rw-lock) &body body)
  `(unwind-protect
	(if (rw-lock-p (acquire-read-lock ,rw-lock))
	    (progn ,@body)
	    (error "Unable to get read-lock: ~A" ,rw-lock))
     (release-read-lock ,rw-lock)))

(defun release-write-lock (rw-lock &key reading-p)
  (bordeaux-threads:with-recursive-lock-held ((lock-lock rw-lock))
    (if (next-in-queue-p rw-lock (bordeaux-threads:current-thread))
        (dequeue (lock-writer-queue rw-lock))
	(error "Cannot release lock I don't own!"))
    (if (next-in-queue-p rw-lock (bordeaux-threads:current-thread))
	nil ;; Not releasing lock;  recursive ownership detected!
	(progn
	  (setf (lock-writer rw-lock) nil)
	  (when reading-p
	    (incf (lock-readers rw-lock)))
          #+sbcl
	  (sb-thread:condition-broadcast (lock-waitqueue rw-lock))
          #+lispworks
	  (mp:condition-variable-broadcast (lock-waitqueue rw-lock))))))

(defun acquire-write-lock (rw-lock &key (max-tries 1000) reading-p (wait-p t))
  (bordeaux-threads:with-recursive-lock-held ((lock-lock rw-lock))
    (cond ((and (next-in-queue-p rw-lock (bordeaux-threads:current-thread))
                (eq (lock-writer rw-lock) (bordeaux-threads:current-thread)))
           (enqueue-front (lock-writer-queue rw-lock)
                          (bordeaux-threads:current-thread))
           (return-from acquire-write-lock rw-lock))
          (wait-p
           (enqueue (lock-writer-queue rw-lock) (bordeaux-threads:current-thread)))
          (t
           (if (lock-unused-p rw-lock)
               (progn
                 (enqueue (lock-writer-queue rw-lock)
                          (bordeaux-threads:current-thread))
                 (setf (lock-writer rw-lock)
                       (bordeaux-threads:current-thread))
                 (when reading-p
                   (decf (lock-readers rw-lock)))
                 (return-from acquire-write-lock rw-lock))
               (return-from acquire-write-lock nil)))))
  (loop for tries from 0 to max-tries do
     (if (eq (lock-writer rw-lock) (bordeaux-threads:current-thread))
         (return-from acquire-write-lock rw-lock)
         (let ((internal-wait-p nil))
           (handler-case
               (bordeaux-threads:with-recursive-lock-held ((lock-lock rw-lock))
                 (if (and (null (lock-writer rw-lock))
                          (next-in-queue-p rw-lock
                                           (bordeaux-threads:current-thread)))
                     (progn
                       (setf (lock-writer rw-lock)
                             (bordeaux-threads:current-thread))
                       (when reading-p
                         (decf (lock-readers rw-lock)))
                       (unless (eql 0 (lock-readers rw-lock))
                         (setf internal-wait-p t)))
                     #+sbcl
                     (sb-thread:condition-wait
                      (lock-waitqueue rw-lock) (lock-lock rw-lock))
                     #+lispworks
                     (mp:condition-variable-wait
                      (lock-waitqueue rw-lock) (lock-lock rw-lock))))
             (error (c)
               (error c)))
           (when internal-wait-p
             (bt-semaphore:wait-on-semaphore (lock-semaphore rw-lock)))))))

(defmacro with-write-lock ((rw-lock &key reading-p) &body body)
  `(unwind-protect
	(if (rw-lock-p (acquire-write-lock ,rw-lock :reading-p ,reading-p))
	    (progn ,@body)
	    (error "Unable to get rw-lock: ~A" ,rw-lock))
     (release-write-lock ,rw-lock :reading-p ,reading-p)))

#|
(defun test-rw-locks ()
  (let ((lock (make-rw-lock)))
    (bordeaux-threads:make-thread
     #'(lambda () (with-write-lock (lock)
		    (format t "1 got write lock.  Sleeping.~%")
		    (sleep 5)
		    (with-write-lock (lock)
		      (format t "1 acquired recursive lock.~%")
		      (sleep 5)
		      (with-write-lock (lock)
			(format t "1 acquired recursive lock.~%")
			(sleep 5)
			(format t "1 releasing recursive write lock.~%"))
		      (format t "1 releasing recursive write lock.~%"))
		    (format t "1 releasing write lock.~%"))))
    (bordeaux-threads:make-thread
     #'(lambda () (with-read-lock (lock) (format t "2 got read lock~%") (sleep 5))))
    (bordeaux-threads:make-thread
     #'(lambda () (with-read-lock (lock) (format t "3 got read lock~%") (sleep 5))))
    (bordeaux-threads:make-thread
     #'(lambda () (with-write-lock (lock)
		    (format t "4 got write lock.  Sleeping.~%")
		    (sleep 5)
		    (with-write-lock (lock)
		      (format t "4 acquired recursive lock.~%")
		      (sleep 5)
		      (with-write-lock (lock)
			(format t "4 acquired recursive lock.~%")
			(sleep 5)
			(format t "4 releasing recursive write lock.~%"))
		      (format t "4 releasing recursive write lock.~%"))
		    (format t "4 releasing write lock.~%"))))
    (bordeaux-threads:make-thread
     #'(lambda () (with-write-lock (lock)
		    (format t "5 got write lock.  Sleeping.~%")
		    (sleep 5)
		    (format t "5 releasing write lock.~%"))))
    (bordeaux-threads:make-thread
     #'(lambda () (with-read-lock (lock) (format t "6 got read lock~%") (sleep 5))))
    (bordeaux-threads:make-thread
     #'(lambda () (with-read-lock (lock) (format t "7 got read lock~%") (sleep 5))))))
|#
