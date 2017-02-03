(in-package #:montezuma)

(defclass trivial-hit-queue (priority-queue)
  ())

(defmethod initialize-instance :after ((queue trivial-hit-queue) &rest args &key &allow-other-keys)
  (with-slots (predicate) queue
    (unless (and predicate (functionp predicate))
      (setf predicate #'(lambda(hit1 hit2) (> (doc hit1) (doc hit2)))))))
