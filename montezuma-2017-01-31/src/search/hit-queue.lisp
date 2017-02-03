(in-package #:montezuma)

(defclass hit-queue (priority-queue)
  ())

(defmethod initialize-instance :after ((queue hit-queue) &rest args &key &allow-other-keys)
  (with-slots (predicate) queue
    (if (or (null predicate) (not (functionp predicate)))
        (setf predicate
              #'(lambda(hit1 hit2)
                  (if (= (score hit1) (score hit2))
                      (> (doc hit1) (doc hit2))
                    (< (score hit1) (score hit2))))))))
