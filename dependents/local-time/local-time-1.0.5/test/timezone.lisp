(in-package #:local-time.test)

(defsuite* (timezone :in test))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (local-time::define-timezone eastern-tz
      (merge-pathnames #p"EST5EDT" local-time::*default-timezone-repository-path*)))



(deftest transition-position/correct-position ()
  (let ((cases '((0 #(1 2 3 4 5) 0)
                 (1 #(1 2 3 4 5) 0)
                 (2 #(1 2 3 4 5) 1)
                 (3 #(1 2 3 4 5) 2)
                 (4 #(1 2 3 4 5) 3)
                 (5 #(1 2 3 4 5) 4)
                 (1 #(1 3 5) 0)
                 (2 #(1 3 5) 0)
                 (3 #(1 3 5) 1)
                 (4 #(1 3 5) 1)
                 (5 #(1 3 5) 2)
                 (6 #(1 3 5) 2)
                 (1 #(1 3 5 7) 0)
                 (2 #(1 3 5 7) 0)
                 (3 #(1 3 5 7) 1)
                 (4 #(1 3 5 7) 1)
                 (5 #(1 3 5 7) 2)
                 (6 #(1 3 5 7) 2)
                 (7 #(1 3 5 7) 3)
                 (8 #(1 3 5 7) 3)
                 )))
    (dolist (case cases)
      (destructuring-bind (needle haystack want)
          case
        (let ((got (local-time::transition-position needle haystack)))
          (is (= got want)
              "(transition-position ~a ~a) got ~a, want ~a"
              needle haystack got want))))))

(deftest test/timezone/decode-timestamp-dst ()
  ;; Testing DST calculation with a known timezone
  (let ((test-cases '(
                      ;; Spring forward
                      ((2008 3 9 6 58) (2008 3 9 1 58))
                      ((2008 3 9 6 59) (2008 3 9 1 59))
                      ((2008 3 9 7  0) (2008 3 9 3  0))
                      ((2008 3 9 7  1) (2008 3 9 3  1))
                      ;; Fall back
                      ((2008 11 2 5 59) (2008 11 2 1 59))
                      ((2008 11 2 6  0) (2008 11 2 1  0))
                      ((2008 11 2 6  1) (2008 11 2 1  1)))))
    (dolist (test-case test-cases)
      (is (equal 
         (let ((timestamp
                (apply 'local-time:encode-timestamp
                       `(0 0 ,@(reverse (first test-case)) :offset 0))))
           (local-time:decode-timestamp timestamp :timezone eastern-tz))
         (apply 'values `(0 0 ,@(reverse (second test-case)))))))))
