(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf)
)

;; for backwards compatibility. When all cl-user references have been changed these parameter coul;d be deleted
(defparameter *this-file* #.(or *compile-file-pathname* *load-pathname*))
(defparameter *working-directory* (make-pathname :defaults *this-file* :type nil :name nil :version nil))
(defparameter *dependencies* (merge-pathnames "../dependents/" *working-directory*))
(defparameter *montezuma-root* *working-directory*)
(defparameter *corpus-root* (merge-pathnames "/corpus/" *working-directory*))
(defparameter *etec-corpus* (merge-pathnames "/corpus/etec/" *corpus-root*))
(defparameter *stop-words-directory* (merge-pathnames "../stop-words/" *working-directory*))
(defparameter *stop-words* *stop-words-directory*)

(defpackage :montezuma
  (:use
   :common-lisp))

(in-package :montezuma)

(defparameter *this-file* #.(or *compile-file-pathname* *load-pathname*))
(defparameter *working-directory* (make-pathname :defaults *this-file* :type nil :name nil :version nil))
(defparameter *dependencies* (merge-pathnames "../dependents/" *working-directory*))
(defparameter *montezuma-root* *working-directory*)
(defparameter *corpus-root* (merge-pathnames "/corpus/" *working-directory*))
(defparameter *etec-corpus* (merge-pathnames "/corpus/etec/" *corpus-root*))
(defparameter *stop-words-directory* (merge-pathnames "../stop-words/" *working-directory*))
(defparameter *stop-words* *stop-words-directory*)

(defparameter *pastes-index-pathname*
  (merge-pathnames "contrib/pastes-1000/pasteindex/" *working-directory*)
  "The path at which to store the pastes index.")

;;(defparameter *pastes* (merge-pathnames "../paste-search" *pastes-index-pathname*))

(dolist (p '(*this-file* *working-directory* *dependencies* *montezuma-root* *corpus-root* *stop-words* *pastes-index-pathname*))
  (format t "; ~a ~s~%" p (probe-file (symbol-value p))))


(defvar *systems* ())

(defparameter systems
  `(
    (:cl-ppcre "cl-ppcre/cl-ppcre-2.0.10/cl-ppcre.asd"
               "http://weitz.de/files/cl-ppcre.tar.gz") ; reqd by montezuma

    (:alexandria "alexandria/alexandria-20140826-git/alexandria.asd"
     "http://common-lisp.net/~loliveira/tarballs/inofficial/alexandria-2008-07-29.tar.gz") ; reqd by babel

    (:trivial-gray-streams "trivial-gray-streams/trivial-gray-streams-master/trivial-gray-streams.asd"
     "https://github.com/trivial-gray-streams/trivial-gray-streams") ; reqd by diff

    (:diff "froydnj-diff/diff-master/diff.asd" "https://github.com/froydnj/diff")

    (:bordeaux-threads "bordeaux-threads/bordeaux-threads-0.8.3/bordeaux-threads.asd"
     "https://gitlab.common-lisp.net/bordeaux-threads/bordeaux-threads") ; reqd by cl-fad

    (:cl-fad "cl-fad/cl-fad-0.7.3/cl-fad.asd" "http://weitz.de/files/cl-fad.tar.gz")

    (:trivial-features "trivial-features/trivial-features_0.8/trivial-features.asd"
     "https://github.com/trivial-features/trivial-features") ; reqd by babel

    (:babel "babel/babel_0.5.0/babel.asd" "https://github.com/cl-babel/babel")

    (:local-time "local-time/local-time-1.0.5/local-time.asd" "https://common-lisp.net/project/local-time/")

    (:split-sequence "split-sequence/split-sequence.asd" "http://quickdocs.org/split-sequence/")

    (:bt-semaphore "bt-semaphore/bt-semaphore-20131003-git/bt-semaphore.asd" "http://quickdocs.org/bt-semaphore/")

    #-:COMMON-LISPWORKS (:usocket "usocket/usocket-0.6.4/usocket.asd" "http://quickdocs.org/usocket/")

    (:trivial-garbage "trivial-garbage/trivial-garbage_0.20/trivial-garbage.asd" "https://common-lisp.net/project/trivial-garbage/")

    (:porter-stemmer "porter-stemmer/porter-stemmer.asd" "http://tartarus.org/martin/PorterStemmer/")

    (:montezuma "../montezuma-2017-01-31/montezuma.asd" "https://sourceforge.net/projects/montezuma/files/")

    ))

(defun loader (name relative-path url)
  (declare (ignore url))
  (unless (find name *systems*)
    (let ((path (merge-pathnames relative-path *dependencies*)))
      (format t "; Loading ~s: ~a~%" name path)
      (unless (probe-file path) (error "system definition file can't be found: ~a" path))
      (let ((*package* (find-package :asdf-user)))
        (load path))
      (asdf::compile-system name)
      (asdf::load-system name)
      (pushnew name *systems*))))

(dolist (system systems) (apply #'loader system))

(defun test-montezuma ()
  (asdf:oos 'asdf:test-op '#:montezuma))

;;(montezuma:make-montezuma-host)

#|
(diff:format-diff 'diff:UNIFIED-DIFF "D:\\dependencies\\montezuma\\montezuma-mods\\src\\index\\index.lisp" "D:\\dependencies\\montezuma-master\\montezuma-2017-01-31\\src\\index\\index.lisp")
|#
