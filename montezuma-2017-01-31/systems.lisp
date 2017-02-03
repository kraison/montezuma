(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf)
)

(defparameter *this-file* (princ-to-string (load-time-value (or #.*compile-file-pathname* *load-pathname*))))
(defparameter *filename* (format () "~a.~a" (pathname-name *this-file*) (pathname-type *this-file*)))
(defparameter *root* (subseq *this-file* 0 3))
(defparameter *working-directory* (subseq *this-file* 0 (search *filename* *this-file*)))
(defparameter *dependencies* (merge-pathnames "../dependents/" *working-directory*))
(defparameter *montezuma-root* *working-directory*) ;(merge-pathnames "../montezuma-reanz59/" *dependencies*))
(defparameter *corpus-root* (merge-pathnames "/corpus/" *root*))
(defparameter *etec-corpus* (merge-pathnames "/corpus/etec/" *root*))

(defparameter *stop-words* (merge-pathnames "../stop-words/" *working-directory*))

(defparameter *pastes-index-pathname*
  (merge-pathnames "contrib/pastes-1000/pasteindex/" *working-directory*)
  "The path at which to store the pastes index.")

;;(defparameter *pastes* (merge-pathnames "../paste-search" *pastes-index-pathname*))

(dolist (p '(*this-file* *filename* *working-directory* *root* *dependencies* *corpus-root* *pastes-index-pathname*))
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

    ;;(:diff "froydnj-diff/diff-master/diff.asd" "https://github.com/froydnj/diff")

    (:bordeaux-threads "bordeaux-threads/bordeaux-threads-0.8.3/bordeaux-threads.asd"
     "https://gitlab.common-lisp.net/bordeaux-threads/bordeaux-threads") ; reqd by cl-fad

    (:cl-fad "cl-fad/cl-fad-0.7.3/cl-fad.asd" "http://weitz.de/files/cl-fad.tar.gz")

    (:trivial-features "trivial-features/trivial-features_0.8/trivial-features.asd"
     "https://github.com/trivial-features/trivial-features") ; reqd by babel

    (:babel "babel/babel_0.5.0/babel.asd" "https://github.com/cl-babel/babel")

    (:local-time "local-time/local-time-1.0.5/local-time.asd" "https://common-lisp.net/project/local-time/")

    ;;(:uiop "uiop/uiop-3.1.4/uiop.asd" "http://quickdocs.org/uiop/api")

    ;;(:xmls "xmls/xmls-1.7.1/xmls.asd" "http://www.cliki.net/XMLS")

    ;;(:trivial-backtrace "trivial-backtrace/trivial-backtrace-20120909-git/trivial-backtrace.asd"
    ;; "http://quickdocs.org/trivial-backtrace/")

    ;;(:rfc2388 "RFC2388/rfc2388/rfc2388.asd" "https://common-lisp.net/project/rfc2388/")

    ;;(:md5 "MD5/md5-2.0.2/md5.asd" "http://www.cliki.net/CL-MD5#")

    #-:COMMON-LISPWORKS (:usocket "usocket/usocket-0.6.4/usocket.asd" "http://quickdocs.org/usocket/")

    ;;(:flexi-streams "flexi-streams/flexi-streams-1.0.15/flexi-streams.asd" "http://weitz.de/files/flexi-streams.tar.gz")

    ;;(:cl-base64 "cl-base64/cl-base64-3.3.3/cl-base64.asd" "http://files.b9.com/cl-base64/cl-base64-latest.tar.gz")

    ;;(:chunga "chunga/chunga-1.1.6/chunga.asd" "http://weitz.de/files/chunga.tar.gz")

    ;;(:chipz "chipz/chipz-master/chipz.asd" "https://github.com/froydnj/chipz")

    ;;(:puri "puri/puri-1.5.5/puri.asd" "http://quickdocs.org/puri/")

    ;;(:drakma "drakma/drakma-2.0.1/drakma.asd" "http://weitz.de/files/drakma.tar.gz")

    ;;(:do-urlencode "do-urlencode-master/do-urlencode.asd" "https://github.com/drdo/do-urlencode")

    ;;(:cl-who "cl-who/cl-who-1.1.4/cl-who.asd" "http://weitz.de/files/cl-who.tar.gz")

    (:trivial-garbage "trivial-garbage/trivial-garbage_0.20/trivial-garbage.asd" "https://common-lisp.net/project/trivial-garbage/")

    ;;(:cffi "cffi/cffi_0.17.1/cffi.asd" "https://common-lisp.net/project/cffi/releases/?M=D")

    ;;(:cl+ssl "cl-plus-ssl/cl-plus-ssl-master/cl+ssl.asd" "https://github.com/cl-plus-ssl/cl-plus-ssl")

    ;;(:hunchentoot "hunchentoot/hunchentoot-1.2.31/hunchentoot.asd" "http://weitz.de/files/hunchentoot.tar.gz")

    (:porter-stemmer "porter-stemmer/porter-stemmer.asd" "http://tartarus.org/martin/PorterStemmer/")

    ;;(:s-xml "s-xml/s-xml.asd" "https://common-lisp.net/project/s-xml/") ; reqd by cl-prevalence

    ;;(:s-sysdeps "s-sysdeps/s-sysdeps.asd" "https://github.com/svenvc/s-sysdeps") ; reqd by cl-prevalence

    ;;(:cl-prevalence "skypher-cl-prevalence/cl-prevalence.asd" "https://common-lisp.net/project/cl-prevalence/")

    (:montezuma "../montezuma-2017-01-31/montezuma.asd" "https://sourceforge.net/projects/montezuma/files/")

    ;;(:oropendola "../oropendola/oropendola.asd" "https://sourceforge.net/projects/montezuma/files/")
    ))

(defun loader (system)
  (unless (find (car system) *systems*)
    (let ((path (merge-pathnames (cadr system) *dependencies*)))
      (format t "; Loading ~s: ~a~%" (car system) path)
      (unless (probe-file path) (error "system definition file can't be found: ~a" path))
      (load path)
      (asdf::compile-system (car system))
      (asdf::load-system (car system))
      (pushnew (car system) *systems*))))

(dolist (system systems) (loader system))

(defun test-montezuma ()
  (asdf:oos 'asdf:test-op '#:montezuma))

;;(montezuma:make-montezuma-host)
