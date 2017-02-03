;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; trivial-garbage.asd --- ASDF system definition for trivial-garbage.
;;;
;;; This software is placed in the public domain by Luis Oliveira
;;; <loliveira@common-lisp.net> and is provided with absolutely no
;;; warranty.

#-(or cmu scl sbcl allegro clisp openmcl corman lispworks ecl abcl)
(error "Sorry, your Lisp is not supported by trivial-garbage.")

(asdf:defsystem #:trivial-garbage
  :version "0.20"
  :description "Portable finalizers, weak hash-tables and weak pointers."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :licence "Public Domain"
  :components ((:file "trivial-garbage")))

(defmethod perform ((op asdf:test-op) (sys (eql (asdf:find-system :trivial-garbage))))
  (asdf:operate 'test-op :trivial-garbage-tests))

(asdf:defsystem #:trivial-garbage-tests
  :description "Unit tests for TRIVIAL-GARBAGE."
  :depends-on (trivial-garbage rt)
  :components ((:file "tests")))

(defmethod perform ((op asdf:test-op)
                    (sys (eql (asdf:find-system :trivial-garbage-tests))))
  (asdf:operate 'load-op :trivial-garbage-tests)
  (funcall (find-symbol (string '#:do-tests) '#:rtest)))
