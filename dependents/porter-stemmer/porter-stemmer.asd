;; -*- mode: common-lisp -*-

(defpackage porter-stemmer
  (:nicknames pstem ps)
  (:use "COMMON-LISP")
  (:export "STEM" "STEM-TO-VALID-WORD" "IS-A-WORD"))

(asdf:defsystem :porter-stemmer
  :description "A wrapper for Porter Stemmer."
  :components ((:file "porter-stemmer")))
