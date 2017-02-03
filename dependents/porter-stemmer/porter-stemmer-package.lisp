(defpackage stemaux
  ;;(:nicknames :stemaux)
  (:use "COMMON-LISP")
  (:export make-dynamic-string last-but last-letter is-a-vowel vowelp
           is-a-consonant consonantp doublec vowel-in-stem short-syllable
           starts-with ends-with ends-with-char is-a-word analyse
           parse-roman-numerals arabic-to-roman split-line make-diffs
           map-words count-words list-words map-lexicon wc unpack
           prune-lexicon binary-search-stream in-sorted-file))
           
(defpackage porter-stemmer1
  (:nicknames pstem1 ps1)
  (:use "COMMON-LISP")
  (:export "STEM"))

(defpackage porter-stemmer2
  (:nicknames pstem2 ps2)
  (:use "COMMON-LISP")
  (:export "STEM")
  (:import-from stemaux
                make-dynamic-string
                last-but last-letter is-a-vowel vowelp is-a-consonant consonantp doublec
                vowel-in-stem short-syllable starts-with ends-with ends-with-char
                parse-roman-numerals arabic-to-roman split-line is-a-word)
)

(defpackage porter-stemmer3
  (:nicknames pstem3 ps3)
  (:use "COMMON-LISP")
  (:export "STEM" "STEM-TO-VALID-WORD")
  (:import-from stemaux
                make-dynamic-string last-but last-letter is-a-vowel vowelp is-a-consonant consonantp doublec
                vowel-in-stem short-syllable starts-with ends-with ends-with-char
                parse-roman-numerals arabic-to-roman split-line is-a-word analyse
                )
  )

(defpackage porter-stemmer4
  (:nicknames pstem4 ps4)
  (:use "COMMON-LISP" "STEMAUX")
  (:export "STEM" "STEM-TO-VALID-WORD")
  (:import-from stemaux
                last-but last-letter is-a-vowel vowelp is-a-consonant consonantp doublec
                vowel-in-stem short-syllable starts-with ends-with ends-with-char
                parse-roman-numerals arabic-to-roman split-line is-a-word unpack
                prune-lexicon map-lexicon binary-search-stream in-sorted-file)
  )

(defpackage porter-stemmer-tests
  (:nicknames pstests)
  (:import-from stemaux
                split-line analyse is-a-word binary-search-stream)
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf)
)

(asdf:defsystem "porter-stemmer"
  :description "A Lisp system providing Porter Stemming."
  :version "1.0"
  :author "Roy Anderson <reanz1959@egmail.com>, Steven M. Haflich <smh@franz.com>"
  :licence "Public Domain"
  ;;:depends-on ("optima.ppcre" "command-line-arguments")
  :components ((:file "stemmer-auxiliary")
               (:file "porter-stemmer-1" :depends-on ("stemmer-auxiliary"))
               (:file "porter-stemmer-2" :depends-on ("stemmer-auxiliary"))
               (:file "porter-stemmer-3" :depends-on ("stemmer-auxiliary"))
               (:file "porter-stemmer-4" :depends-on ("stemmer-auxiliary"))
               (:file "porter-stemmer-tests" :depends-on ("stemmer-auxiliary"
                                                          "porter-stemmer-1"
                                                          "porter-stemmer-2"
                                                          "porter-stemmer-3"
                                                          "porter-stemmer-4"))))

(asdf:operate 'asdf:load-op 'porter-stemmer)
