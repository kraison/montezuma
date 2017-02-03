;; -*- mode: common-lisp -*-

(in-package #:porter-stemmer2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *porter2-source-file* (load-time-value (or #.*compile-file-pathname* *load-pathname*)))
  (defparameter *porter2-data-directory* (merge-pathnames "data/" *porter2-source-file*)))

(defclass stemmer ()
  ((str :initform nil :accessor stemmer-str :initarg :str)   ;; b buffer for word to be stemmed
   (j :initform 0 :accessor stemmer-j :initarg :j)     ;; a general offset into the string
   (p1 :initform 0 :accessor stemmer-p1 :initarg :p1)
   (p2 :initform 0 :accessor stemmer-p2 :initarg :p2)
   (y-found :initform nil :accessor y-found :initarg :y-found)))

(defmethod print-object ((self stemmer) stream)
  (with-slots (str j p1 p2 y-found) self
    (format stream "#<STEMMER :STR ~s :J ~d :P1 ~d :P2 ~d :Y-FOUND ~s>" str j p1 p2 y-found)))

(defmethod ends ((z stemmer) ending)
  (with-slots (str j) z
    (if (ends-with str ending)
        (setf j (- (length str) (length ending))))))

#|
static int ends_ch(struct stemmer * z, char ch) {
    int k = z->k;
    if (ch != z->b[k]) return FALSE;
    z->j = k-1;
    return TRUE;
}
|#
(defmethod ends_ch ((z stemmer) (ch character))
  (with-slots (str j) z
    (if (ends-with-char str ch)
        (setf j (- (length str) 2)))))

(defmethod ends_ch ((z stemmer) (chars list))
  (loop 
    as ch in chars do
    (if (ends_ch z ch) (return z))))

(defmethod equals ((z stemmer) s)
  (with-slots (str) z
    (when (= (length str) (length s))
      (ends z s))))

(defmethod starts ((z stemmer) (prefix string))
  (with-slots (str j) z
    (if (starts-with str prefix)
        (setf j (length prefix)))))

(defmethod setto ((z stemmer) suffix)
  (with-slots (str j) z
    (setf (fill-pointer str) j)
    (loop for char across suffix do (vector-push-extend char str)))
  z)

(defmethod setto0 ((z stemmer))
  (with-slots (str j) z
    (setf (fill-pointer str) j)))

#|
R1 is the region after the first non-vowel following a vowel, 
or the end of the word if there is no such non-vowel. 
This definition may be modified for certain exceptional words, see below.
|#
(defmethod R1 ((z stemmer))
  (with-slots (j p1) z
    (>= (1+ j) p1)))

#|
R2 is the region after the first non-vowel following a vowel in R1, 
or the end of the word if there is no such non-vowel. (See  note on R1 and R2.)
|#
(defmethod R2 ((z stemmer))
  (with-slots (j p2) z
    (>= (1+ j) p2)))

#|
Step 0: + 
Search for the longest among the suffixes, 

' 
's 
's' 

and remove if found.
|#
(defmethod step0 ((z stemmer))
  (with-slots (str p1 p2) z
    (when (and (starts z "'") (> (length str) 2) (alpha-char-p (char str 1)))
      (setf str (make-dynamic-string (subseq str 1)))
      (decf p1)
      (decf p2))
    (cond
     ((ends z "'s'")
      (decf (fill-pointer str) 3))
     ((ends z "'s")
      (if (> (length str) 2)
          (decf (fill-pointer str) 2)))
     ((ends z "'''")
      (if (> (length str) 2) 
          (decf (fill-pointer str) 2)))
     ((ends z "''")
      (if (> (length str) 2) 
          (decf (fill-pointer str))))
     ((ends z "'")
      (if (>= (length str) 2)
          (decf (fill-pointer str))))))
  z)

#|
Step 1a: 

Search for the longest among the following suffixes, and perform the action indicated. 

sses 
replace by ss 

ied+   ies* 
replace by i if preceded by more than one letter, otherwise by ie (so ties -> tie, cries -> cri) 

s 
delete if the preceding word part contains a vowel not immediately before the s 
(so gas and this retain the s, gaps and kiwis lose it) 

us+   ss 
do nothing 
|#
(defmethod step1a ((z stemmer))
  (with-slots (str j) z
    (if (ends z "ied") (setto z "ies"))
    (cond
     ((char/= (last-letter str) #\s) nil)
     ((ends z "sses")
      (decf (fill-pointer str) 2))
     ((ends z "ies")
      (setto z (if (> j 1) "i" "ie")))
     ((or (ends z "ss") (ends z "us"))
      nil)
     ((and (> (length str) 2)
           (find-if #'is-a-vowel str :end (- (length str) 2)))
      (decf (fill-pointer str)))))
  z)

#|
Step 1b: 

Search for the longest among the following suffixes, and perform the action indicated. 

eed   eedly+ 
replace by ee if in R1 

ed   edly+   ing   ingly+ 
delete if the preceding word part contains a vowel, and after the deletion: 
if the word ends at, bl or iz add e (so luxuriat -> luxuriate), or 
if the word ends with a double remove the last letter (so hopp -> hop), or 
if the word is short, add e (so hop -> hope) 
|#
(defmethod step1b ((z stemmer))
  (with-slots (str j p1) z
    (loop
     as (suffix replacement) in '(("eed"  "ee") ("ed") ("eedly" "ee") ("edly") ("ingly") ("ing")) do
     (when (ends z suffix)
       (when replacement
         (if (R1 z)
             (setto z replacement))
         (return))
       
       ;; ed test
       (when (vowel-in-stem str j)
         ;;(format t "step1b ~s" z)
         (setto0 z)
         (loop
          as (suffix replacement) in '(("at" "ate") ("bl" "ble") ("iz" "ize")) do
          (when (ends z suffix)
            (setto z replacement)
            (return-from step1b z)))
         
         ;;(format t "step1b shortv ~s~%" z)
         (cond 
          ((doublec str) (decf (fill-pointer str)))
          ((and (= j p1)
                (short-syllable str (1- j)))
           (setto z "e")))
         (return)))))
  z)

#|
Step 1c: * 

replace suffix y or Y by i if preceded by a consonant which is not the first letter of the word 
(so cry -> cri, by -> by, say -> say) 

|#
(defmethod step1c ((z stemmer))
  (with-slots (str) z
    (if (and (ends_ch z `(#\y #\Y))
             (> (length str) 2)
             (is-a-consonant (last-but str 1)))
        (setf (char str (1- (fill-pointer str))) #\i)))
  z)

#|
Define a valid li-ending as one of 
c   d   e   g   h   k   m   n   r   t 
|#
(defun valid_li_ending (str j)
  (if (> j 0)
      (find (char str (1- j)) "cdeghkmnrt")))

#|
Step 2: 

Search for the longest among the following suffixes, and, if found and in R1, perform the action indicated. 

tional:   replace by tion 
enci:   replace by ence 
anci:   replace by ance 
abli:   replace by able 
entli:   replace by ent 
izer   ization:   replace by ize 
ational   ation   ator:   replace by ate 
alism   aliti   alli:   replace by al 
fulness:   replace by ful 
ousli   ousness:   replace by ous 
iveness   iviti:   replace by ive 
biliti   bli+:   replace by ble 
ogi+:   replace by og if preceded by l 
fulli+:   replace by ful 
lessli+:   replace by less 
li+:   delete if preceded by a valid li-ending 

|#
(defmethod step2 ((z stemmer))
  (with-slots (str j p1) z
    (loop
      as (suffix replacement) in 
      '(("ational" "ate") ("tional" "tion") ("enci" "ence") ("anci" "ance")
        ("izer" "ize") ("bli" "ble") ("alli" "al") ("entli" "ent") ("eli" "e")
        ("ousli" "ous") ("fulli" "ful") ("lessli" "less") ("ization" "ize")
        ("ation" "ate") ("ator" "ate") ("alism" "al") ("iveness" "ive")
        ("fulness" "ful") ("ousness" "ous") ("aliti" "al") ("iviti" "ive")
        ("biliti" "ble")) do
      (when (ends z suffix)
        (if (>= j p1) ;;(R1 z)
            (setto z replacement))
        (return-from step2 z)))
    (cond
     ((and (ends z "li")
           (valid_li_ending str j))
      (if (R1 z)
          (setto z "")))
     ((and (ends z "ogi")
           (char= (char str (1- j)) #\l))
      (if (R1 z)
          (setto z "og")))))
  z)

#|
Step 3: 

Search for the longest among the following suffixes, and, if found and in R1, perform the action indicated. 

tional+:   replace by tion 
ational+:   replace by ate 
alize:   replace by al 
icate   iciti   ical:   replace by ic 
ful   ness:   delete
ative*:   delete if in R2 
|#
(defmethod step3 ((z stemmer))
  (with-slots (str j p1 p2) z
    (loop
      as (suffix replacement) in 
      '(("icate" "ic") ("alize" "al") ("iciti" "ic") ("ical" "ic") ("ful" "") ("ational" "ate")
        ("tional" "tion") ("ness" "")) do
      (when (ends z suffix)
        (if (>= j p1) ; (R1 z)
            (setto z replacement))
        (return)))
    (if (and (ends z "ative")
             (R2 z))
        (setto0 z)))
  z)

#|
Step 4: 

Search for the longest among the following suffixes, and, if found and in R2, perform the action indicated. 

al   ance   ence   er   ic   able   ible   ant   ement   ment   ent   ism   ate   iti   ous   ive   ize 

delete 

ion 

delete if preceded by s or t 
|#
(defmethod step4 ((z stemmer))
  (with-slots (str j p2) z
    (loop
      as suffix in '("al" "ance" "ence" "er" "ic" "able" "ible" "ant" "ement" "ment" "ent" "ism" 
                     "ate" "iti" "ous" "ive" "ize") do
      (when (ends z suffix)
        (when (>= j p2) ; (R2 z)
          (decf (fill-pointer str) (length suffix)))
        (return-from step4 z)))
    (when (or (ends z "sion")
              (ends z "tion"))
      (incf j)
      (when (R2 z)
        (decf (fill-pointer str) 3))))
  z)

#|
Step 5: * 

Search for the the following suffixes, and, if found, perform the action indicated. 

e 
delete if in R2, or in R1 and not preceded by a short syllable 

l 
delete if in R2 and preceded by l 

|#
(defmethod step5 ((z stemmer))
  (with-slots (str j) z
    (cond
     ((and (ends_ch z #\e)
           (or (R2 z)
               (and (R1 z) (not (short-syllable str j)))))
      (decf (fill-pointer str)))
     ((and (ends_ch z #\l)
           (R2 z) 
           (char= (char str j) #\l))
      (decf (fill-pointer str)))))
  z)

#|
define exception1 as (

    [substring] atlimit among(

        /* special changes: */

        'skis'      (<-'ski')
        'skies'     (<-'sky')
        'dying'     (<-'die')
        'lying'     (<-'lie')
        'tying'     (<-'tie')

        /* special -LY cases */

        'idly'      (<-'idl')
        'gently'    (<-'gentl')
        'ugly'      (<-'ugli')
        'early'     (<-'earli')
        'only'      (<-'onli')
        'singly'    (<-'singl')

        // ... extensions possible here ...

        /* invariant forms: */

        'sky'
        'news'
        'howe'

        'atlas' 'cosmos' 'bias' 'andes' // not plural forms

        // ... extensions possible here ...
    )
)
|#
(defmethod exception1 ((z stemmer))
  (loop
    as (word stem) in '(("skies" "sky")
                        ("skis" "ski")
                        ("sky" "sky")
                        ("singly" "singl")
                        ("dying" "die")
                        ("lying" "lie")
                        ("tying" "tie")
                        ("idly" "idl")
                        ("gently" "gentl")
                        ("ugly" "ugli")
                        ("only" "onli")
                        ("early" "earli")
                        ("news" "news")
                        ("howe" "howe")
                        ("atlas" "atlas")
                        ("andes" "andes")
                        ("cosmos" "cosmos")
                        ("bias" "bias"))
    when (equals z word)
    return (setto z stem)))

#|
Following step 1a, leave the following invariant, 

inning       outing       canning       herring       earring  
proceed      exceed       succeed  
|#
(defmethod exception2((z stemmer))
  (with-slots (str) z
    (unless (zerop (length str))
      (loop
        as word in '("inning" "outing" "canning" "herring" "earring" "exceed" "proceed" "succeed") do
        (when (equals z word)
          (setto z word)
          (return z))))))

#|
If the word begins gener, commun or arsen, set R1 to be the remainder of the word. 
|#
(defmethod exception_p1 ((z stemmer))
  (with-slots (p1 j) z
    (loop
      as prefix in '("gener" "commun" "arsen")
      when (starts z prefix)
      return (setf p1 j))))

#| In stem(z, b, k), b is a char pointer, and the string to be stemmed is
   from b[0] to b[k] inclusive.  Possibly b[k+1] == '\0', but it is not
   important. The stemmer adjusts the characters b[0] ... b[k] and returns
   the new end-point of the string, k'. Stemming never increases word
   length, so 0 <= k' <= k.
|#

(defmethod has-y (z)
  (with-slots (str y-found) z
    (setf y-found nil)
    (dotimes (i (length str))
      (when (and (char= #\y (char str i))
                 (or (zerop i)
                     (vowelp str (1- i))))
        (setf y-found t)
        (setf (char str i) #\Y))))
  z)

#|
(defun make-stemmer-instance (str)
  (make-instance 'stemmer :str (make-dynamic-string (string-downcase str))))
|#

(defmethod mark-regions ((z stemmer))
  (with-slots (str j p1 p2) z
    (let ((syllable_count 0)
          (ch_prev_vowel nil)
          (start 0))
      ;(break "mark-regions ~s" z)
      (setf p1 j p2 j)
      (when (exception_p1 z)
        (setf syllable_count 1 start p1))
      ;;(format t "~%mark-regions before loop ~s start ~d j = ~d~%" z start j)
      (loop
        for i from start to (1- (length str)) do ;)(1- j) do
        (let* ((ch (char str i))
               (ch_cons (is-a-consonant ch)))
          ;;(format t "~%mark-regions ~s start ~d~%" z start)
          (when (and ch_cons ch_prev_vowel)
            (incf syllable_count)
            (if (= syllable_count 1)
                (setf p1 (1+ i)))
            (when (= syllable_count 2)
              (setf p2 (1+ i))
              ;;(return)
              ))
          (setf ch_prev_vowel (not ch_cons))))))
  z)

(defmethod restore-y ((z stemmer))
  (with-slots (str Y-found) z
    (if y-found
        (setf str (substitute #\y #\Y str)))))

(defmethod stem ((str string))
  (let ((z (make-instance 'stemmer :str (make-dynamic-string (string-downcase str)))))
    (with-slots (str j) z
      (when (> (length str) 2)
        (setf j (length str))
        (unless (exception1 z)
          (has-y z)
          (mark-regions z)
          (step0 z) (step1a z)
          (unless (exception2 z) (step1b z) (step1c z) (step2 z) (step3 z) (step4 z) (step5 z))
          (restore-y z)))
      (values str))))

;;--------------------stemmer definition ends here------------------------

#+never
(trace exception_p1 exception1 step0 step1a exception2 step1b step1c step2 step3 step4 step5)

#|
(defmethod reinitialize ((z stemmer) inflected-word)
  (with-slots (str j p1 p2 y_found) z
    (setf
     (fill-pointer str) 0
     j 0
     p1 0
     p2 0
     y_found nil)
    (setto z inflected-word))
  z)
|#
