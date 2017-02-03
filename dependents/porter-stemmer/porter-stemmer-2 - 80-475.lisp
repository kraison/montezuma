(defpackage porter-stemmer2
  (:nicknames pstem2 ps2)
  (:use "COMMON-LISP")
  (:export "STEM"))

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

(defmethod len ((z stemmer))
  (length (stemmer-str z)))

(defmethod print-object ((self stemmer) stream)
  (with-slots (str j p1 p2) self
    (format stream "#<STEMMER :STR ~s :J ~d :P1 ~d :P2 ~d>" str j p1 p2)))

(defun make-dynamic-string (string)
  (make-array (length string)
              :element-type 'character
              :adjustable t
              :fill-pointer (length string)
              :initial-contents string))

(defun create-stemmer (word)
  (make-instance 'stemmer :str (make-dynamic-string word)))

(defmethod last-but ((z stemmer) n)
  (last-but (stemmer-str z) n))

(defmethod last-but ((str string) n)
  (let ((index (- (length str) (1+ n))))
    (when (>= index 0)
      (char str index))))

(defmethod last-letter ((str string))
  (unless (zerop (length str))
    (char str (1- (length str)))))

(defmethod last-letter ((z stemmer))
  (last-letter (stemmer-str z)))

(defun is-a-vowel (ch)
  (find ch "aeiouy"))

(defun vowelp (str i)
  (is-a-vowel (char str i)))

(defun is-a-consonant (ch)
  (not (is-a-vowel ch)))

(defun consonantp (str i)
  (is-a-consonant (char str i)))

(defmethod vowelinstem ((z stemmer))
  (with-slots (str j) z
    (find-if #'is-a-vowel str :end j))) ;(1+ j))))

#|
Define a double as one of 
bb   dd   ff   gg   mm   nn   pp   rr   tt 
|#
(defun doublec (str &optional (i (1- (length str))))
  (and
   (> i 1)
   (let ((lastch (char str i)))
     (and (char= lastch (char str (1- i)))
          (find lastch "bdfgmnprt")))))

#|
Define a short syllable in a word as either 
(a) a vowel followed by a non-vowel other than w, x or Y and preceded by a non-vowel, or * 
(b) a vowel at the beginning of the word followed by a non-vowel.

So rap, trap, entrap end with a short syllable, and ow, on, at are classed as short syllables. 
But uproot, bestow, disturb do not end with a short syllable. 
|#
(defun short-syllable (str j)
  ;;(format t "~%shortv ~s chj ~s ch-1 ~s~%" z chj (and (> j 1) (char str (1- j))))
  (cond 
   ((vowelp str j) nil)
   ((and (= j 1)
         (vowelp str 0)))
   ((and (> j 1)
         (consonantp str (- j 2))
         (vowelp str (- j 1))
         (not (find (char str j) "wxY"))))))

(defmethod shortv ((z stemmer))
  (with-slots (str j) z
    (short-syllable str j)))

(defmethod ends ((z stemmer) ending)
  (with-slots (str j) z
    (let ((len1 (length str))
          (len2 (length ending)))
      (when (and (>= len1 len2) 
                 (string= str ending :start1 (- len1 len2)))
        (setf j (- len1 len2))
        z))))

#|
static int ends_ch(struct stemmer * z, char ch) {
    int k = z->k;
    if (ch != z->b[k]) return FALSE;
    z->j = k-1;
    return TRUE;
}
|#
(defmethod ends_ch ((z stemmer) (ch character))
  (with-slots (j) z
    (when (char= ch (last-letter z))
      (setf j (- (len z) 2))
      z)))

(defmethod ends_ch ((z stemmer) (chars list))
  (loop 
    as ch in chars do
    (if (ends_ch z ch) (return z))))

(defmethod equals ((z stemmer) s)
  (when (= (len z) (length s))
    (ends z s)))

(defmethod starts ((z stemmer) (prefix string))
  (let ((length (length prefix)))
    (with-slots (str j) z
      (and (<= length (len z))
         (string= str prefix :end1 length)
         (setf j length)))))

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

(defmethod r ((z stemmer) s)
  (if (R1 z)
      (setto z s)))

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
    (when (and (starts z "'") (> (len z) 2) (alpha-char-p (char str 1)))
      (setf str (make-dynamic-string (subseq str 1)))
      (decf p1)
      (decf p2))
    (cond
     ((ends z "'s'")
      (decf (fill-pointer str) 3))
     ((ends z "'s")
      (if (> (len z) 2)
          (decf (fill-pointer str) 2)))
     ((ends z "'''")
      (if (> (len z) 2) 
          (decf (fill-pointer str) 2)))
     ((ends z "''")
      (if (> (len z) 2) 
          (decf (fill-pointer str))))
     ((ends z "'")
      (if (>= (len z) 2)
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
    ;;(format t "step1a ~s last-letter ~s~%" z (last-letter z))
    (cond
     ((char/= (last-letter z) #\s) nil)
     ((ends z "sses")
      (decf (fill-pointer str) 2))
     ((ends z "ies")
      ;;(format t "~%step1a ~s ies?" z)
      (setto z (if (> j 0) "i" "ie")))
     ((or (ends z "ss") (ends z "us"))
      nil)
     ((and (> (len z) 2)
           ;; (ends z "s") ; we already know str ends in #\s
           (find-if #'is-a-vowel str :end (- (len z) 2)))
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
         (r z replacement)
         (return))
       
       ;; ed test
       (when (vowelinstem z)
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
                (short-syllable str j))
                ;;(shortv z))
           (setto z "e")))))))
  z)

#|
Step 1c: * 

replace suffix y or Y by i if preceded by a consonant which is not the first letter of the word 
(so cry -> cri, by -> by, say -> say) 

|#
(defmethod step1c ((z stemmer))
  (with-slots (str) z
    (if (and (ends_ch z `(#\y #\Y))
             (> (len z) 2)
             (is-a-consonant (last-but z 1)))
        (setf (char str (1- (fill-pointer str))) #\i)))
  z)

#|
Define a valid li-ending as one of 
c   d   e   g   h   k   m   n   r   t 
|#
(defmethod valid_li_ending ((z stemmer))
  (with-slots (str j) z
    (if (> j 0)
        (find (char str (1- j)) "cdeghkmnrt"))))

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
  (with-slots (str j) z
    ;;(unless (zerop (length str))
      (loop
        as (suffix replacement) in 
        '(("ational" "ate") ("tional" "tion") ("enci" "ence") ("anci" "ance")
          ("izer" "ize") ("bli" "ble") ("alli" "al") ("entli" "ent") ("eli" "e")
          ("ousli" "ous") ("fulli" "ful") ("lessli" "less") ("ization" "ize")
          ("ation" "ate") ("ator" "ate") ("alism" "al") ("iveness" "ive")
          ("fulness" "ful") ("ousness" "ous") ("aliti" "al") ("iviti" "ive")
          ("biliti" "ble")) do
        (when (ends z suffix)
          ;;(format t "?step2 ~s suffix ~s~%" z suffix)
          (r z replacement)
          (return-from step2 z)))
      (cond
       ((and (ends z "li")
             (valid_li_ending z))
        (r z ""))
       ((and (ends z "ogi")
             (char= (char str (1- j)) #\l))
        (r z "og"))))
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
  (with-slots (str) z
    (loop
      as (suffix replacement) in 
      '(("icate" "ic") ("alize" "al") ("iciti" "ic") ("ical" "ic") ("ful" "") ("ational" "ate")
        ("tional" "tion") ("ness" "")) do
      (when (ends z suffix)
        (r z replacement)
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
  (with-slots (str j) z
    (loop
      as suffix in '("al" "ance" "ence" "er" "ic" "able" "ible" "ant" "ement" "ment" "ent" "ism" 
                     "ate" "iti" "ous" "ive" "ize") do
      (when (and (ends z suffix) (R2 z))
        (setf (fill-pointer str) j)
        (return-from step4 z)))
    (when (and (ends z "ion")
               (find (char str (1- j)) '(#\s #\t)))
      (setf (fill-pointer str) j)))
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
           (or (R2 z) (and (R1 z) (not (shortv z)))))
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
  (unless (zerop (len z))
    (loop
      as word in '("inning" "outing" "canning" "herring" "earring" "exceed" "proceed" "succeed") do
      (when (equals z word)
        (setto z word)
        (return z)))))

#|
If the word begins gener, commun or arsen, set R1 to be the remainder of the word. 
|#
(defmethod exception_p1 ((z stemmer))
  (loop
    as prefix in '("gener" "commun" "arsen")
    when (starts z prefix)
    return (setf (stemmer-p1 z) (stemmer-j z))))

#| In stem(z, b, k), b is a char pointer, and the string to be stemmed is
   from b[0] to b[k] inclusive.  Possibly b[k+1] == '\0', but it is not
   important. The stemmer adjusts the characters b[0] ... b[k] and returns
   the new end-point of the string, k'. Stemming never increases word
   length, so 0 <= k' <= k.
|#

(defmethod has-y (z)
  (with-slots (str y-found) z
    (setf y-found nil)
    (dotimes (i (len z))
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
  (let ((str (stemmer-str z))
        (syllable_count 0)
        (ch_prev_vowel nil)
        (start 0))
    (setf (stemmer-p1 z) (setf (stemmer-p2 z) (length str)))
    (when (exception_p1 z)
      (setf syllable_count 1)
      (setf start (stemmer-p1 z)))
    (loop
      for i from start to (1- (stemmer-j z))
      as ch = (char str i)
      as ch_cons = (is-a-consonant ch) do
      (when (and ch_cons ch_prev_vowel)
        (incf syllable_count)
        (if (= syllable_count 1)
            (setf (stemmer-p1 z) (1+ i))
          (when (= syllable_count 2)
            (setf (stemmer-p2 z) (1+ i))
            (return))))
      (setf ch_prev_vowel (not ch_cons))))
  z)

(defmethod restore-y ((z stemmer))
  (with-slots (str Y-found) z
    (if y-found
        (setf str (substitute #\y #\Y str)))))

(defmethod stem ((str string))
  (let ((z (make-instance 'stemmer :str (make-dynamic-string (string-downcase str)))))
    (with-slots (j) z
      (when (> (len z) 2)
        (setf j (len z))
        (unless (exception1 z)
          (has-y z)
          (mark-regions z)
          (step0 z) (step1a z)
          (unless (exception2 z) (step1b z) (step1c z) (step2 z) (step3 z) (step4 z) (step5 z))
          (restore-y z)))
      z)))

;;--------------------stemmer definition ends here------------------------

#+never
(trace exception1 step0 step1a exception2 step1b step1c step2 step3 step4 step5)

(defun split-line (line)
  (let* ((i (position #\space line))
         (j (position-if-not #'(lambda(ch) (char= #\space ch)) line :start i)))
    (values (subseq line 0 i) (subseq line j))))    

(defun make-diffs (&optional (path-inputs "stemmer2-input.txt")
                             (path-stems "stemmer2-output.txt")
                             (path-diffs "stemmer2-diffs.txt"))
  ;; collect inputs and outputs into a diffs file
  (with-open-file (f1 (merge-pathnames path-inputs *porter2-data-directory*))
    (with-open-file (f2 (merge-pathnames path-stems *porter2-data-directory*))
      (with-open-file (f3 (merge-pathnames path-diffs *porter2-data-directory*)
                          :direction :output
                          :if-exists :supersede)

        (loop
          as w1 = (read-line f1 nil nil)
          while w1
          as w2 = (read-line f2 nil nil)
          do (format f3 "~20a ~a~%" (string-trim '(#\Space #\Tab #\Newline) w1) (string-trim '(#\Space #\Tab #\Newline) w2))))))
  path-diffs)

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

(defun test-porter2-80 (&optional (path (merge-pathnames "stemmer2-diffs.txt" *porter2-data-directory*)))
  (with-open-file (f path)
    (loop
      with successes = 0
      with failures = 0
      as line = (read-line f nil nil)
      while line do
      (multiple-value-bind (input expected) (split-line line)
        ;;(reinitialize z input)
        (let ((z (stem input)))
          (if (equal expected (stemmer-str z))
              (incf successes)
            (progn
              (incf failures)
              (format t "(stem ~s) => ~s wanted ~s~%" input (stemmer-str z) expected)))))
      finally (format t "sucesses ~d failures ~d~%" successes failures)))
  (values))

(defun test-porter2 (&key (path (merge-pathnames "porter-stemmer2-diffs.txt" *porter2-data-directory*))
                          (max-failure nil)
                          (start 0)
                          (end nil))
  (let ((successes 0)
        (failures 0)
        (line-count 0))
    (with-open-file (f path)
      (loop
        as line = (read-line f nil nil)
        while line do
        (incf line-count)
        (when (and (>= line-count start) (or (null end) (<= line-count end)))
          (multiple-value-bind (input expected) (split-line line)
            (let ((z (stem input)))
              (cond
               ((equal expected (stemmer-str z))
                (incf successes)
                ;;(format t "[~d] (stem ~s) => ~s~%" line-count input (stemmer-str z))
                )
               (t
                (incf failures)
                (format t "[~d] (stem ~s) => ~s wanted ~s~%" line-count input (stemmer-str z) expected)
                (if (and (numberp max-failure) (> failures max-failure))
                    (return))
                ;;(format t "~20a ~20a ~20a~%" input expected (stemmer-str z)))))))
                )))))))
    (format t "sucesses ~d failures ~d~%" successes failures)
    failures))
