;; -*- mode: common-lisp -*-

(in-package #:porter-stemmer4)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *data-directory* (merge-pathnames "data/" (load-time-value (or #.*compile-file-pathname*
                                                                               *load-pathname*)))))

(defclass stemmer ()
  ((str :initform nil :accessor stemmer-str :initarg :str)   ;; buffer for word being stemmed
   (j :initform 0 :accessor stemmer-j :initarg :j)     ;; a general offset into the string
   (p1 :initform 0 :accessor stemmer-p1 :initarg :p1)
   (p2 :initform 0 :accessor stemmer-p2 :initarg :p2)
   (y-found :initform nil :accessor y-found :initarg :y-found)
   (validator :initform nil :accessor validator :initarg :validator)
   (analysis :initform :absent :accessor analysis :initarg :analysis)))

(defmethod print-object ((self stemmer) stream)
  (with-slots (str j p1 p2 y-found) self
    (format stream "#<STEMMER :STR ~s :J ~d :P1 ~d :P2 ~d :Y-FOUND ~s>" str j p1 p2 y-found)))

(defmethod ends ((z stemmer) ending)
  (with-slots (str j) z
    (if (ends-with str ending)
        (setf j (- (length str) (length ending))))))

(defmethod ends_ch ((z stemmer) (ch character))
  (with-slots (str j) z
    (if (and (> (length str) 1)
             (ends-with-char str ch))
        (setf j (- (length str) 2)))))

(defmethod ends_ch ((z stemmer) (chars list))
  (loop 
    as ch in chars do
    (if (ends_ch z ch)
        (return z))))

(defmethod equals ((z stemmer) s)
  (with-slots (str) z
    (when (= (length str) (length s))
      (ends z s))))

(defmethod starts ((z stemmer) (prefix string))
  (with-slots (str j) z
    (if (starts-with str prefix)
        (setf j (length prefix)))))

(defun string-append (str ending)
  (loop
    for char across ending do
    (vector-push-extend char str)))

(defmethod setto0 ((z stemmer))
  (with-slots (str j) z
    (setf (fill-pointer str) j)))

(defmethod setto ((z stemmer) suffix)
  (setto0 z)
  (if (and suffix (> (length suffix) 0))
      (with-slots (str) z
        (string-append str suffix)))
  z)

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

(defmethod append-and-validate ((z stemmer) endings)
  (setto z "")
  (with-slots (str j validator) z 
    (loop
      for ending in (enlist endings) do
      (setf (fill-pointer str) j)
      (setto z ending)
      (if (funcall validator z)
          (return z)))))

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
    ;;(format t "~%step1a ~s~%" z)
    (cond
     ((char/= (last-letter str) #\s) nil)
     ((ends z "sses")
      (decf (fill-pointer str) 2))
     ((ends z "ies")
      ;;(format t "~%step1a ~s ies?" z)
      (setto z (if (> j 1) "i" "ie")))
     ((or (ends z "ss") (ends z "us"))
      nil)

     ((ends z "es")
      (setto z "e"))

     ((and (> (length str) 2)
           (ends z "s")
           (find-if #'is-a-vowel str :end (- (length str) 2)))
      ;;(format t "step1a ~s~%" z)
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
  (with-slots (str j p1 validator) z
    (loop
      as (suffix replacement) in
      '(("eed" "ee")
        ("led" ("le" "e"))
        ("ed" ("" "e"))
        ("eedly" "ee")
        ("edly")
        ("ingly")
        ("ing" ("" "e")) ; choosing
        ) do
     (when (ends z suffix)
       (when (and replacement (R1 z))
         (if (append-and-validate z replacement)
             (return-from step1b z)))
       
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
    (if (and (ends_ch z '(#\y #\Y))
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

(defun enlist (x)
  (if (atom x) (list x) x))

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
  (with-slots (str j p1 validator) z
    ;;(unless (zerop (length str))
      (loop
        as (suffix replacement) in 
        '(("ication" "y") ; magnification
          ("sion" "d")
          ("ately" "") ; affectionately
          ("ational" "ate")
          ("tional" "tion")
          ("enci" "ence")
          ("anci" "ance")
          ("izer" "ize")
          ("bli" "ble")
          ("alli" "al")
          ("entli" "ent")
          ("eli" "e")
          ("ili" "y") ; saucily
          ("inous" ("e" "ine")) ; asparaginous => asparagine
          ("ousli" "ous")
          ("full" "") ; woefull
          ("fulli" "ful")
          ("lessli" "less")
          ("ization" "ize")
          ("ation" "ate")
          ("ator" "ate")
          ("alism" "al")
          ("iveness" "ive")
          ("fulness" "ful")
          ("ousness" "ous")
          ("aliti" "al")
          ("iviti" "ive")
          ("biliti" "ble")
          ("etic" "ecy") ; prophetic
          ("ogical" "ogy") ; biological
          ("logic" "logy") ; entomologic, sociologic
          ;;("tic" "y")
          ("etical" ("i" "esis")) ; hypothetical
          ("ship" "")  ; assistantship
          ("phobia" "")
          ("mania" "")
          ("ist" "e")
          ("ier" "y") ; beastlier
          ("less" "") ; beardless
          ("logi" "logy")
          ("ally" "") ; cubically
          ("ess" "") ; prophetess
          ("like" "") ; cowardlike
          ("ice" "") ; cowardice
          ) do
        (when (ends z suffix)
          ;;(format t "~%step2 ~s R1 ~s~%" z (>= j p1))
          (when (R1 z) ;;(>= j p1)
            (append-and-validate z replacement))
          (return-from step2 z)))
      (cond
       ((and (ends z "li")
             (valid_li_ending str j)
             (R1 z))
            (setto z ""))))
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
  (with-slots (str j p1 p2 validator) z
    (loop
      as (suffix replacement) in 
      '(("icate" "ic")
        ("alize" "al")
        ("rize" "ry")
        ("iciti" "ic")
        ("ophyta" "ophyte") ; cycadophyta, pteridophyte
        ("etical" ("esis" "y")) ; hypothetically 
        ("thecy" "thesis") ; hypothetic
        ("tical" ("r" "tic")) ; theoretical, grammatical
        ("ical" ("ic" "e" "y")) ; judaical, archaical, syllabical, duncical
        ("ish" "") ; biggish
        ("ful" "")
        ("ational" "ate")
        ("tional" "tion")
        ("ness" "")
        ("ific" "ity") ; sonorific
        ("graphic" "graphi") ; biographic
        ("ginous" "gine") ; asparaginous => asparagine
        ("ous" "ate")
        ) do
      (when (ends z suffix)
        ;;(format t "~%step3 ~s R1 ~s~%" z (>= j p1))
        (when (R1 z) ;;(>= j p1)
          (append-and-validate z replacement)
          (return-from step3 z)))))
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
  (with-slots (str j p2 validator) z
    (loop
      as suffix in '("al" "ance" "ence" "er" "ic" "able" "ible" "ant" "ement" "ment" "ent" "ism" 
                     "ate" "iti" "ous" "ive" "ize" "ea" 
                     ) do
      (when (ends z suffix)
        ;;(format t "~%step4 ~s ends ~s R2 ~s j ~d p2 ~d~%" z suffix (R2 z) j p2)
        (when (R2 z)
          (decf (fill-pointer str) (length suffix))
          (if (funcall validator z)
              (return-from step4 z))
          
          (setto z "e")
          (if (funcall validator z)
              (return-from step4 z)))))
    
    (when (or (ends z "sion")
              (ends z "tion"))
      (incf j)
      ;;(format t "~%step4 ion ~s R2 ~s~%" z (R2 z))
      (when (R2 z)
        (decf (fill-pointer str) 3)))
  
    (when (ends z "hood")
      ;;(format t "~%step4 ion ~s R2 ~s~%" z (R2 z))
      ;;(when (R2 z)
      (decf (fill-pointer str) 4)))
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
  (with-slots (str j validator) z
    (cond
     ((and (ends z "ative")
           (R2 z))
      (setto z "ate")
      (if (funcall validator z)
          (return-from step5 z))
      (decf (fill-pointer str) 2)
      ;;(setto0 z)))
      )
     
     ((and (not (ends z "ee"))
           (ends_ch z #\e)
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
    as (word stem) in 
    '(("skies" "sky") ("skis" "ski") ("sky") ("singly" "single") ("dying" "die")
      ("lying" "lie") ("tying" "tie") ("idly" "idle") ("gently" "gentle") ("ugly")
      ("only") ("early") ("news") ("howe") ("atlas")
      ("andes") ("cosmos") ("bias") ("abaci" "abacus") 
      ("evil"))
    when (equals z word) return (setto z (or stem word))))

#|
Following step 1a, leave the following invariant, 

inning       outing       canning       herring       earring  
proceed      exceed       succeed  
|#
(defmethod exception2((z stemmer))
  (with-slots (str) z
    (unless (zerop (length str))
      (loop
        as word in '("inning" "outing" "canning" "herring" "earring" "exceed" "proceed" "succeed")
        when (equals z word) return (setto z word)))))

#|
If the word begins gener, commun or arsen, set R1 to be the remainder of the word. 
|#
(defmethod exception_p1 ((z stemmer))
  (with-slots (p1 j) z
    (loop
      as prefix in '("gener" "commun" "arsen")
      when (starts z prefix)
      return (setf p1 j))))

(defmethod postscript1 ((z stemmer))
  (with-slots (str) z
    (when (doublec str)  ; occurrence
      (decf (fill-pointer str))
      (return-from postscript1 z))
    (loop as (suffix stem) in 
          '(("eress" "erer") ; adulteress
            ("elv" "elf") ; yourselves
            ("arili" "") ; complimentarily
            ("ri" ("ry" "")) ; bakery
            ("i" "y") ; victorious
            ("abl" "")
            ("iest" ("ie" "y")) ; happiest, beliest
            ("ier" "y") ; carrier
            ("ily" "y") ; happily
            ("er" ("" "e")) ; weaker, baker
            ("est" ("e" "")) ; wouldest, bravest, woefullest
            ("eth" ("" "e")) ; vieweth, chargeth
            ("men" "man") ; tradesmen
            ("ment" "") ; cloyment
            )
        when (ends z suffix) do
          ;(break "suffix ~s stem/s ~s z ~s" suffix stem z)
          (append-and-validate z stem)
          (return)))
  ;;(return-from postscript1 z))))
  z)

(defmethod postscript2 ((z stemmer))
  (with-slots (str) z
    (cond 
     ((ends z "lle")           (decf (fill-pointer str) 2)) ; awfullest
     ((ends z "e")             (decf (fill-pointer str))) ; talkest
     ((ends z "ll")            (decf (fill-pointer str))) ; awfullest
     ((doublec str)            (decf (fill-pointer str)))
     ;;((not (ends z "e"))       (setto z "e")) ; average 
     ))
  z)

(defmethod prelude ((z stemmer))
  (cond
   ((ends z "lities")          (setto z "lity")) ; abilities
   ((ends z "tivity")          (setto z "tive")) ; sensitivity
   )
   z)

#| In stem(z, b, k), b is a char pointer, and the string to be stemmed is
   from b[0] to b[k] inclusive.  Possibly b[k+1] == '\0', but it is not
   important. The stemmer adjusts the characters b[0] ... b[k] and returns
   the new end-point of the string, k'. Stemming never increases word
   length, so 0 <= k' <= k.
|#
#|
(defmethod mark-y (z)
  (with-slots (str y-found) z
    (setf y-found nil)
    (let ((was-vowel nil))
      (dotimes (i (length str))
        (when (and (char= #\y (char str i))
                   (or (zerop i)
                       was-vowel))
          (setf y-found t)
          (setf (char str i) #\Y))
        (setf was-vowel (vowelp str i)))))
  z)
|#

#|
(defun make-stemmer-instance (str)
  (make-instance 'stemmer :str (make-dynamic-string (string-downcase str))))
|#

(defmethod mark-regions ((z stemmer))
  (with-slots (str j p1 p2) z
    (let ((syllable_count 0)
          (ch_prev_vowel nil)
          (start 0))
      (setf p1 j p2 j)
      (when (exception_p1 z)
        (setf syllable_count 1 start p1))
      (loop
        for i from start to (1- (length str)) do
        (let* ((ch (char str i))
               (ch_cons (is-a-consonant ch)))
          (when (and ch_cons ch_prev_vowel)
            (incf syllable_count)
            (if (= syllable_count 1)
                (setf p1 (1+ i)))
            (when (= syllable_count 2)
              (setf p2 (1+ i))
              (return)))
          (setf ch_prev_vowel (not ch_cons))))))
  z)

(defun get-validator (word exclude-word validate) 
  (let ((last-word "")
        (last-outcome nil))
    #'(lambda(z1)
         (with-slots (str) z1
           ;; don't call validator if already rejected this word.
           (cond
            ((string-equal last-word str)
             last-outcome)
            ((or (not exclude-word) (string-not-equal str word))
             (setf last-word (copy-seq str))
             (setf last-outcome (funcall validate str))))))))
#|
(defun stem-proper-noun (str validate)
  (let ((capped (string-capitalize str))
        (entry nil))
    (loop
      for (ending replacement) in
      '(("")
        ("'s'")
        ("s")
        ("er")
        ("ian" ("ia"))
        ("ish" "and"))
      do
      (when (ends-with capped ending)
        (let ((trunk (subseq capped 0 (- (length capped)
                                         (length ending)))))
          (dolist (r (enlist (or replacement "")))
            (format t "stem capped ~s ending ~s r ~s trunk ~s~%" capped ending r trunk)
            (if (setf entry (funcall validate (format nil "~a~a" trunk r)))
                (return))))))
    entry))
|#

(defun string-copy (str1 str2)
  (setf (fill-pointer str1) 0)
  (string-append str1 str2))

(defun string-capitalize-first-letter (str)
  (setf (char str 0) (char-upcase (char str 0))))

(defun stem-proper-noun (str validate)
  (let* ((capped (make-dynamic-string str))
         (entry nil))
    (string-capitalize-first-letter capped)
    ;;(if (not (in-sorted-file capped)) (return-from stem-proper-noun nil))
    (let ((capstr (copy-seq capped)))
      ;;(format t "capped ~s~%" capped)
      (loop
        with lastch = (last-letter capped)
        for (ending replacement) in 
        '(("") ; Cartesian
          ("'s") ; Italy's, America's
          ("s'")
          ("ies" "y")
          ("s")
          ("er")
          ("ian" ("ia")) ; Scandinavian
          ("an" ("a" "e"))     ; Roman
          ("ish" "and")) ; Polish
        do
        ;;(format t "ending ~s replacement ~s~%" ending replacement)
        (when (or (zerop (length ending))
                  (and (char= lastch (last-letter ending))
                       (ends-with capped ending)))
          (decf (fill-pointer capped) (length ending))
          (dolist (r (enlist (or replacement "")))
            (string-append capped r)
            ;;(format t "stem capped ~s ending ~s r ~s~%" capped ending r)
            (if (setf entry (funcall validate capped))
                (return-from stem-proper-noun entry))
            (string-copy capped capstr)))))
    nil))

(defun stem (str &key (validate #'is-a-word) (exclude-word nil))
  (let ((entry nil))
    (block nil
      (unless exclude-word
        (when (setf entry (funcall validate str))
          ;;(format t "stem entry ~s~%" entry)
          (return))
        
        (if (setf entry (stem-proper-noun str validate))
            (return))
        
        ;; repair broken word
        (unless (ends-with-char str #\e)
          (let ((trunk (make-dynamic-string str)))
            (string-append trunk "e")
            (when (setf entry (funcall validate trunk))
              (return)))))
      
      (let* ((word (string-downcase str))
             (z (make-instance 'stemmer :str (make-dynamic-string word))))
        (with-slots (str j y-found validator) z
          (setf validator (get-validator word exclude-word validate))
          (if (> (length str) 1)
              (let ((number (parse-roman-numerals str)))
                (when number
                  (return (setf entry (list str :analysis :roman-numerals))))))
          (if (and (not exclude-word) (setf entry (funcall validator z)))
              (return))
          (when (> (length str) 2)
            (setf j (length str))
            (let ((marked-y (stemaux::mark-y-as-consonant str)))
              (if marked-y (setf y-found t str marked-y)))
            (mark-regions z)
            (cond
             ((exception1 z)
              (setf entry (list str :analysis :exception)));funcall validation str))))
             (t
              (loop for step in '(step0 step1a exception2 prelude step1b step1c step2 step3
                                        step4 step5 postscript1 postscript2) do
                    (when (and (funcall step z) (setf entry (funcall validator z)))
                      ;;(break "stem-to-valid-word z ~s entry ~s" z entry)
                      (setf entry `(,str 
                                    ,@(if (member :base entry) `(:base ,(cadr (member :base entry))))
                                    :analysis :stemmed))
                      (return entry))))))
          (when (and entry y-found)
            (setf (car entry) (substitute #\y #\Y str))))))
    (unless (or entry (setf entry (funcall validate str)))
      (setf entry (list str :analysis :absent)))
    (unpack entry)))

(defun stem-exclusive (str)
  (stem str :exclude-word t :validate #'is-a-word))

(defun prune ()
  (stemaux::load-lexicon)
  (prune-lexicon :validate #'stem-exclusive))

;;------------------------

#+never
(trace exception_p1 exception1 step0 step1a exception2 step1b step1c step2 step3 step4 step5 postscript1 postscript2)

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
