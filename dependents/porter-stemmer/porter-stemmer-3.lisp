;; -*- mode: common-lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Porter Stemming Algorithm, somewhat mechanically hand translated to Common Lisp by
;; Steven M. Haflich smh@franz.com Feb 2002.  Most of the inline comments refer to the
;; original C code.  At the time of this translation the code passes the associated Porter
;; test files.  See the function test at the end of this file.

;; This port is intended to be portable ANSI Common Lisp.  However, it has only been
;; compiled and tested with Allegro Common Lisp.  This code is offered in the hope it will
;; be useful, but with no warranty of correctness, suitability, usability, or anything
;; else.  The C implementation from which this code was derived was not reentrant, relying
;; on global variables.  This implementation corrects that.  It is intended that a word to
;; be stemmed will be in a string with fill-pointer, as this is a natural result when
;; parsing user input, web scraping, whatever.  If not, a string with fill-pointer is
;; created, but this is an efficiency hit and is here intended only for lightweight use or
;; testing.  Using some resource mechanism on these strings would be a useful improvement,
;; whether here or in the calling code.

;; Postscript: When I contacted Martin Porter about this anachronism, he decided to fix
;; the C version to implement proper reentrancy.  The CL version is now also served from
;; his central site.  It should be functionally identical to this one, modulo the current
;; comment and a couple harmless formatting and comment changes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the Porter stemming algorithm, originally coded up in ANSI C by the author.
;; It may be be regarded as canonical, in that it follows the algorithm presented in
;;
;; Porter, 1980, An algorithm for suffix stripping, Program, Vol. 14, no. 3, pp 130-137,

;; only differing from it at the points marked --DEPARTURE-- below.

;; See also http://www.tartarus.org/~martin/PorterStemmer

;; The algorithm as described in the paper could be exactly replicated
;; by adjusting the points of DEPARTURE, but this is barely necessary,
;; because (a) the points of DEPARTURE are definitely improvements, and
;; (b) no encoding of the Porter stemmer I have seen is anything like
;; as exact as this version, even with the points of DEPARTURE!

;; You can compile it on Unix with 'gcc -O3 -o stem stem.c' after which
;; 'stem' takes a list of inputs and sends the stemmed equivalent to
;; stdout.

;; The algorithm as encoded here is particularly fast.

;; Release 1

;; The main part of the stemming algorithm starts here. b is a buffer
;; holding a word to be stemmed. The letters are in b[k0], b[k0+1] ...
;; ending at b[k]. In fact k0 = 0 in this demo program. k is readjusted
;; downwards as the stemming progresses. Zero termination is not in fact
;; used in the algorithm.

;; Note that only lower case sequences are stemmed. Forcing to lower case
;; should be done before stem(...) is called.

;; (consonantp str i) is TRUE <=> str[i] is a consonant.

;;;
;;; Common Lisp port Version history
;;;
;;; 1.0  -- smh@franz.com Feb 2002
;;;         initial release
;;;
;;; 1.01 -- smh@franz.com 25 Apr 2004
;;;         step4 signalled error for "ion" "ions".  Thanks to Jeff Heard
;;;         for detecting this and suggesting the fix.
;;;
;;; 3.0  -- reanz1959@gmail.com 16 May 2016
;;;         This version of Porter Stemmer is a hybrid built on Porter Stemmer 1.01 (Common Lisp)
;;;         with new code toan inflected validate stems. Results are either absent or
;;;         a word in the lexicon, or a stem derived by suffix removal.
;;;         In addition, a new step called final-step which takes the last str from 
;;;         the stemmer and the validate function, to make a last attempt at reaching a valid stem.
;;;         In addition, the main routine calls the validate function, if provided, 
;;;         after each stemmer step which potentially short circuits the stemming process,
;;;         if possible. Any conformant validation function can be passed into stem-to-valid-word but
;;;         this implementation uses an efficient hash table to accomodate over 100,000 words.

(in-package #:porter-stemmer3)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *source-file* (load-time-value (or #.*compile-file-pathname* *load-pathname*)))
  (defvar *data-directory* (merge-pathnames "data/" *source-file*)))

(defun consonant (str i)
  (let ((char (char str i)))
    (cond ((member char '(#\a #\e #\i #\o #\u)) nil)
          ((eql char #\y)
           (if (= i 0) t (not (consonant str (1- i)))))
          (t t))))

;; m() measures the number of consonant sequences between k0 and j. if c is
;; a consonant sequence and v a vowel sequence, and <..> indicates arbitrary
;; presence,

;;    <c><v>       gives 0
;;    <c>vc<v>     gives 1
;;    <c>vcvc<v>   gives 2
;;    <c>vcvcvc<v> gives 3
;;    ....

(defun m (str lim)
  (let ((n 0)
        (i 0))
    (loop
      (when (>= i lim) (return-from m n))
      (if (not (consonant str i)) (return nil))
      (incf i))
    (incf i)
    (loop
      (loop
        (if (>= i lim) (return-from m n))
        (if (consonant str i) (return nil))
        (incf i))
      (incf i)
      (incf n)
      (loop
        (if (>= i lim) (return-from m n))
        (if (not (consonant str i)) (return nil))
        (incf i))
      (incf i))))

;; vowelinstem() is TRUE <=> k0,...j contains a vowel

(defun vowelinstem (str)
  (loop for i from 0 below (fill-pointer str)
      unless (consonant str i) return t))

;; doublec(j) is TRUE <=> j,(j-1) contain a double consonant.

(defun double-consonant (str i)
  (cond ((< i 1) nil)
        ((not (eql (char str i) (char str (1- i)))) nil)
        (t (consonant str i))))

;; cvc(i) is TRUE <=> i-2,i-1,i has the form consonant - vowel - consonant
;; and also if the second c is not w,x or y. this is used when trying to
;; restore an e at the end of a short word. e.g.

;;    cav(e), lov(e), hop(e), crim(e), but
;;    snow, box, tray.

(defun cvc (str lim)
  (decf lim)
  (if (or (< lim 2)
          (not (consonant str lim))
          (consonant str (1- lim))
          (not (consonant str (- lim 2))))
      (return-from cvc nil))
  (if (member (char str lim) '(#\w #\x #\y)) (return-from cvc nil))
  t)

;; ends(s) is TRUE <=> k0,...k ends with the string s.

(defun ends (str ending)
  (declare (string str) (simple-string ending))
  (let ((len1 (length str)) (len2 (length ending)))
    (loop
      for pa downfrom (1- len1) to 0
      and pb downfrom (1- len2) to 0
      unless (eql (char str pa) (char ending pb))
      return nil
      finally (return (when (< pb 0)
                        (decf (fill-pointer str) len2)
                        t)))))

;; setto(s) sets (j+1),...k to the characters in the string s, readjusting k.

(defun setto (str suffix)
  (declare (string str) (simple-string suffix))
  (loop for char across suffix
      do (vector-push-extend char str)))

;; r(s) is used further down.

(defun r (str s sfp)
  (if (> (m str (fill-pointer str)) 0)
      (setto str s)
    (setf (fill-pointer str) sfp)))

;; step1ab() gets rid of plurals and -ed or -ing. e.g.

;;     caresses  ->  caress
;;     ponies    ->  poni
;;     ties      ->  ti
;;     caress    ->  caress
;;     cats      ->  cat

;;     feed      ->  feed
;;     agreed    ->  agree
;;     disabled  ->  disable

;;     matting   ->  mat
;;     mating    ->  mate
;;     meeting   ->  meet
;;     milling   ->  mill
;;     messing   ->  mess

;;     meetings  ->  meet

(defun step1ab (str)
  (when (eql (char str (1- (fill-pointer str))) #\s)
    (cond ((ends str "sses") (incf (fill-pointer str) 2))
          ((ends str "ies")  (setto str "i"))
          ((not (eql (char str (- (fill-pointer str) 2)) #\s)) (decf (fill-pointer str)))))
  (cond ((ends str "eed") (if (> (m str (fill-pointer str)) 0)
                              (incf (fill-pointer str) 2)
                            (incf (fill-pointer str) 3)))
        ((let ((sfp (fill-pointer str)))
           (if (or (ends str "ed")
                   (ends str "ing"))
               (if (vowelinstem str)
                   t
                 (progn (setf (fill-pointer str) sfp)
                   nil))))
         (cond ((ends str "at") (setto str "ate"))
               ((ends str "bl") (setto str "ble"))
               ((ends str "iz") (setto str "ize"))
               ((double-consonant str (1- (fill-pointer str)))
                (unless (member (char str (1- (fill-pointer str))) '(#\l #\s #\z))
                  (decf (fill-pointer str))))
               (t (if (and (= (m str (fill-pointer str)) 1)
                           (cvc str (fill-pointer str)))
                      (setto str "e"))))))
  str)

;; step1c() turns terminal y to i when there is another vowel in the stem.

(defun step1c (str)
  (let ((saved-fill-pointer (fill-pointer str)))
    (when (and (ends str "y")
               (vowelinstem str))
      (setf (char str (fill-pointer str)) #\i))
    (setf (fill-pointer str) saved-fill-pointer))
  str)

;; step2() maps double suffices to single ones. so -ization ( = -ize plus
;; -ation) maps to -ize etc. note that the string before the suffix must give
;; m() > 0.

(defun step2 (str)
  (let ((sfp (fill-pointer str)))
    (when (> sfp 2)
      (block nil
        (case (char str (- (length str) 2))
          (#\a (when (ends str "ational") (r str "ate"  sfp)  (return))
           (when (ends str "tional")  (r str "tion" sfp) (return)))
          (#\c (when (ends str "enci")    (r str "ence" sfp) (return))
           (when (ends str "anci")    (r str "ance" sfp) (return)))
          (#\e (when (ends str "izer")    (r str "ize"  sfp)  (return)))
          (#\l (when (ends str "bli")     (r str "ble"  sfp)  (return))
           ;; -DEPARTURE-
           ;; To match the published algorithm, replace prev line with
           ;; ((when (ends str "abli")    (r str "able" sfp) (return))
           (when (ends str "alli")    (r str "al"  sfp)   (return))
           (when (ends str "entli")   (r str "ent" sfp)  (return))
           (when (ends str "eli")     (r str "e"   sfp)    (return))
           (when (ends str "ousli")   (r str "ous" sfp)  (return)))
          (#\o (when (ends str "ization") (r str "ize" sfp)  (return))
           (when (ends str "ation")   (r str "ate" sfp)  (return))
           (when (ends str "ator")    (r str "ate" sfp)  (return)))
          (#\s (when (ends str "alism")   (r str "al"  sfp)   (return))
           (when (ends str "iveness") (r str "ive" sfp)  (return))
           (when (ends str "fulness") (r str "ful" sfp)  (return))
           (when (ends str "ousness") (r str "ous" sfp)  (return)))
          (#\t (when (ends str "aliti")   (r str "al"  sfp)   (return))
           (when (ends str "iviti")   (r str "ive" sfp)  (return))
           (when (ends str "biliti")  (r str "ble" sfp)  (return)))
          ;; -DEPARTURE-
          ;; To match the published algorithm, delete next line.
          (#\g (when (ends str "logi")    (r str "log" sfp)  (return)))))))
  str)

;; step3() deals with -ic-, -full, -ness etc. similar strategy to step2.

(defun step3 (str)
  (let ((sfp (fill-pointer str)))
    (block nil
      (case (char str (1- (length str)))
        (#\e (when (ends str "icate") (r str "ic" sfp) (return))
         (when (ends str "ative") (r str "" sfp)   (return)) ; huh?
         (when (ends str "alize") (r str "al" sfp) (return)))
        (#\i (when (ends str "iciti") (r str "ic" sfp) (return)))
        (#\l (when (ends str "ical")  (r str "ic" sfp) (return))
         (when (ends str "ful")   (r str "" sfp)   (return))) ; huh?
        (#\s (when (ends str "ness")  (r str "" sfp)   (return))) ; huh?
        )))
  str)

;; step4() takes off -ant, -ence etc., in context <c>vcvc<v>.

(defun step4 (str)
  (let ((sfp (fill-pointer str)))
    (when (> sfp 2)			; Unnecessary?
      (block nil
        (case (char str (- sfp 2))
          (#\a (if (ends str "al")    (return)))
          (#\c (if (ends str "ance")  (return))
           (if (ends str "ence")  (return)))
          (#\e (if (ends str "er")    (return)))
          (#\i (if (ends str "ic")    (return)))
          (#\l (if (ends str "able")  (return))
           (if (ends str "ible")  (return)))
          (#\n (if (ends str "ant")   (return))
           (if (ends str "ement") (return))
           (if (ends str "ment")  (return))
           (if (ends str "ent")   (return)))
          (#\o (if (ends str "ion")
                   (let ((len (length str)))
                     (if (and (> len 0)
                              (let ((c (char str (1- len))))
                                (or (eql c #\s) (eql c #\t))))
                         (return)
                       (setf (fill-pointer str) sfp))))
           (if (ends str "ou")    (return))) ; takes care of -ous
          (#\s (if (ends str "ism")   (return)))
          (#\t (if (ends str "ate")   (return))
           (if (ends str "iti")   (return)))
          (#\u (if (ends str "ous")   (return)))
          (#\v (if (ends str "ive")   (return)))
          (#\z (if (ends str "ize")   (return))))
        (return-from step4 str))
      (unless (> (m str (fill-pointer str)) 1)
        (setf (fill-pointer str) sfp)))
    str))

;; step5() removes a final -e if m() > 1, and changes -ll to -l if m() > 1.

(defun step5 (str)
  (let ((len (fill-pointer str)))
    (if (eql (char str (1- len)) #\e)
        (let ((a (m str len)))
          (if (or (> a 1)
                  (and (= a 1)
                       (not (cvc str (1- len)))))
              (decf (fill-pointer str))))))
  (let ((len (fill-pointer str)))
    (if (and (eql (char str (1- len)) #\l)
             (double-consonant str (1- len))
             (> (m str len) 1))
        (decf (fill-pointer str))))
  str)

(defun final-step1 (str)
  ;; Return a STRING from STR
  (let ((len (fill-pointer str)))
    (cond
     ((ends str "eress")         (setto str "erer")) ; adulteress
     ((double-consonant str (1- len))     (decf (fill-pointer str))) ; occurrence
     ((ends str "elv")           (setto str "elf")) ; yourselves
     ((ends str "i")             (setto str "y")) ; victorious
     ((ends str "est")           (setto str "")) ; wouldest, bravest
     ((ends str "eth")           (setto str "")) ; vieweth
     ((ends str "men")           (setto str "man")) ; tradesmen
     )
    str))

(defun final-step2 (str)
  ;; Return a STRING from STR
  (let ((len (fill-pointer str)))
    (cond 
     ((ends str "e")             (decf (fill-pointer str))) ; talkest
     ((double-consonant str (1- len))     (decf (fill-pointer str))) ; awfullest
     ((not (ends str "e"))       (setto str "e")) ; average 
     )
    str))

(defun prelude (str)
  (cond
   ((ends str "lities")          (setto str "lity")) ; abilities
   ((ends str "tivity")          (setto str "tive")) ; sensitivity
   )
   str)

(defun stem (str &optional (validate #'is-a-word))
  ;; Strings of length < 3 don't go through the stemming process.
  ;;
  ;; VALIDATE is an optional function that should return a lexeme 
  ;; and an outcome designator (a keyword) if the argument is valid.
  ;;
  ;; Execute Porter 3 stemming to get the first valid stem
  (let ((inflected-word (make-dynamic-string (string-downcase str)))
        (entry))
    (block nil
      (or (when (parse-roman-numerals str)
            (setf entry (list str :analysis :roman-numerals)))
          (when (find-if-not #'alpha-char-p str)
            (setf entry (list str :analysis :non-alphabetic)))
          (if (<= (length str) 2)
              (setf entry (list str :analysis :undersize)))
          (setf entry (funcall validate inflected-word))
          (let* ((last-word inflected-word)
                 (validation
                  #'(lambda(word)
                      ; don't call the validator if we have already rejected this word.
                      (when (string/= last-word word)
                        (setf last-word (copy-seq word))
                        (funcall validate word)))))
            (loop
              for step in '(prelude step1ab step1c step2 step3 step4 step5 final-step1 final-step2)
              if (setf entry (funcall validation (funcall step inflected-word))) do (return)
              finally (setf entry (list inflected-word :analysis :absent)))))
      (values (or (cadr (member :base entry)) (car entry))
              (cadr (member :analysis entry))))))

#+never
(trace prelude step1ab step1c step2 step3 step4 step5 final-step[1] final-step[2])
