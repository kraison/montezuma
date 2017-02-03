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
;;; 3.0  -- reanz1959@gmail.com 19 Mar 2016
;;;         This version is backwards compatible with the earlier versions of Porter Stemmer
;;;         in that it produces the same results as they did with same arguments. However, 
;;;         if a call is made to the new entry point, stem-to-valid-word with an 
;;;         input string and a validation function, the result is either a validated 
;;;         word or NIL. In addition, if a validation function is provided, a new step:
;;;         called final-step which takes the last str from the stemmer and the validate
;;;         function, to make a final attempt at reaching a valid stem. In addition, the
;;;         main routine calls the validate function, if provided, after each stemmer step
;;;         which potentially short circuits the stemming process, if possible.
;;;         Any conformant validation function can be passed into stem-to-valid-word but
;;;         this implementation uses an efficient hash table to accomodate over 100,000 words.

(in-package #:porter-stemmer3)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *porter-source-file* (load-time-value (or #.*compile-file-pathname* *load-pathname*)))
  (defvar *porter-data-directory* (merge-pathnames "data/" *porter-source-file*)))

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

(defun final-step[1] (str)
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

(defun final-step[2] (str)
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
#|
(defun arabic-to-roman (AN)
  "Translate from an arabic number AN to a roman number,"
  ;; (arabic-to-roman 1959) returns MCMLIX"
  (let ((ro (make-dynamic-string "")))
    (loop
      until (<= AN 0)
      as numerals = (assoc-if
                       #'(lambda(n) (>= AN n))
                       '((1000 "M")
                         (900  "CM")
                         (500  "D")
                         (400  "CD")
                         (100  "C")
                         (90   "XC")
                         (50   "L")
                         (40   "XL")
                         (10   "X")
                         (9    "IX")
                         (5    "V")
                         (4    "IV")
                         (1    "I")))
      do
      (loop for num across (cadr numerals) do
        (vector-push-extend num ro))
      (decf AN (car numerals)))
    ro))

(defun parse-roman (R)
  (let ((nums
         (loop
           as char across R
           as n = (assoc char 
                         '((#\I 1) 
                           (#\V 5)
                           (#\X 10)
                           (#\L 50)
                           (#\C 100)
                           (#\D 500)
                           (#\M 1000))
                         :test #'char-equal)
           unless n return nil ; not a Roman numeral
           collect (cadr n))))
    (if nums
        (loop as (A B) on nums if A sum (if (and B (< A B)) (- A) A)))))
 
(defun parse-roman-numerals (numerals)
  ;; Developed from code at https://www.rosettacode.org/wiki/Roman_numerals/Decode
  ;; Parse-Roman-numerals translates Roman numerals assuming a well formed 
  ;; Roman number but then encodes the result as a Roman number. 
  ;; Return T if the number is encoded to the same string as the input number
  ;; and return the corresponding Arabic number.
  (let ((number (parse-roman numerals)))    
    (if (and number (string-equal numerals (arabic-to-roman number)))
        (make-instance 'roman-number :word numerals :analysis :roman-numerals :arabic number))))

(defun test-roman (&optional (number 4000))
  (dotimes (n number)
    (let* ((num (1+ n))
           (ro (arabic-to-roman num))
           (rof (format nil "~@R" num)))
      (multiple-value-bind (is-roman arabic) (parse-roman ro)
        (unless (and is-roman (string-equal rof ro))
          (format t "Roman failure: ~d parse: ~d roman: ~a format: ~a~%" 
            num arabic ro rof))))))
|#

;;(defvar lexicon (make-hash-table :test #'equal) "Global table of valid word symbols.")

(defclass lexeme ()
  ((word :initarg :word :accessor word)
   (base :initarg :base :accessor base :initform nil)
   (analysis :initarg :analysis :accessor analysis)))

(defmethod print-object ((self lexeme) stream)
  (format stream "#<~s :word ~s :base ~s ~s>" (type-of self) (word self) (base self) (analysis self)))

(defclass roman-number (lexeme)
  ((arabic :initarg :arabic :accessor arabic)))

(defmethod print-object ((self roman-number) stream)
  (format stream "#<~s ~s :arabic ~d :analysis ~s>" (type-of self) (word self) (arabic self) (analysis self)))

(defun make-dynamic-string (string)
  (make-array (length string)
              :element-type 'character
              :adjustable t
              :fill-pointer (length string)
              :initial-contents string))

(defmethod get-lexeme ((entry lexeme))
  (values (or (base entry) (word entry)) (analysis entry)))

(defun Porter3a (inflected-word validator)
  (let* ((str (make-dynamic-string inflected-word))
         (last-word inflected-word)
         (entry nil)
         (validation
          #'(lambda(word)
              ; don't call the validator if we have already rejected this word.
              (when (string/= last-word word)
                (setf last-word (copy-seq word))
                (funcall validator word)))))
    (find-if #'(lambda(step) (setf entry (funcall validation (funcall step str))))
             '(prelude step1ab step1c step2 step3 step4 step5 final-step[1] final-step[2]))
    (if entry (make-instance 'lexeme :word (word entry) :base (base entry) :analysis :stemmed))))

(defun non-alphanumeric (str)
  (if (find-if-not #'alpha-char-p str)
      (make-instance 'lexeme :word str :analysis :non-alphabetic)))

(defun undersize (str)
  (if (<= (length str) 2)
      (make-instance 'lexeme :word str :analysis :undersize)))

(defun absent (str)
  (make-instance 'lexeme :word str :analysis :absent))

(defun Porter3 (str &optional (validator #'is-a-word))
  ;; Execute Porter 3 stemming to get the first valid stem
  (let* ((inflected-word (string-downcase str)))
    (get-lexeme
     (or (funcall validator inflected-word)
         (parse-roman-numerals str)
         (non-alphanumeric str)
         (undersize str)
         (Porter3a inflected-word validator)
         (absent inflected-word)))))

(defmethod stem-to-valid-word ((word symbol) &key (validate #'is-a-word))
  (stem-to-valid-word (string word) :validate validate))

(defmethod stem-to-valid-word ((word null) &key (validate #'is-a-word))
  (declare (ignore validate))
  (error "Symbol or string expected for stem-to-valid-word! Got ~s." word))

(defmethod stem-to-valid-word ((str string) &key (validate #'is-a-word))
  ;; Strings of length < 3 don't go through the stemming process.
  ;;
  ;; VALIDATE is an optional function that should return a lexeme 
  ;; and an outcome designator (a keyword) if the argument is valid.
  (cond
   (validate
    (Porter3 str validate))
   ((find-if-not #'alpha-char-p str)
    (values str :non-alphabetic))
   ((<= (length str) 2)
    (values str :undersize))
   (t
    (setf str (make-dynamic-string str))
    ;; execute the stemming rules to get the shortest stem
    (step1ab str)
    (step1c str)
    (step2 str)
    (step3 str)
    (step4 str)
    (step5 str))))

;; STEM provides Porter Stemming with the same output as Porter Stemmer 1.01 
(defun stem (str)
  (stem-to-valid-word str :validate nil))

;;;; This is the original stemmer test which may not yield real words
(defun test-stemmer () ; Run against the distributed test files without validation
  (with-open-file (f1 (merge-pathnames "porter-stemmer-input.txt" *porter-data-directory*))
    (with-open-file (f2  (merge-pathnames "porter-stemmer-output.txt" *porter-data-directory*))
      (loop
        as w1 = (read-line f1 nil nil)
        while w1
        as w2 = (read-line f2 nil nil)
        as w3 = (stem w1)
        if (equal w2 w3)
        count t into successes
        else count t into failures
        and do (format t "(stem ~s) => ~s wanted ~s~%" w1 w3 w2)
        finally (progn (format t "sucesses ~d failures ~d~%" successes failures)
                  (return failures))))))

(defun summary-line (stream total style name &rest counts)
  (format stream "  <tr class='~(~a~)'>~%" style)
  (format stream "    <td>~(~a~)</td>~%" name)
  (dolist (count counts)
    (format stream "    <td align='right'>~d</td>~%" count))
  (dolist (count counts)
    (format stream "    <td align='right'>~d%</td>~%" (round (float (* 100 (/ count total))))))
  (format stream "  </tr>~%"))

(defun summary-totals (stream total style name &rest counts)
  (format stream "  <tr class='~(~a~)'>~%" style)
  (format stream "    <td>~(~a~)</td>~%" name)
  (dolist (count counts)
    (format stream "    <td colspan=3 align='right'>~d</td>~%" count))
  (dolist (count counts)
    (format stream "    <td colspan=3 align='right'>~d%</td>~%" (round (float (* 100 (/ count total))))))
  (format stream "  </tr>~%"))

(defun test-stemmer2 (&optional (number 0))
  ;; Run against the distributed test file but count successes when we stem to a validated word
  (with-open-file (f1 (merge-pathnames "porter-stemmer-input.txt" *porter-data-directory*))
    (with-open-file (f2 (merge-pathnames "porter-stemmer-test-output.txt" *porter-data-directory*)
                        :direction :output
                        :if-exists :supersede)
      (let ((line-count 0)
            (results ()))
        (loop
          for w1 = (read-line f1 nil nil)
          while (and w1 (or (zerop number) (< line-count number))) do
          (multiple-value-bind (w2 analysis) (stem-to-valid-word w1)
            ;;(if w2 (incf successes) (incf failures))
            (format f2 "<tr><td>~d</td><td><code>~a</code></td>" (incf line-count) w1)
            (format f2 "<td><code>~a</code></td><td><code>~a</code></td>~%"
              (stem w1) ; Porter Stemmer 1.01
              w2)
            (format f2 "<td class='~(~a~)'>~(~a~)</td></tr>~%" analysis analysis)
            (unless (assoc analysis results :test #'string=) (push (list analysis 0) results))
            (incf (cadr (assoc analysis results)))))
        (format t "<table>~%")
        (dolist (a results)
          (apply #'summary-line f2 line-count (car a) a))
        (summary-line t "total" "Total Input Words" line-count line-count)
        (format t "</table>~%")
        line-count))))
#|
(defun split-line (line)
  (let* ((i (position #\space line))
         (j (position-if-not #'(lambda(ch) (char= ch #\space)) line :start i)))
    (values (subseq line 0 i) (subseq line j))))         
|#

(defun sanity-check (&optional (wordlist (collect-words (merge-pathnames "word-list.txt" *porter-data-directory*))))
  (format t "<table id='sanity-check' class='mytable' width='600' align='center'>~%")
  (format t "<thead>~%")
  (format t "<tr>~%")
  (format t "  <th width='10%' align='left'>Line</th>~%")
  (format t "  <th width='5%' align='left'>Input</th>~%")
  (format t "  <th width='10%' align='left'>Analysis<sup>1</sup></th>~%")
  (format t "  <th width='10%' align='left'>Porter<sup>1</sup></th>~%")
  (format t "  <th width='10%' align='left'>Analysis<sup>3</sup></th>~%")
  (format t "  <th width='10%' align='left'>Porter<sup>3</sup></th>~%")
  (format t "</tr>~%")
  (format t "</thead>~%")
  (format t "<tbody>~%")
  (let ((count 0))
    (dolist (wordin wordlist)
      (incf count)
      (multiple-value-bind (word3out analysis3) (stem-to-valid-word wordin)
        (let ((word-out1 (stem wordin)))
          (format t "<tr>~%")
          (format t "  <td align='right'>~d</td>~%" count)          
          (format t "  <td><code>~a</code></td>~%" wordin)
          (format t "  <td class='~(~a~)'>~(~:*~a~)</td>~%" (analyse wordin word-out1))
          (format t "  <td><code>~a</code></td>~%" word-out1)
          (format t "  <td class='~(~a~)'>~(~:*~a~)</td>~%"  analysis3)
          (format t "  <td><code>~a</code></td>~%" word3out)
          (format t "</tr>~%"))))
    (format t "</tbody>~%")
    (format t "</table>~%")))
 
(defun collect-words (&optional (path (merge-pathnames "porter-stemmer-input.txt" *porter-data-directory*)))
  (with-open-file (f1 path)
    (loop 
      as word = (read-line f1 nil nil) while word 
      collect (string-trim '(#\Space #\Tab #\Newline) word))))

(defun time-check (&key (algorithm 3) (wordlist (collect-words)) (repeats 10000))
  (time
   (cond
    ((= algorithm 1)
     (dotimes (n repeats)
       (dolist (wordin wordlist)
         (stem wordin))))
    (t
     (dotimes (n repeats)
       (dolist (wordin wordlist)
         (porter3 wordin))))))
  (* repeats (length wordlist)))

(defun timings ()
  (compile-file *porter-source-file*)
  (let ((wordlist1 (collect-words (merge-pathnames "word-list.txt" *porter-data-directory*))))
    (format t "timings[1]: word-list.txt Porter1~%")
    (time-check :algorithm 1 :repeats 10000 :wordlist wordlist1)
    (format t "timings[1]: word-list.txt Porter3~%")
    (time-check :algorithm 3 :repeats 10000 :wordlist wordlist1))
  (let ((wordlist2 (collect-words (merge-pathnames "porter-stemmer-input.txt" *porter-data-directory*))))
    (format t "timings[2]: porter-stemmer-input.txt Porter1~%")
    (time-check :algorithm 1 :repeats 50 :wordlist wordlist2)
    (format t "timings[2]: porter-stemmer-input.txt Porter3~%")
    (time-check :algorithm 3 :repeats 50 :wordlist wordlist2)))

(defun test-stemmer3 (&optional (number ()) (filter ()))
  ;; Run against the distributed test file for Porter Stemmer2 counting successes when we stem to a validated word
  (with-open-file (f1 (merge-pathnames "porter-stemmer2-diffs.txt" *porter-data-directory*))
    (with-open-file (f2 (merge-pathnames (format nil "porter-stemmer-diffs-~a-output.html"
                                           (or number "complete"))
                                         *porter-data-directory*)
                        :direction :output
                        :if-exists :supersede)
      (let ((line-count 0)
            (results ()))
        (format f2 "<table id='Outcomes' class='mytable' width='600'>~%")
        (format f2 "<thead>~%")
        (format f2 "<tr>~%")
        (format f2 "  <th width='4%' align='left'>Line</th>~%")
        (format f2 "  <th width='10%' align='left'>Input</th>~%")
        (format f2 "  <th width='10%' align='left'>Analysis<sup>1</sup></th>~%")
        (format f2 "  <th width='10%' align='left'>Porter<sup>1</sup></th>~%")
        (format f2 "  <th width='10%' align='left'>Analysis<sup>2</sup></th>~%")
        (format f2 "  <th width='10%' align='left'>Porter<sup>2</sup></th>~%")
        (format f2 "  <th width='10%' align='left'>Analysis<sup>3</sup></th>~%")
        (format f2 "  <th width='10%' align='left'>Porter<sup>3</sup></th>~%")
        (format f2 "</tr>~%")
        (format f2 "</thead>~%")
        (format f2 "<tbody>~%")
        (loop
          for line = (read-line f1 nil nil)
          while (and line (or (null number) (< line-count number))) do
          (multiple-value-bind (wordin word2out) (split-line line)
            (multiple-value-bind (word3out analysis3) (stem-to-valid-word wordin)
              (when (or (null filter) (find analysis3 filter))
                (incf line-count)
                (let* ((word-out1 (stem wordin))
                       (analysis1 (analyse wordin word-out1))
                       (analysis2 (analyse wordin word2out)))
                  (format f2 "<tr>~%")
                  (format f2 "  <td align='right'>~d</td>~%" line-count)  
                  (format f2 "  <td><code>~a</code></td>~%" wordin)              
                  (format f2 "  <td class='~(~a~)'>~(~:*~a~)</td>~%" analysis1)
                  (format f2 "  <td><code>~a</code></td>~%" word-out1)
                  (format f2 "  <td class='~(~a~)'>~(~:*~a~)</td>~%" analysis2)
                  (format f2 "  <td><code>~a</code></td>~%" word2out)                
                  (format f2 "  <td class='~(~a~)'>~(~:*~a~)</td>~%" analysis3)
                  (format f2 "  <td><code>~a</code></td>~%"  word3out)
                  (format f2 "</tr>~%")
                
                  (let ((columns (list analysis1 analysis2 analysis3)))
                    (dolist (column columns)
                      (unless (assoc column results)
                        (push (list column 0 0 0) results)))
                    (dotimes (i (length columns))
                      (let ((column (nth i columns)))
                        (incf (nth i (cdr (assoc column results))))))))))))
        (format f2 "</tbody>~%")
        (format t "<table>~%")
        (dolist (a (sort results #'string-lessp :key #'(lambda(x)(string (car x)))))
          (apply #'summary-line t line-count (car a) a))
        (summary-totals t line-count "tfoot" "Total Input Words" line-count)
        (format t "</table>~%")
        line-count))))


(defun make-artefacts ()
  (test-stemmer3 2000)
  (test-stemmer3)
  (test-stemmer3 nil '(:irregular :roman-numerals)))

#+never
(trace prelude step1ab step1c step2 step3 step4 step5 final-step[1] final-step[2])

(defmethod map-words ((str string)
                      (func function)
                      &key (start 0)
                      (end (length str))
                      (letterp #'alphanumericp))
  ;; Call FUNC with ranges of any unbroken sequences of characters
  ;; that pass LETTERP in STR from START to END
  (setf
   start (min (length str) (max 0 start))
   end (max 0 (min end (length str))))
  (loop
    for i from start to (1- end) do
    (unless (funcall letterp (char str i))
      (if (> i start) (funcall func start i))
      (setf start (1+ i)))
    finally (if (> end start) (funcall func start end))))

(defun count-words (str &key (start 0)
                        (end (length str))
                        (letterp #'alphanumericp))
  ;; Return the number of unbroken sequences of letters and numbers in LINE
  (let ((n 0))
    (map-words str
               #'(lambda(&rest args) (declare (ignore args)) (incf n))
               :start start :end end :letterp letterp)
    n))

(defun list-words (str &key (start 0) (end (length str)) (letterp #'alphanumericp))
  ;; Return a list of the unbroken sequences of letters and numbers in LINE
  (let ((word-list ()))
    (map-words str #'(lambda(x y) (push (subseq str x y) word-list)) 
               :start start
               :end end
               :letterp letterp)
    (nreverse word-list)))

(defun test-words (str)
  (map-words str #'(lambda(start end) (format t "~s~%" (subseq str start end)))))

(defun time-map-words (str repeats)
  (time (dotimes (i repeats) (map-words str #'(lambda(&rest args) (declare (ignore args)))))))

(defun wc (&optional (path *porter-source-file*)) 
  ;; Return the number of lines, words, and characters in the file pathname PATH
  (with-open-file (f path)
    (loop
      as line = (read-line f nil nil)
      while line
      count t into line-count
      summing (count-words line) into word-count
      summing (1+ (length line)) into character-count
      finally (return (values line-count word-count character-count)))))

#|
(ps::wc (merge-pathnames "porter-stemmer-words-original.txt" ps::*porter-data-directory*))
(ps::wc (merge-pathnames "porter-stemmer-words.txt" PS::*porter-data-directory*))
(toronto::diff "D:\\allegro-projects\\toronto\\src\\porter-stemmer\\porter-stemmer-1.01.lisp" "D:\\allegro-projects\\toronto\\src\\porter-stemmer\\porter-stemmer-2.0.lisp")
|#
