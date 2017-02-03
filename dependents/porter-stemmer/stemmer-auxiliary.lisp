(in-package :stemaux)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *source-file* (load-time-value (or #.*compile-file-pathname* *load-pathname*)))
  (defconstant *data-directory* (merge-pathnames "data/" *source-file*))
  (defconstant *mobypos* (merge-pathnames "english-words-master/mobypos.txt" *data-directory*))
  (defconstant *propernouns* (merge-pathnames "proper-nouns.txt" *data-directory*))
  (defconstant *words* (merge-pathnames "english-words-master/words.txt" *data-directory*))
  (defconstant *words2* (merge-pathnames "english-words-master/words2.txt" *data-directory*)))

(defun make-dynamic-string (string)
  (make-array (length string)
              :element-type 'character
              :adjustable t
              :fill-pointer (length string)
              :initial-contents string))

(defvar lexicon (make-hash-table :test #'equal) "Global table of valid word symbols.")

(defun map-lexicon (func)
  (maphash func lexicon))

(defun y-as-consonant (word &key (start 0))
  (let ((last-ch nil))
    (position-if #'(lambda (ch)
                     (prog1
                         (and (char= ch #\y)
                              (or (null last-ch)
                                  (is-a-vowel last-ch)))
                       (setf last-ch ch)))
                 word
                 :start start)))

(defun mark-y-as-consonant (word)
  (let ((ypos (y-as-consonant word)))
    (when ypos
      (let ((str (make-dynamic-string word)))
        (loop
          while ypos do
          (setf (char str ypos) #\Y)
          (setf ypos (y-as-consonant word :start (1+ ypos))))
        str))))

(defun get-analysis (entry)
  (or (cadr (member :analysis entry))
      :present))

(defun get-base (entry)
  (cadr (member :base entry)))

(defun get-source (entry)
  (or (cadr (member :source entry))
      (if (eq :irregular (get-analysis entry))
          "irregulars")
      "GCIDE"))

(defun is-a-word (str)
  "Return a lexicon entry for a valid word and load the lexicon, if necessary."
  (if (zerop (hash-table-count lexicon))
      (load-lexicon))
  (let* ((entry (gethash str lexicon))
         (base (get-base entry)))
    (if entry
        `(,str
          ,@(if base (list :base base))
          :analysis ,(get-analysis entry)
          :source ,(get-source entry)))))
    
(defun prune-lexicon (&key (validate nil))
  ;; remove lexicon entries that are redundant which means they have
  ;; an entry in the lexicon but it can also be reached by stemming.
  (let ((visited (make-hash-table :test #'equal)))
    (block nil
      (map-lexicon 
       #'(lambda(key value)
           (multiple-value-bind (base analysis) (funcall validate key)
             (declare (ignore base))
             (when (eq analysis :stemmed)
               (setf (gethash key visited) value)
               ;;(format t "~s ~s ~s~%" key value analysis)
               ))))

      (map-lexicon
       #'(lambda(key value)
           (declare (ignore key))
           ;;(format t "+~s ~s~%" key value)
           (remhash (get-base value) visited)))

      (maphash
       #'(lambda(key value)
           ;;(format t "-~s ~s~%" key value))
           (declare (ignore value))
           (remhash key lexicon))
       visited)
        
      (let ((keys ()))
        (map-lexicon #'(lambda(key value) 
                         (declare (ignore value))
                         (push key keys)))
        (setf keys (sort keys #'string< :key #'(lambda(x) (reverse x))))
        (with-open-file (lexemes (merge-pathnames "lexemes-pruned.txt" *data-directory*)
                                 :direction :output :if-exists :supersede)

          (dolist (key keys)
            (let ((value (gethash key lexicon)))
              (format lexemes "~s~%" value))))))
         
      (values (hash-table-count lexicon)
              (hash-table-count visited))))

(defmethod unpack ((self list))
  (values (or (cadr (member :base self)) (car self))
          (cadr (member :analysis self))))

(defmethod last-but ((str string) n)
  (let ((index (- (length str) (1+ n))))
    (when (>= index 0)
      (char str index))))

(defmethod last-letter ((str string))
  (unless (zerop (length str))
    (char str (1- (length str)))))

(defun is-a-vowel (ch)
  (find ch "aeiouy"))

(defun vowelp (str i)
  (is-a-vowel (char str i)))

(defun is-a-consonant (ch)
  (not (is-a-vowel ch)))

(defun consonantp (str i)
  (is-a-consonant (char str i)))

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

(defun vowel-in-stem (str j)
  (find-if #'is-a-vowel str :end j))

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

(defun starts-with (str prefix)
  (let ((pref-length (length prefix)))
    (and (<= pref-length (length str))
         (string= str prefix :end1 pref-length))))

(defun ends-with (str ending)
  (let ((len1 (length str))
        (len2 (length ending)))
    (and (>= len1 len2) 
         (string= str ending :start1 (- len1 len2)))))

(defun ends-with-char (str ch)
  (char= ch (last-letter str)))

(defconstant roman-magnitudes
    '((1000 "M") (900 "CM") (500  "D") (400  "CD") (100  "C") (90  "XC") (50   "L")
      (40   "XL") (10   "X") (9   "IX") (5    "V") (4    "IV") (1    "I")))

(defun arabic-to-roman (AN)
  "Translate from an arabic number AN to a roman number,"
  ;; (arabic-to-roman 1959) returns MCMLIX"
  (let ((ro (make-dynamic-string "")))
    (loop
      until (<= AN 0)
      as (number letters) = (assoc-if #'(lambda(n) (>= AN n)) roman-magnitudes) do
      (loop for num across letters do (vector-push-extend num ro))
      (decf AN number))
    ro))

(defconstant roman-numerals '((#\I 1) (#\V 5) (#\X 10) (#\L 50) (#\C 100) (#\D 500) (#\M 1000)))

(defun parse-roman (R)
  (loop as (A B) on (loop
                      as char across R
                      as (letter number) = (assoc (char-upcase char) roman-numerals)
                      unless letter do (return-from parse-roman nil)
                      collect number)
        if A sum (if (and B (< A B)) (- A) A)))

(defun parse-roman-numerals (numerals)
  ;; Developed from code at https://www.rosettacode.org/wiki/Roman_numerals/Decode.
  ;; Parse-Roman-Numerals translates Roman numerals assuming a well formed Roman number
  ;; but then encodes the result as a Roman number to make sure the number is well formed. 
  ;; Return true value if the number is encoded to the same string as the input number
  ;; and return the corresponding Arabic number.
  (let ((number (parse-roman numerals)))    
    (if (and number (string-equal numerals (arabic-to-roman number)))
        number)))

(defun test-roman (&optional (number 4000))
  (dotimes (n number)
    (let* ((num (1+ n))
           (ro (arabic-to-roman num))
           (rof (format nil "~@R" num)))
      (multiple-value-bind (is-roman arabic) (parse-roman ro)
        (unless (and is-roman (string-equal rof ro))
          (format t "Roman failure: ~d parse: ~d roman: ~a format: ~a~%" 
            num arabic ro rof))))))

(defun split-line (line)
  (let* ((i (position #\space line))
         (j (position-if-not #'(lambda(ch) (char= #\space ch)) line :start i)))
    (values (subseq line 0 i) (subseq line j))))    

(defun split-moby-line (line)
  (let ((i (position #\\ line)))
    (values (subseq line 0 i) (subseq line (1+ i)))))

(defun make-diffs (&optional (directory *data-directory*)
                             (path-inputs "stemmer2-input.txt")
                             (path-stems "stemmer2-output.txt")
                             (path-diffs "stemmer2-diffs.txt"))
  ;; collect inputs and outputs into a diffs file
  (with-open-file (f1 (merge-pathnames path-inputs directory))
    (with-open-file (f2 (merge-pathnames path-stems directory))
      (with-open-file (f3 (merge-pathnames path-diffs directory)
                          :direction :output
                          :if-exists :supersede)

        (loop
          as w1 = (read-line f1 nil nil)
          while w1
          as w2 = (read-line f2 nil nil)
          do (format f3 "~20a ~a~%"
               (string-trim '(#\Space #\Tab #\Newline) w1)
               (string-trim '(#\Space #\Tab #\Newline) w2))))))
  path-diffs)

(defun prepare-gcide-index (&optional (path (merge-pathnames "gcide-index.dat" *data-directory*)))
  ;; GCIDE index lines begin with an entry key followed by a gcide index entry.
  ;; We are only interested in the entry word in index entry and 
  ;; the base form which is either NIL or a string. E.g.
  ;; Absconder	("Absconder" #\A 227364 "n." ENT NIL)
  ;; Absconding	("Absconding" #\A 225923 "p. pr. & vb. n." CONJF "Abscond")
  (clrhash lexicon)
  (let ((chosen 0) (total 0))
    (with-open-file (wordsin path)
      (with-open-file (lexicon (merge-pathnames "lexemes.txt" *data-directory*)
                               :direction :output :if-exists :supersede)
        (loop
          for line = (read-line wordsin nil nil) while line do
          (let* ((defn (read-from-string line nil nil :start (position #\( line)))
                 (word (pop defn))
                 (base (car (last defn))))
            (if base (setf base (string-downcase base)))
            (if (string-equal base word) (setf base nil))
            (if word (setf word (string-downcase word)))
            (incf total)
            (unless (find-if #'(lambda(ch) (or (char= ch #\space) (char= ch #\-))) word)
              (setf word (delete-if-not #'alphanumericp word))
              (let ((entry (if base
                               (list word :base base :analysis :base)
                             (list word :analysis :present))))
                (when (or (null (gethash word lexicon)) (member :base entry))
                  (incf chosen)
                  (setf (gethash word lexicon) entry)
                  (format lexicon "(~{~s~^ ~})~%" entry))))))))
    (values total chosen)))

(defun add-to-lexicon (entry)
  (setf (gethash (car entry) lexicon) entry))

(defun analyse (wordin wordout)
  ;; Return a keyword analysis of the stemming of wordin to wordout
  (cond
   ((find-if-not #'alpha-char-p wordin)
    :non-alphabetic)
   ((string= wordin wordout)
    :unchanged)
   ((gethash wordout lexicon)
    :stemmed)
   ((<= (length wordin) 2)
    :undersize)
   (t
    :invalid)))

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

(defun wc (path) 
  ;; Return the number of lines, words, and characters in the file pathname PATH
  (with-open-file (f path)
    (loop
      as line = (read-line f nil nil)
      while line
      count t into line-count
      summing (count-words line) into word-count
      ;;summing (1+ (length line)) into character-count
      finally (return (values line-count word-count (file-length f))))))

(defun sort-endings (&optional (path (merge-pathnames "endings.txt" *data-directory*)))
  (with-open-file (endings path)
    (let ((ends (loop for ending = (read endings nil nil) while ending collect ending)))
      (loop for key in (sort ends #'string< :key #'reverse)
          do (format t "~s~%" key)))))


(defun load-lexicon (&optional (path (merge-pathnames "lexicon.txt" *data-directory*)))
  ;; Lexicon entries are a list beginning with the entry word followed by an 
  ;; analysis keyword and possibly a base form.
  ;; E.g. ("sing" :ANALYSIS :PRESENT)
  ;;      ("sang" :BASE "sing" :ANALYSIS :IRREGULAR)
  (clrhash lexicon)
  (with-open-file (lexemes path)
    (loop
      for form = (read lexemes nil nil)
      as word = (car form)
      as ymark =  (mark-y-as-consonant word)
      as base = (cadr (member :base form))
      as analysis = (or (cadr (member :analysis form)) :present)
      as source = (cadr (member :source form))
      while form do
      ;;(format t "~s~%" form)
      (unless (gethash word lexicon)
        (unless source (if (eq analysis :irregular) "irregular"))
        (let ((value `(,word
                       ,@(if (and base (string/= base word)) `(:base ,base))
                       ,@(unless (eq analysis :present) `(:analysis ,analysis))
                       ,@(if source `(:source ,source)))))
          (setf (gethash word lexicon) value)
          (if ymark (setf (gethash ymark lexicon) value))))))
  lexicon)

(defun load-pruned-lexicon ()
  (load-lexicon (merge-pathnames "lexemes-pruned.txt" *data-directory*)))

(defun split-columns (line)
  (read-from-string (format nil "(~a)" line)))

#|
  (loop
    for start = 0 then (+ space 1)
    for space = (position #\Tab line :start start)
    for token = (subseq line start space)
    collect token until (not space)))
|#

(defun whitespace (ch)
  (not (or (alphanumericp ch) (find ch "_-'/"))))

#|
(defun split-line-words (line &optional (filter))
  (loop
    for start = 0 then (position-if-not #'whitespace line :start (1+ space))
    while start
    for space = (position-if #'whitespace line :start start)
    for token = (subseq line start space)
    if filter do (funcall filter token)
    else collect token until (not space)))
|#

(defun split-line-words (line &optional (filter))
  (loop
    for start = (position-if-not #'whitespace line) 
    then (position-if-not #'whitespace line :start (1+ space))
    while start
    for space = (position-if #'whitespace line :start start)
    for token = (subseq line start space)
    if filter do (funcall filter token)
    else collect token until (not space)))

(defun load-chemical-elements (&optional (path (merge-pathnames "chemical-elements.txt" *data-directory*)))
  ;; http://sciencenotes.org/list-elements-atomic-number/
  ;; load lexicon entries: expressions begin with a word symbol 
  ;; followed by a base form if they are inflected.
  ;; E.g. (can (n ^"metal container") (v ^"be able to" :v= mv aux :aux m))
  ;;      (could can (v :tns ved))
  (with-open-file (stream path)
    (loop
      as line = (read-line stream nil nil)
      while line do
      (let* ((form (split-line-words line))
             (atomic-number (read-from-string (car form) nil nil))
             (symbol (cadr form))
             (name (caddr form))
             (common (list :POS "N" :atomic-number atomic-number :source :|Periodic Table| :analysis :chemical-element)))
        ;;(format t "~s~%" common)
        (setf (gethash symbol lexicon) `(,symbol :name ,name ,@common))
        (setf (gethash name lexicon) `(,name :symbol ,symbol ,@common))))))

(defun load-endings (path)
  (with-open-file (stream path)
    (loop for str = (read stream nil nil) while str collect str)))

(defparameter endings 
  '("ic"
    "ed"
    "ance"
    "ence"
    "ble"
    "ate"
    "ive"
    "ize"
    "ing"
    "ish"
    "ity"
    "al"
    "ful"
    "ism"
    "ation"
    "er"
    "ator"
    "s" ; not "ness"
    "ant"
    "ent"
    "ist"
    "thecy"
    "ancy"
    "ency"
    "logy"
    "ly"
    "ality"
    "bility"
    "ivity"))

(defun find-with-an-ending (key)
  (find-if #'(lambda(ending) (ends-with key ending)) endings))

(defun defrost (&key (stemmer #'ps4::stem-exclusive))
  (loop
    for key being the hash-keys in lexicon do
    (if (find-with-an-ending key)
        (multiple-value-bind (stem analysis) (funcall stemmer key)
          (declare (ignore stem))
          (when (eq analysis :stemmed)
            ;(format t "defrost deleting ~s ~s ~s~%" analysis stem key)
            (remhash key lexicon)))))
  lexicon)

(defun load-irregulars (&optional (path (merge-pathnames "irregular-words.txt" *data-directory*)))
  ;; load irregular words into lexicon entries:
  ;; expressions begin with a word symbol and followed by a base form if they are inflected.
  ;; (can (n ^"metal container") (v ^"be able to" :v= mv aux :aux m))
  ;; (could can (v :tns ved))
  (with-open-file (stream path)
    (loop
      for form = (read stream nil nil)
      while form do
      (let ((word (string-downcase (string (pop form))))
            (base (car form)))
        (setf (gethash word lexicon) 
          (if (atom base)
              (list word
                    :base (string-downcase (string base))
                    :analysis :irregular
                    :source :irregulars)
            (list word :source :irregulars))))))
  lexicon)

(defun load-moby-words (&key (path *mobypos*)) ; (stemmer 'ps4::stem-exclusive))
  (clrhash lexicon)
  (with-open-file (lexemes path)
    (loop
      for form = (read-line lexemes nil nil)
      while form do
      (multiple-value-bind (word pos) (split-moby-line form)
        (unless (find-if #'(lambda(ch) (find ch '(#\space #\-))) word)
          (unless (gethash word lexicon)
            (let ((value `(,word :pos ,pos :source :mobypos))
                  (ymark (mark-y-as-consonant word)))
              (setf (gethash word lexicon) value)
              (if ymark (setf (gethash ymark lexicon) value))))))))
  (load-irregulars)
  ;;(defrost stemmer)
  (load-chemical-elements)
  lexicon)
 
(defun save-lexicon (&optional (name "lexicon.txt"))
  (let ((path (merge-pathnames name *data-directory*)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (let ((keys (loop for key being each hash-key in lexicon collect key)))
        (loop for key in (sort keys #'string-lessp) do
              (prin1 (gethash key lexicon) out)
              (terpri out))))
    path))

(defun starts-uppercase (str)
  (and (string/= str "")
       (upper-case-p (char str 0))))

(defun binary-search-stream (value stream &key (key #'car))
  (let* ((low 0)
         (mid 0)
         (high (file-length stream))
         (points (list low mid high)))
    (loop
      while (<= low high) do
      (setf mid (truncate (+ low high) 2))
      (file-position stream mid)
      (read-line stream nil nil) ; skip to the beginning of the next new line
      (setf mid (file-position stream))
      (let* ((line (or (read stream nil nil) (return)))
             (new-points (list low mid high))
             (data-key (funcall key line)))
        ;;(format t "low ~a, high ~a mid ~a~%" low high mid)
        (if (or (string-equal data-key value) 
                (equal points new-points))
            (return))
        (if (string-greaterp data-key value)
            (setf high mid)
          (if (string-lessp data-key value)
              (setf low mid)))
        (setf points new-points)))
    (file-position stream (min low mid))
    (loop
      for line = (read stream nil nil)
      while line
      as line-key = (funcall key line)
      if (string-equal line-key value) collect line into hits
      else if (string-greaterp line-key value) return hits
      finally (return hits))))

(defun search-file (word &optional (path *propernouns*))
  (with-open-file (stream path)
    (binary-search-stream word stream :key #'car)))

(defun search-nouns (word)
  (with-open-file (stream (merge-pathnames "data-nouns.txt" *data-directory*) )
    (binary-search-stream word stream :key #'car)))

(defun starts-uppercase (str)
  (and (string/= str "")
       (upper-case-p (char str 0))))

(defun starts-with-letter (str)
  (and (string/= str "")
       (alpha-char-p (char str 0))))
  
(defun split-data-line (line)
  (let ((divider (or (position #\| line) (return-from split-data-line nil))))
    (values (subseq line 0 divider)
            (string-trim " " (subseq line (1+ divider))))))

(defun harvest-proper-nouns (nouns stream source)
  (let ((table (make-hash-table :test #'equal)))    
    (loop
      for line = (read-line nouns nil nil)
      while line do
      (when (> (length line) 0)
        (multiple-value-bind (words definition) (split-data-line line)
          (when (and words definition)
            (split-line-words
             words
             #'(lambda(word)
                 (when (starts-with-letter word)
                   (setf word (substitute #\space #\_ word))
                   (setf (gethash word table) definition))))))))
    (let ((list ()))
      (maphash #'(lambda (key value) (push (list key value) list)) table)
      (setf list (sort list #'string-lessp :key #'car))
      (dolist (pair list)
        (format stream "(~s :pos ~s :definition ~s :source ~s)~%" (car pair) "N" (cadr pair) source))
      (length list))))


(defun harvest-wordnet (nouns stream pos)
  (let ((table (make-hash-table :test #'equal)))    
    (loop
      for line = (read-line nouns nil nil)
      while line do
      (when (> (length line) 0)
        (multiple-value-bind (words definition) (split-data-line line)
          (when (and words definition)
            (split-line-words
             words
             #'(lambda(word)
                 (when (starts-with-letter word)
                   (setf word (substitute #\space #\_ word))
                   (setf (gethash word table) definition))))))))
    (let ((list ()))
      (maphash #'(lambda (key value) (push (list key value) list)) table)
      (setf list (sort list #'string-lessp :key #'car))
      (dolist (pair list)
        (format stream "(~s :pos ~s :definition ~s :source ~s)~%" (car pair) pos (cadr pair) :WORDNET3.1))
      (length list))))

(defun harvest-nouns-files ()
  (let ((count 0)
        (directory (merge-pathnames "WordNet3.1/dict/dbfiles/" *data-directory*)))
    (with-open-file (stream (merge-pathnames "proper-nouns.txt" *data-directory*)
                            :direction :output :if-exists :supersede)
      (dolist (filename '("NOUN.ACT" "NOUN.ANIMAL" "NOUN.ARTIFACT" "NOUN.ATTIBUTE" "NOUN.BODY"
                          "NOUN.COGNITION" "NOUN.COMMUNICATION" "NOUN.COMMUNICATION"
                          "NOUN.EVENT" "NOUN.FEELING" "NOUN.FOOD" "NOUN.GROUP" "NOUN.LOCATION"
                          "NOUN.MOTIVE" "NOUN.OBJECT" "NOUN.PERSON" "NOUN.PHENOMENON"
                          "NOUN.PLANT" "NOUN.POSSESSION" "NOUN.PROCESS" "NOUN.QUANTITY"
                          "NOUN.RELATION" "NOUN.SHAPE" "NOUN.STATE" "NOUN.SUBSTANCE"
                          "NOUN.TIME"))
        (with-open-file (nouns (merge-pathnames filename directory))
          (incf count (harvest-proper-nouns nouns stream (intern filename "KEYWORD"))))))
    count))


(defun harvest-data (&optional (filename "DATA.NOUN") (pos "N") (output-file "WordNet.txt"))
  (let ((directory (merge-pathnames "WordNet3.1/dict/" *data-directory*)))
    (with-open-file (stream (merge-pathnames output-file *data-directory*) :direction :output :if-exists :supersede)
        (with-open-file (nouns (merge-pathnames filename directory))
          (harvest-wordnet nouns stream pos)))))
#|
(harvest-data "data.noun" "N" "data-nouns.txt")
(harvest-data "data.verb" "V" "data-verbs.txt")
(harvest-data "data.adj" "A" "data-adjectives.txt")
(harvest-data "data.adv" "v" "data-adverbs.txt")

|#
#+never
(wc (merge-pathnames "gcide-index.dat" *data-directory*))
