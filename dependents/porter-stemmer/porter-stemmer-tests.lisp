;; -*- mode: common-lisp -*-

(in-package porter-stemmer-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant *data-directory* (merge-pathnames "data/" (load-time-value (or #.*compile-file-pathname* *load-pathname*))))
  (defconstant *porter2-diffs* (merge-pathnames "porter-stemmer2-diffs.txt" *data-directory*))
  (defconstant *porter1-input* (merge-pathnames "porter-stemmer-input.txt" *data-directory*))
  (defconstant *porter1-output* (merge-pathnames "porter-stemmer-output.txt" *data-directory*))
  (defconstant *words* (merge-pathnames "english-words-master/words.txt" *data-directory*))
  (defconstant *mobypos* (merge-pathnames "english-words-master/mobypos.txt" *data-directory*))
  (defconstant *words2* (merge-pathnames "english-words-master/words2.txt" *data-directory*))
  (defconstant *english-words* (merge-pathnames "wordsEn.txt" *data-directory*))
  )

(defun get-stemmer (algorithm)
  (case algorithm
    (1 (values #'ps1:stem :ps1))
    (2 (values #'ps2:stem :ps2))
    (3 (values #'ps3:stem :ps3))
    (4 (values #'ps4:stem :ps4))))

(defun collect-words (&optional (path (merge-pathnames "porter-stemmer-input.txt" *data-directory*)))
  (with-open-file (f1 path)
    (loop
      as word = (read-line f1 nil nil) while word 
      collect (string-trim '(#\Space #\Tab #\Newline) word))))

(defun time-check (&key (algorithm 3) (wordlist (collect-words)) (repeats 10000))
  (let ((stemmer (get-stemmer algorithm)))
    (time
     (dotimes (n repeats)
       (dolist (wordin wordlist)
         (funcall stemmer wordin)))))
  (* repeats (length wordlist)))

;;;; This is the original stemmer test for Porter Stemmer 1 which may not yield real words
(defun test-stemmer1 () ; Run against the distributed Porter1 test files without validation
  (with-open-file (f1 *porter1-input*)
    (with-open-file (f2  *porter1-output*)
      (loop
        as w1 = (read-line f1 nil nil)
        while w1
        as w2 = (read-line f2 nil nil)
        as w3 = (ps1:stem w1)
        if (equal w2 w3)
        count t into successes
        else count t into failures
        and do (format t "(ps1:stem ~s) => ~s wanted ~s~%" w1 w3 w2)
        finally (progn (format t "sucesses ~d failures ~d~%" successes failures)
                  (return failures))))))

(defun test-porter-diffs (&key (path *porter2-diffs*)
                               (max-failure nil)
                               (start 0)
                               (end nil)
                               (stemmer #'pstem2:stem))
  (with-open-file (f path)
    (loop
      with successes = 0
      with failures = 0
      with line-count = 0
      as line = (read-line f nil nil)
      while line do
      (incf line-count)
      (when (and (>= line-count start) (or (null end) (<= line-count end)))
        (multiple-value-bind (input expected) (split-line line)
          (multiple-value-bind (str analysis) (ps2:stem input)
            (declare (ignore analysis))
            (cond
             ((equal expected str)
              (incf successes))
             (t
              (incf failures)
              (format t "[~d] (~a:stem ~s) => ~s wanted ~s~%"
                line-count (package-name (symbol-package stemmer)) (symbol-name stemmer)
                input str expected)
              (if (and (numberp max-failure) (> failures max-failure))
                  (return)))))))
      finally (format t "sucesses: ~d failures: ~d~%" successes failures)))
  (values))

(defun test-porter2-examples ()
  (test-porter-diffs :path (merge-pathnames "stemmer2-extract-diffs.txt" *data-directory*)))

(defun test-hybrid-stemmers (&key (path *porter1-input*) (number nil))
  ;; Run against the distributed test file but count successes when we stem to a validated word
  (with-open-file (f1 path)
      (loop
        with successes = 0
        with failures = 0
        for w1 = (read-line f1 nil nil)
        while w1
        count t into line-count 
        if (and (numberp number) (> line-count number)) do (return)
        do
        (multiple-value-bind (w3 analysis3) (ps3:stem w1)
          (multiple-value-bind (w4 analysis4) (ps4:stem w1)
            (cond
             ((or (eq :absent analysis3) (eq :absent analysis4) (string/= w3 w4))
              (incf failures)
              (format t "~d (ps3:stem ~s) => ~s: ~s, (ps4:stem ~s) => ~s: ~s~%" line-count w1 w3 analysis3 w1 w4 analysis4))
             (t
              (incf successes)))))
        finally (format t "successes ~d failures ~d~%" successes failures)))
  (values))

(defun get-analysis (stemmer word)
  (multiple-value-bind (stem analysis) (funcall stemmer word)
    (values analysis stem)))

(defun test-hybrid-stemmers-with-english-words ()
  (test-hybrid-stemmers :path *english-words*))

#|
(defun test-hybrid-stemmer (&key (algorithm 4) (path *porter1-input*) (number nil))
  ;; Run against the distributed test file but count successes when we stem to a validated word
  (let ((misses 0)
        (source (intern (pathname-name path) "KEYWORD"))
        (stemmer (get-stemmer algorithm)))
    (with-open-file (f1 path)
      (loop
        for w1 = (read-line f1 nil nil)
        while w1
        count t into line-count 
        if (and (numberp number) (> line-count number)) do (return) end
        do
        (multiple-value-bind (w3 analysis) (funcall stemmer w1)
          ;;with analysis = (get-analysis stemmer w1)
          (when (eq :absent analysis)
            (incf misses)
            (format t "(~s :source ~s)~%" w1 source)))
        finally (progn (format t "successes ~d misses ~d~%" (- line-count misses) misses) (return misses)))))))
|#

(defun test-hybrid-stemmer (&key (algorithm 4) (path *porter1-input*) (number nil))
  ;; Run against the distributed test file but count successes when we stem to a validated word
  (with-open-file (f1 path)
    (loop
      with source = (intern (string-upcase (pathname-name path)) "KEYWORD")
      with stemmer = (get-stemmer algorithm)
      for w1 = (read-line f1 nil nil)
      while w1
      as analysis = (get-analysis stemmer w1)
      count t into line-count 
      if (and (numberp number) (> line-count number)) return misses
      count (eq :absent analysis) into misses
      when (eq :absent analysis) do
      (format t "(~s :source ~s)~%" w1 source)
      finally (progn (format t "successes ~d misses ~d~%" (- line-count misses) misses) (return misses)))))

#|
(defun audit-stemmer (&key (algorithm 4) (path *porter1-input*) (number nil))
  ;; Run against the distributed test file but count successes when we stem to a validated word
  (let (;(table (make-hash-table :test #'equal))
        (source (intern (string-upcase (pathname-name path)) "KEYWORD"))
        (stemmer (get-stemmer algorithm))
        (misses 0)
        (absent 0))
    
    (with-open-file (f *words*)
      (with-open-file (f1 path)
        (loop
         for w1 = (read-line f1 nil nil)
         while w1
         as analysis = (get-analysis stemmer w1)
         count t into line-count 
         if (and (numberp number) (> line-count number)) return misses
         else do
         (when (eq :absent analysis)
           (incf misses)
           (when (binary-search-stream w1 f)
             (incf absent)
             (format t "(~s :source ~s)~%" w1 source)))
         finally (progn (format t "successes ~d misses ~d absent ~d~%" (- line-count misses) misses absent) (return misses)))))))
|#

(defun test-stemmer-with-english-words ()
  (test-hybrid-stemmer :path *words* :algorithm 4))

(defun absences (&key (algorithm 4) (path *porter1-input*))
  ;; Run against the distributed test file but count successes when we stem to a validated word
  (let ((successes 0)
        (misses 0)
        (stemmer (get-stemmer algorithm)))
    (with-open-file (f1 path)
      (loop
        for w1 = (read-line f1 nil nil)
        while w1
        count t into line-count 
        do
        (multiple-value-bind (w2 analysis) (funcall stemmer w1)
          (declare (ignore w2))
          (cond
           ((eq :absent analysis)
            (incf misses)
            (format t "(~s)~%" w1))
           (t
            (incf successes))))
        finally (progn (format t ";; successes ~d misses ~d~%" successes misses) (return misses))))))

(defun delete-dead-heads (&key (path *words*)) ; (algorithm 4) 
  ;; Report any words in lexicon not in words2
  (let (;(stemmer (get-stemmer algorithm))
        (table (make-hash-table :test #'equal)))
    (with-open-file (f1 path)
      (loop
        for w1 = (read-line f1 nil nil)
        while w1 do
        (setf (gethash w1 table) t)))
    (format t "Loaded ~d words from ~a~%" (hash-table-count table) path)
    (loop for key being the hash-keys of stemaux::lexicon
        do (when (null (gethash key table))
             (format t "(~s)~%" key)))))

(defun add-lexemes (&key (algorithm 4) (path *porter1-input*) (number nil))
  ;; Run against the given path counting successes when we stem to a validated word
  (let ((successes 0)
        (misses 0)
        (stemmer (get-stemmer algorithm)))
    (with-open-file (f1 path)
      (loop
        for w1 = (read-line f1 nil nil)
        while w1
        count t into line-count 
        if (and (numberp number) (> line-count number)) do (return)
        do
        (multiple-value-bind (w3 analysis) (funcall stemmer w1)
          (cond
           ((eq :absent analysis)
            (incf misses)
            (let ((entry (list w1 :analysis :present :source (pathname-name path))))
              (setf (gethash (string-downcase w1) stemaux::lexicon) entry)
              (format t "~s ; ~s~%" entry w3)))
           (t
            (incf successes))))
        finally (progn (format t "successes ~d misses ~d~%" successes misses) (return misses))))))

(defun add-english-words (&key (algorithm 4))
  (add-lexemes :path *english-words* :algorithm algorithm))

(defun test-hybrid-stemmer-with-english-words (&key (algorithm 4))
  (test-hybrid-stemmer :path *english-words* :algorithm algorithm))

;;; Porter Stemmer 3

(defun summary-totals (stream total style name &rest counts)
  (format stream "  <tr class='~(~a~)'>~%" style)
  (format stream "    <td>~(~a~)</td>~%" name)
  (dolist (count counts)
    (format stream "    <td colspan=3 align='right'>~d</td>~%" count))
  (dolist (count counts)
    (format stream "    <td colspan=3 align='right'>~d%</td>~%" (round (float (* 100 (/ count total))))))
  (format stream "  </tr>~%"))

(defun summary-line (stream total style name &rest counts)
  (format stream "  <tr class='~(~a~)'>~%" style)
  (format stream "    <td>~(~a~)</td>~%" name)
  (dolist (count counts)
    (format stream "    <td align='right'>~d</td>~%" count))
  (dolist (count counts)
    (format stream "    <td align='right'>~d%</td>~%" (round (float (* 100 (/ count total))))))
  (format stream "  </tr>~%"))

(defun test-stemmer3 (&optional (number ()) (filter ()))
  ;; Run against the distributed test file for Porter Stemmer2 counting successes when we stem to a validated word
  (with-open-file (f1 (merge-pathnames "porter-stemmer2-diffs.txt" *data-directory*))
    (with-open-file (f2 (merge-pathnames (format nil "porter-stemmer-diffs-~a-output.html"
                                           (or number "complete"))
                                         *data-directory*)
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
            (multiple-value-bind (word3out analysis3) (ps3::stem wordin)
              (when (or (null filter) (find analysis3 filter))
                (incf line-count)
                (let* ((word-out1 (ps3::stem wordin))
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


#|
(defun test-stemmer2 (&optional (number 0))
  ;; Run against the distributed test file but count successes when we stem to a validated word
  (with-open-file (f1 (merge-pathnames "porter-stemmer-input.txt" *data-directory*))
    (with-open-file (f2 (merge-pathnames "porter-stemmer-test-output.txt" *data-directory*)
                        :direction :output
                        :if-exists :supersede)
      (let ((line-count 0)
            (results ()))
        (loop
          for w1 = (read-line f1 nil nil)
          while (and w1 (or (zerop number) (< line-count number))) do
          (multiple-value-bind (w2 analysis) (stem w1)
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

(defun split-line (line)
  (let* ((i (position #\space line))
         (j (position-if-not #'(lambda(ch) (char= ch #\space)) line :start i)))
    (values (subseq line 0 i) (subseq line j))))         
|#

(defun sanity-check (&optional (wordlist (collect-words (merge-pathnames "word-list.txt" *data-directory*))))
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
      (multiple-value-bind (word3out analysis3) (ps3:stem wordin)
        (let ((word-out1 (ps3:stem wordin)))
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

(defun timings ()
  ;;(compile-file *porter-source-file*)
  (let ((wordlist1 (collect-words (merge-pathnames "word-list.txt" *data-directory*))))
    (format t "timings[1]: word-list.txt Porter1~%")
    (time-check :algorithm 1 :repeats 10000 :wordlist wordlist1)
    (format t "timings[1]: word-list.txt Porter3~%")
    (time-check :algorithm 3 :repeats 10000 :wordlist wordlist1))
  (let ((wordlist2 (collect-words (merge-pathnames "porter-stemmer-input.txt" *data-directory*))))
    (format t "timings[2]: porter-stemmer-input.txt Porter1~%")
    (time-check :algorithm 1 :repeats 50 :wordlist wordlist2)
    (format t "timings[2]: porter-stemmer-input.txt Porter3~%")
    (time-check :algorithm 3 :repeats 50 :wordlist wordlist2)))

(defun test-porter4 (&key (path (merge-pathnames "wordsEn.txt" *data-directory*))
                          (start 0)
                          (end nil))
  ;; http://www-01.sil.org/linguistics/wordlists/english/
  (with-open-file (f path)
    (let ((line-count 0))
      (loop
        as line = (read-line f nil nil)
        while line do
        (incf line-count)
        (when (and (>= line-count start)
                   (or (null end) (<= line-count end)))
          (setf line (string-trim '(#\Space #\Tab #\Newline) line))
          (let ((outcome (ps4:stem line)))
            (format t "(ps4:stem ~s) => ~s~%" line outcome)))))))

(defun make-artefacts ()
  (test-stemmer3 2000)
  (test-stemmer3)
  (test-stemmer3 nil '(:irregular :roman-numerals)))

#|
(ps::wc (merge-pathnames "porter-stemmer-words-original.txt" ps::*data-directory*))
(ps::wc (merge-pathnames "porter-stemmer-words.txt" PS::*data-directory*))
(toronto::diff "D:\\allegro-projects\\toronto\\src\\porter-stemmer\\porter-stemmer-1.01.lisp" "D:\\allegro-projects\\toronto\\src\\porter-stemmer\\porter-stemmer-2.0.lisp")
|#
