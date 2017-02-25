(in-package #:montezuma)

(defun bytes-to-string (sequence &key (start 0) (end (length sequence)))
  "Converts a sequence of octets to a string using the implementation's
default character encoding."
   (let ((s (coerce (subseq sequence start end) '(vector (unsigned-byte 8)))))
     ;;(break "is this a problem?")
     (babel:octets-to-string s :errorp nil)))


(defun string-to-bytes (string &key (start 0) end)
  "Converts a string to a sequence of octets using the implementation's
default character encoding.

NB: START and END denote bounds for the final octet vector,
not for STRING."
  (let ((s (babel:string-to-octets string)))
    (subseq s start (or end (length s)))))


(defun string-compare (s1 s2  &key (start1 0) (end1 (length s1)) (start2 0) (end2 (length s2)))
  (let ((pos (mismatch s1 s2 :test #'char= :start1 start1 :end1 end1 :start2 start2 :end2 end2)))
    (if (null pos)
	0
	(cond ((>= pos end1)
	       -1)
	      ((>= pos end2)
	       1)
	      (T (if (char< (char s1 pos) (char s2 pos))
		     -1
		     1))))))


(defun make-adjustable-string (length &optional s)
  (if s
      (make-array (list length)
		  :element-type 'character
		  :fill-pointer T
		  :adjustable T
		  :initial-contents s)
    (make-array (list length)
                :element-type 'character
                :fill-pointer T
                :adjustable T)))

(defun split-string (str &optional (filter " "))
  (when (and str (stringp str))
    (let ((elements ()))
      (loop
       for i = 0 then (1+ j)
       as j = (position-if #'(lambda(ch) (find ch filter)) str :start i) do
       (cond
        ((and j (> j i))
         (push (subseq str i j) elements))
        ((null j)
         (push (subseq str i) elements)
         (return))))
      (reverse elements))))

(defun string-begins (string pattern)
  (let ((m (mismatch string pattern :test #'char=)))
    (or (null m) (= m (length pattern)))))

(defun string-ends (string pattern)
  "Determine whether `string ends with pattern`"
  (let ((p (mismatch pattern string :from-end t)))
    (or (not p) (= 0 p))))

(defun string-contains (string pattern)
  "Determine whether `string contains `pattern.
   Return a list of starting locations for every occurence of pattern in `string`"
   (unless (string= pattern "")
     (loop for p = (search pattern string) then (search pattern string :start2 (1+ p))
           while p 
           collect p)))
 
(defun greedy-wrap (str width)
  (unless (string-ends str " ")
    (setf str (concatenate 'string str " "))) ; sentinel
  ;;(setf str (format nil "~a " str)) ; sentinel
  (do* ((len (length str))
        (lines nil)
        (begin-curr-line 0)
        (prev-space 0 pos-space)
        (pos-space (position #\space str) (when (< (1+ prev-space) len) (position #\space str :start (1+ prev-space)))))
       ((null pos-space) (progn (push (subseq str begin-curr-line (1- len)) lines) (nreverse lines)))
    (when (> (- pos-space begin-curr-line) width)
      (push (subseq str begin-curr-line prev-space) lines)
      (setq begin-curr-line (1+ prev-space)))))

(defun trim (str)
  (string-trim '(#\Space #\Newline #\Return #\Tab) str))

