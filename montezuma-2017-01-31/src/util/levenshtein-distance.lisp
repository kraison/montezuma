(in-package #:montezuma)

#|
https://en.wikipedia.org/wiki/Levenshtein_distance

The Levenshtein distance is a string metric for measuring the difference between two sequences.
Informally, the Levenshtein distance between two words is the minimum number of single-character edits
(i.e. insertions, deletions or substitutions) required to change one word into the other.
It is named after Vladimir Levenshtein, who considered this distance in 1965.
|#

(defun levenshtein-distance (str1 str2)
  "Calculates the Levenshtein distance between str1 and str2, returns an editing distance (int)."
  (let ((n (length str1))
	(m (length str2)))
    ;; Check trivial cases
    (cond ((= 0 n) (return-from levenshtein-distance m))
	  ((= 0 m) (return-from levenshtein-distance n)))
    (let ((col (make-array (1+ m) :element-type 'integer))
	  (prev-col (make-array (1+ m) :element-type 'integer)))
      ;; We need to store only two columns---the current one that
      ;; is being built and the previous one
      (dotimes (i (1+ m))
	(setf (svref prev-col i) i))
      ;; Loop across all chars of each string
      (dotimes (i n)
	(setf (svref col 0) (1+ i))
	(dotimes (j m)
	  (setf (svref col (1+ j))
		(min (1+ (svref col j))
		     (1+ (svref prev-col (1+ j)))
		     (+ (svref prev-col j)
			(if (char-equal (schar str1 i) (schar str2 j)) 0 1)))))
	(rotatef col prev-col))
      (svref prev-col m))))

(defun fuzzy-factor (str1 str2)
  "Compute the factor (0 - 1) for the levenshtein-distance between two strings"
  (let ((distance (levenshtein-distance str1 str2)))
    (float (/ distance (length str1)))))
