(in-package #:montezuma)

(defun asdf-system-directory (asdf-system-name)
  "Computes the directory in which the .asdf file for a given ASDF
  system resides."
  (slot-value (asdf:find-system asdf-system-name) 'asdf::absolute-pathname))

(defun montezuma.asd-relative (path)
  (merge-pathnames path (asdf-system-directory :montezuma)))

(defparameter *word-file-path* (montezuma.asd-relative "tests/unit/analysis/data/wordfile"))

(deftestfun test-stop-filter
  (with-input-from-string (input "The Quick AND the DEAD the and to it there their")
    (let ((filter (make-instance 'stop-filter
				 :input (make-instance 'lowercase-tokenizer :input input)
				 :file *word-file-path*)))
      (test stop-filter-1 (next-token filter) (make-token "quick" 4 9) #'token=)
      (test stop-filter-2 (next-token filter) (make-token "dead" 18 22) #'token=)
      (test stop-filter-3 (next-token filter) nil))))

      
