(in-package :montezuma)

(defparameter *library* (merge-pathnames "library/" cl-user::*corpus-root*))
(defparameter library cl-user::library)

#|
(defparameter library
  (make-instance
   'index
   :document-root *library*
   :path (merge-pathnames "library/index" *corpus-root*)
   :index-key "Library"
   :document-key "key"
   :title "Collectionof documents"
   :name "Library"
   :field-definitions 
   '(("path" :stored t :index :untokenized)
     ("name" :stored t :index :untokenized :query "name:cm04")
     ("key" :stored t :index :tokenized)
     ("title" :stored t :index :tokenized)
     ("date" :stored t :index :tokenized)
     ("size" :stored t :index :tokenized)
     ("source" :stored t :index :tokenized)
     ("url" :stored t :index :tokenized)
     ("author" :stored t :index :tokenized)
     ("text" :stored t :index :tokenized :query "justice"))
   :retrieved-fields '("name" "author" "title" "date" "source")
   :default-search-field "text"
   :language :ENGLISH))
|#

(defparameter details
  '(
#|
    (:name "DeclarationOfIndependence.txt"
     :key "USDI"
     :author "Thomas Jefferson"
     :title "The Declaration of Independence of The United States of America"
     :date "July 4, 1776"
     :size "10042"
     :source "GUTENBERG")

    (:name "UniversalDeclarationOfHumanRights.txt"
     :key "UDHR"
     :title "The Universal Declaration of Human Rights"
     :author "United Nations General Assembly"
     :source "www"
     :size "13848"
     :url "http://www.un.org/en/universal-declaration-human-rights/"
     :date "10 December 1948")

    (:name "US_constitution.txt"
     :key "CUSA"
     :source "GUTENBERG"
     :url "http://www.gutenberg.org/cache/epub/5/pg5.txt"
     :date "1787"
     :author "Founding Fathers"
     :title "The Constitution Of The United States Of America")

    (:name "analects.txt"
     :key "analects"
     :title "The Analects of Confucius"
     :date "?"
     :size "154539"
     :source "www"
     :url "http://classics.mit.edu//Confucius/analects.html"
     :author "Confucius")

    (:name "EgyptianIdeasOfTheFutureLife.txt"
     :key "EIFL"
     :title "Egyptian Ideas of the Future Life"
     :date "?"
     :size "289867"
     :source "www"
     :url "http://www.sacred-texts.com/egy/efl/index.htm"
     :author "Ben Courtney")

    (;:path "OriginOfSpecies.txt"
     :name "OriginOfSpecies.txt"
     :key "OSCD"
     :title "On the Origin of Species 6th Edition"
     :date "1859-11-24"
     :size "1263000"
     :source "Gutenberg Project"
     :url "http://www.gutenberg.org/ebooks/2009"
     :author "Charles Darwin")
|#
    (:name "pg2554 dostoevsky - crime and punishment.txt"
     ;:name "pg2554.txt"
     :key "CPFD"
     :title "Crime and Punishment"
     :author "Fyodor Dostoevsky"
     :source "Gutenberg Project")))

(defun already-loaded (index key)
  (not (zerop (total-hits (search-index index (format nil "key:~a" key))))))

(defun load-into-library (detail)
  (let* ((filename (cadr (member :name detail)))
         (source (cadr (member :source detail)))
         (key (cadr (member :key detail)))
         (url (cadr (member :url detail)))
         (path (merge-pathnames *library* filename))
         (relative-path (relative-path *library* (merge-pathnames *library* filename)))
         (title (cadr (member :title detail)))
         (contents (file-contents path))
         (date (cadr (member :date detail)))
         (author (cadr (member :author detail)))
         (size (princ-to-string (length contents))))
    (format t "key ~a~%name ~a~%path ~a~%date ~a~%author ~a~%size ~a~%" key filename path date author size)
    ;;(setf profile `(:path ,pathstr :name ,name :date ,(cadr details) :size ,(car details) ,@profile :text ,text)) 
    (format t "~%;;; Indexing: ~s~%" key)

    (let ((doc (make-instance 'document)))
      (dolist (field-definition (field-definitions library))
        ;;(break "name ~s" (car field-definition))
        (let* ((name (car field-definition))
               (value
                (cond
                 ((string-equal name "KEY") key)
                 ((string-equal name "TITLE") title)
                 ((string-equal name "URL") url)
                 ((string-equal name "SIZE") size)
                 ((string-equal name "DATE") date)
                 ((string-equal name "SOURCE") source)
                 ((string-equal name "NAME") filename)
                 ((string-equal name "PATH") relative-path)
                 ((string-equal name "TEXT") contents)
                 ((string-equal name "AUTHOR") author))))
          (unless (string-equal "TEXT" name)
            (format t ";;; field ~s: ~s~%" name value))
          (add-field doc (make-field 
                          name value
                          :stored (cadr (member :stored field-definition))
                          :index (cadr (member :index field-definition))))))
      (add-document-to-index library doc :uniqueness t))))

(defun load-library ()
  (dolist (detail details) (load-into-library detail)))
