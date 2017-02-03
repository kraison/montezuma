(in-package #:montezuma)

(defun quick-search (index query)
  (let* ((pqr (process-query index query))
         (fields (or (chosen-fields pqr) (fields-defined index)))
         (sort (sort-specification pqr)))
    (values
     (search-each 
      index
      (query pqr)
      #'(lambda (docnum score)
          (format t "Docnum: ~d Score: ~$ " docnum score)
          (let ((doc (get-document index docnum)))
            (loop
             for field in fields do
             (format t "~a: ~s " field (fetch-field-value index doc field))))
          (terpri))
      `(:first-doc ,(first-document pqr)
        :num-docs ,(number-of-documents pqr)
        :sort ,sort
        :reverse-p ,(if sort (reverse-p sort))))
     pqr
     (rewrite (query pqr) (reader index)))))

(defun annotated-search-results1 (index query)
  (multiple-value-bind (docs total)
      (annotated-search-results
       index
       #'(lambda(name) (and name "<b>"))
       #'(lambda(name) (and name "</b>"))
       #'identity
       query)
    (pprint docs)
    total))

(defun annotated-search-results2 (index query)
  (annotated-search-results index "<b>" "</b>" #'identity query))

#|
(setf text (fetch-field-value oanc-corpus (montezuma:get-document oanc-corpus 0) "text"))
(setf text (fetch-field-value etec-corpus (montezuma:get-document etec-corpus 0) "text"))
(annotated-search-results1 oanc-corpus "\"agricultural chemicals\"")
(cl-user::annotated-search-results1 oanc-corpus "Vallarta")
(cl-user::annotated-search-results2 oanc-corpus "Vallarta")
(cl-user::annotated-search-results1 oanc-corpus "Title:vallarta")

(process-query oanc-corpus "jakarta \"full text index\" wine dine")
(extract-query-terms (process-query oanc-corpus "jakarta \"full text index\" wine dine")
(extract-query-terms (query (process-query oanc-corpus "jakarta \"full text index\" wine dine")))
(extract-query-terms (query (process-query oanc-corpus "jakarta \"full text index\"~10 wine dine")))

(setq pq (query (process-query oanc-corpus "\"popular towns\"~10")))
(setq qt (extract-query-terms pq))
(setq re (marshall oanc-corpus qt))
(setq pq (query (process-query oanc-corpus "towns !popular")))
(setq qt (query-terms pq))
>> (("text" "towns") ("text" "popular")) ; bug!

(marshall oanc-corpus (extract-query-terms (query (parsed-query oanc-corpus "popular towns"))))
(marshall oanc-corpus (query-terms (parsed-query oanc-corpus "jakarta \"full text index\" wine dine")))
(setq text (fetch-field-value etec-corpus 0 "text"))
>> ""TEI.2 
An Artificial Apology Smyth, R...
(setq title (fetch-field-value oanc-corpus (get-document oanc-corpus 0) "title"))
>> "Vallarta"
(setq title (fetch-field-value oanc-corpus 0 "title"))
>> "Vallarta"
(setq text (fetch-field-value oanc-corpus 0 "text"))
"...
Where to Go
Puerto Vallarta..."
(setq mq (marshall oanc-corpus (query-terms (query (parsed-query oanc-corpus "popular towns")))))
(#S(FIELD-EXPRESSION :NAME "text" :GROUP ("popular" "towns") :REGEXP "\\W(popular|towns)\\W" :SCANNER #<Closure 8 subfunction of CL-PPCRE::CREATE-SCANNER-AUX 200D03E2>))
(get-document oanc-corpus 0)
>>#<DOCUMENT...
(field-values oanc-corpus (get-document oanc-corpus 0))
>>(("path" "data/written_2/travel_guides/berlitz2/Vallarta-WhereToGo.txt")...
(pprint (annotated-search-results oanc-corpus "<b>" "</b>" "title:clozapine"))
>> ((1252
  75.13486
  (("path" "data/written_2/technical/biomed/1471-2210-2-6.txt")
(compile-regular-expression '("skelton" ("fire" "and" "brimstone")) :tokenized)
"\\W(skelton|fire\\W+and\\W+brimstone)\\W"
(compile-regular-expression '("skelton" ("fire" "and" "brimstone")) t)
>> "\\W(skelton|fire\\W+and\\W+brimstone)\\W"
(pprint (annotated-search-results etec-corpus "<b>" "</b>" "skelton"))
>> 
((125
  8.093524
  (("path" "X:\\corpus\\Skelton2_22.xml")...
(cl-user::annotated-search-results1 etec-corpus "skelton")
>>
(125
 8.093524
 (("path" "X:\\corpus\\Skelton2_22.xml")
(cl-user::annotated-search-results1 etec-corpus "title:apology")
>> 
(0
 82.625244
 (("path" "X:\\corpus\\artif_apologie22877-6.xml")...
(setq text (fetch-field-value etec-corpus 0 "title"))
>> "An Artificial Apology"
(annotated-search-results1 etec-corpus "title:art*")
>>
(0
 89.112686
 (("path" "X:\\corpus\\artif_apologie22877-6.xml")
  ("author" "Smyth, R.")
  ("title" "An <b>Artificial</b> Apology")
(annotated-search-results1 etec-corpus "title:artificial")
(annotated-search-results1 etec-corpus "text:apology")
(defparameter *author-title-order* (make-instance 'sort-definition :fields '("author" "title")))
*author-title-order*
(quick-search oanc-corpus "chemical" :fields '("author" "title"))
(quick-search oanc-corpus "title:chemical" :fields '("author" "title"))

(extract-query-terms (rewrite (query (parsed-query oanc-corpus "popula*")) (reader oanc-corpus)))
(marshall oanc-corpus (extract-query-terms (rewrite (query (parsed-query oanc-corpus "popula* and growth")) (reader oanc-corpus))))

(setq mq (marshall oanc-corpus (query-terms (query (process-query oanc-corpus "town~ popul*")) (reader oanc-corpus))))
(setq mq (marshall oanc-corpus (query-terms (query (process-query oanc-corpus "town~ popular")))))
(rewrite (query (process-query oanc-corpus "town~ popular")) (reader oanc-corpus))
(rewrite (query (process-query oanc-corpus "tone not popular")) (reader oanc-corpus))
(cl-user::annotated-search-results1 etec-corpus (rewrite (query (process-query oanc-corpus "town~ popular")) (reader oanc-corpus)))
(cl-user::annotated-search-results1 etec-corpus (rewrite (query (process-query oanc-corpus "tone* popular")) (reader oanc-corpus)))
(cl-user::annotated-search-results1 etec-corpus (rewrite (query (process-query oanc-corpus "tone not popular")) (reader oanc-corpus)))
(annotated-search-results1 oanc-corpus (rewrite (query (process-query oanc-corpus "title:ton* popular")) (reader oanc-corpus)))
(annotated-search-results1 oanc-corpus (rewrite (query (process-query oanc-corpus "title:ton* title:popular")) (reader oanc-corpus)))
(rewrite (query (process-query oanc-corpus "title:to*")) (reader oanc-corpus))
(query-terms (rewrite (query (process-query oanc-corpus "title:to*")) (reader oanc-corpus)))

(annotated-search-each oanc-corpus "title:ton* title:popular")
(annotated-search-results1 etec-corpus "art*/score")
(annotated-search-results1 etec-corpus "/document")
(quick-search oanc-corpus "chemical/-document:0,50" '("author" "title"))
|#

