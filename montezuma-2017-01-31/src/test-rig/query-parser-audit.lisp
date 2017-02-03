(in-package #:montezuma)


(defparameter boundary-cases ; they are parsing but not executing yet
  '(
    "title:(ton popular)"
))

(defparameter query-parser-tests 
  '(
    "title:(ton popular)"
    "\"\""
    "/document"
    "/-document"
    "/document:10,20" ; 20 documents starting at the 10 document in the result set
    "/title"
    "\"\"/title"

    "title:reactions~0.8"
    "reactions~0.8"
    "chemical^2 reactions"
    "chemical and reactions"
    "chemical -reactions"

    "\"apostrophy\"/author"
    "\"apostrophy bracket\"~23^19/(-author +title)"
    "\"apostrophy\"/document"
    "\"apostrophy\"/score"
    "\"apostrophy\"/-document"
    "\"apostrophy\"/+(author title)"

    "*:amalgamation"
    "\"apostrophy english\"~10"
    "\"apostrophy germany\"~10^5"
    "ti*:\"apostrophy germany\""
    "ti*:\"apostrophy germany\"~10^5"
    "*:\"apostrophy germany\""
    "trial^"
    "trial^10~"
    "trial^10~8"
    "trial^"
    
    "date:[20020101 TO 20030101]"
    "title:{A TO B}"
    "date:[20020101 to 20030101]"
    "title:{acre to acrimonious}"

    "\"chemical reactions\""
    "\"chemical reactions absolute zero\""
    "\"chemical reactions\"^4 acid"
    "chemical reactions"
    "chemical^2 reactions"
    "chemical and reactions"
    "chemical -reactions"
    "chemical reactions not acid"
    "chemical reactions or acid"
    "chemical reactions and acid"

    "date:[1984 to 2015]"
    "1984 and 2015"

    "chemical not reactions"
    "+chemical"

    "\"april fools day\"~100"
    "date:(20020101 or 20030101)"
    "\"april fools day\"~100"
    "title:\"The Right Way\" path:\"x y z\""
    "title:Do it right"

    "title:\"The Right Way\" and text:go"
    "title:\"The Right Way\" AND text:go"
    "title:\"The Right Way\" or path:\"x y z\""
    "title:\"The Right Way\" NOT text:go"
    "title:\"The Right Way\""
    "title:\"The Right Way\" text:go"

    "te?t"
    "test*"
    "test~"

    "title:(jakarta or apache) AND website"
    "title:(jakarta apache) AND website"
    "(jakarta or apache) and website"

    "(20020101 or 20030101)"

    "(jakarta OR apache) AND website"
    "title:(+return +\"pink panther\")"


    "\"april fools\""
    "\"april fools day\"~100"
    "\"april fools day\""
))

(defun make-query-parser ()
  (make-instance
   'query-parser
   ;;:analyzer (make-instance 'standard-analyzer)
   :default-field "text"
   :field-definitions '(("title") ("author") ("date"))
   :default-occur 'default-occur))

(defun test-parser (query)
  (montezuma-parse (make-query-parser) query))

(defun test-query-parser ()
  (dolist (test query-parser-tests)
    (format t " < ~s~%" test)
    (format t " > ~s~%Press enter to continue: " (test-parser test))
    (read-line *standard-input*)))

(defun print-document (index docnum score fields)
  (format t "~d ~a " docnum score)
  (let ((doc (get-document index docnum)))
    (loop for field across fields do
          (format t "~s " (fetch-field-value index doc field))))
  (terpri))

(defun test-search (index query &key (fields) (reverse))
  (let ((field-vector (coerce fields 'vector)))
    (montezuma:search-each 
     index
     query
     #'(lambda (docnum score) (print-document index docnum score field-vector))
     `(:num-docs 10 :sort ,field-vector :reverse ,reverse))))

(defun test-searches (&optional (tests query-parser-tests) (corpus))
  (dolist (test tests)
    (format t " < ~s~%" test)
    (let ((pq (test-parser test)))
      (format t " >> ~s~%" pq)
      (format t " > ~s~%Press enter to continue: "
              (test-search corpus pq :fields '("title" "author" ))))
    (read-line *standard-input*)))

#|
 < "chemical reactions"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"chemical"^1.0 2261A26B>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"reactions"^1.0 22619D63>>>>
 
 < "chemical reactions not acid"
 > #<BOOLEAN-QUERY with 3 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"chemical"^1.0 22618963>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"reactions"^1.0 2261848B>>> #<BOOLEAN-CLAUSE
                                   :MUST-NOT-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"acid"^1.0 22617FDB>>>>
 
 < "chemical reactions or acid"
 > #<BOOLEAN-QUERY with 3 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"chemical"^1.0 226481D3>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"reactions"^1.0 22647CFB>>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"acid"^1.0 2264784B>>>>
 
 < "chemical reactions and acid"
 > #<BOOLEAN-QUERY with 3 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"chemical"^1.0 226468D3>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"reactions"^1.0 2266AAB3>>> #<BOOLEAN-CLAUSE
                                   :MUST-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"acid"^1.0 2266A603>>>>
 
 < "\"chemical reactions\""
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "text" terms: [chemical:0 reactions:1] proximity ~0 boost ^1.0>>>
 
 < "\"chemical reactions\"^4 acid"
$set-query-boost-factor #<PHRASE-QUERY field: "text" terms: [chemical:0 reactions:1] proximity ~0 boost ^1.0> "4"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "text" terms: [chemical:0 reactions:1] proximity ~0 boost ^4>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"acid"^1.0 20091D7B>>>>
 
 < "date:[1984 to 2015]"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE :SHOULD-OCCUR #<RANGE-QUERY date:["1984" to "2015"] 200911D7>>>
 
 < "date:[1984 - 2015]"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE :SHOULD-OCCUR #<RANGE-QUERY date:["1984" to "2015"] 200CA7F7>>>
 
 < "date:[1984-2015]"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE :SHOULD-OCCUR #<RANGE-QUERY date:["1984" to "2015"] 200C9F6F>>>
 
 < "1984 and 2015"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"1984"^1.0 200EC2E3>> #<BOOLEAN-CLAUSE
                                   :MUST-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"2015"^1.0 200EBE6F>>>>
 
 < "reactions~"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE :SHOULD-OCCUR #<FUZZY-QUERY Field: "text" Word: "reactions" Similarity 0.5>>>
 
 < "reactions~0.8"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE :SHOULD-OCCUR #<FUZZY-QUERY Field: "text" Word: "reactions" Similarity 0.8>>>
 
 < "chemical^2 reactions"
$set-query-boost-factor #<TERM-QUERY "text":"chemical"^1.0 2010A6D3> "2"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"chemical"^2 2010A6D3>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"reactions"^1.0 200D9FD3>>>>
 
 < "chemical and reactions"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"chemical"^1.0 200F9AC7>> #<BOOLEAN-CLAUSE
                                   :MUST-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"reactions"^1.0 225CF5D3>>>>
 
 < "chemical !reactions"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"chemical"^1.0 225CE82F>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :MUST-NOT-OCCUR
                                   #<TERM-QUERY "text":"reactions"^1.0 225CE35F>>>>
 
 < "!chemical"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE :MUST-NOT-OCCUR #<TERM-QUERY "text":"chemical"^1.0 225E9DFB>>>
 
 < "+chemical"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE :MUST-OCCUR #<TERM-QUERY "text":"chemical"^1.0 225E930F>>>
 
 < "\"april fools day\"~100"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "text" terms: [april:0 fools:1 day:2] proximity ~100 boost ^1.0>>>
 
 < "date:[20020101 TO 20030101]"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE :SHOULD-OCCUR #<RANGE-QUERY date:["20020101" to "20030101"] 2260DB13>>>
 
 < "title:{Aida TO Carmen}"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE :SHOULD-OCCUR #<RANGE-QUERY title:{"aida" to "carmen"} 2260D27F>>>
 
 < "date:(20020101 or 20030101)"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   (#<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<TERM-QUERY "date":"20030101"^1.0 2260B903>>> #<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<TERM-QUERY "date":"20020101"^1.0 2260BDBB>>)>>
 
 < "\"april fools day\"~100"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "text" terms: [april:0 fools:1 day:2] proximity ~100 boost ^1.0>>>
 
 < "title:\"The Right Way\" path:\"x y z\""
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "title" terms: [the:0 right:1 way:2] proximity ~0 boost ^1.0>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "path" terms: [x:0 y:1 z:2] proximity ~0 boost ^1.0>>>>
 
 < "title:Do it right"
 > #<BOOLEAN-QUERY with 3 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "title":"do"^1.0 2009D273>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"it"^1.0 2009CE53>>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"right"^1.0 2009C983>>>>
 
 < "title:\"The Right Way\" and text:go"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "title" terms: [the:0 right:1 way:2] proximity ~0 boost ^1.0>> #<BOOLEAN-CLAUSE
                                   :MUST-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"go"^1.0 200B26A7>>>>
 
 < "title:\"The Right Way\" AND text:go"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "title" terms: [the:0 right:1 way:2] proximity ~0 boost ^1.0>> #<BOOLEAN-CLAUSE
                                   :MUST-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"go"^1.0 200B0C6B>>>>
 
 < "title:\"The Right Way\" or path:\"x y z\""
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "title" terms: [the:0 right:1 way:2] proximity ~0 boost ^1.0>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "path" terms: [x:0 y:1 z:2] proximity ~0 boost ^1.0>>>>
 
 < "title:\"The Right Way\" NOT text:go"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "title" terms: [the:0 right:1 way:2] proximity ~0 boost ^1.0>> #<BOOLEAN-CLAUSE
                                   :MUST-NOT-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"go"^1.0 200C2937>>>>
 
 < "title:\"The Right Way\""
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "title" terms: [the:0 right:1 way:2] proximity ~0 boost ^1.0>>>
 
 < "title:\"The Right Way\" text:go"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "title" terms: [the:0 right:1 way:2] proximity ~0 boost ^1.0>> #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"go"^1.0 200BFB27>>>>
 
 < "te?t"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE :SHOULD-OCCUR #<WILDCARD-QUERY "text":"te?t">>>
 
 < "test*"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE :SHOULD-OCCUR #<WILDCARD-QUERY "text":"test*">>>
 
 < "test~"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE :SHOULD-OCCUR #<FUZZY-QUERY Field: "text" Word: "test" Similarity 0.5>>>
 
 < "title:(jakarta or apache) AND website"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   (#<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<TERM-QUERY "title":"apache"^1.0 200ED39B>>> #<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<TERM-QUERY "title":"jakarta"^1.0 200CA017>>)> #<BOOLEAN-CLAUSE
                                   :MUST-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"website"^1.0 200ECE27>>>>
 
 < "title:(jakarta apache) AND website"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   (#<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<TERM-QUERY "title":"apache"^1.0 225E8613>>> #<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<TERM-QUERY "title":"jakarta"^1.0 225E8A77>>)> #<BOOLEAN-CLAUSE
                                   :MUST-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"website"^1.0 225E809F>>>>
 
 < "(jakarta or apache) and website"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   (#<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<TERM-QUERY "text":"apache"^1.0 2260C92B>>> #<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<TERM-QUERY "text":"jakarta"^1.0 225E6723>>)> #<BOOLEAN-CLAUSE
                                   :MUST-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"website"^1.0 2260C3B7>>>>
 
 < "(20020101 or 20030101)"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   (#<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<TERM-QUERY "text":"20030101"^1.0 22645D63>>> #<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<TERM-QUERY "text":"20020101"^1.0 2264621B>>)>>
 
 < "(jakarta OR apache) AND website"
 > #<BOOLEAN-QUERY with 2 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   (#<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<TERM-QUERY "text":"apache"^1.0 2264409F>>> #<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<TERM-QUERY "text":"jakarta"^1.0 22644533>>)> #<BOOLEAN-CLAUSE
                                   :MUST-OCCUR
                                   #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<TERM-QUERY "text":"website"^1.0 22643B5B>>>>
 
 < "title:(+return +\"pink panther\")"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   (#<BOOLEAN-CLAUSE
                                    :SHOULD-OCCUR
                                    #<BOOLEAN-CLAUSE
                                    :MUST-OCCUR
                                    #<PHRASE-QUERY field: "title" terms: [pink:0 panther:1] proximity ~0 boost ^1.0>>> #<BOOLEAN-CLAUSE
                                    :MUST-OCCUR
                                    #<TERM-QUERY "title":"return"^1.0 22641CD7>>)>>
 
 < "\"april fools\""
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "text" terms: [april:0 fools:1] proximity ~0 boost ^1.0>>>
 
 < "\"april fools day\"~100"
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "text" terms: [april:0 fools:1 day:2] proximity ~100 boost ^1.0>>>
 
 < "\"april fools day\""
 > #<BOOLEAN-QUERY with 1 clauses: #<BOOLEAN-CLAUSE
                                   :SHOULD-OCCUR
                                   #<PHRASE-QUERY field: "text" terms: [april:0 fools:1 day:2] proximity ~0 boost ^1.0>>>
 

|#