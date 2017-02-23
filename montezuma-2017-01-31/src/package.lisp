(in-package #:common-lisp)

(defpackage #:montezuma.parser
  (:use #:cl)
  ;;(:shadow #:type)
  (:export
   #:defprod
   #:defchartype
   #:deflexer
   #:defparser
   #:parselet
   #:?
   #:*
   #:+
   #:~
   #:!
   #:/
   #:&
   #:@
   #:^
   #:%))


;; All shadowing has been resolved.

(defpackage #:montezuma
  (:nicknames "MTZ")
  (:use #:common-lisp #:montezuma.parser #:tg)
  (:export
   #:make-vector
   #:document-result
   #:annotated-field-value
   #:annotations
   #:*montezuma-indexes*
   #:*stop-words-directory*
   #:get-corpus
   #:get-absolute-path
   #:relative-path
   #:make-fs-directory
   #:standard-analyzer
   #:whitespace-analyzer
   #:index-writer
   #:document
   #:search-field-value
   #:docnum
   #:language
   #:field-width
   #:field-height
   #:document-key
   #:add-field
   #:make-field
   #:deleted-p
   #:add-document
   #:add-document-to-index
   #:bulk-add-documents
   #:add-document-to-index-writer
   #:make-field-value-query
   #:make-key-field-query
   #:search-each
   #:optimize
   #:close-down ;;#:close
   #:field
   #:field-value
   #:field-is-defined
   #:field-stored-p
   #:field-data
   #:file-contents
   #:uniqueness
   #:overwrite
   #:search-index
   #:index
   #:url
   #:title
   #:index-key
   #:index-name
   #:writer
   #:optimize-index
   #:make-term
   #:max-doc
   #:search
   #:weight
   #:scorer
   #:reader
   #:each-hit
   #:each
   #:doc
   #:score
   #:size
   #:get-document
   #:get-document-number
   #:get-document-for-key
   #:search-field-value
   #:document-field
   #:query
   #:term
   #:term-field
   #:term-text
   #:lower-term
   #:upper-term
   #:include-lower-p
   #:include-upper-p
   #:boost
   #:rewrite-query
   #:term-query
   #:index-searcher
   #:definition-aspect
   #:chosen-fields
   #:retrieved-fields
   #:field-definitions
   #:field-definition
   #:field-defined-stored
   #:default-search-field
   #:field-path
   #:document-root
   #:key
   #:index-path
   #:fields
   #:fields-defined
   #:all-field-names
   #:reopen
   #:query-update
   #:query-delete
   #:delete-document
   #:update
   #:all-field-values
   #:normalised-field-name
   #:commit
   #:flush
   #:delete

   #:document-value
   #:all-fields
   #:field-name

   #:token-image
   #:all-tokens

   #:analyzer

   #:compiled-query
   #:boolean-query
   #:regular-expression-query
   #:boolean-clause
   #:clauses
   #:range-query
   #:phrase-query
   #:match-all-query
   #:phrase-term-positions
   #:add-query
   #:occur
   #:slop

   #:sort-specification
   #:number-of-documents
   #:first-document
   #:document-root

   #:offset
   #:message
   #:field-definitions
   #:definition-aspect
   #:field-definition
   #:invalid-query
   #:invalid-field
   #:no-matching-fields

   #:make-phrase-query
   #:field-expressions
   #:field-expression
   #:annotate-document
   #:annotated-search-apparatus
   #:annotated-search
   #:annotated-search-results
   #:search-files
   #:relative-path
   #:get-absolute-path
   #:text-phrases

   #:make-document-result
   #:add-target-expression
   #:default-number-retrieved

   #:process-query
   #:add-clause
   #:rewrite
   #:marshall
   #:make-apparatus
   #:add-condition

   #:annotate-document-result
   #:word
   #:keys
   #:keys-count
   #:overlap
   #:frequency
   #:corep
   #:compute-wordmap
   #:orbit-color
   #:orbit-background
   #:text-phrases
   #:parent
   #:token-frequencies
   #:abstract-document
      
   #:fetch-field-value
   #:key-term-query
   #:precis
   #:annotate
   #:metadata-vector
   #:document-digest
   #:make-document-order-sort-field
   #:delete-docs-with-term
   #:porter-analyzer

))
