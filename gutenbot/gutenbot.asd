(in-package :asdf)

(asdf:defsystem #:gutenbot
    :name "Gutenbot Crawler"
    :author "Roy Anderson <reanz1959@gmail.com>"
    :version "1.0"
    :licence "MIT/Expat"
    :description "Gutenbot Crawler downloads zip files and add unzipped text files to a Montezuma index"
    :depends-on (#:cl-ppcre #:zip #:drakma)
    :components
    ((:module "src"
      :components 
      (;(:file "loader")
       (:file "gutenbot") ; :depends-on ("loader"))
       ))))

