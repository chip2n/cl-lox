(asdf:defsystem #:lox
  :description "Common Lisp implementation of the Lox language"
  :author "Andreas Arvidsson <andreas@arvidsson.io>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:str #:parse-number #:parachute)
  :components ((:file "package")
               (:file "scanner")
               (:file "parser")
               (:file "pprint")
               (:file "interpreter")
               (:file "core")))
