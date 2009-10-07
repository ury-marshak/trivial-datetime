;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-



(asdf:defsystem :trivial-datetime
  :description "Date and time manipulation"
  :version "0.0.3"
  :serial t
  :components ((:file "packages")
               (:file "datetime")
               )
  :depends-on ())

