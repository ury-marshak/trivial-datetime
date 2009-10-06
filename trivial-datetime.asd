;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-


(asdf:defsystem :trivial-datetime
  :description "Date and time manipulation"
  :version "0.0.2"
  :serial t
  :components ((:file "packages")
               (:file "datetime")
               )
  :depends-on ())

