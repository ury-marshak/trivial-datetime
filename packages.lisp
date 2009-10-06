;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

(in-package #:cl-user)

(defpackage #:trivial-datetime
  (:nicknames #:datetime)
  (:export #:date-time
           #:date-value #:date-delta
           #:make-date
;           #:date-to-ordinal
;           #:ordinal-to-date
           #:date-hash #:date-unhash
           #:date= #:date/= #:date> #:date>= #:date< #:date<=
           #:date+ #:date-
           
           #:time-value #:time-delta
           #:make-time
           #:time-hash #:time-unhash
           #:time= #:time/= #:time> #:time>= #:time< #:time<=
           #:time+ #:time-
           )
  (:shadow #:time)
  (:use :cl))

