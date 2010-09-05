;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-


(in-package #:cl-user)

(defpackage #:trivial-datetime
  (:nicknames #:datetime)
  (:export #:date-time
           #:date-value #:date-delta #:date-delta-days
           #:make-date
;           #:date-to-ordinal
;           #:ordinal-to-date
           #:date-year #:date-month #:date-day
           #:date-hash #:date-unhash
           #:date= #:date/= #:date> #:date>= #:date< #:date<=
           #:date+ #:date-
           #:day-of-week
           ;;#:leap-year-p #:days-in-month-for-date    ; cannot export: names conflict with METABANG.UTILITIES
           #:last-date-in-month
           
           #:time-value #:time-delta
           #:make-time
           #:time-hash #:time-unhash
           #:time= #:time/= #:time> #:time>= #:time< #:time<=
           #:time+ #:time-
           #:date-format #:time-format
           )
  (:shadow #:time)
  (:use :cl))

