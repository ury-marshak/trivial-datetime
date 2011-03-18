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
           #:date-min #:date-max
           
           #:time-value #:time-delta
           #:make-time
           ;; #:time-hour #:time-minute #:time-second #:time-fraction  ; cannot export: names conflict with METABANG.UTILITIES
           #:time-hash #:time-unhash
           #:time= #:time/= #:time> #:time>= #:time< #:time<=
           #:time+ #:time-
           #:date-format #:time-format
           #:time-delta-as-seconds #:make-time-delta-from-seconds

           #:datetime-value #:make-datetime
           #:datetime-now
           #:datetime-delta #:datetime-delta-as-seconds
           #:datetime-date #:datetime-time
           #:datetime-
           #:datetime= #:datetime/= #:datetime> #:datetime>= #:datetime< #:datetime<=
           #:datetime+ #:datetime-
           #:elapsed-seconds-since #:elapsed-minutes-since
           )
  (:shadow #:time)
  (:use :cl))

