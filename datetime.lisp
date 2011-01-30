;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Module DATETIME
;;; License: ?
;;;
;;; ------
;;; DATE
;;;   Support Gregorian calendar in a reasonable range of years
;;;
;;; Class: date-value
;;;   slots: year, month, day
;;;   accessors: date-year, date-month, date-day
;;;   (Possibly store the ordinal day number instead, such as julian day
;;;   number or rata die? Or maybe just cache it?)
;;;
;;; Class: date-delta
;;;   slots: days, ???
;;;   probably should allow creation of "precise" deltas (measured in days)
;;;   and "imprecise" deltas, such as "plus one month", "minus one week"
;;; 
;;; Creation:
;;;   (make-instance 'date :year 2006 :month 12 :day 1)
;;;   convenience function:
;;;   (make-date 2006 12 1)
;;;   (make-date '(2006 12 1))
;;;   (make-date "2006/12/1")  ;; Full parsing?
;;;   (make-date :today)
;;;   ??? probably more weird stuff, like "first Monday of April 2006"
;;;   (make-date date1 :use-year 2006)
;;;   (make-date date1 :use-year date2)
;;;   (make-date datetime)  - extract the date portion
;;;
;;; Informative:
;;;   days-in-month, day-of-week, leap-year-p etc.
;;;
;;; Operations:
;;;   (date-to-ordinal date)  -  number of days since "epoch" start, some
;;;       analog of julian day number,http://en.wikipedia.org/wiki/Julian_day,
;;;       at the moment it's "rata die"
;;;   (ordinal-to-date ordinal-days) - reverse
;;;       maybe the ordinal functions should not be exported
;;;   ??? - conversions to other calendars?
;;;   (date- date2 date1)  - return date-delta object
;;;   (date- date1 date-delta) - a date preceding date1 by date-delta
;;;   (date+ date1 date-delta) - a date succeeding date1 by date-delta
;;;   (date+ date-delta1 date-delta2) - addition and substaction of deltas,
;;;   (date- date-delta1 date-delta2) - ??? not clear what to do with them
;;;                                     in the case of imprecise deltas,
;;;                                     maybe only define these for precise
;;;
;;; Comparison:
;;;   date=, date/=, date>, date<, date>=, date<=, (date-beween-dates?)
;;;
;;; Formatting:
;;;   (date-format date &optional (format-string *default-date-format*))
;;;   format strings of the "YYYY-MM-DD" kind
;;;   ideally should follow locale settings for month and day-of-week names,
;;;   in the beginning might allow for user-specified arrays of names,
;;;   default to English
;;;
;;; Iteration:
;;;   1. (do-between-dates (date1 date2 :inclusive)...  macro
;;;   2. ITERATE extension ?
;;;
;;; Hashing:
;;;   1. genhash module ?
;;;   2. functions date-hash and date-unhash, to be used as
;;;      (gethash (date-hash date1) my-hash-table)  ?
;;;
;;; ------
;;; TIME
;;;   This is NOT a representation of an "absolute" point on time axis,
;;;   and therefore does not carry any timezone or related information
;;;   (see DATETIME for that)
;;;   This represents the notion of time-of-day, relative to some locally
;;;   accepted day start
;;;
;;;   Probably should support more than 24 hours, since it's actually a
;;;   requirement for some business and also will allow to easily define
;;;   a time+ operation. Should have a normalize-time operation, that will
;;;   return (values normalized-time overflow-days)
;;;
;;; Class: time
;;;   slots: hour, minute, second, millisecond
;;;         (do we need microsecond resolution for this?) 
;;;
;;; Class: time-delta
;;;   slots: milliseconds ?
;;;
;;; Creation:
;;;   (make-instance 'time :hour 17 :minute 25 :second 15 :millisecond 0)
;;;   convenience function:
;;;   (make-time '(17 25 15))
;;;   (make-time "17:25:15")
;;;   (make-time :now)
;;;   (make-time datetime)  - extract the time portion (discarding the tz?)
;;;
;;; Operations:
;;;   (time-to-seconds time)  -  number of seconds since day start
;;;                             (possibly fractional)
;;;   (seconds-to-time num-seconds) - reverse
;;;        (are these necessary?)
;;;
;;;   (time+ ...    (see date)
;;;   (time- ...
;;;   (normalize-time time)  - (or normalise?) see above
;;;   
;;; Comparison:
;;;   (see date)
;;;
;;; Formatting:
;;;   (time-format time &optional (format-string *default-time-format*))
;;;   format-string like "HH:MM" (handle am/pm?)
;;;   
;;; Iteration:
;;;   Is it actually necessary?
;;;
;;; Hashing:
;;;   see date
;;;
;;;
;;; ------
;;; TIME-INTERVALS
;;;   It might be useful to provide "time interval" objects that have start time
;;;   and duration (or end time) and operations on them (intersection,union etc)
;;;   also operations on lists of intervals, checking if a specific time falls
;;;   into one of the list of intervals etc. But this probably does not belong
;;;   here and should be separate?
;;;   
;;;
;;; ------
;;; DATETIME
;;;   Should allow for both timezoned and non-timezoned operation
;;;   (tz slot null?)
;;;   
;;; Class: datetime
;;;   slots: year, month, day, hour, minute, second, millisecond, tz ?
;;;
;;; Class: datetime-delta
;;;   slots: ?
;;;
;;; 
;;; Creation:
;;;   ???
;;;   (make-datetime "2006-10-...") ;; ISO 8601 ?
;;;   (make-datetime '(year, month, ...))
;;;   (make-datetime '(date, time))
;;;   (make-datetime '(date, time, tz))
;;;
;;; Operations:
;;;   
;;; Comparison:
;;;   (see date)
;;;
;;; Formatting:
;;;   
;;; Iteration:
;;;   Probably not defined?
;;;
;;; Hashing:
;;;   Is it useful?


;;; ------

;; (defpackage #:simple-datetime
;;   (:export #:date-time
;;            #:date
;;            #:make-date
;; ;           #:date-to-ordinal
;; ;           #:ordinal-to-date
;;            #:date-hash #:date-unhash
;;            #:date= #:date/= #:date> #:date>= #:date< #:date<=
;;            #:date+ #:date-
           
;;            #:time
;;            #:make-time
;;            #:time-hash #:time-unhash
;;            #:time= #:time/= #:time> #:time>= #:time< #:time<=
;;            #:time+ #:time-
;;            )
;;   (:shadow #:time)
;;   (:use :cl))

(in-package #:trivial-datetime)


(declaim (optimize (safety 3) (debug 3) (speed 0) (space 0))) ;; FIXME: remove when debugged


; FIXME: replace defparameters with defconstant?


;;; ------------------
;;; DATE
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defparameter +max-hour+ 47)           ; Yes, more than 24
 (defparameter +fractions-in-second+ 1000)
 (defparameter +max-fractions+ (1- (* (1+ +max-hour+) 60 60 +fractions-in-second+)))
 (defparameter +max-year+ 2999)
 (defparameter +fractions-per-day+ (* +fractions-in-second+ 60 60 24)))

(deftype year-type () '(integer 0 #.+max-year+))
(deftype month-type () '(integer 1 12))
(deftype day-type () '(integer 1 31))

(deftype hour-type () '(integer 0 #.+max-hour+))  
(deftype minute-type () '(integer 0 59))
(deftype second-type () '(integer 0 59))
(deftype second-fraction-type () '(integer 0 1000))
(deftype fractions-type () '(integer 0 #.+max-fractions+))
(deftype delta-fractions-type () '(integer #.(- +max-fractions+) #.+max-fractions+))

(deftype full-fractions-type () '(integer))


;; ; Ensure during compilation time that fixnum is big enough for our purposes
;; (eval-when (:compile-toplevel)
;;   (assert (typep (* 24 60 60 1000) 'fixnum)))


(deftype date-ordinal-type () '(integer 0 #.(* 365 3000)))  ;; FIXME


;; --
; "Date Algorithms" By Peter Baum, http://www.vsg.cape.com/~pbaum/date/date0.htm
;
; We use Rata Die: Rata Die day one occurs on January 1 of the year 1
; which begins at Julian Day Number 1721425.5
;
(defparameter +rata-die-months-vector+ (make-array '(13)
                                           :element-type '(integer 0 400)
                                           :initial-contents  '(000 306 337 0 31 61 92 122 153 184 214 245 275))) ; months are 1-based, so elt 0 is unused

;; (defconstant +rata-die-months-vector+ #.(make-array '(13)
;;                                            ;:element-type '(integer 0 400)
;;                                            :initial-contents  '(000 306 337 0 31 61 92 122 153 184 214 245 275))) ; months are 1-based, so elt 0 is unused


(declaim (ftype (function (year-type month-type day-type) date-ordinal-type)  ymd-to-rata-die))
(defun ymd-to-rata-die (y m d)
  (declare (type year-type y) (type month-type m) (type day-type d))
  (let ((z (if (< m 3) (1- y) y))
        (f (aref +rata-die-months-vector+ m)))
    (declare (type (integer 0 400) f))   ; FIXME
    (- (+ d f (* 365 z) (floor z 4) (floor z 400))
       (floor z 100) 306)))


(declaim (ftype (function (date-ordinal-type)
                          (values year-type month-type day-type)) rata-die-to-ymd))
(defun rata-die-to-ymd (rd)
  (declare (type date-ordinal-type rd))
  (let* ((z (+ rd 306))
         (h (- (* 100 z) 25))
         (a (floor h 3652425))
         (b (- a (floor a 4)))
         (year (floor (+ (* 100 b) h) 36525))
         (c (- (+ b z) (* 365 year) (floor year 4)))
         (month (truncate (+ (* 5 c) 456) 153))
         (day (- c (truncate (- (* 153 month) 457) 5))))
    (when (> month 12)
      (incf year)
      (decf month 12))
    (values year month day)))


;; --

(defclass date-value ()
  ((ordinal :initarg :ordinal
         :type date-ordinal-type
         :accessor date-ordinal))
  (:documentation "Gregorian date, extended to before Cregorian calendar creation"))


(defgeneric date-year (date-obj))
(defmethod date-year ((date date-value))
  (multiple-value-bind (y m d) (rata-die-to-ymd (date-to-ordinal date))
    (declare (ignore m d))
    y))

(defgeneric date-month (date-obj))
(defmethod date-month ((date date-value))
  (multiple-value-bind (y m d) (rata-die-to-ymd (date-to-ordinal date))
    (declare (ignore y d))    
    m))

(defgeneric date-day (date-obj))
(defmethod date-day ((date date-value))
  (multiple-value-bind (y m d) (rata-die-to-ymd (date-to-ordinal date))
    (declare (ignore y m))    
    d))


(defmethod print-object ((obj date-value) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (format stream "[~4,'0d-~2,'0d-~2,'0d]"
             (date-year obj)
             (date-month obj)
             (date-day obj))))

(defgeneric make-date (param))

(defun make-date-ymd (y m d)
  (declare (type year-type y) (type month-type m) (type day-type d))
  "Make a date from three numbers, year, month and day "
  (assert (and (typep y 'year-type)
               (typep m 'month-type)
               (typep d 'day-type)))
  (ordinal-to-date (ymd-to-rata-die y m d)))

(defmethod make-date ((param list))
  "Make a date from a list like '(2006 10 11)"
  (destructuring-bind (y m d) param
    (declare (type year-type y) (type month-type m) (type day-type d))
    (assert (and (typep y 'year-type)
                 (typep m 'month-type)
                 (typep d 'day-type)))
    
    (make-date-ymd y m d)))

(defmethod make-date ((param date-value))
  "Copy date"
  (ordinal-to-date (date-to-ordinal param)))

(defmethod make-date ((param (eql :today)))
  "Today's date"
  (today))


(defmethod make-date ((param (eql :epoch-start)))
  (ordinal-to-date 1))

;; --
(defclass date-delta ()
  ((days :initarg :days
         :type integer ;; date-ordinal-type   ;; has to allow for negative values
         :accessor date-delta-days))
  (:documentation "Object representing difference between two dates"))

(defmethod print-object ((obj date-delta) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "[Days: ~d]"
            (date-delta-days obj))))


(defun make-date-delta (&key (days 0) (months 0) (years 0))
  (declare (type integer days)
           (type integer months)
           (type integer years))
  (when (or (/= 0 months)
            (/= 0 years))
    (error "Not implemented"))
  (make-instance 'date-delta :days days))

(defgeneric negate-delta (delta))
(defmethod negate-delta ((delta date-delta))
  (make-date-delta :days (- (date-delta-days delta))))


;; (declaim (ftype (function (date) fixnum) date-to-days))
;; (defun date-to-ordinal (date)
;;   (assert (typep date 'date))
;;   (with-accessors ((y date-year) (m date-month) (d date-day)) date
;;     (ymd-to-rata-die y m d)))

;; (declaim (ftype (function (fixnum) date) ordinal-to-date))
;; (defun ordinal-to-date (ordinal)
;;   (multiple-value-bind (year month day) (rata-die-to-ymd ordinal)
;;     (make-date year month day)))

(defun date-to-ordinal (date)
  (declare (type date-value date))
  (assert (typep date 'date-value))
  (date-ordinal date))

(declaim (ftype (function (date-ordinal-type) date-value) ordinal-to-date))
(defun ordinal-to-date (ordinal)
  (declare (type date-ordinal-type ordinal))
  (make-instance 'date-value :ordinal ordinal))

;; --

(defun today ()
  "Return today's date object."
  (multiple-value-bind (sec minute hour day month year) (get-decoded-time)
    (declare (ignorable sec minute hour))
    (make-date-ymd year month day)))

;; --
(declaim (inline date-hash))
(declaim (ftype (function (date-value) date-ordinal-type) date-hash))
(defun date-hash (date)
  "Return a hash suitable to use as a key in hash-tables"
  (date-to-ordinal date))

(declaim (inline date-unhash))
(declaim (ftype (function (date-ordinal-type) date-value) date-unhash))
(defun date-unhash (hash)
  "Reverse the hash function"
  (ordinal-to-date hash))

;; --

(eval-when (:compile-toplevel :load-toplevel)
  (defun exported-p (sym-to-check) 
    (do-external-symbols (sym)
      (if (eq sym sym-to-check) (return t)))))

(defmacro with-verified-export ((sym) form)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel)
       (assert (exported-p ',sym)))
     ,form))

(defmacro def-date-comparison-op (date-op-name op-name docstring)
  `(with-verified-export (,date-op-name)
     (defun ,date-op-name (date1 date2)
       ,docstring
       (,op-name (date-to-ordinal date1) (date-to-ordinal date2)))) )

(def-date-comparison-op date= = "Date equality")
(def-date-comparison-op date/= /= "Date inequality")
(def-date-comparison-op date> > "Date comparison")
(def-date-comparison-op date< < "Date comparison")
(def-date-comparison-op date>= >= "Date comparison")
(def-date-comparison-op date<= <= "Date comparison")

;; --
(defgeneric date- (obj2 obj1)
  (:documentation "Difference between date or date-delta objects"))

(defmethod date- ((date2 date-value) (date1 date-value))
  (let ((datediff (- (date-to-ordinal date2) (date-to-ordinal date1))))
    (make-date-delta :days datediff)))

(defmethod date- ((delta2 date-delta) (delta1 date-delta))
  (let ((newdeltadays (- (date-delta-days delta2)
                         (date-delta-days delta1))))
    (make-date-delta :days newdeltadays)))

(defmethod date- ((date date-value) (delta date-delta))
  (date+ date (negate-delta delta)))

(defmethod date- ((date date-value) (int-delta integer))
  (date+ date (negate-delta (make-date-delta :days int-delta))))


(defgeneric date+ (obj1 obj2)
  (:documentation "Sum of a date and a delta object"))


(defmethod date+ ((date date-value) (delta date-delta))
  (let ((newordinal (+ (date-to-ordinal date) (date-delta-days delta))))
    (ordinal-to-date newordinal)))


(defmethod date+ ((delta date-delta) (date date-value))
  (date+ date delta))


(defmethod date+ ((delta1 date-delta) (delta2 date-delta))
  (let ((newdeltadays (+ (date-delta-days delta2)
                         (date-delta-days delta1))))
    (make-date-delta :days newdeltadays)))


(defmethod date+ ((date date-value) (int-delta integer))
  (date+ date (make-date-delta :days int-delta)))


(defgeneric day-of-week (date)
  (:documentation "Day of week for the DATE or DATETIME object, 0=Sunday"))

(defmethod day-of-week ((date date-value))
  (rem (date-to-ordinal date) 7))



(defun leap-year-p (year)
  (or (and (zerop (mod year 4))
           (not (zerop (mod year 100))))
      (zerop (mod year 400))))


(defparameter *days-in-month-arr* (make-array '(13) :initial-contents '(0 31 28 31 30 31 30 31 31 30 31 30 31)))


(defun %days-in-month (year month)
  (if (and (= month 2)
           (leap-year-p year))
      29
      (aref *days-in-month-arr* month)))

(defun days-in-month (date)
  (%days-in-month (date-year date) (date-month date)))


(defun last-date-in-month (date)
  (make-date-ymd (date-year date) (date-month date) (days-in-month date)))


(defun normalize-for-M-Y-and-make-date (y m d)
  ;; The -FOR-M-Y- part of the name means that we do not properly
  ;; process the day rollover, i.e. the 2011-02-31 does not become
  ;; the 2011-03-03, it gets truncated to 2011-02-28
  (flet ((normalize-y-m (year month)
           (declare (integer year month))
           (multiple-value-bind (ty tm)
                   (floor (1- month) 12)
                 (values (+ year ty)
                         (1+ tm)))))

    (declare (inline normalize-y-m))
    (multiple-value-setq (y m) (normalize-y-m y m))
    (let ((d-in-m (%days-in-month y m)))
      (when (> d d-in-m)
        (setf d d-in-m)))
    (when (< d 1)
      (setf d 1))
    (make-date-ymd y m d)))

(defun date+months (date months-delta)
  (let ((year (date-year date))
        (month (date-month date))
        (day (date-day date)))
    (normalize-for-M-Y-and-make-date year
                                     (+ month months-delta)
                                     day)))


(defun date-min (&rest args)
  (ordinal-to-date (apply #'min (mapcar #'date-to-ordinal args))))
(defun date-max (&rest args)
  (ordinal-to-date (apply #'max (mapcar #'date-to-ordinal args))))



;;; ------------------
;;; TIME

(defclass time-value ()
  ((hour :initarg :hour
         :type hour-type
         :accessor time-hour)
   (minute :initarg :minute
           :type minute-type
           :initform 0
           :accessor time-minute)
   (second :initarg :second
           :type second-type
           :initform 0
           :accessor time-second)
   (fraction :initarg :fraction
             :type second-fraction-type
             :initform 0
             :accessor time-fraction))
  (:documentation "Time of day, relative to some local 'start of day'"))

(defmethod print-object ((obj time-value) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (format stream "[~2,'0d:~2,'0d:~2,'0d.~3,'0d]"
             (time-hour obj)
             (time-minute obj)
             (time-second obj)
             (time-fraction obj))))


(defun make-time-hmsf (h &optional (m 0) (s 0) (fraction 0))
  (make-instance 'time-value :hour h :minute m
                   :second s :fraction fraction))

(defun time-now ()
  "Return current time object."
  ; FIXME: make it more precise (implementation dependent?)
  (multiple-value-bind (sec minute hour day month year) (get-decoded-time)
    (declare (ignorable day month year))
    (make-time-hmsf hour minute sec)))


(defgeneric make-time (param)
  (:documentation "Convenience function for creating time instances"))

(defmethod make-time ((param list))
  (destructuring-bind (hour &optional (minute 0)
                            (second 0) (fraction 0)) param
    (make-time-hmsf hour minute second  fraction)))

(defmethod make-time ((param time-value))
  "Copy time"
  (fractions-to-time (time-to-fractions param)))


(defmethod make-time ((param symbol))
  "Make special times"
  (ecase param
    (:now (time-now))
    (:zero (make-time-hmsf 0))))

;; (defmethod make-time ((param integer))
;;   "Fractions to time"
;;   (fractions-to-time param))


;;
(declaim (inline hmsfrac-to-fractions))
(declaim (ftype (function (hour-type minute-type second-type second-fraction-type) fractions-type) hmsfrac-to-fractions))
(defun hmsfrac-to-fractions (h m s frac)
  (+ frac (* +fractions-in-second+ (+ s (* 60 (+ m (* 60 h)))))))

(declaim (ftype (function (time-value) fractions-type) time-to-fractions))
(defun time-to-fractions (time)
  (with-accessors ((h time-hour) (m time-minute)
                   (s time-second) (frac time-fraction)) time
    (hmsfrac-to-fractions h m s frac)))

(declaim (ftype (function (fractions-type) time-value) fractions-to-time))
(defun fractions-to-time (fractions)
  (multiple-value-bind (total-secs frac) (floor fractions +fractions-in-second+)
    (multiple-value-bind (total-mins secs) (floor total-secs 60)
      (multiple-value-bind (hours mins) (floor total-mins 60)
        (make-time-hmsf hours mins secs frac)))))

;; --

(defmacro def-time-comparison-op (time-op-name op-name docstring)
  `(with-verified-export (,time-op-name)
     (defun ,time-op-name (time1 time2)
       ,docstring
       (,op-name (time-to-fractions time1) (time-to-fractions time2)))) )

(def-time-comparison-op time= = "Time equality")
(def-time-comparison-op time/= /= "Time inequality")
(def-time-comparison-op time> > "Time comparison")
(def-time-comparison-op time< < "Time comparison")
(def-time-comparison-op time>= >= "Time comparison")
(def-time-comparison-op time<= <= "Time comparison")

;; --
(declaim (inline time-hash))
(declaim (ftype (function (time-value) fractions-type) time-hash))
(defun time-hash (time)
  "Return a hash suitable to use as a key in hash-tables"
  (time-to-fractions time))

(declaim (inline time-unhash))
(declaim (ftype (function (fractions-type) time-value) time-unhash))
(defun time-unhash (hash)
  "Reverse the hash function"
  (fractions-to-time hash))

;; --
(defclass time-delta ()
  ((fractions :initarg :fractions
         :type delta-fractions-type
         :accessor time-delta-fractions))
  (:documentation "Object representing difference between two times"))

(defmethod print-object ((obj time-delta) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (format stream "[fractions: ~d]"
             (time-delta-fractions obj))))


(defun make-time-delta (&key (fractions 0))
  (declare (type delta-fractions-type fractions))
  (make-instance 'time-delta :fractions fractions))

(defun make-time-delta-from-seconds (seconds)
  (make-time-delta :fractions (* seconds +fractions-in-second+)))


(defmethod negate-delta ((delta time-delta))
  (make-time-delta :fractions (- (time-delta-fractions delta))))

;; --

(defgeneric time- (obj2 obj1)
  (:documentation "Difference between time or time-delta objects"))

(defmethod time- ((time2 time-value) (time1 time-value))
  (let ((timediff (- (time-to-fractions time2) (time-to-fractions time1))))
    (make-time-delta :fractions timediff)))

(defmethod time- ((delta2 time-delta) (delta1 time-delta))
  (let ((new-delta-fracs (- (time-delta-fractions delta2)
                         (time-delta-fractions delta1))))
    (make-time-delta :fractions new-delta-fracs)))

(defmethod time- ((time time-value) (delta time-delta))
  (time+ time (negate-delta delta)))



(defgeneric time+ (obj1 obj2)
  (:documentation "Sum of a time and a delta object"))


(defmethod time+ ((time time-value) (delta time-delta))
  (let ((new-fracs (+ (time-to-fractions time) (time-delta-fractions delta))))
    (fractions-to-time new-fracs)))


(defmethod time+ ((delta time-delta) (time time-value))
  (time+ time delta))


(defmethod time+ ((delta1 time-delta) (delta2 time-delta))
  (let ((new-delta-fracs (+ (time-delta-fractions delta2)
                         (time-delta-fractions delta1))))
    (make-time-delta :fractions new-delta-fracs)))



(defun time-delta-as-seconds (delta)
  (/ (time-delta-fractions delta) +fractions-in-second+))



;; --

(defun date-format (date &optional (format "YYYY-MM-DD"))
  (assert (string-equal format "YYYY-MM-DD"))
  (multiple-value-bind (y m d) (rata-die-to-ymd (date-to-ordinal date))
    (format nil "~4,'0d-~2,'0d-~2,'0d" y m d)))

(defun time-format (timeval &optional (format "HH:MM"))
  (assert (string-equal format "HH:MM"))
  (let ((h (time-hour timeval))
        (m (time-minute timeval)))
    (format nil "~2,'0d:~2,'0d" h m)))


(defun time-delta-format (timedeltaval &optional (format "HH:MM"))
  (assert (string-equal format "HH:MM"))
  (let* ((secs (time-delta-as-seconds timedeltaval))
         (m (mod (truncate secs 60) 60))
         (h (truncate secs (* 60 60) )))
    (format nil "~2,'0d:~2,'0d" h m)))

;;; -- datetime
(defclass datetime-value ()
  ((date-val :initarg :date
         :type date-value
         :accessor datetime-date)
   (time-val :initarg :time
         :type time-value
         :accessor datetime-time))
  (:documentation "Combined DATE and TIME class"))


(defmacro def%%passthrough (meth-name accessor)
  (let ((var (gensym)))
    `(defmethod ,meth-name ((,var datetime-value))
         (,meth-name (,accessor ,var)))))

(def%%passthrough date-year datetime-date)
(def%%passthrough date-month datetime-date)
(def%%passthrough date-day datetime-date)
(def%%passthrough day-of-week datetime-date)

(def%%passthrough time-hour datetime-time)
(def%%passthrough time-minute datetime-time)
(def%%passthrough time-second datetime-time)
(def%%passthrough time-fraction datetime-time)


(defun datetime-to-fractions (datetime)
  (with-accessors ((dval datetime-date) (tval datetime-time)) datetime
    (+ (* (date-to-ordinal dval)
          +fractions-per-day+)
       (time-to-fractions tval))))

(defun fractions-to-datetime (full-fractions)
  (multiple-value-bind (days fracs) (truncate full-fractions +fractions-per-day+)
    (make-datetime (ordinal-to-date days) (fractions-to-time fracs))))




(defgeneric make-datetime (param1 &optional param2))

(defmethod make-datetime ((date-param date-value) &optional time-param)
  "Make DATETIME from DATE and time"
  (make-instance 'datetime-value
                 :date (make-date date-param)
                 :time (if time-param
                           (make-time time-param)
                           (fractions-to-time 0))))


(defconstant +unix-epoch-fractions+ 62135683200000)  ;; (datetime-to-fractions (make-datetime '(1970 1 1)))

(defun datetime-now ()
  "Return the current DATETIME"
  #+sbcl (progn
           (multiple-value-bind (sec microsec) (sb-ext:get-time-of-day)
             (let ((frac (+ (* (- sec (* (%get-time-zone-offset) 60 60))
                               +fractions-in-second+)
                            (truncate (* microsec (/ +fractions-in-second+ 1000000))))))
               (fractions-to-datetime (+ +unix-epoch-fractions+ frac)))))
  #-sbcl (error "don't know how to get datetime-now"))


(defmethod make-datetime ((date-param (eql :now)) &optional time-param)
  "Make current DATETIME"
  (declare (ignore time-param))
  (datetime-now))

(defmethod print-object ((obj datetime-value) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (format stream "[~4,'0d-~2,'0d-~2,'0d -- ~2,'0d:~2,'0d:~2,'0d.~3,'0d]"
             (date-year obj)
             (date-month obj)
             (date-day obj)
             (time-hour obj)
             (time-minute obj)
             (time-second obj)
             (time-fraction obj))))


(defmethod make-date ((param datetime-value))
  "Copy the date portion from DATETIME"
  (make-date (datetime-date param)))

(defmethod make-time ((param datetime-value))
  "Copy the time portion from DATETIME"
  (make-time (datetime-time param)))


(defclass datetime-delta ()
  ((fractions :initarg :fractions
              :type full-fractions-type
              :accessor datetime-delta-fractions))
  (:documentation "Object representing difference between two datetimes"))


(defmethod print-object ((obj datetime-delta) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (format stream "[total fractions: ~d]"
             (datetime-delta-fractions obj))))


(defun make-datetime-delta (&key (fractions 0))
  (make-instance 'datetime-delta :fractions fractions))

(defun datetime-delta-as-seconds (delta)
  (/ (datetime-delta-fractions delta) +fractions-in-second+))


(defmethod datetime- ((datetime2 datetime-value) (datetime1 datetime-value))
  (let ((datetimediff (- (datetime-to-fractions datetime2) (datetime-to-fractions datetime1))))
    (make-datetime-delta :fractions datetimediff)))

(defun elapsed-seconds-since (datetime1)
  (datetime-delta-as-seconds (datetime- (datetime-now) datetime1)))

(defun elapsed-minutes-since (datetime1)
  (/ (elapsed-seconds-since datetime1) 60))



;; TZ
(defun %get-time-zone-offset ()
  (- (nth-value 8 (get-decoded-time))
     (if (nth-value 8 (get-decoded-time)) 1 0)))
