;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;
;

(declaim (optimize (safety 3) (debug 3) (speed 0) (space 0)))

(in-package #:trivial-datetime)



#|
 (defun format-date (format date &optional stream time-zone)
  "Formats universal dates using the same format specifiers as NSDateFormatter. The format is:

%% - A '%' character
%a - Abbreviated weekday name
%A - Full weekday name
%b - Abbreviated month name
%B - Full month name
%c - Shorthand for \"%X %x\", the locale format for date and time
%d - Day of the month as a decimal number [01-31]
%e - Same as %d but does not print the leading 0 for days 1 through 9 
     [unlike strftime[], does not print a leading space]
%F - Milliseconds as a decimal number [000-999]
%H - Hour based on a 24-hour clock as a decimal number [00-23]
%I - Hour based on a 12-hour clock as a decimal number [01-12]
%j - Day of the year as a decimal number [001-366]
%m - Month as a decimal number [01-12]
%M - Minute as a decimal number [00-59]
%p - AM/PM designation for the locale
%S - Second as a decimal number [00-59]
%w - Weekday as a decimal number [0-6], where Sunday is 0
%x - Date using the date representation for the locale, including 
     the time zone [produces different results from strftime[]]
%X - Time using the time representation for the locale [produces 
     different results from strftime[]]
%y - Year without century [00-99]
%Y - Year with century [such as 1990]
%Z - Time zone name [such as Pacific Daylight Time; 
     produces different results from strftime[]]
%z - Time zone offset in hours and minutes from GMT [HHMM]

None of %c, %F, %p, %x, %X, %Z, %z are implemented."
  (declare (ignore time-zone))
  (let ((format-length (length format)))
    (format 
     stream "~{~A~}"
     (loop for index = 0 then (1+ index) 
        while (< index format-length) collect 
          (let ((char (aref format index)))
            (cond 
              ((char= #\% char)
               (setf char (aref format (incf index)))
               (cond 
                 ;; %% - A '%' character
                 ((char= char #\%) #\%)
                            
                 ;; %a - Abbreviated weekday name
                 ((char= char #\a) (day->string (time-day-of-week date) :short))
                            
                 ;; %A - Full weekday name
                 ((char= char #\A) (day->string (time-day-of-week date) :long))
                            
                 ;; %b - Abbreviated month name
                 ((char= char #\b) (month->string (time-month date) :short))
                            
                 ;; %B - Full month name
                 ((char= char #\B) (month->string (time-month date) :long))
                            
                 ;; %d - Day of the month as a decimal number [01-31]
                 ((char= char #\d) (format nil "~2,'0D" (time-date date)))
                            
                 ;; %e - Same as %d but does not print the leading 0 for days 1 through 9 
                 ;;      Unlike strftime, does not print a leading space
                 ((char= char #\e) (format nil "~D" (time-date date)))
                            
                 ;; %H - Hour based on a 24-hour clock as a decimal number [00-23]
                 ((char= char #\H) (format nil "~2,'0D" (time-hour date)))
                            
                 ;; %I - Hour based on a 12-hour clock as a decimal number [01-12]
                 ((char= char #\I) (format nil "~2,'0D" (1+ (mod (time-hour date) 12))))
                            
                 ;; %j - Day of the year as a decimal number [001-366]
                 ((char= char #\j) (format nil "~3,'0D" (day-of-year date)))
                            
                 ;; %m - Month as a decimal number [01-12]
                 ((char= char #\m) (format nil "~2,'0D" (time-month date)))
                            
                 ;; %M - Minute as a decimal number [00-59]
                 ((char= char #\M) (format nil "~2,'0D" (time-minute date)))
                            
                 ;; %S - Second as a decimal number [00-59]
                 ((char= char #\S) (format nil "~2,'0D" (time-second date)))
                            
                 ;; %w - Weekday as a decimal number [0-6], where Sunday is 0
                 ((char= char #\w) (format nil "~D" (time-day-of-week date)))
                            
                 ;; %y - Year without century [00-99]
                 ((char= char #\y) 
                  (let ((year-string (format nil "~,2A" (time-year date))))
                    (subseq year-string (- (length year-string) 2))))
                            
                 ;; %Y - Year with century [such as 1990]
                 ((char= char #\Y) (format nil "~D" (time-year date)))
                                                        
                 (t
                  (error "Ouch - unknown formatter '%~c" char))))
              (t char)))))))

|#
