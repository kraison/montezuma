(in-package #:montezuma)

(defparameter +iso-8601-date-format+
  '((:year 4) #\- (:month 2) #\- (:day 2)))

(defparameter +rfc3339-format+ +iso-8601-date-format+)

(defparameter +iso-8601-time-format+
  '((:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:usec 6)))

#|
(defun parse-date-time (text)
  (local-time:parse-timestring text))
|#

(defstruct (datetime (:print-function print-datetime))
  year
  month
  day
  hour
  min
  sec
  subsecs
  offset)

(defun print-datetime (self stream depth)
  (declare (ignore depth))
  (format stream "#S<DATETIME :YEAR ~4d" (datetime-year self))
  (when (datetime-month self) (format stream " :MONTH ~2d" (datetime-month self)))
  (when (datetime-day self) (format stream " :DAY ~2d" (datetime-day self)))
  (when (datetime-hour self) (format stream " :HOUR ~2d" (datetime-hour self)))
  (when (datetime-min self) (format stream " :MIN ~2d" (datetime-min self)))
  (when (datetime-sec self) (format stream " :SEC ~2d" (datetime-sec self)))
  (when (datetime-subsecs self) (format stream " :SUBSECS ~2d" (datetime-subsecs self)))
  (when (datetime-offset self) (format stream " :OFFSET ~2d" (datetime-offset self)))
  (format stream ">"))

(defmethod first-absent-date-field ((self datetime))
  (if (null (datetime-year self)) :year
    (if (null (datetime-month self)) :month
      (if (null (datetime-day self)) :day
        (if (null (datetime-hour self)) :hour
          (if (null (datetime-min self)) :min
            (if (null (datetime-sec self)) :sec
              (if (null (datetime-subsecs self)) :subsecs
                (if (null (datetime-offset self)) :offset)))))))))

(defmethod make-timestamps ((self datetime))
  (if (< (datetime-year self) 100)
      (error "Only 4 digit years are allowed, got ~a" (datetime-year self)))
  (let ((first-absent (first-absent-date-field self))
        (subsecs (datetime-subsecs self))
        (sec (datetime-sec self))
        (min (datetime-min self))
        (hour (datetime-hour self))
        (day (datetime-day self))
        (month (datetime-month self))
        (year (datetime-year self))
        (offset (datetime-offset self))
        timestamp1 timestamp2)
    (case first-absent
      (:year)
      (:month
       (setf timestamp1 (local-time:encode-timestamp 0 0 0 0 1 1 year :offset 0))
       (setf timestamp2 (local-time:encode-timestamp 0 0 0 0 1 1 (1+ year) :offset 0)))
      (:day
       (setf timestamp1 (local-time:encode-timestamp 0 0 0 0 1 month year :offset 0))
       (setf timestamp2 (local-time:encode-timestamp 0 0 0 0 1 (1+ month) year :offset 0)))
      (:hour
       (setf timestamp1 (local-time:encode-timestamp 0 0 0 0 day month year :offset 0))
       (setf timestamp2 (local-time:encode-timestamp 0 0 0 0 (1+ day) month year :offset 0)))
      (:min
       (setf timestamp1 (local-time:encode-timestamp 0 0 0 hour day month year :offset 0))
       (setf timestamp2 (local-time:encode-timestamp 0 0 0 (1+ hour) day month year :offset 0)))
      (:sec
       (setf timestamp1 (local-time:encode-timestamp 0 0 min hour day month year :offset 0))
       (setf timestamp2 (local-time:encode-timestamp 0 0 (1+ min) (1+ hour) day month year :offset 0)))
      (:subsecs
       (setf timestamp1 (local-time:encode-timestamp 0 sec min hour day month year :offset 0))
       (setf timestamp2 (local-time:encode-timestamp 0 (1+ sec) (1+ min) (1+ hour) day month year :offset 0)))
      (:offset
       (setf timestamp1 (local-time:encode-timestamp subsecs sec min hour day month year :offset (* 12 60)))
       (setf timestamp2 (local-time:encode-timestamp subsecs sec min hour day month year :offset (- (* 12 60)))))
      (t 
       (setf timestamp1 (local-time:encode-timestamp subsecs sec min hour day month year :offset offset))
       (setf timestamp2 (local-time:encode-timestamp subsecs sec min hour day month year :offset offset))))
    (values timestamp1 timestamp2 first-absent)))

(defun calculate-time-offset (sign minutes)
  (multiple-value-bind (offset-hours offset-mins) (truncate minutes 100)
    (* sign (+ offset-mins (* offset-hours 60)))))

(defun parse-date-integers (ymd time offset)
  (let ((date (parse-integer ymd))
        (ym 0)
        (y 0)
        (m nil)
        (d nil)
        (h nil)
        (min nil)
        (s nil)
        (subsec nil))
    (if (<= date 10000)
        (setf y date)
      (if (<= date 1000000)
          (multiple-value-setq (y m) (floor date 100))
        (progn
          (multiple-value-setq (ym d) (floor date 100))
          (multiple-value-setq (y m) (floor ym 100)))))

    ;;(break "parse-date-integers ~s" time)
    (when time
      ;;(setf time (subseq (concatenate 'string time "000000000000") 0 8))
      (let ((cl-ppcre:*allow-named-registers* t))
        (multiple-value-bind (scanner registers) 
            (cl-ppcre:create-scanner "^(?<hour>\\d{2})?(?<min>\\d{2})?(?<sec>\\d{2})?(?<subsec>\\d+)?$")
          (multiple-value-bind (start end starts ends) (cl-ppcre:scan scanner time)
            (if (and start end)
                (setf h (get-integer-register registers starts ends "hour" time nil)
                      min (get-integer-register registers starts ends "min" time nil)
                      s (get-integer-register registers starts ends "sec" time nil)
                      subsec (if s (get-integer-register registers starts ends "subsec" time nil))))))))

    (make-datetime :year y :month m :day d :hour h :min min :sec s :subsecs subsec :offset offset)))

(defun parse-date (registers scanner data)
  (unless data (return-from parse-date nil))
  (unless (stringp data) (return-from parse-date data))
  (multiple-value-bind (match-whole matches) (cl-ppcre:scan-to-strings scanner data :sharedp t)
    (if match-whole
        (local-time:encode-timestamp 
         (* 1000000000 (get-scanned-float registers matches "subsec"))
         (get-scanned-integer registers matches "sec" 0)
         (get-scanned-integer registers matches "min" 0)
         (get-scanned-integer registers matches "hour" 0)
         (get-scanned-integer registers matches "day" 1)
         (get-scanned-integer registers matches "month" 1)
         (get-scanned-integer registers matches "year" (local-time:timestamp-year (local-time:now)))
         :offset (let ((zone (get-scanned-register registers matches "tzd")))
                   (if (string-equal "Z" zone)
                       0
                     (let* ((sign (string= zone "-"))
                            (hours (get-scanned-integer registers matches "hours" 0))
                            (minutes (get-scanned-integer registers matches "minutes" 0))
                            (seconds (if (and hours minutes) (+ (* hours 60 60)) (* minutes 60))))
                       (if sign (- seconds) seconds))))))))

(defparameter ddmmyyyyhhmm-format '((DAY 2) / (MONTH 2) / (YEAR 4) " " (HOUR 2) #\: (MIN 2)))
;;(test-date-parser '((DAY 2) / (MONTH 2) / (YEAR 4) " " (HOUR 2) #\: (MIN 2)) "03/02/1981 12:30")
;;(defparameter dmyhm-exp (make-date-parser dmyhm))

(defparameter ddmmyyyyhhmm-regular-expression
  "^\\W*(?<DAY>\\d{2})/(?<MONTH>\\d{2})/(?<YEAR>\\d{4})( (?<HOUR>\\d{2}):(?<MIN>\\d{2}))?\\W*$")

(defparameter ISO-8601-format
  ;; (:year 4) #\- (:month 2) #\- (:day 2) T (:hour 2) (:min 2) (:sec 2) (:usec 6) (Z|((+|-) (:hours 2) (:mins 2)))
  (concatenate 'string
               "^\\W*"
               "(?<year>\\d{4})[\\-/ ]?(?<month>\\d{2})?[\\-/ ]?(?<day>\\d{2})?"
               "([T ](?<hour>\\d{2}):(?<min>\\d{2}))?"
               "(:(?<sec>\\d{2})([\\.,](?<subsec>\\d+))?)?"
               "((?<tzd>[Z+\\-])((?<hours>\\d{2}):?(?<minutes>\\d{2}))?)?"
               "\\W*$"))

  ;;`(,@+iso-8601-date-format+ #\T ,@+iso-8601-time-format+ :gmt-offset-or-z))

(defparameter +iso-8601-format+
  ;; 2008-11-18T02:32:00.586931+01:00
  (concatenate 'string
               "^\\W*"
               "(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})"
               "(T(?<hour>\\d{2}):(?<min>\\d{2}))?"
               "((:(?<sec>\\d{2}))?(\\.(?<subsec>\\d{6}))?)?"
               "((?<tzd>[Z+\\-])((?<hours>\\d{2}):(?<minutes>\\d{2}))?)?"
               "\\W*$"))


(defparameter ddmmyyy-format 
  (concatenate 'string
               "^\\W*"
               "(?<day>\\d{2})/(?<month>\\d{2})/(?<year>\\d{4})"
               "( (?<hour>\\d{2}):(?<min>\\d{2}))?"
               "(:(?<sec>\\d{2})(\\.(?<subsec>\\d+)))?"
               "((?<tzd>[Z+\\-])((?<hours>\\d{2}):(?<minutes>\\d{2}))?)?"
               "\\W*$"))

(defparameter mmddyyy-format 
  (concatenate 'string
               "^\\W*"
               "(?<month>\\d{2})[/ \\-]?(?<day>\\d{2})[/ \\-](?<year>\\d{4})"
               "( (?<hour>\\d{2}):(?<min>\\d{2}))?"
               "(:(?<sec>\\d{2})(\\.(?<subsec>\\d+)))?"
               "((?<tzd>[Z+\\-])((?<hours>\\d{2}):(?<minutes>\\d{2}))?)?"
               "\\W*$"))

(defun test-date-parser (format data)
  (let ((cl-ppcre:*allow-named-registers* t))
    (multiple-value-bind (scanner registers)
        (cl-ppcre:create-scanner format)
      (parse-date registers scanner data))))

(let ((cl-ppcre:*allow-named-registers* t))
  (multiple-value-bind (scanner registers)
      (cl-ppcre:create-scanner
       (concatenate 'string
                    "^\\W*"
                    "(?<year>\\d{4})[\\-/ ]?(?<month>\\d{2})?[\\-/ ]?(?<day>\\d{2})?"
                    "([tT ](?<hour>\\d{2})[:]?(?<min>\\d{2}))?"
                    "([:]?(?<sec>\\d{2})([\\.,]?(?<subsec>\\d+))?)?"
                    "((?<tzd>[zZ+\\-])((?<houroff>\\d{2}):?(?<minoff>\\d{2}))?)?"
                    "\\W*$"))
    (defun parse-query-date (text)
      (multiple-value-bind (matched substrings) (cl-ppcre:scan-to-strings scanner text :sharedp t)
        (let ((alist (collect-named-substrings registers substrings)))
          (if matched
              (make-datetime
               :year (get-named-integer "year" alist)
               :month (get-named-integer "month" alist)
               :day (get-named-integer "day" alist)
               :hour (get-named-integer "hour" alist)
               :min (get-named-integer "min" alist)
               :sec  (get-named-integer "sec" alist)
               :subsecs (get-named-integer "subsecs" alist)
               :offset (if (get-named-substring "tzd" alist)
                           (+ (* (get-named-integer "houroff" alist) 60)
                              (get-named-integer "minoff" alist))))))))))
