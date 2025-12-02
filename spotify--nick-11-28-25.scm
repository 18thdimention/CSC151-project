; spotify-data.scm

(import data)
(import test)


;;; (time year month day) -> time?
;;;   year: integer?
;;;   month: integer?
;;;   day: integer?
(struct time (year month day))

;;; (string->time str) -> time?
;;;   str: string?
;;; Converts a string in the 
;;; YYYY-MM-DD format into the time
;;; data structure.
(define string->time
  (lambda (str)
    (let ([time-list (map string->number (string-split str "-"))])
      (time (car time-list)
            (cadr time-list)
            (caddr time-list)))))

(test-case "string->time: 2025-10-23"
  equal? (time 2025 10 23)
  (lambda () (string->time "2025-10-23")))

;;; (get-release-date list) -> string?
;;;   list: list?, row of csv file
;;; Given a parsed csv file, Returns the release date
;;; as a string.
(define get-release-date
  (section list-ref _ 11))

;;; days-in-months
;;; Defines the number of days in each month.
(define days-in-months
  (list (pair 1 31)
        (pair 2 28) ;;;29 in leap year? fix;;;
        (pair 3 31)
        (pair 4 30)
        (pair 5 31)
        (pair 6 30)
        (pair 7 31)
        (pair 8 31)
        (pair 9 30)
        (pair 10 31)
        (pair 11 30)
        (pair 12 31)))


;;; (leap-year? year) -> boolean?
;;;   year : integer?
;;; Returns #t if given year is a leap year.
(define leap-year?
  (lambda (year)
    (or (= 0 (remainder year 400))
        (and (not (= 0 (remainder year 100)))
             (= 0 (remainder year 4))))))

(test-case "leap-year? divisible by 100"
  equal? #f
  (lambda () (leap-year? 1900)))

(test-case "leap-year? divisible by 400"
  equal? #t
  (lambda () (leap-year? 2000)))

(test-case "leap-year? 2020"
  equal? #t
  (lambda () (leap-year? 2020)))

(test-case "leap-year? 2024"
  equal? #t
  (lambda () (leap-year? 2024)))


;;; (days-in-year year) -> integer?
;;;   year : integer?
;;; Checks if a day is a leap year and
;;; returns the number of days in the year
(define days-in-year
  (lambda (year)
    (if (leap-year? year)
        366
        365)))

(test-case "days-in-year 2024"
  equal? 366
  (lambda () (days-in-year 2024)))

(test-case "days-in-year 2023"
  equal? 365
  (lambda () (days-in-year 2023)))


;;; (months->days-helper month months-list) -> integer?
;;;   month : integer?
;;;   months-list : assoc-list?
;;; Returns the number of days in the months since the
;;; start of the year.
(define months->days-helper
  (lambda (month months-list)
    (match month
      [0 0]
      [_ (+ (assoc-ref month months-list) (months->days-helper (- month 1) months-list))])))

;;; (months->days month leap?) -> integer?
;;;   month : integer?
;;;   leap? : boolean?
;;; Returns the number of days in the months since the 
;;; start of the year, adjusted for a leap year.
(define months->days 
  (lambda (month leap?)
    (if leap?
        (months->days-helper month (assoc-set 2 29 days-in-months))
        (months->days-helper month days-in-months))))

(test-case "months->days january nonleap"
  equal? 31
  (lambda () (months->days 1 #f)))

(test-case "months->days january leap"
  equal? 31
  (lambda () (months->days 1 #t)))

(test-case "months->days march nonleap"
  equal? 90
  (lambda () (months->days 3 #f)))

(test-case "months->days march leap"
  equal? 91
  (lambda () (months->days 3 #t)))


;;; (years->days year) -> integer?
;;;   year : integer?
;;;  Returns the number of days in the years since 1970.
(define years->days 
  (lambda (year)
    (match year
      [1970 0]
      [_ (+ (days-in-year (- year 1)) (years->days (- year 1)))])))


(test-case "years->days 1971"
  equal? 365
  (lambda () (years->days 1971)))

(test-case "years->days 2025"
  equal? 20089
  (lambda () (years->days 2025)))

(test-case "years->days 2001" 
  equal? 11323
  (lambda () (years->days 2001)))

(test-case "years->days 2000" ;;;FAILED! expected 10,957 recieved 10,958;;;
  equal? 10957
  (lambda () (years->days 2000)))

(test-case "years->days 1999" 
  equal? 10592
  (lambda () (years->days 1999)))

(test-case "years->days 1998" 
  equal? 10227
  (lambda () (years->days 1998)))

(test-case "years->days 1997" 
  equal? 9862
  (lambda () (years->days 1997)))

(test-case "years->days 1996" 
  equal? 9496
  (lambda () (years->days 1996)))

(test-case "years->days 1995" 
  equal? 9131
  (lambda () (years->days 1995)))

(test-case "years->days 1994"
  equal? 8766
  (lambda () (years->days 1994)))

(test-case "years->days 1993" 
  equal? 8401
  (lambda () (years->days 1993)))

(test-case "years->days 1992" 
  equal? 8035
  (lambda () (years->days 1992)))

(test-case "years->days 1991" 
  equal? 7670
  (lambda () (years->days 1991)))

(test-case "years->days 1990" 
  equal? 7305
  (lambda () (years->days 1990)))



;;; (time->timestamp time) -> integer?
;;;   time : time?
;;; Converts a time data structure into
;;; days since january 1st, 1970
(define time->timestamp
  (lambda (time)
      (- (+ (time-day time)
            (months->days (- (time-month time) 1) (leap-year? (time-year time)))
            (years->days (time-year time)))
         1)))


(test-case "time->timestamp 2025-11-24"
  equal? 20416
  (lambda () (time->timestamp (time 2025 11 24))))

(test-case "time->timestamp 2024-5-10"
  equal? 19853
  (lambda () (time->timestamp (time 2024 5 10))))

(test-case "time->timestamp 2000-7-21"
  equal? 11159
  (lambda () (time->timestamp (time 2000 7 21))))

(test-case "time->timestamp 1970-12-21"
  equal? 354
  (lambda () (time->timestamp (time 1970 12 21))))

(test-case "time->timestamp 1999" 
  equal? 10592
  (lambda () (time->timestamp (time 1999 1 1))))


;;; (day-of-week time) -> integer?
;;;   time : time?
;;; Converts a time into a day of the week,
;;; Where sunday = 0, Monday = 1 and so on.
(define day-of-week
  (lambda (time)
    (remainder (+ (time->timestamp time) 4) 7)))

;;;TO DO!!!!! MAKE MORE TESTS!!!!;;;
(test-case "day-of-week 2025-11-24 (monday)"
  equal? 1
  (lambda () (day-of-week (time 2025 11 24))))

(test-case "day-of-week 2024-5-10 (friday)"
  equal? 5
  (lambda () (day-of-week (time 2024 5 10))))


(define day-code
  (list 
        (pair 2 "Tuesday")
        (pair 0 "Sunday")
        (pair 3 "Wednesday")
        (pair 5 "Friday")
        (pair 4 "Thursday")
        (pair 1 "Monday")
        (pair 6 "Saturday")))

;;; (sort-tally-< tally) -> assoc-list?
;;;   tally : tally?
;;; When given an assoc-list with numeric
;;; keys, sorts the keys in ascending order
;;; and returns the tally.
(define sort-tally-<
  (lambda (tally)
    (let ([sorted-keys (sort (map car tally) <)])
      (map (lambda (key) (pair key (assoc-ref key tally)))
        sorted-keys))))

;;; (chart-days tally) -> histogram?
;;;   tally : assoc-list?
;;; Given an assoc-list with keys in numeric
;;; order from 0 to 6 representing days of the week,
;;; returns a histogram plotting those values.
(define chart-days
  (lambda (tally)
    (plot-category
      (list "Sunday" "Monday" "Tuesday" "Wednesday"
            "Thursday" "Friday" "Saturday")
      (dataset-bar
        "Tallies"
        (map cdr tally)))))

(with-file-chooser
  (lambda (data)
    (chart-days 
      (sort-tally-<
        (tally-all
          (map (o day-of-week string->time get-release-date)
            (cdr (parse-csv data))))))))

