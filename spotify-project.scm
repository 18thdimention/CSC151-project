; spotify-12-3-25.scm

(import data)
(import test)
(import lab)


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
        (pair 2 28) 
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
      [1950 0]
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
    (let ([day-offset (- (remainder (+ (time->timestamp time) 4) 7) 4)])
      (if (< day-offset 0)
          (+ day-offset 7)
          day-offset))))
        

;;;TO DO!!!!! MAKE MORE TESTS!!!!;;;
(test-case "day-of-week 2025-11-24 (monday)"
  equal? 1
  (lambda () (day-of-week (time 2025 11 24))))

(test-case "day-of-week 2024-5-10 (friday)"
  equal? 5
  (lambda () (day-of-week (time 2024 5 10))))

(test-case "day-of-week 1952-5-12 (Thursday)"
  equal? 4
  (lambda () (day-of-week (time 1952 6 12))))

(test-case "day-of-week 1956-07-28 (Saturday)"
  equal? 6
  (lambda () (day-of-week (time 1956 7 28))))

(test-case "day-of-week 1964-12-16 (Wednesday)"
  equal? 3
  (lambda () (day-of-week (time 1964 12 16))))



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

;;; (chart-days tally) -> histogram?
;;;   tally : assoc-list?
;;; Given an assoc-list with keys in numeric
;;; order from 1 to 12 representing months,
;;; returns a histogram plotting those values.
(define chart-months
  (lambda (tally)
    (plot-category
      (list "January" "February" "March" "April" "May" "June" 
            "July" "August" "September" "October" "November" "December")
        (dataset-bar
          "Tallies"
          (map cdr tally)))))


;;; (clean-unusable-rows csv-list) -> list?
;;;   csv-list : list?
;;; Deletes the first and last rows in the dataset.
(define clean-unusable-rows
  (lambda (csv-list)
    (cdr (list-take csv-list (- (length csv-list) 1)))))

;;; (get-track-popularity list) -> string?
;;;   list: list?, row of csv file
;;; Given a parsed csv file, Returns the track popularity
;;; as a string.
(define get-track-popularity
  (section list-ref _ 3))


;;; (day-popularity-pair csv-line) -> pair? (of integer?)
;;;   csv-line : list? line of the csv file
;;; Returns a pair in the form of
;;; (pair track-popularity day-of-week)
(define day-popularity-pair
  (lambda (csv-line)
    (let ([track-popularity 
           (string->number 
             (get-track-popularity csv-line))]
          [day 
           (day-of-week
              (string->time
                (get-release-date csv-line)))])
      (pair track-popularity day))))

;;; (cdr-< pair1 pair2) -> boolean?
;;;   pair1 : pair?
;;;   pair2 : pair?
;;; Returns #t if the second
;;; element of pair1 is less than
;;; the second element of pair2
(define cdr-<
  (lambda (pair1 pair2)
    (< (cdr pair1) (cdr pair2))))


;;; (total-and-number-helper pairs n total elements) -> assoc-list?
;;;   pairs : list of pair values, car = popularity & cdr = day,
;;;           ascending order
;;;   n : zero?
;;;   total : zero?
;;;   elements : zero?
;;; Helper for total-and-number.
(define total-and-number-helper
  (lambda (pairs n total elements)
    (match pairs
      [null (cons (pair n (list total elements)) null)]
      [(cons head tail)
       (if (= n (cdr head))
           (total-and-number-helper tail n (+ total (car head)) (+ elements 1))
           (cons (pair n (list total elements)) 
                 (total-and-number-helper tail (+ n 1) (car head) 1)))])))

;;; (total-and-number-helper pairs n total elements) -> assoc-list?
;;;   pairs : list of pair values, car = popularity & cdr = day,
;;;           ascending order
;;; Given a list of pair values, returns an association list with the day
;;; as a key and the total summed score and number of elements as a value.
(define total-and-number
  (lambda (pairs)
    (total-and-number-helper pairs 0 0 0)))


(total-and-number (list (pair 12 0) (pair 20 0) (pair 23 0) (pair 23 1)) 0 0 0)

(test-case "total-and-number: empty list"
  equal? (list (pair 0 (list 0 0)))
  (lambda () (total-and-number null)))

(test-case "total-and-number: one pair"
  equal? (list (pair 0 (list 12 1)))
  (lambda () (total-and-number (list (pair 12 0)))))

(test-case "total-and-number: multiple pairs pair"
  equal? (list (pair 0 (list 55 3)))
  (lambda () (total-and-number (list (pair 12 0) (pair 20 0) (pair 23 0)))))


(test-case "total-and-number: n increments"
  equal? (list (pair 0 (list 55 3)) (pair 1 (list 43 2)))
  (lambda () (total-and-number (list (pair 12 0) (pair 20 0) (pair 23 0) (pair 23 1)
                                     (pair 20 1)))))

(define average
  (lambda (pair1)
    (let* ([tail (cdr pair1)]
           [head (car pair1)]
           [average (/ (car tail)
                       (cadr tail))])
      (pair head average))))

;;; for satisfactory outcome: visualize track vs artist popularity and release date, color
;;; coding based on day of week

(description "Day of the Week VS Released Tracks")

;;; DAYS BAR GRAPH ;;;
(with-file-chooser
  (lambda (data)
    (chart-days 
      (sort-tally-<
        (tally-all
          (map (o day-of-week string->time get-release-date)
            (clean-unusable-rows (parse-csv data))))))))

(description "Month of the Year VS Released Tracks")

;;; MONTHS BAR GRAPH ;;;
(with-file-chooser
  (lambda (data)
    (chart-months
      (sort-tally-<
        (tally-all
          (map (o time-month string->time get-release-date)
            (clean-unusable-rows (parse-csv data))))))))

;;; AVERAGE TRACK POPULARITY BY DAY ;;; STILL NEEDED: FILTERING OUT CAR 0 FOR THE PAIRS
(with-file-chooser
  (lambda (data)
    (map average
      (total-and-number
        (sort
          (map day-popularity-pair
            (clean-unusable-rows (parse-csv data)))
          cdr-<)))))       


;;; (track-popularity data) -> list?
;;;   data: data?
;;; Returns a list of track popularity scores.
(define track-popularity
  (with-file-chooser
    (lambda (data)
      (map (o string->number get-track-popularity)
        (clean-unusable-rows (parse-csv data))))))

track-popularity


;;; (get-track-popularity list) -> string?
;;;   list: list?, row of csv file
;;; Given a parsed csv file, Returns the track popularity
;;; as a string.
(define get-artist-popularity
  (section list-ref _ 6))

;;; (artist-popularity data) -> list?
;;;   data: data?
;;; Returns a list of artist popularity scores.
(define artist-popularity
  (with-file-chooser
    (lambda (data)
      (map (o string->number get-artist-popularity)
        (clean-unusable-rows (parse-csv data))))))

artist-popularity


;;; (track-artist data) -> list?
;;;   data: data?
;;; Returns a list of pairs of track popularity and artist popularity 
;;; (removed the pairs that have 0 as a track popularity score).
(define track-artist
  (with-file-chooser
    (lambda (data)
      (filter (section not (equal? 0 (car _)))
        (map pair
          (map (o string->number get-track-popularity)
            (clean-unusable-rows (parse-csv data)))
          (map (o string->number get-artist-popularity)
            (clean-unusable-rows (parse-csv data))))))))

track-artist

;;; Scatterplot of track popularity and artist popularity ;;;
(define scatter-for-track-artist
  (lambda (lst)
    (with-plot-options
      (list (pair "x-label" "Track popularity")
            (pair "y-label" "Artist popularity")
            (pair "title" "Scatter plot"))
    (plot-linear
      (dataset-scatter "Track-popularity and artist-popularity"
        lst)))))

(with-file-chooser
  (lambda (data)
    (scatter-for-track-artist
      (filter (section not (equal? 0 (car _)))
        (map pair
          (map (o string->number get-track-popularity)
            (clean-unusable-rows (parse-csv data)))
          (map (o string->number get-artist-popularity)
            (clean-unusable-rows (parse-csv data))))))))
