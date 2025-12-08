;; CSC 151 Fall
;; Spotify Data Visualization
;; Authors: Temni A, Doyeon K, Mayu I, Nick R.
;; Date: 12/10/2025
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

(import data)
(import test)
(import lab)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-of-days
  (list "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday"
        "Friday" "Saturday"))

(define list-of-months
  (list "January" "February" "March" "April" "May" "June" "July"
        "August" "September" "October" "November" "December"))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; multi-purpose functions ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; (get-release-date list) -> string?
;;;   list: list?, row of csv file
;;; Given a parsed csv file, Returns the release date
;;; as a string.
(define get-release-date
  (section list-ref _ 11))

;;; (get-track-popularity list) -> string?
;;;   list: list?, row of csv file
;;; Given a parsed csv file, Returns the track popularity
;;; as a string.
(define get-track-popularity
  (section list-ref _ 3))

;;; (get-track-popularity list) -> string?
;;;   list: list?, row of csv file
;;; Given a parsed csv file, Returns the track popularity
;;; as a string.
(define get-artist-popularity
  (section list-ref _ 6))

;;; (chart-tally x-labels dataset-options dataset-name tally) -> histogram?
;;;   dataset-options: list of key-value pairs
;;;   x-labels: list? of string?, (equal? (length x-labels) (length tally))
;;;   dataset-name: string?
;;;   tally : assoc-list?
;;; Plots a histogram with the y axis consisting of the cdr of 
;;; every tally pair and the x axis consisting of the list of strings.
(define chart-tally
  (lambda (x-labels dataset-options dataset-name tally)
    (plot-category
      x-labels
      (with-dataset-options
        dataset-options
        (dataset-bar
          dataset-name
          (map cdr tally))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; day-of-week implementation ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (leap-year? year) -> boolean?
;;;   year : integer?
;;; Returns #t if given year is a leap year.
(define leap-year?
  (lambda (year)
    (or (= 0 (remainder year 400))
        (and (not (= 0 (remainder year 100)))
             (= 0 (remainder year 4))))))

;;; (days-in-year year) -> integer?
;;;   year : integer?
;;; Checks if a day is a leap year and
;;; returns the number of days in the year
(define days-in-year
  (lambda (year)
    (if (leap-year? year)
        366
        365)))

;;; (months->days-helper month months-list) -> integer?
;;;   month : integer?
;;;   months-list : assoc-list?
;;; Returns the number of days in the months since the
;;; start of the year.
(define months->days-helper
  (lambda (month months-list)
    (match month
      [0 0]
      [_ (+ (assoc-ref month months-list) 
         (months->days-helper (- month 1) months-list))])))

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

;;; (years->days year) -> integer?
;;;   year : integer?
;;;  Returns the number of days in the years since 1950.
(define years->days 
  (lambda (year)
    (match year
      [1950 0]
      [_ (+ (days-in-year (- year 1)) (years->days (- year 1)))])))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; visualizing number of tracks by date ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; (tally-by-time proc data) -> list?
;;;   proc : procedure?, operates on a time and 
;;;          returns an integer representing a specific element.
;;;   data : string? from a file, csv format.
(define tally-by-time
  (lambda (proc data)
    (sort-tally-<
      (tally-all
        (map (o proc string->time get-release-date)
          (parse-csv data))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; implementing average popularity by time ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (time-popularity-pair proc csv-line) -> pair? (of integer?)
;;;   proc : procedure? operates on a time and returns 
;;;          an integer representing a specific element.
;;;   csv-line : list? line of the csv file
;;; Returns a pair in the form of
;;; (pair time-element track-popularity)
(define time-popularity-pair
  (lambda (proc csv-line)
    (let ([track-popularity 
           (string->number 
             (get-track-popularity csv-line))]
          [time
           (proc
              (string->time
                (get-release-date csv-line)))])
      (pair time track-popularity))))

;;; (car-< pair1 pair2) -> boolean?
;;;   pair1 : pair?
;;;   pair2 : pair?
;;; Returns #t if the first
;;; element of pair1 is less than
;;; the first element of pair2
(define car-<
  (lambda (pair1 pair2)
    (< (car pair1) (car pair2))))

;;; (total-and-number-helper pairs n total elements) -> assoc-list?
;;;   pairs : list of pair values, car = day & cdr = popularity,
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
       (if (= n (car head))
           (total-and-number-helper tail n (+ total (cdr head)) (+ elements 1))
           (cons (pair n (list total elements)) 
                 (total-and-number-helper tail (+ n 1) (cdr head) 1)))])))

;;; (total-and-number-helper pairs n total elements) -> assoc-list?
;;;   pairs : list of pair values, car = time element & cdr = popularity,
;;;           ascending order
;;;   n : integer? starting point of time value
;;; Given a list of pair values, returns an association list with the time
;;; as a key and the total summed score and number of elements as a value.
(define total-and-number
  (lambda (pairs n)
    (total-and-number-helper pairs n 0 0)))

;;; (average pair1) -> pair?
;;;   pair1 : pair?
;;; Given a pair containing a head element and
;;; a list tail, returns the average by dividing the
;;; first element of the list by the second element.
(define average
  (lambda (pair1)
    (let* ([tail (cdr pair1)]
           [head (car pair1)]
           [average (/ (car tail)
                       (cadr tail))])
      (pair head average))))

;;; (average-popularity-by-time proc data) -> list?
;;;   proc: procedure?, takes one row as input and outputs a pair
;;;         of an integer representing a time element and and integer
;;;         representing the track popularity.
;;;   data: string? from a file, csv format.
(define average-popularity-by-time
  (lambda (proc data)
    (let* ([sorted-data
           (sort
             (filter (section not (equal? 0 (cdr _)))
               (map proc
                 (parse-csv data)))
             car-<)]
          [n (caar sorted-data)])
      (map average
        (total-and-number sorted-data n)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HIGH LEVEL / USER-LEVEL ALGORITHMNS ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(description "Day of the Week VS Released Tracks")

;;; DAYS BAR GRAPH ;;;

(define days-dataset-options
  (list (pair "background-color" "darkgreen")))

(with-file-chooser
  (lambda (data)
    (with-plot-options
      (list (pair "title" "Amount of released tracks vs. day of week")
            (pair "x-label" "Days")
            (pair "y-label" "Number of tracks"))
      (chart-tally list-of-days days-dataset-options "Amount released on given day"
        (tally-by-time day-of-week data)))))

(description "Month of the Year VS Released Tracks")

;;; MONTHS BAR GRAPH ;;;

(define months-dataset-options
  (list (pair "background-color" "red")))

(with-file-chooser
  (lambda (data)
    (with-plot-options
      (list (pair "title" "Amount of released tracks vs. month")
            (pair "x-label" "Months")
            (pair "y-label" "Number of tracks"))
      (chart-tally list-of-months months-dataset-options "Amount released in given month"
        (tally-by-time time-month data)))))

(description "Average track popularity by day")

;;; AVERAGE TRACK POPULARITY BY DAY ;;;
(define day-pop-dataset-options
  (list (pair "background-color" "orange")))

(with-file-chooser
  (lambda (data)
    (with-plot-options
      (list (pair "title" "Average track popularity vs. day of week")
            (pair "x-label" "Days")
            (pair "y-label" "Average track popularity"))
      (chart-tally list-of-days day-pop-dataset-options "Average track popularity on given day"
        (average-popularity-by-time
          (section time-popularity-pair day-of-week _) data)))))

(description "Average track popularity by month")
;;; AVERAGE TRACK POPULARITY BY MONTH
(define month-pop-dataset-options
  (list (pair "background-color" "pink")))
  
(with-file-chooser
  (lambda (data)
    (with-plot-options
      (list (pair "title" "Average track popularity vs. month")
            (pair "x-label" "Months")
            (pair "y-label" "Average track popularity"))
      (chart-tally list-of-months month-pop-dataset-options "Average track popularity in given month"
        (average-popularity-by-time
          (section time-popularity-pair time-month _) data)))))

;;;;;;;;;;;;;;;;;;;;;;
;;;; mayu's stuff ;;;;
;;;;;;;;;;;;;;;;;;;;;;

;;; (track-popularity data) -> list?
;;;   data: data?
;;; Returns a list of track popularity scores.
(define track-popularity
  (with-file-chooser
    (lambda (data)
      (map (o string->number get-track-popularity)
        (parse-csv data)))))

track-popularity

;;; (artist-popularity data) -> list?
;;;   data: data?
;;; Returns a list of artist popularity scores.
(define artist-popularity
  (with-file-chooser
    (lambda (data)
      (map (o string->number get-artist-popularity)
        (parse-csv data)))))

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
            (parse-csv data))
          (map (o string->number get-artist-popularity)
            (parse-csv data)))))))

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
            (parse-csv data))
          (map (o string->number get-artist-popularity)
            (parse-csv data)))))))

(define get-artist
  (lambda (x)
    (list-ref x 5)))

(define get-artist-list
  (lambda (data)
    (map (section list-ref _ 5) data)))

(define get-artists-with-49-tracks
  (lambda (data)
    (filter (section >= (cdr _) 49) (tally-all (get-artist-list data)))))


;(define idk-helper
 ; (lambda (artist-data data)
  ;  (pair (car artist-data) (filter (section equal? (car artist-data) (get-artist _)) data))))

  
; (with-file "spotify_data_no_nested_quotes.csv"
;  (lambda (data)
;    (let ([d (clean-unusable-rows (parse-csv data))])
;      (get-artists-with-49-tracks (get-artist-list d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; test cases ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-case "string->time: 2025-10-23"
  equal? (time 2025 10 23)
  (lambda () (string->time "2025-10-23")))

(test-case "leap-year?: divisible by 100"
  equal? #f
  (lambda () (leap-year? 1900)))

(test-case "leap-year?: divisible by 400"
  equal? #t
  (lambda () (leap-year? 2000)))

(test-case "leap-year?: 2020"
  equal? #t
  (lambda () (leap-year? 2020)))

(test-case "leap-year?: 2024"
  equal? #t
  (lambda () (leap-year? 2024)))

(test-case "days-in-year: 2024"
  equal? 366
  (lambda () (days-in-year 2024)))

(test-case "days-in-year: 2023"
  equal? 365
  (lambda () (days-in-year 2023)))

(test-case "months->days: january nonleap"
  equal? 31
  (lambda () (months->days 1 #f)))

(test-case "months->days: january leap"
  equal? 31
  (lambda () (months->days 1 #t)))

(test-case "months->days: march nonleap"
  equal? 90
  (lambda () (months->days 3 #f)))

(test-case "months->days: march leap"
  equal? 91
  (lambda () (months->days 3 #t)))

(test-case "years->days: 1950"
  equal? 0
  (lambda () (years->days 1950)))

(test-case "years->days: 1971"
  equal? 7670
  (lambda () (years->days 1971)))

(test-case "years->days: 2025"
  equal? 27394
  (lambda () (years->days 2025)))

(test-case "years->days: 2001" 
  equal? 18628
  (lambda () (years->days 2001)))

(test-case "years->days: 2000"
  equal? 18262
  (lambda () (years->days 2000)))

(test-case "time->timestamp: 2025-11-24"
  equal? 27721
  (lambda () (time->timestamp (time 2025 11 24))))

(test-case "time->timestamp: 2024-5-10"
  equal? 27158
  (lambda () (time->timestamp (time 2024 5 10))))

(test-case "time->timestamp: 2000-7-21"
  equal? 18464
  (lambda () (time->timestamp (time 2000 7 21))))

(test-case "time->timestamp: 1970-12-21"
  equal? 7659
  (lambda () (time->timestamp (time 1970 12 21))))

(test-case "time->timestamp: 1999-1-1" 
  equal? 17897
  (lambda () (time->timestamp (time 1999 1 1))))

(test-case "day-of-week: 2025-11-24 (monday)"
  equal? 1
  (lambda () (day-of-week (time 2025 11 24))))

(test-case "day-of-week: 2024-5-10 (friday)"
  equal? 5
  (lambda () (day-of-week (time 2024 5 10))))

(test-case "day-of-week: 1952-5-12 (Thursday)"
  equal? 4
  (lambda () (day-of-week (time 1952 6 12))))

(test-case "day-of-week: 1956-07-28 (Saturday)"
  equal? 6
  (lambda () (day-of-week (time 1956 7 28))))

(test-case "day-of-week: 1964-12-16 (Wednesday)"
  equal? 3
  (lambda () (day-of-week (time 1964 12 16))))

(test-case "sort-tally-<: numeric tally"
  equal? (list (pair 0 234) (pair 1 34) (pair 12 23) (pair 234 2))
  (lambda () (sort-tally-< 
               (list (pair 0 234) (pair 12 23) (pair 234 2) (pair 1 34)))))

(test-case "total-and-number: empty list"
  equal? (list (pair 0 (list 0 0)))
  (lambda () (total-and-number null 0)))

(test-case "total-and-number: one pair"
  equal? (list (pair 0 (list 12 1)))
  (lambda () (total-and-number (list (pair 0 12)) 0)))

(test-case "total-and-number: multiple pairs pair"
  equal? (list (pair 0 (list 55 3)))
  (lambda () (total-and-number (list (pair 0 12) (pair 0 20) (pair 0 23)) 0)))


(test-case "total-and-number: n increments"
  equal? (list (pair 0 (list 55 3)) (pair 1 (list 43 2)))
  (lambda () (total-and-number (list (pair 0 12) (pair 0 20) (pair 0 23) (pair 1 23)
                                     (pair 1 20)) 0)))

(test-case "total-and-number: start with 1"
  equal? (list (pair 1 (list 43 2)))
  (lambda () (total-and-number (list (pair 1 23)
                                     (pair 1 20)) 1)))


