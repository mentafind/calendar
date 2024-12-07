;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname calendar) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; (calendar m y) consumes a month, m, (as a number) and a year, y, (as a number) and produces
;;   the calendar for that month
;; m=month ; y=year

;; examples
(check-expect (calendar 1 2025) '(((Sun 29) (Mon 30) (Tue 31) (Wed 1) (Thu 2) (Fri 3) (Sat 4))
                                  ((Sun 5) (Mon 6) (Tue 7) (Wed 8) (Thu 9) (Fri 10) (Sat 11))
                                  ((Sun 12) (Mon 13) (Tue 14) (Wed 15) (Thu 16) (Fri 17) (Sat 18))
                                  ((Sun 19) (Mon 20) (Tue 21) (Wed 22) (Thu 23) (Fri 24) (Sat 25))
                                  ((Sun 26) (Mon 27) (Tue 28) (Wed 29) (Thu 30) (Fri 31) (Sat 1))))
(check-expect (calendar 2 2016) '(((Sun 31) (Mon 1) (Tue 2) (Wed 3) (Thu 4) (Fri 5) (Sat 6))
                                  ((Sun 7) (Mon 8) (Tue 9) (Wed 10) (Thu 11) (Fri 12) (Sat 13))
                                  ((Sun 14) (Mon 15) (Tue 16) (Wed 17) (Thu 18) (Fri 19) (Sat 20))
                                  ((Sun 21) (Mon 22) (Tue 23) (Wed 24) (Thu 25) (Fri 26) (Sat 27))
                                  ((Sun 28) (Mon 29) (Tue 1) (Wed 2) (Thu 3) (Fri 4) (Sat 5))))

;; calendar: Int Int -> (listof (listof (Sym Int)))
;; Requires:
;; * Month is an integer less than or equal to 12
;; * Year is an integer
(define (calendar m y)
  (cond [(or (> m 12)
             (< m 1)
             (not (integer? m)))
         "invalid month"] ; tests for invalid month input
        [(not (integer? y))
         "invalid year"] ; tests for invalid year input
        [else
         (local [(define (search dict m)
                   (cond [(= m (first (first dict))) (second (first dict))]
                         [else (search (rest dict) m)]))
                 (define diem '((1 31)
                                (2 28)
                                (3 31)
                                (4 30)
                                (5 31)
                                (6 30)
                                (7 31)
                                (8 31)
                                (9 30)
                                (10 31)
                                (11 30)
                                (12 31)))
                 (define (collapse lst)
                   (reverse (foldl (lambda (x rror) (append x rror))
                                   (first lst)
                                   (rest lst))))
                 ;; name of the first day of the inputed month
                 (define (first-day m y)
                   (local [(define base-date '(Sun 1 Jan 2017))
                           (define jan1 (floor (* 365.25 (- y (fourth base-date)))))
                           ;; sum of days of previous month
                           (define (sdpm dict m)
                             (cond [(= (first (first dict)) m) 0]
                                   [else (+ (second (first dict)) (sdpm (rest dict) m))]))]
                     (cond [(and (= 0 (remainder y 4))
                                 (> m 2))
                            (modulo (+ 1 jan1 (sdpm diem m)) 7)]
                           [else (modulo (+ jan1 (sdpm diem m)) 7)])))
                 (define fd (first-day m y))
                 ;; dipm = (number of) days in previous month
                 (define (dipm-func m y)
                   (cond [(= m 1) 31]
                         [(and (= m 3) (= (remainder y 4) 0)) 29]
                         [else (search diem (sub1 m))]))
                 (define dipm (dipm-func m y))
                 ;; dicm = (number of) days in current month
                 (define (dicm-func m y)
                   (cond [(and (= m 2) (= (remainder y 4) 0)) 29]
                         [else (search diem m)]))
                 (define dicm (dicm-func m y))
                 ;; number of rows needed to fit all days of the month on a calendar
                 (define (num-of-rows m y)
                   (cond [(and (= fd 0)
                               (= dicm 28))
                          4]
                         [(or (<= fd 4)
                              (and (= fd 5)
                                   (<= dicm 30))
                              (and (= fd 6)
                                   (<= dicm 29)))
                          5]
                         [(or (and (= fd 5)
                                   (= dicm 31))
                              (and (= fd 6)
                                   (>= dicm 30)))
                          6]))
                 (define rows (num-of-rows m y))
                 ;; building list of each day's number
                 (define (build-days-num m y)
                   (local [(define days-list (build-list dicm (lambda (x) (add1 x))))
                           (define before (build-list fd (lambda (x) (+ 1 x (- dipm fd)))))
                           (define after (build-list (modulo (- 7 (modulo (+ fd dicm) 7)) 7)
                                                     (lambda (x) (add1 x))))]
                     (append before
                             days-list
                             after)))
                 ;; building list of each day's name
                 (define (build-days-name m y)
                   (reverse (collapse (build-list rows (lambda (x) '(Sun Mon Tue Wed Thu Fri Sat))))))
                 ;; combining each day and it's corresponding number
                 (define (build-calendar m y)
                   (map (lambda (name day) (list name day))
                        (build-days-name m y)
                        (build-days-num m y)))
                 ;; splitting the days into weeks
                 (define (split lst m y)
                   (local [(define mapped
                             (map (lambda (index date) (list index date))
                                  (collapse (build-list rows
                                                        (lambda (x) (build-list 7 (lambda (y) x)))))
                                  lst))]
                     (foldr (lambda (list-to-filter index-to-compare rror)
                              (cons (map second
                                         (filter (lambda (item) (= (first item) index-to-compare))
                                                 list-to-filter))
                                    rror)) 
                            empty
                            (build-list rows (lambda (x) mapped))
                            (build-list rows (lambda (x) x)))))]
           (split (build-calendar m y) m y))]))

;; tests
;; testing invalid input
(check-expect (calendar 1.5 2025) "invalid month")
(check-expect (calendar 13 2025) "invalid month")
(check-expect (calendar -3 2025) "invalid month")
(check-expect (calendar 12 2025.2) "invalid year")

;; testing february starting on sunday during leap year
(check-expect (calendar 2 2004) '(((Sun 1) (Mon 2) (Tue 3) (Wed 4) (Thu 5) (Fri 6) (Sat 7))
                                  ((Sun 8) (Mon 9) (Tue 10) (Wed 11) (Thu 12) (Fri 13) (Sat 14))
                                  ((Sun 15) (Mon 16) (Tue 17) (Wed 18) (Thu 19) (Fri 20) (Sat 21))
                                  ((Sun 22) (Mon 23) (Tue 24) (Wed 25) (Thu 26) (Fri 27) (Sat 28))
                                  ((Sun 29) (Mon 1) (Tue 2) (Wed 3) (Thu 4) (Fri 5) (Sat 6))))
;; testing febraury starting on sunday not during a leap year
(check-expect (calendar 2 2015) '(((Sun 1) (Mon 2) (Tue 3) (Wed 4) (Thu 5) (Fri 6) (Sat 7))
                                  ((Sun 8) (Mon 9) (Tue 10) (Wed 11) (Thu 12) (Fri 13) (Sat 14))
                                  ((Sun 15) (Mon 16) (Tue 17) (Wed 18) (Thu 19) (Fri 20) (Sat 21))
                                  ((Sun 22) (Mon 23) (Tue 24) (Wed 25) (Thu 26) (Fri 27) (Sat 28))))

;; testing march right after a february during a leap year
(check-expect (calendar 3 2012) '(((Sun 26) (Mon 27) (Tue 28) (Wed 29) (Thu 1) (Fri 2) (Sat 3))
                                  ((Sun 4) (Mon 5) (Tue 6) (Wed 7) (Thu 8) (Fri 9) (Sat 10))
                                  ((Sun 11) (Mon 12) (Tue 13) (Wed 14) (Thu 15) (Fri 16) (Sat 17))
                                  ((Sun 18) (Mon 19) (Tue 20) (Wed 21) (Thu 22) (Fri 23) (Sat 24))
                                  ((Sun 25) (Mon 26) (Tue 27) (Wed 28) (Thu 29) (Fri 30) (Sat 31))))

;; testing march right after february not during a leap year
(check-expect (calendar 3 2013) '(((Sun 24) (Mon 25) (Tue 26) (Wed 27) (Thu 28) (Fri 1) (Sat 2))
                                  ((Sun 3) (Mon 4) (Tue 5) (Wed 6) (Thu 7) (Fri 8) (Sat 9))
                                  ((Sun 10) (Mon 11) (Tue 12) (Wed 13) (Thu 14) (Fri 15) (Sat 16))
                                  ((Sun 17) (Mon 18) (Tue 19) (Wed 20) (Thu 21) (Fri 22) (Sat 23))
                                  ((Sun 24) (Mon 25) (Tue 26) (Wed 27) (Thu 28) (Fri 29) (Sat 30))
                                  ((Sun 31) (Mon 1) (Tue 2) (Wed 3) (Thu 4) (Fri 5) (Sat 6))))

;; testing a month that requires 6 rows to fit all its days in the calendar
(check-expect (calendar 6 2013) '(((Sun 26) (Mon 27) (Tue 28) (Wed 29) (Thu 30) (Fri 31) (Sat 1))
                                  ((Sun 2) (Mon 3) (Tue 4) (Wed 5) (Thu 6) (Fri 7) (Sat 8))
                                  ((Sun 9) (Mon 10) (Tue 11) (Wed 12) (Thu 13) (Fri 14) (Sat 15))
                                  ((Sun 16) (Mon 17) (Tue 18) (Wed 19) (Thu 20) (Fri 21) (Sat 22))
                                  ((Sun 23) (Mon 24) (Tue 25) (Wed 26) (Thu 27) (Fri 28) (Sat 29))
                                  ((Sun 30) (Mon 1) (Tue 2) (Wed 3) (Thu 4) (Fri 5) (Sat 6))))