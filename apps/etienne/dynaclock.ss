(define dynaclock%
  (class text%
    (init    _dynapad)
    (init-field (_year 1980)  (_month 8)   (_day 27)
                (_hour 22)    (_minute 30) (_24-hour #f))

    (public inc-minute inc-hour inc-day inc-month inc-year)
    (public set-time year month day hour minute)
    (public 24-hour)

    (public monthName daysInMonth isLeapYear syncWithSystem)
    (public date time)

    (inherit dynaclass)

    (rename (super-text text))
    (override text)

    (super-instantiate (_dynapad))
    (dynaclass 'dynaclock%)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Set time methods
    (define (set-time tYear tMonth tDay tHour tMinute)
      (set! _year   tYear)
      (set! _month  tMonth)
      (set! _day    tDay)
      (set! _hour   tHour)
      (set! _minute tMinute)
      (text))

    (define year
      (case-lambda
        (() _year)
        ((x) (set! _year x) (text))))

    (define month
      (case-lambda
        (() _month)
        ((x) (set! _month x) (text))))

    (define day
      (case-lambda
        (() _day)
        ((x) (set! _day x) (text))))

    (define hour
      (case-lambda
        (() _hour)
        ((x) (set! _hour x) (text))))

    (define minute
      (case-lambda
        (() _minute)
        ((x) (set! _minute x) (text))))

    (define 24-hour
      (case-lambda
        (()  _24-hour)
        ((x) (set! _24-hour x) (text))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Increment time methods
    (define (inc-minute n)
      (define total (+ _minute n))
      (define hours (floor (/ total 60)))

      (set! _minute (- total (* 60 hours)))
      (if (> hours 0)
          (inc-hour hours))
      (text))

    (define (inc-hour n)
      (define total (+ _hour n))
      (define days  (floor (/ total 24)))

      (set! _hour (- total (* 24 days)))
      (if (> days 0)
          (inc-day days))
      (text))

    (define (inc-day n)
      (define e (- (daysInMonth _month 1) _day))

      (if (<= n e)
          (set! _day (+ _day n))
          (begin
            (set! n (- n e 1))
            (inc-month 1)
            (set! _day 1)
            (inc-day n)))
      (text))

    (define (inc-month n)
      (define total (+ _month n))
      (define years (floor (/ (- total 1) 12)))

      (set! _month (- total (* 12 years)))
      (if (= _month 0)
          (set! _month 12))

      (if (> years 0)
          (inc-year years))
      (text))

    (define (inc-year n)
      (set! _year (+ _year n))
      (text))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Helper methods
    (define (monthName m)
      (cond ((= m 1) "January")
            ((= m 2) "February")
            ((= m 3) "March")
            ((= m 4) "April")
            ((= m 5) "May")
            ((= m 6) "June")
            ((= m 7) "July")
            ((= m 8) "August")
            ((= m 9) "September")
            ((= m 10) "October")
            ((= m 11) "November")
            ((= m 12) "December")))

    (define (daysInMonth m . y)
      (if (null? y)
          (set! y _year)
          (set! y (car y)))

      (cond ((or (= m 1) (= m 3) (= m 5) (= m 7) (= m 8) (= m 10) (= m 12))
             31)
            ((or (= m 4) (= m 6) (= m 9) (= m 11))
             30)
            ((= m 2)
             (if (isLeapYear y)
                 29
                 28))))

    (define (isLeapYear y)
      (if (= (modulo y 4) 0)
          (if (= (modulo y 100) 0)
              (if (= (modulo y 400) 0)
                  #t
                  #f)
              #t)
          #f))

    (define (syncWithSystem)
      (define cur (seconds->date (current-seconds)))
      (set! _year   (date-year cur))
      (set! _month  (date-month cur))
      (set! _day    (date-day cur))
      (set! _hour   (date-hour cur))
      (set! _minute (date-minute cur))
      (text))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Override methods
    (define (time)
      (define return "")

      ; The hour
      (cond ((and (not _24-hour) (= _hour 0))
             (set! return (string-append return "12")))
            ((and (not _24-hour) (> _hour 12))
             (set! return (string-append return (number->string (- _hour 12)))))
            (else
             (set! return (string-append return (number->string _hour)))))

      ; The minute
      (if (< _minute 10)
          (set! return (string-append return ":0" (number->string _minute)))
          (set! return (string-append return ":"  (number->string _minute))))

      ; Am/Pm
      (if (not _24-hour)
          (if (< _hour 12)
              (set! return (string-append return "am"))
              (set! return (string-append return "pm"))))

      return)

    (define (date)
      (string-append
       (monthName _month) " "
       (number->string _day) ", "
       (number->string _year)))

    (define (text)
      (super-text (string-append (date) " " (time))))
    )
  )
