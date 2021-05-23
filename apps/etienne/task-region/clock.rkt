
(define clock%
  (class object%
    (super-instantiate ())

    (define/public (today)
      (define now   (seconds->date (current-seconds)))
      (define year  (date-year now))
      (define month (date-month now))
      (define day   (date-day now))
      (if (< month 10) (set! month (format "0~a" month)))
      (if (< day 10)   (set! day   (format "0~a" day)))
      (format "~a~a~a000000" year month day))

    (define/public (tomorrow)
      (define now   (seconds->date (current-seconds)))
      (define year  (date-year now))
      (define month (date-month now))
      (define day   (+ 1 (date-day now)))
      (if (< month 10) (set! month (format "0~a" month)))
      (if (< day 10)   (set! day   (format "0~a" day)))
      (format "~a~a~a000000" year month day))
    )
  )
