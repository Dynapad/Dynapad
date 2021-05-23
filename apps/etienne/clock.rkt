(define clock%
  (class object%
    (super-instantiate ())

    (define/public (today)
      (- (current-seconds) (* 24 60 60)))

    (define/public (tomorrow)
      (current-seconds))
    )
  )
