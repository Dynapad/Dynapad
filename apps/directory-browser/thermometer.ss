(define thermometer%
  (class object%
     (super-instantiate ())

     (field (_hot "#a00000")   ;;modified within last day
        (_warm "#a0a000")  ;;modified within last week
        (_cold "#00a000")  ;;modified withing last 30 days
        (_freezing "#0000a0")) ;;not recently modified

     ;; -- Methods --
     (define/public (set-temp icon)
       (let ((age (- (current-seconds)
             (file-or-directory-modify-seconds (send icon path))))
         (thermo (send icon boundary)))
     (cond ((< age 86400)
        (send thermo fill _hot)
        (send thermo pen _hot))
           ((and (>= age 86400) (< age 604800))
        (send thermo fill _warm)
        (send thermo pen _warm))
           ((and (>= age 604800) (< age 2592000))
        (send thermo fill _cold)
        (send thermo pen _cold))
           ((>= age 2592000)
        (send thermo fill _freezing)
        (send thermo pen _freezing)))
     ;(send thermo transparency 1)
     ))

))

