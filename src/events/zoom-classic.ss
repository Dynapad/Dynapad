;======= Zoom Multiplier ========

(define zoomtimer%
  (class timer%
    (init-field _PAD _zoomproc)
    (field (zoom_speed 0.1) (zoom_dir 1) (prev_iterations 0) (prev_msec 0))
    (override start)
    (public speed)

    (define speed
      (case-lambda
        (() zoom_speed)
	((newspeed) (set! zoom_speed newspeed))))

    (define (start direction interval)
      (set! zoom_dir direction)
      (set! prev_msec (current-milliseconds))
      (set! prev_iterations 0)
      (super start interval))

    (define (zmult)
      (let*
	((cur_msec (current-milliseconds))
	 (elapsed (- cur_msec prev_msec))
	 (elapsed (if (> elapsed 0) elapsed (+ elapsed 1000)))
         (requested_iterations (/ elapsed 50))
	 (iterations (* 0.5 (+ prev_iterations requested_iterations))))

	(set! prev_msec cur_msec)
	(set! prev_iterations iterations)
	(when (> iterations 5) (set! iterations 5))

	(+ 1 (* iterations zoom_speed zoom_dir))))

    (define (proc)
      (_zoomproc _PAD (zmult)))

    (super-instantiate(proc #f #f))))

;======= Zoom, Pan, and Follow Hyperlink ========

(define Zoom-In-lambda
  (lambda (eventPAD e) (set! currentPAD eventPAD)
    (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
    (send (send eventPAD ztimer) start 1 (send eventPAD zinterval))))

(define Zoom-In-Stop-lambda
  (lambda (eventPAD e) (set! currentPAD eventPAD)
    (send (send eventPAD ztimer) stop)))

(define Zoom-Out-lambda
  (lambda (eventPAD e) (set! currentPAD eventPAD)
    (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
      (send (send eventPAD ztimer) start -1 (send eventPAD zinterval))))

(define Zoom-Out-Stop-lambda
  (lambda (eventPAD e) (set! currentPAD eventPAD)
    (send (send eventPAD ztimer) stop)))

(define (bindClassicZoom argPAD)
  (let (( zoomproc
    (lambda (arg_PAD zmult)
      (send arg_PAD zoom zmult 0 (sendf arg_PAD evs lastx)
                                 (sendf arg_PAD evs lasty)))))

  (send argPAD ztimer (make-object zoomtimer% argPAD zoomproc))
  (send argPAD zinterval 10)
  
  (send argPAD bind "<Run-ButtonPress-2>"   Zoom-In-lambda)
  (send argPAD bind "<Run-ButtonRelease-2>" Zoom-In-Stop-lambda)
  (send argPAD bind "<Run-ButtonPress-3>"   Zoom-Out-lambda)
  (send argPAD bind "<Run-ButtonRelease-3>" Zoom-Out-Stop-lambda)

  (send argPAD setvar 'Zoom-In-lambda       Zoom-In-lambda)
  (send argPAD setvar 'Zoom-In-Stop-lambda  Zoom-In-Stop-lambda)
  (send argPAD setvar 'Zoom-Out-lambda      Zoom-Out-lambda)
  (send argPAD setvar 'Zoom-Out-Stop-lambda Zoom-Out-Stop-lambda)
))
