(dynaload "lerp.ss")

(define (wider-view? v1 v2)
  (let ((z1 (caddr v1))
	(z2 (caddr v2)))
    (< z1 z2)))

(define newpad-event-state%
  (class combo-event-state%
    (init argPAD)
    (super-instantiate (argPAD))

    (public-field v0 _v0)            ; view position initially
    (public-field vfar _vfar)        ; view position for everything
    (public-field vclose _vclose)    ; view position for closeup

    (field (_recent-wide-view (send argPAD view)))
    (public-field vwide _vwide)      ; last wide view (last z min)
    (set! _vwide _recent-wide-view)

    (public-field dirflag _dirflag)  ; -1 = zoom out, 1 = zoom in

;    (define/public (update-wide-view)
;      (let ((currview (sendf this pad view)))
;	(case _dirflag
;	  ((-1) (set! _recent-wide-view currview))
;	  ((1)  (begin
;		  (set! _vwide _recent-wide-view)
;		  (say "resetting wide-view...")))
;	  (else #f))))

    (define/public (update-wide-view)
      (let ((currview (sendf this pad view)))
	(if (wider-view? currview _recent-wide-view)
	    (set! _vwide currview))
	(set! _recent-wide-view currview)
	))

))

;---------------------------------------------

(define *last-zoom-targets* #f)

(define *closeup_scale* 0.85)
(define precompute-closeup-view (lambda (PAD object_list target-obj x y)
  (set! *last-zoom-targets* object_list)
  (let*((bbv (send PAD bbox))
        (bbo (bbunion-objects object_list))
	(oldz (send PAD getzoom))
        (newz (* oldz
		 (min (/ (* *closeup_scale* (bbwidth bbv))
			 (bbwidth bbo))
		      (/ (* *closeup_scale* (bbheight bbv))
			 (bbheight bbo)))))
	(target-view (append (bbcenter bbo) (list newz))))
    (cond ((not target-obj) target-view)
	  ((< newz (* 1.01 oldz)) ;already at or adequately closer than target(s)...
	     (if (or
		  (null? object_list)
		  (equal? object_list (list target-obj)))
		 ;only target; zoom in 2X toward x y (presumably drag origin)
		 (let ((closeup (list x y (* 1000 oldz))))
		   (set! *pull-range* (adjust-push/pull-range
				       (send PAD view)
				       closeup))
		   closeup)
		 ;else zoom to single target object
		 (precompute-closeup-view PAD (list target-obj) target-obj x y)))
	  (else target-view) ;else target normally
	  )
  
)))

(define *distant_scale* 0.7) ;used to be .25
(define precompute-distant-view (lambda (PAD object_list)
  (let*((bbv (send PAD bbox))
        (bbo (bbunion-objects object_list))
        (newz (* (send PAD getzoom)
                   (min (/ (* *distant_scale* (bbwidth bbv))
                           (bbwidth bbo))
                        (/ (* *distant_scale* (bbheight bbv))
                           (bbheight bbo)))))
       )
    (append (bbcenter bbo) (list newz))
  )
))

; little util function
(define (scale-view frac view) (list (car view) (cadr view) (* frac (caddr view))))

;---------------------------------------------
(define *default-push-pull-range* 100.0)
(define *pull-range* *default-push-pull-range*) ;sensitivity for zooming in
(define *push-range* *default-push-pull-range*) ;sensitivity for zooming out
;(define (set-push-pull-sensitivity s)
;  (set! *push-pull-range* (/ 100.0 s)))

(define (adjust-push/pull-range farview nearview)
  (let* ((zfac (/ (third nearview) (third farview)))
	 (newrange (* *default-push-pull-range* (log zfac))))
    (if (< newrange 25) (set! newrange 0))
    newrange))

(define (init-push-pull-motion argPAD e)
  (sendf argPAD evs sy0 (event-sy e)))

(define (update-push-pull-motion argPAD e)
  (let* ((dy (- (sendf argPAD evs sy0) (event-sy e)))
	 (-dy (- dy))
	 (denom (if (positive? dy) *pull-range* *push-range*)))
    ;(send argPAD warp-pointer 0 -dy)
    ;problem: this generates reverse mouse-motion pseudo-event, counteracting real event e
    ; so cache these -dy's and ignore each when it appears
	  (if (zero? denom)
	      0
	      (/ dy denom))))
	;returns frac used by update-lerp-zooming
  
;---------------------------------------------

; perhaps this should be a field of evs?
(define *at-zoom-limit?* #f)  ; either #f (if zoom at intermediate val)
                              ;        1  (if at close-up limit)
                              ;        0  (if at starting/midpoint limit)
                              ;        -1 (if at far-away limit)

(define (init-lerp-zooming argPAD e is-background? everything-list)
  (def pos (send argPAD view))
  (def closeup_set (event-obj e))
  (def ex (event-x e))
  (def ey (event-y e))

  (sendf argPAD evs v0 (send argPAD view))
  (sendf argPAD evs vfar (precompute-distant-view argPAD everything-list))
  (sendf argPAD evs dirflag #f)

  (set! *push-range* (adjust-push/pull-range (sendf argPAD evs vfar) pos))
  (set! *pull-range* *default-push-pull-range*)
  (set! *at-zoom-limit?* 0) ; start at midpoint

  (if (or (not is-background?)
	  (and (any-selected?)
	       (bbenclosedoron ex ey
			       (bbunion-objects (send argPAD selected)))))
      (begin ;object or within bbox of selected objs
	(set! closeup_set
	      (if (or is-background?
		      (send (event-obj e) selected?))
		  (send argPAD selected)
		  (list (get-top-group closeup_set))))
	(sendf argPAD evs vclose (precompute-closeup-view argPAD
							  closeup_set
							  (if is-background?
							      #f
							      (event-obj e))
							  ex ey))
	)
	  ;else background outside selection bbox:
          ; zoom in 2X closer to that spot
      (let ((closeup (list ex ey (* 1000 (send argPAD getzoom)))))
	(sendf argPAD evs vclose closeup)
	(set! *pull-range* (adjust-push/pull-range pos closeup)))
    )
  )

; span range is either near<-->midfar or far<-->midnear
(define *hit-far-zoomlimit-callbacks* null)  ;(lambda (pad event)...)
(define *hit-near-zoomlimit-callbacks* null)
(define *hit-midfar-zoomlimit-callbacks* null)
(define *hit-midnear-zoomlimit-callbacks* null)

(define access-hit-near-zoomlimit-callbacks    (callback-accessor-functions *hit-near-zoomlimit-callbacks*))
(define access-hit-far-zoomlimit-callbacks     (callback-accessor-functions *hit-far-zoomlimit-callbacks*))
(define access-hit-midnear-zoomlimit-callbacks (callback-accessor-functions *hit-midnear-zoomlimit-callbacks*))
(define access-hit-midfar-zoomlimit-callbacks  (callback-accessor-functions *hit-midfar-zoomlimit-callbacks*))
; Usage: create a new callback with e.g.
;(access-hit-near-zoomlimit-callbacks 'add (lambda (argPAD event) ...))
; Execute with
;(exec-any-callbacks cb-list argPAD event)

(define (update-lerp-zooming argPAD evnt frac . args)
  (def view0 (sendf argPAD evs v0))
  (def z0 (caddr view0))
  (def newz (* z0 (expt 10.0 frac)))
  (def view_closeup (sendf argPAD evs vclose))
  (def view_of_everything (sendf argPAD evs vfar))
  (def do-zoomlimit-cbs #f)

  (when (not (null? args))
    (set! view_closeup       (scale-view 5.0   view_closeup))
    (set! view_of_everything (scale-view 0.125 view_of_everything)))

  ; if direction flag is set, add bound at zero crossing
  (if (sendf argPAD evs dirflag)
    (if (= 1 (sendf argPAD evs dirflag))
      (when (< frac 0) (set! frac 0)
	    (unless (eq? *at-zoom-limit?* 0)
		    (set! *at-zoom-limit?* 0)
		    (set! do-zoomlimit-cbs *hit-midfar-zoomlimit-callbacks*)))
      ;else
      (when (> frac 0) (set! frac 0) 
	    (unless (eq? *at-zoom-limit?* 0)
		    (set! *at-zoom-limit?* 0)
		    (set! do-zoomlimit-cbs *hit-midnear-zoomlimit-callbacks*)))
      )
    ;else set direction flag
    (when (or (> frac 0.25) (< frac -0.25))
	  ;(apply adjust-push-pull-range (if (> frac 0)
	;				    (list view0 view_closeup)
	;				    (list view_of_everything view0)))
	  (sendf argPAD evs dirflag (sign frac)))) ;(if (> frac 0) 1 -1))))
  ;all conditions so far may fail...

  ; enforce hard bounds
  (when (> frac  1) (set! frac 1)
	(unless (eq? *at-zoom-limit?* 1)
		; 1st time hitting near-limit
		(set! *at-zoom-limit?* 1)
		(set! do-zoomlimit-cbs *hit-near-zoomlimit-callbacks*)))
  (when (< frac -1) (set! frac -1)
	(unless (eq? *at-zoom-limit?* -1)
		; 1st time hitting far-limit
		(set! *at-zoom-limit?* -1)
		(set! do-zoomlimit-cbs *hit-far-zoomlimit-callbacks*)))
  ;if not -1, 0 or 1, *at-zoom-limit?* --> intermediate (#f)
  (when (!= (round frac) frac) (set! *at-zoom-limit?* #f))

  (if (> frac 0)
    (send argPAD moveto (view_lerp (siso_lerp_mp frac)
                                   view0
                                   view_closeup))
    ;else
    (send argPAD moveto (view_lerp (siso_lerp_mp (- frac))
                                   view0
                                   view_of_everything))
    )

  (if do-zoomlimit-cbs
      (exec-any-callbacks do-zoomlimit-cbs argPAD evnt))
  )

