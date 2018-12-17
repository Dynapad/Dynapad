(dynaload "actor.ss")
(dynaload "actortimer.ss")


; there are several motion timers with diff. freqs;
;  subscribe motion-actors to appropriate freq timer
;  depending on motion speed
(define *timer-periods* (list 500 400 300 200 175 150 125 100 90 80 70 60 50 40))
(define *min-tick* (apply min *timer-periods*))
(define *max-tick* (apply max *timer-periods*))
(define *motion-timers*
  (map (lambda (t)
	 (let ((timer (make-object auto-unsubscribe-timer% dynapad t)))
	   (send timer start)
	   (list t timer)))
       *timer-periods*))

(define (n-factor n)
  (max (/ n 4) 1))

(define *recount-timer* (make-object subscription-timer% dynapad 500))
(send *recount-timer* start)

(define subscriber-counter%
  (class object%
    (super-instantiate ())
    (field (_total-subscribers 0))

    (define/public (total-subscribers) _total-subscribers)
    (send *recount-timer* subscribe this)

    (define/public (update abstime dtime)
      (set! _total-subscribers 0)
      (foreach (map cadr *motion-timers*)
	       (lambda (tmr) (+= _total-subscribers (send tmr num-subscribers))))
      ;(say _total-subscribers)
      )
))

(define *recounter* (make-object subscriber-counter%))

(define (choose-motion-timer pad num howfar howlong)
;chooses an appropriate timer based on number of other moving objs,
; howfar and howlong to target, and z-view of pad
  (let* ((pixelsize (/ 1 (caddr (send pad view))))
	 (idealtick (/ (* howlong pixelsize (n-factor num)) howfar)))
    (set! idealtick (min idealtick *max-tick*))
    (let ((timer-pair (first-valid *motion-timers*
			       (lambda (pair) (<= (car pair) idealtick)))))
      (if (not timer-pair)
        (set! timer-pair (car (reverse *motion-timers*))) ; ron's hack fix
        (set! timer-pair (caar timer-pair)))
      (and timer-pair
	   ;(> howfar pixelsize) ;if less than 1 pixel; don't bother animating
	   (cadr timer-pair)))))

(define homing-actor%
; (make-object homing-actor% obj (list x y))  OR
; (make-object homing-actor% obj [lambda () ... (list x y)])  OR
; (make-object homing-actor% obj (list xy) duration) ...
  (class named-actor%
    (init _obj _target (_duration 1000) (_del-cbk #f))
    (field (_target-x #f) (_target-y #f))
    (field (_target-fn #f))
    (field (_duedate 0))
    (field (_tick #f) (_timer #f))
    (super-instantiate ())

    (cond ((procedure? _target) (set! _target-fn _target))
	  ((list? _target)      (set! _target-x (car _target))
	                        (set! _target-y (cadr _target)))
	  (else (error "homing-actor% requires list or lambda target")))
    (send this attach-to _obj 'homer)

    (if _del-cbk
	(send this delete-callbacks 'add _del-cbk))

    (define (remaining-distance) ;returns greater of x or y dist to target
      (when _target-fn  ;handle moving target
	  (let ((target-xy (_target-fn)))
	    (set! _target-x (car target-xy))
	    (set! _target-y (cadr target-xy))))
      (let ((here (send (send this object) xy)))
	(list (- _target-x (car here))
	      (- _target-y (cadr here)))))


    (set! _duedate (+ (current-milliseconds) _duration))

    (define/public duedate (get/set _duedate))

    (define/public (go)
      (send _timer subscribe this))
    (define/public (stop)
      (if _timer
	  (send _timer unsubscribe this)))

    (define (resubscribe)
      (send this stop) ;unsubscribe to old
      (set! _timer
	    (choose-motion-timer dynapad (send *recounter* total-subscribers)
				 (apply max (remaining-distance)) _duration))
      (when _timer
	    (set! _tick (send _timer interval))
	    (send this go)))

    (define/override (delete)
      (stop)
      (super delete))

    (define/public target
      (case-lambda
       (() (list _target-x _target-y)) ;return momentary target coords
       ((targ) (if (list? targ)
		  (target (car targ) (cadr targ)) ;fixed coord pair
		  (set! _target-fn targ)))        ;else lambda
       ((x y) (set! _target-fn #f)
	      (set! _target-x x)
	      (set! _target-y y))))

    (define/public (update abstime dtime)
      (resubscribe)  ;possibly change timers
      (let* ((timeleft (- _duedate abstime)))
	(if (positive? timeleft)
	    (let* ((rawfraction (/ dtime timeleft))
		   (fraction (if (> rawfraction 1)
				 1
				 rawfraction))
		   (dist (remaining-distance))
		   (dx (* fraction (car dist)))
		   (dy (* fraction (cadr dist))))
	      (send (send this object) slide dx dy))
	    ; else jump directly there
	    (begin
	      (send (send this object) xy _target-x _target-y)
	      (send this delete)))))

    (resubscribe)
))

(define animate-to
  (case-lambda
   ((obj target) (animate-to obj target 1000)) ;1000ms by default
   ((obj target howlong) (animate-to obj target howlong homing-actor%))
   ((obj target howlong homer-type . args)
    (let ((homer (get-actor-named obj 'homer)))
      (if homer (send homer delete))
      (set! homer (apply make-object homer-type obj target howlong args))
      homer))))

#|EXAMPLES:
 (animate-to obj '(0 0))        take 1 sec
 (animate-to obj '(0 0) 500)    take 500 ms
 (animate-to obj
   (lambda () (let ((xy (send target position)))
		(list (+ (car xy) xoffset)
		      (+ (cadr xy) yoffset)))))
|#

(define chaperone%
  (class object%
    ;performs lambda _final-action after receiving message
    ; (done obj) for each obj in _objs
    (init-field _objs)
    (init-field _final-action)
    (super-instantiate ())

    (define (done-yet?)
      (when (and _final-action (null? _objs))
	    (_final-action)
	    (set! _final-action #f)))

    (define/public (done obj)
      (set! _objs (remq obj _objs))
      (done-yet?))

    (done-yet?)

))

(define animate-batch-then-do
  (case-lambda
;   ((objs tgts) (animate-batch-blocking objs tgts 1000))
   ((objs tgts howlong do-what)
    (let* ((chap (make-object chaperone% objs do-what))
	   (cbk  (lambda (homer) (send chap done (send homer object)))))
      (for-each (lambda (o t)
		   (animate-to o t howlong homing-actor% cbk))
			   objs tgts)))
   ))