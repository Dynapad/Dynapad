(require (lib "class.ss"))

(define broadcast-timer%
  (class timer%
    (init-field dynapad (default_interval 100))
    (field (prev_msec 0) (subscribed_actors ()))

    (define/override (start . args)
      (let ((interval (if (null? args) default_interval (car args))))
	(set! prev_msec (current-milliseconds))
	(super start interval)))

    (define (update)
      (let*
	((cur_msec (current-milliseconds))
	 (dtime (- cur_msec prev_msec)))

	 ; broadcast update message to all dynapad objects
	 (for-each 
	   (lambda (x) (send-actor x 'update cur_msec dtime))
	   (send dynapad objects))

	 ; send update message to all actors on the list
	 (for-each 
	   (lambda (x) (send x update cur_msec dtime))
	   subscribed_actors)

	(set! prev_msec cur_msec)))

    (define/public (subscribe actor)
      (if (method-in-interface? 'update (object-interface actor))
	  (set! subscribed_actors (append subscribed_actors (list actor)))))

    (define/public (unsubscribe actor)
      (set! subscribed_actors (remove actor subscribed_actors)))

    (super-instantiate(update #f #f))))


(define subscription-timer%
  (class timer%
    (init-field dynapad (default_interval 100))
    (init (update-fn #f))
    (field (prev_msec 0) (subscribed_dynaobjects ()) (subscribed_actors ()))
    (field (_num-subscribers 0))

    (define/override (start . args)
      (let ((interval (if (null? args) default_interval (car args))))
	(set! prev_msec (current-milliseconds))
	(super start interval)))

    (define (update)
      (let*
	((cur_msec (current-milliseconds))
	 (dtime (- cur_msec prev_msec)))

	 ; broadcast update message to all dynapad objects
	 (for-each 
	   (lambda (x) (send-actor x 'update cur_msec dtime))
	   subscribed_dynaobjects)

	 ; send update message to all actors on the list
	 (for-each 
	   (lambda (x) (send x update cur_msec dtime))
	   subscribed_actors)

	(set! prev_msec cur_msec)))

    (define/public (subscribe actor_or_padobject)
      ;(display "subscribing...")(newline)
      (+= _num-subscribers 1)
      (if (is-a? actor_or_padobject dynaobject%)
	(set! subscribed_dynaobjects
	  (append subscribed_dynaobjects (list actor_or_padobject)))
	(if (method-in-interface? 'update (object-interface actor_or_padobject))
	    (set! subscribed_actors
		  (append subscribed_actors (list actor_or_padobject))))))

    (define/public (subscribers) subscribed_actors)
    (define/public (num-subscribers) _num-subscribers)

    (define/public (unsubscribe actor_or_padobject)
       ;(display "UNsubscribing...")(newline)
      (let ((found #f))
	(if (is-a? actor_or_padobject dynaobject%)
	    (begin
	      (set! found (memq actor_or_padobject subscribed_dynaobjects))	       
	      (set! subscribed_dynaobjects (remq actor_or_padobject subscribed_dynaobjects)))
	    (begin
	      (set! found (memq actor_or_padobject subscribed_actors))
	      (set! subscribed_actors (remq actor_or_padobject subscribed_actors))))
	(if found
	    (-= _num-subscribers 1))))

    (if (not update-fn)
	(set! update-fn update))
    (super-instantiate (update-fn #f #f))))


(define auto-unsubscribe-timer%
  (class subscription-timer%
	 (init dynapad (interval 100))
	 (init (update-fn #f))
	 (inherit-field prev_msec subscribed_dynaobjects subscribed_actors)
	 
	 (define (update)
	   (let*
	       ((cur_msec (current-milliseconds))
		(dtime (- cur_msec prev_msec))
		(cancel-list null))
    
		; broadcast update message to all dynapad objects
	     (for-each 
	      (lambda (x) (if (eqv? (send-actor x 'update cur_msec dtime)
				    'unsubscribe)
			      (set! cancel-list (cons x cancel-list))))
	      subscribed_dynaobjects)
	     
		; send update message to all actors on the list
	     (for-each 
	      (lambda (x) (if (eqv? (send x update cur_msec dtime)
				    'unsubscribe)
			      (set! cancel-list (cons x cancel-list))))
	      subscribed_actors)

	     (for-each (lambda (it) (send this unsubscribe it)) cancel-list)
	     
	     (set! prev_msec cur_msec)))

;	 (define (proc) (update))

	 (if (not update-fn)
	     (set! update-fn update))
	 (super-instantiate (dynapad interval update-fn))))
