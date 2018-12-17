(define recent-event-pile%
  (class event-pile%
    (init __obj __username)

    (inherit-field _mysql)

    (field (_number 10))

    (super-instantiate (__obj __username))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define/override (events)
      (define q (format "id from event order by id desc limit 0,~a" _number))
      (define r (send _mysql select q))
      (say r)
      (map car r))

    (define/public number
      (case-lambda
       (()  _number)
       ((n) (set! _number n))))
    )
  )

(define shared-pile%
  (class event-pile%
    (init __obj __username)

    (inherit-field _mysql)

    (field (_pile-id 1))

    (say "about to instantiate")

    (super-instantiate (__obj __username))

    (say "instantiated")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Override the events method
    (define/override (events)
      (say "in events")
      (map car (send _mysql select
	(format "event from event_shared_pile where shared_pile=~a" _pile-id))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Override the enter and leave methods
;    (define/override (enter-object obj)
;      (define event (string->number (get-object-keyval obj 'event -1)))
;      (if (> event 0)
;          (send _mysql shared-pile-add-event _pile-id event)))
;
;    (define/override (leave-object obj)
;      (define event (string->number (get-object-keyval obj 'event -1)))
;      (if (> event 0)
;	  (send _mysql shared-pile-remove-event _pile-id event)))
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    )
)
