
(define (make-pile type username arguments)
  (define p (make-object polygon% dynapad '(-100 0 0 -100 100 0 0 100)))
  (define o (regionize p (append (list type username) arguments) frame-container%))
  (get-actor-named o region-actor-name))

(define event-pile%
  (class fusing-pile%
    (init __obj __username)

    (field (_formation __obj)
	   (_username  (if __username __username (getenv "USER"))))

    (super-instantiate (__obj))

    (field (_poly    (send _formation frame)))
    (field (_bindbox (list)))

    (field (_mysql (make-object mysql-history%)))
    (field (_timer (make-object timer%
				(lambda () (send this refresh-pile)) 60000)))

    (field (_user-id     (send _mysql get-user-id _username)))
    (field (_image-depot "/home/Share/.dynalog/image_depot"))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; instantiation
    ;;
    (send _poly fill "#999999")
    (send _poly penwidth 1)
    (send _poly bbox '(-100 -100 200 200))
    (send _poly transparency 1)
    (send _poly pen "white")

    ;; By default fill the with event objects
    (send this refresh-event-objects)

    (send _poly bbox '(-1000 -1000 1000 1000))
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Start and stop the timer
    (define/public (stop)  (send _timer stop))
    (define/public (start) (send _timer start))
    ;; User-id and polygon methods
    (define/public (user-id) _user-id)
    (define/public (poly)    _poly)
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Bindbox methods
    (define/public bindbox
      (case-lambda
       (()  _bindbox)
       ((b)
	(set! _bindbox b)
	(for-each
	 (lambda (object)
	   (send _bindbox set-bindings object))
	 (send this contents)))))
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Event Object methods
    (define/public (event-list)
      (map (lambda (o) (get-object-keyval o 'event -1)) (send this contents)))
    ;;
    (define/public (events) ())
    ;;
    ;; Refresh event objects in pile
    (define/public (refresh-event-objects)
      (define all-events (send this events))
      (define old-events (diff (event-list) all-events))
      (define new-events (diff all-events (event-list)))

      ;; Remove the old events and their objects
      (for-each
       (lambda (object)
	 (define event-id (get-object-keyval object 'event -1))
	 (if (member event-id old-events)
	     (send this remove object)))
       (send this contents))

      ;; Add the new events and their objects
      (for-each
       (lambda (event-string)
	 (define event-id (string->number event-string))
	 (define path     (format "~a/~a.jpg" _image-depot event-id))
	 (define image    (make-object image% dynapad path))
	 (if (file-exists? path)
	     (send this receive image)
	     (say "screenshot not found:" event-id)))
       new-events)

      (say "contents post receive: " (send this contents))
      (for-each
       (lambda (o)
	 (send o scale 0.1))
       (send this contents))

      ;; arrange the contents into a spiral
      (arrange-in-spiral-onto-object-no-scaling (send this contents) _poly)

      ;; Tell the region to finish -- marking the end of a batch
      ;;  to tell the pile to redraw zone
      (send this finish)
      )
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Delete method override
    (rename (super-delete delete))
    (define/override (delete)
      (send this stop)
      (super-delete))
    )
  )

; bind to <Enter> and <Leave> of the region actor (web-domain-pile%)
;
; make the pile: (define pile (make-web-pile "www.salon.com"))
; the region actor: (define reg (get-actor-named pile region-actor-name))
; refresh: (send reg refresh-pile)
