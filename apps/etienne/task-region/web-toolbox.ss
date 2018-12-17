
(define web-toolbox%
  (class object%
    (init __dynaptr)
    (init __task-region)

    (field (_dynaptr     __dynaptr)
	   (_task-region __task-region)
	   (_task-area   (send __task-region area))
	   (_task-panel  (send __task-region panel))
	   (_history     (make-object mysql-history%)))

    (field (_working-events '()))
    (field (_working-icons  '()))

    (super-instantiate ())

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Give our task region its buttons
    (define b (make-object base-button% dynapad "/home/etienne/dynalog/images/star.jpg"))
    (send b execute (lambda () (send this organize-by-domain)))
    (send _task-panel add b)

    (set! b (make-object base-button% dynapad "/home/etienne/dynalog/images/star.jpg"))
    (send b execute (lambda () (send this build-from-query "today")))
    (send _task-panel add b)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Accessor methods
    (define/public (working-events)
      _working-events)

    (define/public (working-icons)
      _working-icons)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Helper methods
    (define/public (maintenance)
      (for-each
       (lambda (icon)
	 (if (send icon deleted?)
	     (set! _working-icons
		   (remq icon _working-icons))))
       _working-icons))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Builds an icon given an event with all meta data attached.
    (define/public (build-working-icon event)
      (define event-url   (send _history event-meta event 2))
      (define event-title (send _history event-meta event 3))
      (define event-path  (format "/home/etienne/.dynalog/image_depot/~a.jpg" event))
      (define event-icon  (make-object icon% _dynaptr event-path event-title))

      ;; attach some information to the icon
      (set-object-keyval event-icon 'event event)
      (set-object-keyval event-icon 'url   event-url)
      (set-object-keyval event-icon 'title event-title)

      ;; make the icon take group events
      (send event-icon takegroupevents #t)

      ;; add it to our working icons
      (push! event-icon _working-icons)

      ;; tell the region it has a new icon
      (send _task-region add event-icon))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Builds all the working icons from the working events. 
    (define/public (build-working-icons)
      ;; remove our old working icons
      (for-each
       (lambda (icon)
	 (send _task-region remove icon)
	 (send icon delete))
       _working-icons)
      (set! _working-icons '())

      ;; build our new working events
      (for-each
       (lambda (event)
	 (build-working-icon event))
       _working-events))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Build the working events  (sets them)
    (define/public (build-working-events events)
      ;; remove our old working events
      (set! _working-events '())

      ;; set our new events
      (set! _working-events events))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Queries
    (define/public (query-today)
      (format
       "id from event, event_meta where event.id=event_meta.event and time>\"2003-07-17 00:00:00\" and time<NOW() and event_meta.meta=3"))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Build from query: builds the events from query
    ;;  and then calls our placement function
    (define/public (build-from-query keyword)
      (define events '())
      (define query   "")
      (define result '())

      ;; get the right query
      (cond ((string=? keyword "today")
	     (set! query (query-today))))

      ;; query the db
      (set! result (send _history select query))

      ;; build event list
      (for-each
       (lambda (row)
	 (push! (car row) events))
       result)

      ;; build the events and icons
      (build-working-events events)
      (build-working-icons)
      (arrange-in-grid-onto-object _working-icons _task-area 1 1))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Standard organization
    (define/public (organize-by-time)
      (arrange-in-grid-onto-object _working-icons _task-area 1 1))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Organize our working icons by domain
    (define/public (organize-by-domain)
      (define domains '())
      (define labels  '())
      (define rows 0)
      (define cols 0)
      (define rowh 0)
      (define colw 0)

      ;; Organize our working icons by domain internally
      (for-each
       (lambda (icon)
	 (define url           (get-object-keyval icon    'url))
	 (define event         (get-object-keyval icon    'event))
	 (define domain        (cadr (regexp-match domain-from-url url)))
	 (define domain-symbol (string->symbol domain))
	 (define domain-list   (get-alist-keyval domains domain-symbol '()))

	 (push! icon domain-list)
	 (set! domains (set-alist-keyval domains domain-symbol domain-list)))
       _working-icons)

      ;; number of rows and cols
      (set! cols (ceiling (sqrt (length domains))))
      (set! colw (/ (send _task-area width)  cols))
      (set! rows (ceiling (/ (length domains) cols)))
      (set! rowh (/ (send _task-area height) rows))

      ;; Build the text objects as labeled-groups with
      ;;  the icons as members
      (for-each 
       (lambda (domain-list)
	 (define domain  (symbol->string (car domain-list)))
	 (define icons   (cadr domain-list))
	 (define rectobj (make-object rect% _dynaptr (list 0 0 colw rowh)))
	 (define lgroup  (make-object labeled-group% _dynaptr rectobj))

	 (say "domain: " domain " is " (/ (length icons) (length _working-icons)) "%")

	 (push! lgroup labels)

	 (for-each
	  (lambda (icon)
	    (send _task-region remove icon)
	    (send lgroup add icon))
	  icons)

	 ;; add the lgroup to the region
	 (send _task-region add lgroup)

	 ;; have the lgroup take group events
	 (send lgroup takegroupevents #t)

	 ;; arrange icons behind the label
	 (arrange-in-grid-onto-object icons lgroup 1 1))
       domains)

      ;; arrange the labels in the workspace
      (let ((x (car (send _task-area bbox)))
	    (y (cadr (send _task-area bbox)))
	    (r 0)
	    (c 0))
	(for-each
	 (lambda (g)
	   (send g bbox (list (+ x (* c colw))      (+ y (* r rowh))
			      (+ x colw (* c colw)) (+ y rowh (* r rowh))))
	   (if (>= c (- cols 1))
	       (begin
		 (set! c 0)
		 (set! r (+ r 1)))
	       (set! c (+ c 1))))
	 labels))

      )
    )
  )

(define domain-from-url (regexp "^[hftp]+://([1-9a-zA-Z\\-\\.]+)/?"))

#|
(load "/home/etienne/dynapad/apps/task-region/labeled-group.ss")
(load "/home/etienne/dynapad/apps/task-region/task-region.ss")
(define a (make-object task-region% dynapad))
(define b (make-object web-region-toolbox% dynapad a))
(send b build-from-query "today")
(send b organize-by-domain)


|#
