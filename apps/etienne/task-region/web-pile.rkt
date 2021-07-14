(load "/home/hci/dynapad/arrange.rkt")
(load "/home/hci/dynapad/piles.rkt")
(load "/home/hci/dynapad/apps/mysql/mysql.rkt")
(load "/home/etienne/dynapad/apps/task-region/mysql-history.rkt")
(load "/home/etienne/dynapad/apps/task-region/clock.rkt")

(define (make-web-pile domain)
  (define p (make-object polygon% dynapad '(-20 0 0 -20 20 0 0 20)))
  (regionize p (list web-domain-pile% dynapad domain) frame-container%))

(define web-domain-pile%
  (class fusing-pile%
    (init __obj __dynaptr __domain)

    (field (_formation      __obj)
           (_dynaptr   __dynaptr)
           (_domain    __domain))

    (super-instantiate (__obj))

    (field (_id     0)
           (_events (list))
           (_icons  (list))
           (_poly   (send _formation frame)))

    (field (_clock (make-object clock%)))
    (field (_mysql (make-object web-mysql%)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; instantiation
    ;;
    ;; Get the domain's id from the database
    (set! _id (send _mysql get-domain-id _domain))

    ;(send this margin 100)  ;shouldn't need this

    (send _poly fill "#999999")
    (send _poly penwidth 1)
    (send _poly bbox '(-100 -100 200 200))
    (send _poly transparency 1)
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Accessor methods
    (define/public domain
      (case-lambda
        (()  _domain)
        ((d)
         (set! _domain d)
         (set! _id (send _mysql get-domain-id _domain))
         (refresh-pile))))

    (define/public (domain-id) _id)
    (define/public (events)    _events)
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Refresh the pile display
    (define/public (refresh-pile)
      ;; Fetch all event ids from the database.
      (define _all (send _mysql get-domain-events-between
                         (send _clock today) (send _clock tomorrow) _id))

      ;; Update _icons by adding only new _events
      (for-each
       (lambda (e)
         (if (not (member e _events))  ;= (unless (member e _events) ...)
             (let ((p (format "/home/etienne/.dynalog/image_depot/~a.jpg" e)))
               (if (file-exists? p)
                   (let ((i (make-object image% _dynaptr p)))
                     (push! e _events)
                     (push! i _icons))))))
       _all)

      ;; *Objs should be moved to correct position before adding to pile*

      ;; Arrange the images into a spiral
      (arrange-objs-in-pile _icons (send _poly bbox) 0.9)  ;arrange-...-spiral looks better

      ;; Now refresh contents to be _icons
      (send this contents _icons)
      ;; Tell the region to finish -- marking the end of a batch
      ;;  to tell the pile to redraw zone
      (send this finish)

      ;; We're done
      (say "refresh-pile done"))
    )
  )

; make the pile: (define pile (make-web-pile "www.salon.com"))
; the region actor: (define reg (get-actor-named pile region-actor-name))
; refresh: (send reg refresh-pile)
