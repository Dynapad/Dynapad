(dynaload "apps/mysql/mysql.ss")
(load "/home/etienne/dynapad/apps/task-region/etienne.ss")

(define mysql-default%
  (class mysql%
    (init __host __user __pass __database)
    (inherit connect database select)

    (super-instantiate())
    (connect  __host __user __pass)
    (database __database)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods
    (define/public (select-one-value query . default)
      (define result (select query))
      (cond ((null? default) (set! default #f))
            ((list? default) (set! default (car default))))
      (if (not result)
          default
          (caar result)))
    )
  )

(define web-mysql%
  (class mysql-default%
    (inherit select select-one-value)

    (super-instantiate("tex" "etienne" "hi.etienne" "history"))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods
    (define/public (get-domain-id domain)
      (select-one-value
       (format " id from z_web_domain where domain=\"~a\"" domain)))

    (define/public (get-domain-events-between begin end domain-id)
      (define q (format "event from z_web_domain_event, event where z_web_domain_event.event=event.id and time>~a and time<~a and domain=~a" begin end domain-id))
      (map car (select q)))

    )
  )

(define mysql-history%
  (class mysql%
    (inherit connect database select)

    (super-instantiate())
    (connect "tex" "etienne" "hi.etienne")
    (database "history")

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Public methods

    (define/public (web-pages-today)
      (define d (seconds->date (current-seconds)))
      (select
       (format "id from event where time>\"~a\" and time<\"~a\" and type>200"
               (format "~a-~a-~a 00:00:00" (date-year d) (date-month d) (date-day d))
               (format "~a-~a-~a 23:59:59" (date-year d) (date-month d) (date-day d)))))

    (define/public (event-meta event meta)
      (define r
        (select
         (format
          "value from event_meta where event=~a and meta=~a"
          event meta)))
      (if (null? r)
          #f
          (caar r)))

    (define/public (domain-title domain)
      (let ((event (select-one-value
                    (format
                     "event from event_meta where value=\"http://~a/\" order by id desc"
                     domain)
                    #f)))
        (if event
            (select-one-value
             (format "value from event_meta where meta=3 and event=~a" event))
            domain)))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Private methods
    )
  )

