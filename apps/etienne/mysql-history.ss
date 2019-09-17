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

    (define/public (select-list query)
      (map car (select query)))

    )
  )

(define mysql-history%
  (class mysql-default%
    (inherit select select-one-value select-list)
    (super-instantiate("tex" "etienne" "hi.etienne" "history"))

    (field (_clock (make-object clock%)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Meta/event methods
    (define/public (get-meta-value event meta)
      (select-one-value
       (format
        "value from meta_event_value where meta=~a and event=~a" meta event)))

    (define/public (get-events-from-meta-value value)
      (map car (select
                (format "event from meta_event_value where value=\"~a\"" value))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Time methods
    (define/public (time-diff event)
      (select-one-value
       (format
        "round((unix_timestamp(NOW())-time)/60) from event where id=~a" event)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; User methods
    (define/public (get-user-id username)
      (select-one-value
       (format "id from username where username=\"~a\"" username) -1))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Web methods
    ;;
    ;; domain-related
    (define/public (get-domain-id domain)
      (select-one-value
       (format " id from web_domain where domain=\"~a\"" domain) -1))

    (define/public (get-domain-title domain-id)
      (define q (format "event from web_event where domain=~a" domain-id))
      (define e (car (select-list q)))
      (if e (get-meta-value e 3) ""))

    ;; global-user related
    (define/public (web-between begin end)
      (map car (select (format "event from web_event, event where web_event.event=event.id and time>~a and time<~a" begin end))))

    (define/public (web-today)
      (web-between (send _clock today) (send _clock tomorrow)))

    ;; user-related
    (define/public (web-user-between user-id begin end)
      (map car (select (format "event from web_event, event where web_event.event=event.id and username=~a and time>~a and time<~a" user-id begin end))))

    (define/public (web-domain-user-between domain-id user-id begin end)
      (map car (select (format "event from web_event, event where web_event.event=event.id and domain=~a and username=~a and time>~a and time<~a" domain-id user-id begin end))))

    (define/public (web-user-today user-id)
      (web-user-betweeen user-id (send _clock today) (send _clock tomorrow)))

    (define/public (web-domain-user-today domain-id user-id)
      (web-domain-user-between domain-id user-id (send _clock today) (send _clock tomorrow)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Shared-pile-related
    (define/public (shared-pile-add-event pile-id event-id)
      (send this insert (format "into event_shared_pile values(~a, ~a)" pile-id event-id)))

    (define/public (shared-pile-remove-event pile-id event-id)
      (send this delete (format "from event_shared_pile where shared_pile=~a and event=~a" pile-id event-id)))

    )
  )

