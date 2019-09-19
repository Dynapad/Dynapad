(dynaload "layout.ss")
(dynaload "spirals.ss")

(define web-pile%
  (class event-pile%
    (init __obj __username __domain)

    (inherit-field _mysql)

    (field (_domain     __domain))
    (field (_launcher  (make-object launcher%)))

    (super-instantiate (__obj __username))

    (send this bindbox (make-object web-region-bindbox%))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Override the events method
    (define/override (events)
      (send _mysql web-domain-user-today
            (send _mysql get-domain-id _domain)
            (send this user-id)))
    )
  )


;(define wpile (make-pile web-pile% "etienne" (list "salon.com")))
;(define bindb (make-object web-region-bindbox%))

;(send bindb region wpile)
;(send wpile bindbox bindb)

