(load-relative "dt.rkt")
(load-relative "dt-events.rkt")

(define *dt-mouse-button* "1")

(define (dt-equiv-event-type dt-event-type)
  (cond
    ((equal? dt-event-type "dt-down")
     (string-append "Button-" *dt-mouse-button*))
    ((equal? dt-event-type "dt-move")
     (string-append "B" *dt-mouse-button* "-Motion"))
    ((equal? dt-event-type "dt-up")
     (string-append "ButtonRelease-" *dt-mouse-button*))))

(define (dt-like-mouse pad event)
  (let* ((mod (send pad modifier 'get))
         (mouse-event (dt-equiv-event-type (event-type event)))
         (mod-event-string (string-append "<" mod "-" mouse-event ">"))
         (event-string (string-append "<" mouse-event ">"))
         (procs (append (bind pad mod-event-string) (bind pad event-string))))
    (dt-apply-until-false procs pad event)))

(bind dynapad "dt-down" dt-like-mouse)
(bind dynapad "dt-move" dt-like-mouse)
(bind dynapad "dt-up" dt-like-mouse)
