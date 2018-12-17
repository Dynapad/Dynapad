; Load dt.ss first.

; Make an event structure subtype for DiamondTouch events.
(define-struct (dt-event event) (bbox touch-id user-id table-x table-y is-flow mouse-button))

(define *dt-prev-events* null)
(define *double-tap-ms* 400)
(define *double-tap-proximity* 3)
(define *dt-mouse-button* "1")
(define *dt-mouse-touch-id* #f)

; the main event-processing loop
(define (dt-read-and-interpret-touches)
  (let ((events (map make-dt-event-from-touch (dt-touches))))
    (for-each dt-interpret-event events)
    (for-each dt-touch-disappeared *dt-prev-events*)
    (for-each dt-mouse-unlock *dt-prev-events*)
    (set! *dt-prev-events* events)))

(define (make-dt-event-from-touch touch)
  (let* ((table-x (dt-x touch))
         (table-y (dt-y touch))
         (xy (table->dynapad (list table-x table-y)))
         (x (first xy))
         (y (second xy))
         (type #f)  ; can't determine this here
         (key #f)
         (obj #f)  ; can't determine this here
         (sxy (screen-xy x y))
         (sx (first sxy))
         (sy (second sxy))
         (touch-id (dt-touch-id touch))
         (user-id (dt-user-id touch))
         (bbox (table->dynapad (dt-bbox touch)))
         (is-flow #f)
         (mouse-button #f))
    (make-dt-event x y type key obj sx sy
                   bbox touch-id user-id table-x table-y is-flow mouse-button)))

; Initialize supplementary values in the dt-event structure,
; get the appropriate bindings, then apply them.
(define (dt-interpret-event event)
  (let* ((prev-event (event-with-id (dt-event-touch-id event) *dt-prev-events*)))
    (if prev-event
      (dt-initialize-event-from event prev-event)
      (dt-initialize-new-event event))
    (dt-dispatch-event event)))

(define (dt-touch-disappeared event)
  (set-event-type! event "dt-up")
  (dt-dispatch-event event))

; set the values in a dt-event structure with no preceding event
(define (dt-initialize-new-event event)
  (set-event-obj! event (choose-event-recipient event))
  (set-dt-event-mouse-button! event *dt-mouse-button*)
  (if (dt-double? event)
    (begin
      (set-event-type! event "dt-double")
      (set-dt-event-is-flow! event #t))
    (begin
      (set-event-type! event "dt-down")
      (set-dt-event-is-flow! event #f))))

; set the values in a dt-event structure with a preceding event
(define (dt-initialize-event-from event prev-event)
  (set-dt-event-mouse-button! event (dt-event-mouse-button prev-event))
  (set-dt-event-is-flow! event (dt-event-is-flow prev-event))
  (if (dt-event-is-flow event)
    (set-event-obj! event (choose-event-recipient event))
    (set-event-obj! event (event-obj prev-event)))
  (set-event-type! event "dt-move")
  (set! *dt-prev-events* (remove prev-event *dt-prev-events*)))

(define (dt-dispatch-event event)
  (dt-apply-until-false (dt-event-bindings event) dynapad event))

; get bindings to handle event in this order:
(define (dt-event-bindings event)
  (let* ((obj (event-obj event))
         (bullshit (and (not (eq? dynapad obj)) (send obj deleted?)))
         (dt-type (event-type event))
         (mouse-type (dt-mouse-event-type event))
         (mod (send dynapad modifier 'get))
         (modifier-event-string (string-append "<" mod "-" mouse-type ">"))
         (generic-event-string (string-append "<" mouse-type ">")))
    (append
      (if bullshit () (bind obj dt-type))  ; dt-event bindings on object
      (list (lambda (d e) (dt-mouse-lock e)))
      (if bullshit () (bind obj modifier-event-string))  ; modifier mouse bindings on object
      (if bullshit () (bind obj generic-event-string))  ; generic mouse bindings on object
      (if (eq? obj dynapad) ()
        (append
          (bind dynapad dt-type)  ; dt-event bindings on dynapad
          (bind dynapad modifier-event-string)  ; modifier mouse bindings on dynapad
          (bind dynapad generic-event-string))))))  ; generic mouse bindings on dynapad

; Ensure at most one touch-id at a time is handled by mouse bindings.
(define (dt-mouse-lock event)
  (let ((touch-id (dt-event-touch-id event)))
    (if *dt-mouse-touch-id*
      (eq? touch-id *dt-mouse-touch-id*)
      (set! *dt-mouse-touch-id* touch-id))))

(define (dt-mouse-unlock event)
  (when (eq? (dt-event-touch-id event) *dt-mouse-touch-id*)
    (set! *dt-mouse-touch-id* #f)))

(define (dt-mouse-event-type event)
  (let* ((type (event-type event))
         (button (dt-event-mouse-button event)))
    (cond
      ((equal? type "dt-double")
       (string-append "Double-Button-" button))
      ((equal? type "dt-down")
       (string-append "Button-" button))
      ((equal? type "dt-move")
       (string-append "B" button "-Motion"))
      ((equal? type "dt-up")
       (string-append "ButtonRelease-" button)))))

(define (dt-apply-until-false procs pad event)
  (when (not (null? procs))
    (and ((first procs) pad event)
         (dt-apply-until-false (rest procs) pad event))))

(define (event-with-id touch-id events)
  (if (null? events) #f
    (let* ((first-event (first events))
           (first-touch-id (dt-event-touch-id first-event)))
      (if (equal? touch-id first-touch-id) first-event
        (event-with-id touch-id (rest events))))))

(define (choose-event-recipient event)
  (let ((obj (top-visible-object-at-event event)))
    (if (eq? obj dynapad)
      dynapad
      (get-top-group obj))))

(define (top-visible-object-at-event event)
  (send dynapad pick (list (event-x event) (event-y event))))
; (top-visible-object-under-point (list (event-x event) (event-y event))))

(define (top-visible-object-under-point xy)
  (let ((objs (visible-objects-under-point xy)))
    (if (null? objs) #f (first objs))))

(define (visible-objects-under-point xy)
  (reverse
    (filter visible?
            (send dynapad find 'groupmembers 'overlapping (append xy xy)))))

(define (visible? obj)
  (and (not (zero? (send obj transparency)))
       (send (send obj layer) visible)
       (below-maxsize? obj)
       (above-minsize? obj)))

; Resist the urge to unify these! This is part of a big nasty kludge.
(define (below-maxsize? obj)
  (let* ((bbox (send obj bbox))
         (sw (screen-width bbox))
         (sh (screen-height bbox)))
    (< (max sw sh) (first (send obj maxsize)))))

(define (above-minsize? obj)
  (let* ((bbox (send obj bbox))
         (sw (screen-width bbox))
         (sh (screen-height bbox)))
    (> (max sw sh) (first (send obj minsize)))))


; returns #t if the event should be a dt-double, #f otherwise
(define dt-double?
  (let ((last-ms 0) (last-table-x 0) (last-table-y 0))
    (lambda (event)
      (let ((ms (current-milliseconds))
            (table-x (dt-event-table-x event))
            (table-y (dt-event-table-y event)))
        (if (and (<= (- ms last-ms) *double-tap-ms*)
                 (<= (abs (- table-x last-table-x)) *double-tap-proximity*)
                 (<= (abs (- table-y last-table-y)) *double-tap-proximity*))
          (begin
            (set! last-ms 0)
            #t)
          (begin
            (set! last-ms ms)
            (set! last-table-x table-x)
            (set! last-table-y table-y)
            #f))))))

(define *dt-timer* (make-object timer% dt-read-and-interpret-touches))
(send *dt-timer* start 33)
