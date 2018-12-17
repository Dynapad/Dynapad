; Load dt-events.ss first.

(let ((grabbed-by-touch #f)
      (grabbed-at-x #f)
      (grabbed-at-y #f))
  (bind dynapad "dt-down"
    (lambda (pad event)
      (set! grabbed-by-touch (dt-event-touch-id event))
      (let* ((view (send dynapad view)))
        (set! grabbed-at-x (event-x event))
        (set! grabbed-at-y (event-y event)))))
  (bind dynapad "dt-move"
    (lambda (pad event)
      (when (equal? grabbed-by-touch (dt-event-touch-id event))
        (let* ((view (send dynapad view))
               (x (first view))
               (y (second view))
               (zoom (third view))
               (dx (- (event-x event) grabbed-at-x))
               (dy (- (event-y event) grabbed-at-y)))
          (send dynapad moveto (list (- x dx) (- y dy) zoom))))))
  (bind dynapad "dt-up"
    (lambda (pad event)
      (when (equal? grabbed-by-touch (dt-event-touch-id event))
        (set! grabbed-by-touch #f)))))
