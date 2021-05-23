(load "dt.rkt")
(load "dt-actor.rkt")
(load "dt-events.rkt")
(load "dt-drag.rkt")
(load "dt-drag-bg.rkt")

; a layer for tools to live on above the drawing surface
(define *dt-tool-layer* (make-object layer% dynapad "dt-tool"))

(define (dt-make-filltool object color)
  (let ((drag-actor (make-object dt-actor%)))
    (send drag-actor down-function
          (lambda (touch)
            (let ((objects (objects-under-touch touch)))
              (when (>= (length objects) 4)
                (send (fourth (objects-under-touch touch)) fill color)))))
    (send drag-actor attach-to object)
    (send object layer *dt-tool-layer*)
    (send object transparency 0.5)))

(define (dt-make-zoomintool object)
  (let ((zfac 1.05)
        (animtime 0)
        (drag-actor (make-object dt-actor%)))
    (send drag-actor down-function
          (lambda (touch)
            (send dynapad zoom zfac animtime (dt-x touch) (dt-y touch))))
    (send drag-actor move-function
          (lambda (old-touch new-touch)
            (send dynapad zoom zfac animtime (dt-x new-touch) (dt-y new-touch))))
    (send drag-actor attach-to object)
    (send object layer *dt-tool-layer*)))

(define (dt-make-zoomouttool object)
  (let ((zfac 0.95)
        (animtime 0)
        (drag-actor (make-object dt-actor%)))
    (send drag-actor down-function
          (lambda (touch)
            (send dynapad zoom zfac animtime (dt-x touch) (dt-y touch))))
    (send drag-actor move-function
          (lambda (old-touch new-touch)
            (send dynapad zoom zfac animtime (dt-x new-touch) (dt-y new-touch))))
    (send drag-actor attach-to object)
    (send object layer *dt-tool-layer*)))
