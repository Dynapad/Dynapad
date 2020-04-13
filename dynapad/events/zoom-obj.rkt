#lang racket/base
(require (only-in racket/class send make-object)
         (only-in dynapad/pad-state
                  set-currentPAD!
                  event-x
                  event-y
                  )
         (only-in dynapad/misc/misc
                  sendf)
         (only-in dynapad/misc/user-preferences
                  *cowboy-zoom-on-objects*
                  )
         (only-in dynapad/events/zoom-classic
                  zoomtimer%)
         )

(provide Select-Zoom-In-lambda
         Select-Zoom-In-Stop-lambda
         Select-Zoom-Out-lambda
         Select-Zoom-Out-Stop-lambda
         )

(define Select-Zoom-In-lambda
  (lambda (eventPAD e) (set-currentPAD! eventPAD)
          (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
          (if (and (not (null? (send eventPAD selected))) *cowboy-zoom-on-objects*)
              (send (send eventPAD stimer) start 1 (send eventPAD zinterval))
              (send (send eventPAD ztimer) start 1 (send eventPAD zinterval)))))

(define Select-Zoom-In-Stop-lambda
  (lambda (eventPAD e) (set-currentPAD! eventPAD)
          (send (send eventPAD stimer) stop)
          (send (send eventPAD ztimer) stop)))

(define Select-Zoom-Out-lambda
  (lambda (eventPAD e) (set-currentPAD! eventPAD)
          (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
          (if (and (not (null? (send eventPAD selected))) *cowboy-zoom-on-objects*)
              (send (send eventPAD stimer) start -1 (send eventPAD zinterval))
              (send (send eventPAD ztimer) start -1 (send eventPAD zinterval)))))

(define Select-Zoom-Out-Stop-lambda
  (lambda (eventPAD e) (set-currentPAD! eventPAD)
          (send (send eventPAD stimer) stop)
          (send (send eventPAD ztimer) stop)))


; must call bindZoom first to create ztimer and zinterval
(define (bindZoomSelection argPAD)
  (define scaleproc (lambda (argPAD zmult)
                      (let
                          ((x (sendf argPAD evs lastx))
                           (y (sendf argPAD evs lasty))
                           (scalefac zmult))
                        (map (lambda (o) (send o scale scalefac x y)) (send argPAD selected)))))

  (send argPAD stimer (make-object zoomtimer% argPAD scaleproc))

  (send argPAD bind "<Select-ButtonPress-2>"   Select-Zoom-In-lambda)
  (send argPAD bind "<Select-ButtonRelease-2>" Select-Zoom-In-Stop-lambda)
  (send argPAD bind "<Select-ButtonPress-3>"   Select-Zoom-Out-lambda)
  (send argPAD bind "<Select-ButtonRelease-3>" Select-Zoom-Out-Stop-lambda)

  (send argPAD setvar 'Select-Zoom-In-lambda       Select-Zoom-In-lambda)
  (send argPAD setvar 'Select-Zoom-In-Stop-lambda  Select-Zoom-In-Stop-lambda)
  (send argPAD setvar 'Select-Zoom-Out-lambda      Select-Zoom-Out-lambda)
  (send argPAD setvar 'Select-Zoom-Out-Stop-lambda Select-Zoom-Out-Stop-lambda)
  )
