

; Redefine the default binding for image% hires<-->thumb swap
; note: this must be loaded before any images are constructed.
;(define (init-image-bindings img)
;  (send img bind "<Double-ButtonPress-1>"
;    (image-toggle-hires-thumb img) ))
; Use new aftermake callbacks:
(image-aftermake-callbacks 'add
                           (lambda (img) (send img bind "<Double-ButtonPress-1>"
                                               (image-toggle-hires-thumb img))))

(define *beforezoom-callbacks* null)  ;(lambda (pad event)...)
(define *afterzoom-callbacks* null)
(define *beforepan-callbacks* null)
(define *afterpan-callbacks* null)

(define access-beforezoom-callbacks   (callback-accessor-functions *beforezoom-callbacks*))
(define access-afterzoom-callbacks    (callback-accessor-functions *afterzoom-callbacks*))
(define access-beforepan-callbacks    (callback-accessor-functions *beforepan-callbacks*))
(define access-afterpan-callbacks     (callback-accessor-functions *afterpan-callbacks*))

(define (do-beforezoom-callbacks argPAD)
  (exec-any-callbacks *beforezoom-callbacks* argPAD))
(define (do-afterzoom-callbacks argPAD)
  (exec-any-callbacks *afterzoom-callbacks* argPAD))
(define (do-beforepan-callbacks argPAD)
  (exec-any-callbacks *beforepan-callbacks* argPAD))
(define (do-afterpan-callbacks argPAD)
  (exec-any-callbacks *afterpan-callbacks* argPAD))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dynaload "zoom-pushpull.ss")

(define *startpan-dx-threshold* 4)
(define *startzoom-dy-threshold* 5)

(define newpad-event-binder%
  (class basepad-event-binder%
    (init argPAD evs-class)
    (super-instantiate (argPAD evs-class))

    ;--- pad-specific methods; subclass (e.g. for minipad) should override
    (define/public (get-zoom-relevant-objects eventPAD)
      (filter (lambda (obj) (not (send obj sticky))) (send  eventPAD objects)))

    (define/public (precompute-view argPAD objs)
      (precompute-center-view argPAD objs))

    (define/public (is-background? obj)
      (is-a? obj dynapad%))

    ;--- middle button zooming ---------------------------------------
    ; Combined "Gearshift" Pan/Zoom:
    (define init-drag-view
      (lambda (eventPAD e)
        (set! currentPAD eventPAD)
        (sendf eventPAD evs sy0 (event-sy e))
        (sendf eventPAD evs sx0 (event-sx e))))

    (define update-drag-view
      (lambda (eventPAD e)
        (let* ((sx (event-sx e))
               (sy (event-sy e))
               (dx (- sx (sendf eventPAD evs sx0)))
               (dy (- sy (sendf eventPAD evs sy0))))
          (cond
            ; panning
            ((>= (abs dx) *startpan-dx-threshold*)
             (Start-Pan-Event eventPAD e)
             (do-beforepan-callbacks eventPAD)
             (push-event-mode eventPAD "Pan"))
            ; zooming
            ((>= (abs dy) *startzoom-dy-threshold*)
             (init-pushpull-zooming eventPAD e))
            (else #f)))))

    (define init-pushpull-zooming
      (lambda (eventPAD e)
        (init-push-pull-motion eventPAD e)
        (init-lerp-zooming eventPAD e
                           (is-background? (event-obj e))
                           (get-zoom-relevant-objects eventPAD))
        (do-beforezoom-callbacks eventPAD)
        (push-event-mode eventPAD "Zoom")))

    (send argPAD bind "<Run-ButtonPress-2>" init-drag-view)
    (send argPAD bind "<Drag-ButtonPress-2>" init-drag-view)

    (send argPAD bind "<Run-B2-Motion>" update-drag-view)
    (send argPAD bind "<Drag-B2-Motion>" update-drag-view)

    (send argPAD bind "<Pan-B2-Motion>" Do-Pan-Event)
    (send argPAD bind "<Zoom-B2-Motion>"
          (lambda (eventPAD e)
            (let ((frac (update-push-pull-motion eventPAD e)))
              (and frac (update-lerp-zooming eventPAD e frac)))))
    (send argPAD bind "<Zoom-ButtonRelease-2>"
          (lambda (eventPAD e)
            (sendf eventPAD evs update-wide-view)
            (do-afterzoom-callbacks eventPAD)
            (pop-event-mode eventPAD "Zoom")))
    (send argPAD bind "<Pan-ButtonRelease-2>"
          (lambda (eventPAD e)
            (do-afterpan-callbacks eventPAD)
            (pop-event-mode eventPAD "Pan")))

    (send argPAD bind "<Run-Shift-B2-Motion>"
          (lambda (eventPAD e) (set! currentPAD eventPAD)
                  (let ((frac (update-push-pull-motion eventPAD e)))
                    (update-lerp-zooming eventPAD e frac 'relax-constraints))))


    (bindSelect argPAD "Select") ;select.ss
    (bindSelect argPAD "Run")

    ;(send argPAD bind "<Run-Shift-ButtonPress-1>"
    ;  (lambda (eventPAD e) (set! currentPAD eventPAD)
    ;    (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
    ;    (sendf eventPAD evs sx0 (event-sx e))
    ;    (sendf eventPAD evs sy0 (event-sy e))
    ;    (choose-appropriate-drag-find-function (event-obj e))
    ;    #f
    ;    ))

    ;=== do the actual bindings ====================================

    ;; copy/move some existing bindings into xxx-Control-xxx versions:
    (define (copy-binding newbind oldbind)
      (send argPAD bind newbind (send argPAD bind oldbind)))

    ;  (copy-binding "<Run-Control-ButtonPress-1>"   "<Run-ButtonPress-1>")
    ;  (copy-binding "<Run-Control-B1-Motion>"       "<Run-B1-Motion>")

    ;  (copy-binding "<Select-Control-ButtonPress-1>"   "<Run-ButtonPress-1>")
    ;  (copy-binding "<Select-Control-B1-Motion>"       "<Run-B1-Motion>")


    ;  (send argPAD bind "<Run-Control-ButtonPress-2>"   Zoom-In-lambda)
    ;  (send argPAD bind "<Run-Control-ButtonRelease-2>" Zoom-In-Stop-lambda)
    ;  (send argPAD bind "<Run-Control-ButtonPress-3>"   Zoom-Out-lambda)
    ;  (send argPAD bind "<Run-Control-ButtonRelease-3>" Zoom-Out-Stop-lambda)

    ;  (send argPAD bind "<Select-Control-ButtonPress-2>"   Select-Zoom-In-lambda)
    ;  (send argPAD bind "<Select-Control-ButtonRelease-2>" Select-Zoom-In-Stop-lambda)
    ;  (send argPAD bind "<Select-Control-ButtonPress-3>"   Select-Zoom-Out-lambda)
    ;  (send argPAD bind "<Select-Control-ButtonRelease-3>" Select-Zoom-Out-Stop-lambda)


    ;; blow away some existing bindings:
    ;  (send argPAD bind "<Run-ButtonPress-2>" ())
    ;  (send argPAD bind "<Run-ButtonRelease-2>" ())
    ;  (send argPAD bind "<Run-ButtonPress-3>" ())
    ;  (send argPAD bind "<Run-ButtonRelease-3>" ())
    ;  (send argPAD bind "<Select-ButtonPress-2>" ())
    ;  (send argPAD bind "<Select-ButtonRelease-2>" ())
    ;  (send argPAD bind "<Select-ButtonPress-3>" ())
    ;  (send argPAD bind "<Select-ButtonRelease-3>" ())
    ;  (send argPAD bind "<Run-Control-B2-Motion>" ())

    (addbinding argPAD "<Select-ButtonRelease-1>"
                (lambda (eventPAD e) (changemode eventPAD "Run")))


    ;  (send argPAD bind "<Run-ButtonPress-2>"         (Run-ButtonPress-2))
    ;  (send argPAD bind "<Run-B2-Motion>"             (Run-B2-Motion))
    ;  (send argPAD bind "<Run-Shift-B2-Motion>"       (Run-Shift-B2-Motion))

    ; Dragging
    ;  (send argPAD bind "<Run-ButtonPress-1>"         (Run-ButtonPress-1))
    ;  (send argPAD bind "<Run-B1-Motion>"             (Run-B1-Motion))
    ;  (send argPAD bind "<Run-ButtonRelease-1>"       (Run-ButtonRelease-1))

    ; Multiple Selection
    ;  (send argPAD bind "<Run-Shift-ButtonPress-1>"   (Run-Shift-ButtonPress-1))
    ;  (send argPAD bind "<Run-Shift-B1-Motion>"       (Run-Shift-B1-Motion))
    ;  (send argPAD bind "<Run-Shift-ButtonRelease-1>" (Run-Shift-ButtonRelease-1))

    ; set up some other variables for this dynapad

    ;  (send argPAD select-render-script
    ;    (lambda (cptr obj)
    ;      (sch_pen cptr "red")
    ;      (sch_transparency cptr 0.5)
    ;      (sch_penwidth cptr 0)))

    (send argPAD setvar 'default-mode "Run")
    (changemode argPAD "Run")


    )) ;;; end newpad-event-binder%

;(define (apply-newpad-bindings argPAD)
;  (make-object newpad-event-binder% argPAD))


