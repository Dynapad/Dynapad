; If destination is list of numbers (position on pad surface, rather than object),
; then third number is x dimension of pad at time link was created.
; Zoom so that view fills pad the way it did when link was created; that is
; zoom so that x dimension of current pad equals x dimension of pad at time was created.
; Genghis Ron
(define (link-hook o)
  (if (send o link)
      (send o bind "<Run-Double-ButtonPress-1>"
            (lambda (eventPAD e) (set! currentPAD eventPAD)
                    (let
                        ((link (send (event-obj e) link)))
                      (cond
                        ((is-a? link dynaobject%) (send link center 1000))
                        ((list? link)
                         (let*
                             ((x (car link))
                              (y (cadr link))
                              (xdim (caddr link))
                              (bbox (send eventPAD bbox))
                              (curxdim (- (caddr bbox) (car bbox)))
                              (curzoom (caddr (send eventPAD view)))
                              (zoom (* curzoom (/ curxdim xdim)))
                              (link (list x y zoom)))
                           (send eventPAD moveto link 1000)))))))
      (send o bind "<Run-Double-ButtonPress-1>" #f)))

;======= Create Hyperlink ========
;(define rubberband%
;  (class line%
;     (init dynaptr)
;     (super-instantiate (dynaptr))
;     ...finish...

(define *enact-link-fn* #f)

(define (initCreateLink argPAD)
  (send argPAD focus)
  (set! *enact-link-fn*
        (lambda (source target)
          (send source link target)))
  (changemode argPAD "CreateLink"))

(define (finishLink argPAD)
  (changemode--no-gui argPAD (default-mode argPAD))
  (gui-update-mode argPAD))

; must call bindZoom first to create ztimer and zinterval
(define (bindHyperlink argPAD)
  (let ((CreateLink #f)
        (LinkArrow #f)
        )

    ;;; enable zooming during hyperlink mode
    (send argPAD bind "<CreateLink-ButtonPress-2>"
          (lambda (eventPAD e) (set! currentPAD eventPAD)
                  (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
                  (send (send eventPAD ztimer) start 1 (send eventPAD zinterval))))

    (send argPAD bind "<CreateLink-ButtonRelease-2>"
          (lambda (eventPAD e) (set! currentPAD eventPAD)
                  (send (send eventPAD ztimer) stop)))

    (send argPAD bind "<CreateLink-ButtonPress-3>"
          (lambda (eventPAD e) (set! currentPAD eventPAD)
                  (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
                  (send (send eventPAD ztimer) start -1 (send eventPAD zinterval))))

    (send argPAD bind "<CreateLink-ButtonRelease-3>"
          (lambda (eventPAD e) (set! currentPAD eventPAD)
                  (send (send eventPAD ztimer) stop)))

    ;;; start the link
    (send argPAD bind "<CreateLink-ButtonRelease-1>"
          (lambda (eventPAD e) (set! currentPAD eventPAD)
                  (when (not CreateLink)
                    (let*
                        ((x (event-x e))
                         (y (event-y e))
                         (l (reverse (send eventPAD find 'overlapping (list x y x y)))))

                      (when (not (null? l))
                        (set! CreateLink (car l)))
                      (when (not LinkArrow)
                        (set! LinkArrow (ic (make-object line% eventPAD (list x y x y))
                                            (penwidth 0)
                                            (pen "yellow"))))

                      (sendf eventPAD evs set-last-xy x y)))))

    ;;; make the link
    ; see link-hook for explanation of use of xdim
    (send argPAD bind "<CreateLink-KeyPress-space>"
          (lambda (eventPAD e) (set! currentPAD eventPAD)
                  (let*
                      ((x (event-x e))
                       (y (event-y e))
                       (l (remove LinkArrow
                                  (reverse (send eventPAD find 'overlapping (list x y x y)))))
                       (bbox (send eventPAD bbox))
                       (xdim (- (caddr bbox) (car bbox))))

                    (when LinkArrow
                      (send LinkArrow delete)
                      (set! LinkArrow #f)
                      )
                    (when CreateLink
                      (if (not (null? l))
                          (*enact-link-fn* CreateLink (car l))
                          (*enact-link-fn* CreateLink (list x y xdim)))
                      (set! CreateLink #f))

                    (unless Draw-multiple (finishLink argPAD))
                    )))

    ;;; abort making the link
    (send argPAD bind "<CreateLink-KeyPress-Escape>"
          (lambda (eventPAD e) (set! currentPAD eventPAD)
                  (when LinkArrow
                    (send LinkArrow delete)
                    (set! LinkArrow #f))
                  (if (not CreateLink)
                      (finishLink argPAD)
                      (set! CreateLink #f))))

    ;;; enable pan during hyperlink mode
    (send argPAD bind "<CreateLink-ButtonPress-1>"
          (lambda (eventPAD e) (set! currentPAD eventPAD)
                  (sendf eventPAD evs set-last-xy (event-x e) (event-y e)) ))

    (send argPAD bind "<CreateLink-B1-Motion>"
          (lambda (eventPAD e) (set! currentPAD eventPAD)
                  (let* ((l (send eventPAD view))
                         (view_x (car l))
                         (view_y (cadr l))
                         (zoom (caddr l))
                         (last_x (sendf eventPAD evs lastx))
                         (last_y (sendf eventPAD evs lasty))
                         (newx (- view_x (- (event-x e) last_x)))
                         (newy (- view_y (- (event-y e) last_y)))
                         )
                    (when LinkArrow
                      (send LinkArrow coords (list last_x last_y (event-x e)(event-y e))))
                    (send eventPAD moveto (list newx newy zoom)) )))

    (send argPAD bind "<CreateLink-Motion>"
          (lambda (eventPAD e) (set! currentPAD eventPAD)
                  (let* ((last_x (sendf eventPAD evs lastx))
                         (last_y (sendf eventPAD evs lasty)))
                    (when LinkArrow
                      (send LinkArrow coords (list last_x last_y (event-x e)(event-y e)))) )))
    ))

