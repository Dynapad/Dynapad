(dynaload "event-binders.rkt")

(define minipad% (class panel%

                   (field (_viewx 0) (_viewy 0) (_viewz 1))
                   (field (_centered_scale 1))
                   (inherit get-cptr)

                   (define/override (add obj)
                     (def bb (send this bbox))
                     (send obj scale (/ 1.0 (send this z)) (b0 bb) (b1 bb))
                     (send obj slide (- (b0 bb)) (- (b1 bb)))
                     (super add obj)
                     (send obj takegroupevents #t)
                     (when (send obj selected?) (send obj unselect) (send obj select))
                     (add-minipad-member-bindings this obj)
                     )

                   (define/override (remove obj)
                     (def bb (send this bbox))
                     (send obj scale (send this z) (b0 bb) (b1 bb))
                     (super remove obj)
                     (send obj slide (b0 bb) (b1 bb))
                     (send obj takegroupevents #f)
                     ;(when (send obj selected?) (send obj unselect) (send obj select))
                     (remove-minipad-member-bindings this obj)
                     )

                   (define/public centered_scale (get/set _centered_scale))

                   (define/public (selected) (send dynapad selected))

                   (define/public view (case-lambda
                                         (() (list _viewx _viewy _viewz))
                                         ((x y s) (moveto x y s)) ))

                   (define/public (getzoom) _viewz)

                   (define/public pad->mini (case-lambda
                                              ((lst) (pad->mini (car lst) (cadr lst)))
                                              ((padx pady)
                                               (define mxy (bbcenter (send this bbox)))
                                               (list
                                                (+ _viewx (/  (* (send dynapad getzoom) (- padx (car  mxy))) _viewz))
                                                (+ _viewy (/  (* (send dynapad getzoom) (- pady (cadr mxy))) _viewz))
                                                )
                                               )
                                              ))

                   (define/public moveto (case-lambda
                                           ((lst) (moveto (car lst) (cadr lst) (caddr lst)))
                                           ((x y s)
                                            (define scl (/ s _viewz))
                                            (sch_panelmoveview (get-cptr) scl
                                                               (+
                                                                (* (- 1 scl) 0.5 (send dynapad getzoom) (send this width))
                                                                (* (- _viewx x) s) )
                                                               (+
                                                                (* (- 1 scl) 0.5 (send dynapad getzoom) (send this height))
                                                                (* (- _viewy y) s) )
                                                               )
                                            (set! _viewz s)
                                            (set! _viewx x)
                                            (set! _viewy y)
                                            )
                                           ))

                   (define/public slideview (case-lambda
                                              ((lst) (slideview (car lst) (cadr lst)))
                                              ((dx dy)
                                               (let ((vu (view)))
                                                 ; this is lazy
                                                 (moveto (- (car vu) dx) (- (cadr vu) dy) (caddr vu))
                                                 ))
                                              ))

                   (define/public (event-binder-type)
                     minipad-event-binder%)
                   (define/public (add-minipad-bindings)
                     (make-object (send this event-binder-type) this)) ;subclass needs to override

                   (super-instantiate ())
                   (send this sticky #t)
                   (add-minipad-bindings)
                   ))

;=================== MINIPAD bindings ==================================

(define precompute-minipad-view (lambda (argMINIPAD object_list)
                                  (let*((bbv (send argMINIPAD bbox))
                                        (bbo (bbunion-objects object_list))
                                        (newz (* (send argMINIPAD getzoom)
                                                 (min (/ (* (send argMINIPAD centered_scale) (bbwidth bbv))
                                                         (bbwidth bbo))
                                                      (/ (* (send argMINIPAD centered_scale) (bbheight bbv))
                                                         (bbheight bbo)))))
                                        )
                                    (append (send argMINIPAD pad->mini (bbcenter bbo)) (list newz))
                                    )
                                  ))

(define (members-of-minipad mp lst)
  (if (not (is-a? mp minipad%))
      lst
      ;else
      (let ((mems (send mp members)))
        (filter
         (lambda (o) (member o mems))
         lst))))

(define (add-minipad-member-bindings argMINIPAD obj)
  (send obj bind "<Run-ButtonPress-2>"
        (send argMINIPAD bind "<Run-ButtonPress-2>"))
  (send obj bind "<Run-B2-Motion>"
        (send argMINIPAD bind "<Run-B2-Motion>"))

  (send obj bind "<Run-Control-ButtonPress-1>"
        (send argMINIPAD bind "<Run-Control-ButtonPress-1>"))
  (send obj bind "<Run-Control-B1-Motion>"
        (send argMINIPAD bind "<Run-Control-B1-Motion>"))
  )

(define (remove-minipad-member-bindings argMINIPAD obj)
  (send obj bind "<Run-ButtonPress-2>" #f)
  (send obj bind "<Run-B2-Motion>" #f)
  (send obj bind "<Run-Control-ButtonPress-1>" #f)
  (send obj bind "<Run-Control-B1-Motion>" #f)
  )

(define minipad-event-binder%
  (class newpad-event-binder%
    (init _arg0)  ;_arg0 = _pad is a minipad
    (inherit-field _pad)
    (if (not (is-a? _arg0 minipad%))
        (error "Requires minipad% argument"))
    (super-instantiate (_arg0))

    ;--- local tools ---------------------------------------------

    ;--- pad-specific methods; subclass (e.g. for minipad) should override
    (define/override (get-zoom-relevant-objects)
      (send _pad members))

    (define/override (enclosed-members eventPAD bbox)
      (members-of-minipad
       _pad
       (send eventPAD find 'groupmembers 'enclosed bbox)))

    (define/override (precompute-view objs)
      (precompute-minipad-view _pad objs))

    (define/override (is-padtype? obj)
      (is-a? obj minipad%))

    ;--- bindings
    (define/public (Run-Control-ButtonPress-1)
      (lambda (eventPAD e)
        (send eventPAD store-prev_xy (event-x e) (event-y e))
        #f))

    (define/public (Run-Control-B1-Motion)
      (lambda (eventPAD e)
        (def vu (send minipad view))
        (def prev_x (send eventPAD get-prev_x))
        (def prev_y (send eventPAD get-prev_y))
        (send eventPAD store-prev_xy (event-x e) (event-y e))

        (when (and prev_x prev_y)
          (send _pad slideview
                (/ (* (- (event-x e) prev_x) (send eventPAD getzoom)) (caddr vu))
                (/ (* (- (event-y e) prev_y) (send eventPAD getzoom)) (caddr vu))
                ))
        #f))

    ; Panning
    (send _pad bind "<Run-Control-ButtonPress-1>" (Run-Control-ButtonPress-1))
    (send _pad bind "<Run-Control-B1-Motion>"     (Run-Control-B1-Motion))

    ))
