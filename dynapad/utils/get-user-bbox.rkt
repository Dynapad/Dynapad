#lang racket/base

(provide ask-user-for-bbox)

(require racket/class
         dynapad/history/undo
         dynapad/misc/misc
         dynapad/pad-state
         dynapad/layout/bbox
         (only-in dynapad/objects rect%)
         (only-in dynapad/events/mode
                  push-event-mode
                  pop-event-mode))

(define (dt-event? e) #f)  ;may be overridden by DiamondTouch table package

(define ask-user-for-bbox--event-binder%
  (class object%
    (init-field _dynapad)
    (init-field _release_callback)
    ;width & height: #f --> unconstrained
    (init-field (_dx #f)
                (_dy #f))

    (super-instantiate ())

    (field (_upperleft_x #f))
    (field (_upperleft_y #f))
    ;    (field (_orig_x 0))
    ;    (field (_orig_y 0))
    ;    (field (_orig_crds 0))
    (field (_resize_mode 0))
    (field (_rect #f))


    (field (_await-second-release? #f)) ;needed only in case DT table needs
    ;  two-touch sequence

    (define (refresh-dxy)
      (let ((default (/ 100 (send _dynapad getzoom))))
        (list (or _dx default)
              (or _dy default))))

    (push-event-mode _dynapad "GetBBox")

    (let* ((dxy (refresh-dxy))
           (dx (car dxy))
           (dy (cadr dxy))
           (view (send _dynapad view))
           (x0 (car view))
           (y0 (cadr view)))
      (set! _rect (ic (make-object rect% _dynapad (list (- x0 dx) y0 x0 (+ y0 dy)))
                      (fill "none")
                      (pen "red")
                      (re-anchor "se")
                      (penwidth 0))))

    (define (GetBBox_TrackOrigin evnt)
      ;      (define size (/ 1 (send _dynapad getzoom)))
      ;      (if _dx
      ;          (send _rect xy (list (event-x evnt) (event-y evnt)))...
      (let* ((dxy (refresh-dxy))
             (dx  (car dxy))
             (dy  (cadr dxy)))
        (send _rect coords (list
                            (- (event-x evnt) dx)
                            (event-y evnt)
                            (event-x evnt)
                            (+ (event-y evnt) dy)
                            )))
      )

    (define (GetBBox_SetOrigin evnt)
      ;     (define size (/ 1 (send _dynapad getzoom)))
      ;     (set! _upperleft_x (- (event-x evnt) (* _dx size)))
      ;     (set! _upperleft_y (+ (event-y evnt) (* _dy size)))
      (let ((bb (send _rect coords)))
        (set! _upperleft_x (b0 bb))
        (set! _upperleft_y (b3 bb)))
      )

    (define (GetBBox_TrackSize evnt)
      ;(define size (/ 100 (send _dynapad getzoom)))
      (let ((ex (event-x evnt))
            (ey (event-y evnt)))
        (send _rect coords
              (list
               (if _dx (- ex _dx) ;fixed width
                   _upperleft_x) ;adjustable width
               ey
               ex
               (if _dy (+ ey _dy) ;fixed height
                   _upperleft_y) ;adjustable height
               ))))

    (define (GetBBox_Finish evnt)
      (let ((bb (send _rect bbox)))
        (send _rect delete) (set! _rect #f)
        (send _dynapad bind "<GetBBox-Motion>"          #f)
        (send _dynapad bind "<GetBBox-ButtonPress-1>"   #f)
        (send _dynapad bind "<GetBBox-B1-Motion>"       #f)
        (send _dynapad bind "<GetBBox-ButtonRelease-1>" #f)
        (pop-event-mode _dynapad)
        (when _release_callback
          (_release_callback bb))
        ))

    (send _dynapad bind "<GetBBox-Motion>"          (lambda (d e)
                                                      (if _upperleft_x ;already set origin?
                                                          (GetBBox_TrackSize e)
                                                          (GetBBox_TrackOrigin e))))
    (send _dynapad bind "<GetBBox-ButtonPress-1>"   (lambda (d e)
                                                      (if (dt-event? e)
                                                          (if _upperleft_x
                                                              #f
                                                              (GetBBox_TrackOrigin e))
                                                          (GetBBox_SetOrigin e))))
    (send _dynapad bind "<GetBBox-B1-Motion>"       (lambda (d e)
                                                      (if _upperleft_x
                                                          (GetBBox_TrackSize e)
                                                          (GetBBox_TrackOrigin e))
                                                      #f))
    (send _dynapad bind "<GetBBox-ButtonRelease-1>" (lambda (d e)
                                                      (if (or _upperleft_x
                                                              (and _dx _dy))
                                                          (GetBBox_Finish e)
                                                          (GetBBox_SetOrigin e))
                                                      ))

    (send _dynapad bind "<GetBBox-KeyPress-Escape>"
          (lambda (eventPAD e)
            (pop-event-mode _dynapad)
            (when _rect (send _rect delete) (set! _rect #f))
            (when _release_callback
              (_release_callback #f))))

    ; allow zooming (oldschool) while getting bbox
    ;  (send _dynapad bind "<GetBBox-ButtonPress-2>"   Zoom-In-lambda)
    ;  (send _dynapad bind "<GetBBox-ButtonRelease-2>" Zoom-In-Stop-lambda)
    ;  (send _dynapad bind "<GetBBox-ButtonPress-3>"   Zoom-Out-lambda)
    ;  (send _dynapad bind "<GetBBox-ButtonRelease-3>" Zoom-Out-Stop-lambda)
    ))

(define (ask-user-for-bbox argPAD callback . dxy)
  ; if dxy supplied, locks bbox size
  (apply make-object ask-user-for-bbox--event-binder% argPAD callback dxy))

#|
; example
(define (make-oval argPAD coords) (ic (make-object oval% argPAD coords) (fill white)))

(ask-user-for-bbox dynapad (lambda (bbox) (make-oval dynapad bbox)))
|#
