#lang racket/base

(require racket/class
         dynapad/objects
         dynapad/pad-state
         dynapad/undo-state
         dynapad/misc/misc
         dynapad/events/reshape
         dynapad/events/mode
         dynapad/layout/bbox
         )

(provide make-selected-bigger
         make-selected-smaller
         resize-object
         resizer-bindings
         undoable-resizer-bindings
         )

(define (make-selected-bigger)
  (let* ((selected (send dynapad selected))
         (grp (make-object group% dynapad (send dynapad selected))) )
    (send grp scale 1.414)
    (send grp ungroup)
    (foreach selected (lambda (i) (send i unselect)(send i select)))
    ))

(define (make-selected-smaller)
  (let* ((selected (send dynapad selected))
         (grp (make-object group% dynapad (send dynapad selected))) )
    (send grp scale 0.707)
    (send grp ungroup)
    (foreach selected (lambda (i) (send i unselect)(send i select)))
    ))




(define (resize-object argPAD obj)
  ; creates resize affordance until unselected
  ;(changemode argPAD "Select")
  (send obj select)
  (let ((hlist
         (add-resize-handles-to-object argPAD obj)))

    ; cleanup: when obj is unselected, delete handles, callbacks, etc
    (send obj select-callbacks
          'add
          (lambda (o on?) (when (not on?) ; whenunselected
                            (send obj afterslide-callbacks 'remove #t 'resize)
                            (send obj afterposition-callbacks 'remove #t 'resize)
                            (send obj select-callbacks 'remove #t 'resize)
                            (send obj delete-callbacks 'remove #f 'resize)
                            (send argPAD afterzoom-callbacks 'remove #t 'resize)
                            (foreach hlist (lambda (h) (send h delete))) ))
          'resize))
  )

(define (add-resize-handles-to-object argPAD obj)
  ; permanent (or until cleaned above) resize affordance
  (def hlist '())
  (def reposition-handles)
  (def cx0)
  (def cy0)
  (def w0)
  (def h0)
  (def addbindings)
  (def save_anchor (send obj anchor))
  (def bb (send obj bbox))

  ; create a list of handle objects
  (set! hlist (list
               (make-object general-handle% argPAD (b0  bb) (b1 bb))
               (make-object general-handle% argPAD (bxc bb) (b1 bb))
               (make-object general-handle% argPAD (b2  bb) (b1 bb))
               (make-object general-handle% argPAD (b2  bb) (byc bb))
               (make-object general-handle% argPAD (b2  bb) (b3 bb))
               (make-object general-handle% argPAD (bxc bb) (b3 bb))
               (make-object general-handle% argPAD (b0  bb) (b3 bb))
               (make-object general-handle% argPAD (b0  bb) (byc bb))
               ))

  ; create a function to reposition the handles
  (set! reposition-handles (lambda (o) (let ((bb (send o bbox)))
                                         (map
                                          (lambda (h fx fy) (send h xy (fx bb) (fy bb)))
                                          hlist
                                          (list b0 bxc b2 b2  b2 bxc b0 b0 )
                                          (list b1 b1  b1 byc b3 b3  b3 byc)
                                          )
                                         )))

  ; create a function to add bindings
  (set! addbindings (lambda (h corner sclx scly)
                      (send h bind "<Select-ButtonPress-1>"   '())
                      (send h bind "<Run-ButtonPress-1>"      '())
                      (send h bind "<Select-ButtonRelease-1>" '())
                      (send h bind "<Run-ButtonRelease-1>"    '())

                      (send h bind "<ButtonPress-1>" (lambda (d e)
                                                       (send obj re-anchor corner)
                                                       (reposition-handles obj)
                                                       (set! w0 (send obj width))
                                                       (set! h0 (send obj height))
                                                       (set! cx0 (event-x e))
                                                       (set! cy0 (event-y e))
                                                       #f ))

                      (send h bind "<ButtonRelease-1>" (lambda (d e)
                                                         (send obj re-anchor save_anchor)
                                                         (reposition-handles obj)
                                                         #f ))

                      (send h drag-callback
                            (lambda (x y)
                              (let ((newwidth (+ w0 (* sclx (- x cx0))))
                                    (newheight (+ h0 (* scly (- y cy0))))
                                    (z (send argPAD getzoom)) )
                                (when (> (* newwidth  z) 4) (send obj width  newwidth))
                                (when (> (* newheight z) 4) (send obj height newheight)) )
                              (reposition-handles obj) ))
                      ))

  ; apply the function to the list of handles
  (map addbindings
       hlist
       (list "ne" "n" "nw" "w" "sw" "s" "se" "e")
       (list  -1   0    1   1   1    0   -1  -1 )
       (list  -1  -1   -1   0   1    1    1   0 ) )

  ; when obj is deleted, kill handles
  (send obj delete-callbacks
        'add
        (lambda (d) (foreach hlist (lambda (h) (send h delete))))
        'resize)

  ; when object slides, reposition handles
  (send obj afterslide-callbacks
        'add
        (lambda (d dx dy) (reposition-handles obj))
        'resize)
  (send obj afterposition-callbacks
        'add
        (lambda (d . args) (reposition-handles obj))
        'resize)

  ; when dynapad zooms, reposition handles
  (send argPAD afterzoom-callbacks
        'add
        (lambda (d x y z) (reposition-handles obj))
        'resize)

  (foreach hlist (lambda (h)
                   (send h sticky (send obj sticky))
                   (send h layer (send obj layer))
                   (send h raise obj)
                   ))
  hlist
  )



(define resize-border--event-binder%
  (class object%
    (init-field _dynapad)
    (init-field _object)

    (init (motion-callback #f))
    (init (release-callback #f))
    (init-field (_default-rescale-lock? #f)) ;should be #t if target is image
    (init-field (_min-units 0))
    (init-field (_min-pixels 0))
    ; use stricter (max) of two minima
    (field (_minsize (max _min-units (/ _min-pixels (send _dynapad getzoom)))))
    (field (_motion_callback motion-callback))
    (field (_release_callback release-callback))

    (super-instantiate ())

    (field (_last_x 0))
    (field (_last_y 0))
    (field (_resize_mode -1))
    (field (_button_down #f))

    (define (Resize_Enter e)
      (SetResizeMode e)
      (send _dynapad bind "<Motion>" (lambda (d e) (SetResizeMode e))) )

    (define (SetResizeMode evnt)
      (when (not _button_down)
        (let*((bb (send (event-obj evnt) bbox))
              (xmarg (* 0.1 (bbwidth bb)))
              (ymarg (* 0.1 (bbheight bb)))
              (x (event-x evnt))
              (y (event-y evnt))
              (xmode 1)
              (ymode 1)
              (lastmode _resize_mode)
              )
          (when (> x (- (b2 bb) xmarg)) (set! xmode 2))
          (when (< x (+ (b0 bb) xmarg)) (set! xmode 0))
          (when (> y (- (b3 bb) ymarg)) (set! ymode 2))
          (when (< y (+ (b1 bb) ymarg)) (set! ymode 0))

          (set! _resize_mode (+ (* 3 ymode) xmode))

          (when (!= _resize_mode lastmode)
            (case _resize_mode
              ((0) (send _dynapad cursor 4))   ;   modes       cursors
              ((1) (send _dynapad cursor 9))   ;  6  7  8      6  8  7
              ((2) (send _dynapad cursor 5))   ;  3     5 ==> 10     11
              ((3) (send _dynapad cursor 10))  ;  0  1  2      4  9  5
              ((5) (send _dynapad cursor 11))
              ((6) (send _dynapad cursor 6))
              ((7) (send _dynapad cursor 8))
              ((8) (send _dynapad cursor 7))))
          )))

    (define (Resize_Leave e)
      (when (not _button_down)
        (set! _resize_mode -1)
        (gui-update-mode _dynapad)
        (send _dynapad bind "<Motion>" #f) ))

    (define/public (Resize_ButtonPress evnt)
      (set! _last_x (event-x evnt))
      (set! _last_y (event-y evnt))
      (set! _button_down #t)
      )

    (define (Rescale_ButtonMotion evnt)
      ; like Resize_ButtonMotion, but locks aspect ratio
      (define obj (event-obj evnt))
      (define bb (send obj bbox))
      (define x0 (b0 bb))
      (define x2 (b2 bb))
      (define y1 (b1 bb))
      (define y3 (b3 bb))
      (define x (event-x evnt))
      (define y (event-y evnt))
      (define m (/ (bbheight bb) (bbwidth bb)))
      (define dy (- y y1))
      (define mx0 (* m (- x x0))) ;y threshold of 0-8 diagonal
      (define mx2 (* m (- x2 x))) ;y threshold of 2-6 diagonal

      ; Sends target a bbox with one #f (free side)
      ; If dragging side (resize 1,3,5,7) that side gets #f
      ; If dragging corner (resize 0,2,6,8), the #f side depends on cursor octant:
      ;    resize_mode:    #f side:
      ;         |           \b0|b2/
      ;      6  7  8         \ | /
      ;         |          b3 \|/ b3
      ;     -3--+--5-      ----+----
      ;         |          b1 /|\ b1
      ;      0  1  2         / | \
      ;         |           /b0|b2\

      (define new-bb
        (case _resize_mode
          ((0) (if (> dy mx0) (list (min x (- x2 _minsize)) #f x2 y3)
                   (list #f (min y (- y3 _minsize)) x2 y3)))
          ((1)                (list #f (min y (- y3 _minsize)) #f y3))
          ((2) (if (> dy mx2) (list x0 #f (max x (+ x0 _minsize)) y3)
                   (list x0 (min y (- y3 _minsize)) #f y3)))
          ((3)                (list (min x (- x2 _minsize)) #f x2 #f))
          ((4) bb) ;no change
          ((5)                (list x0 #f (max x (+ x0 _minsize)) #f))
          ((6) (if (> dy mx2) (list #f y1 x2 (max y (+ y1 _minsize)))
                   (list (min x (- x2 _minsize)) y1 x2 #f)))
          ((7)                (list #f y1 #f (max y (+ y1 _minsize))))
          ((8) (if (> dy mx0) (list x0 y1 #f (max y (+ y1 _minsize)))
                   (list x0 y1 (max x (+ x0 _minsize)) #f)))
          ))

      (send obj bbox new-bb)

      (when _motion_callback (_motion_callback obj new-bb))

      (set! _last_x (event-x evnt))
      (set! _last_y (event-y evnt))
      )

    (define (Resize_ButtonMotion evnt)
      ; unconstrained aspect ratio
      (define obj (event-obj evnt))
      (define crds (send obj bbox))
      (define x (event-x evnt))
      (define y (event-y evnt))
      (define x0 (b0 crds))
      (define x2 (b2 crds))
      (define y1 (b1 crds))
      (define y3 (b3 crds))
      (define newbb null)

      (case _resize_mode
        ((0) (set! x0 (min x (- x2 _minsize)))  (set! y1 (min y (- y3 _minsize))))
        ((1)                                    (set! y1 (min y (- y3 _minsize))))
        ((2) (set! x2 (max x (+ x0 _minsize)))  (set! y1 (min y (- y3 _minsize))))
        ((3) (set! x0 (min x (- x2 _minsize))))
        ((5) (set! x2 (max x (+ x0 _minsize))))
        ((6) (set! x0 (min x (- x2 _minsize)))  (set! y3 (max y (+ y1 _minsize))))
        ((7)                                    (set! y3 (max y (+ y1 _minsize))))
        ((8) (set! x2 (max x (+ x0 _minsize)))  (set! y3 (max y (+ y1 _minsize)))))

      ;      (case _resize_mode
      ;        ((0) (set! x0 (event-x evnt)) (set! y1 (event-y evnt)) )
      ;        ((1)                          (set! y1 (event-y evnt)) )
      ;        ((2) (set! x2 (event-x evnt)) (set! y1 (event-y evnt)) )
      ;        ((3) (set! x0 (event-x evnt))                          )
      ;        ((5) (set! x2 (event-x evnt))                          )
      ;        ((6) (set! x0 (event-x evnt)) (set! y3 (event-y evnt)) )
      ;        ((7)                          (set! y3 (event-y evnt)) )
      ;        ((8) (set! x2 (event-x evnt)) (set! y3 (event-y evnt)) )
      ;        )

      (set! newbb  (list x0 y1 x2 y3))
      (send obj bbox newbb)
      (when _motion_callback (_motion_callback obj newbb))

      (set! _last_x x)
      (set! _last_y y)
      )

    (define (Default_ButtonMotion evnt)
      (if _default-rescale-lock?
          (Rescale_ButtonMotion evnt)
          (Resize_ButtonMotion evnt)))
    (define (Override_ButtonMotion evnt)
      (if _default-rescale-lock?
          (Resize_ButtonMotion evnt)
          (Rescale_ButtonMotion evnt)))

    (define/public (Resize_ButtonRelease evnt)
      (let ((obj (event-obj evnt)))
        (set! _button_down #f)
        (when _release_callback
          (_release_callback obj)))
      )

    (send _object bind "<Enter>" (lambda (d e) (Resize_Enter e)))
    (send _object bind "<Leave>" (lambda (d e) (Resize_Leave e)))
    (send _object bind "dt-down" (lambda (d e) (Resize_Enter e) #t))  ; do cascade to mouse bindings
    (send _object bind "dt-up"   (lambda (d e) (Resize_Leave e) #t))  ; do cascade to mouse bindings
    (send _object bind "<Run-ButtonPress-1>"    (lambda (d e) (Resize_ButtonPress e) #f))
    (send _object bind "<Run-B1-Motion>"        (lambda (d e) (Default_ButtonMotion e) #f))
    (send _object bind "<Run-Shift-B1-Motion>"  (lambda (d e) (Override_ButtonMotion e) #f))
    (send _object bind "<Run-ButtonRelease-1>"  (lambda (d e) (Resize_ButtonRelease e)))
    (send _object bind "<Select-ButtonPress-1>" (lambda (d e) (Resize_ButtonPress e) #f))
    (send _object bind "<Select-B1-Motion>"     (lambda (d e) (Default_ButtonMotion e) #f))
    (send _object bind "<Select-Shift-B1-Motion>" (lambda (d e) (Override_ButtonMotion e) #f))
    (send _object bind "<Select-ButtonRelease-1>" (lambda (d e) (Resize_ButtonRelease e)))
    ))

(define-syntax resizer-bindings (syntax-rules ()
                                  ((_ args ...) (instantiate resize-border--event-binder% (args ...)))))


(define undoable-resize-border--event-binder%
  (class resize-border--event-binder%
    (init dynapad-arg)
    (init object-arg)

    (init motion-cb-arg)
    (init release-cb-arg)
    (init lock-arg)
    (init-field _undo/redo-expr-fn)
    (init (minunits 0))
    (init (minpixels 0))

    (super-instantiate (dynapad-arg object-arg motion-cb-arg release-cb-arg lock-arg minunits minpixels))

    (define/override (Resize_ButtonPress evnt)
      (push-undo-op (_undo/redo-expr-fn))
      (super Resize_ButtonPress evnt))

    (define/override (Resize_ButtonRelease evnt)
      (super Resize_ButtonRelease evnt)
      (push-redo-op (_undo/redo-expr-fn))
      (push-ops-no-exec))
    ))

(define-syntax undoable-resizer-bindings (syntax-rules ()
                                           ((_ args ...) (instantiate undoable-resize-border--event-binder% (args ...)))))

#|
; resize-borders, example 1.
(define resizer-obj
  (ic (make-object rect% PAD '(-200 -200 100 100))
      (fill "none")
      (pen "#00aa44")
      (penwidth 5)))
;(make-object resize-border--event-binder% PAD resizer-obj)
(resizer-bindings PAD resizer-obj)

; resize-borders, example 2.
(define bck (make-object oval% PAD '(-200 -200 100 100)))
(define btn (make-object oval% PAD '(-250 50 -200 100)))
(define bar (make-object rect% PAD '(-200 100 100 150)))
(send* bck (fill "yellow"))
(send* btn (fill "red") (re-anchor "ne"))
(send* bar (fill "blue"))
(define (my-update-fnc obj bbox)
  (send bck coords bbox)
  (send btn position (list (b0 bbox) (b3 bbox) 1))
  (send bar coords (list (b0 bbox) (b3 bbox) (b2 bbox) (+ (b3 bbox) 50))))

(define resizer-obj
  (ic (make-object rect% PAD '(-200 -200 100 100))
      (fill "none")
      (pen "#00aa44")
      (penwidth 5)))

;(make-object resize-border--event-binder% PAD resizer-obj my-update-fnc)
(resizer-bindings PAD resizer-obj my-update-fnc)

|#


