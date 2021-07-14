#lang racket/base

;; FIXME NOTE TODO breaking this out into its own file did something really weird to gtk

;; Gtk: _gtk_widget_captured_event: assertion 'WIDGET_REALIZED_FOR_EVENT (widget, event)' failed

;; prints twice, when right clicking in the pad window sometimes,
;; seems correlated to having made modifications to the files without
;; running racocgc make again, not sure what is going on

(provide graph-edge%
         graph-arc%
         xycenter
         )

(require racket/class
         dynapad/ffs
         dynapad/misc/misc ; ic item config
         dynapad/layout/bbox
         dynapad/history/ids
         (only-in dynapad/dynapad-c-api slowgrow)
         (only-in dynapad/base oval% line% polygon%)
         )

(define (xycenter obj) (bbcenter (send obj bbox)))

;================

(define graph-edge%
  (class line%
    (init _dynapad)
    (init-field (_from #f) (_to #f))
    (field (_trunc #t))
    (inherit dynaclass)
    (super-instantiate (_dynapad))
    (dynaclass 'graph-edge%)
    ;(send this findable #f)
    (send this penwidth 0)
    (send this maxsize -1)

    (define/public (to-tip trunc?)
      (cond ((not _to) #f)
            (trunc?
             (let* ((mycrds ;(send this coords))
                     (append (xycenter _from) (xycenter _to)))
                    (to-bb (send _to bbox)))
               (bb-crossing mycrds to-bb)))
            ;            (xfract (abs (/ (bbwidth to-bb)
            ;                    (* 2 (bbwidth mycrds)))))
            ;            (yfract (abs (/ (bbheight to-bb)
            ;                    (* 2 (bbheight mycrds)))))
            ;            (fract (min xfract yfract))
            ;        ;use car-cadddr instead of b0-b3 to override auto-correct
            ;            (tipx  (lerp fract (caddr mycrds) (car mycrds)))
            ;            (tipy  (lerp fract (cadddr mycrds) (cadr mycrds))))
            ;           (list tipx tipy)))
            (else (xycenter _to))))

    (define/public (from-tip trunc?)
      (cond ((not _from) #f)
            (trunc?
             (let* ((mycrds ;(send this coords))
                     (append (xycenter _to) (xycenter _from)))
                    (to-bb (send _from bbox)))
               (bb-crossing mycrds to-bb)))
            ;            (xfract (abs (/ (bbwidth to-bb)
            ;                    (* 2 (bbwidth mycrds)))))
            ;            (yfract (abs (/ (bbheight to-bb)
            ;                    (* 2 (bbheight mycrds)))))
            ;            (fract (min xfract yfract))
            ;        ;use car-cadddr instead of b0-b3 to override auto-correct
            ;            (tipx  (lerp fract (car mycrds) (caddr mycrds)))
            ;            (tipy  (lerp fract (cadr mycrds) (cadddr mycrds))))
            ;           (list tipx tipy)))
            (else (xycenter _from))))

    (define/public (update)
      (when (and _from _to)
        (send this coords (append (from-tip _trunc) (to-tip _trunc)))))

    (define (do-update . args)
      (send this update))

    (define (detach obj)
      (when obj
        (send obj afterslide-callbacks 'remove #f this)
        (send obj afterposition-callbacks 'remove #f this)
        (send obj delete-callbacks 'remove #f this)
        ))
    (define (attach obj)
      (when obj
        (ensure-id obj)
        (send obj afterslide-callbacks 'add do-update this)
        (send obj afterposition-callbacks 'add do-update this)
        (send obj delete-callbacks 'add (lambda (o) (send this delete)) this)
        ))
    (define (drop)
      (let ((lower-obj (cond ((not _from) _to)
                             ((not _to)  _from)
                             (else (car (sendf this dynapad order
                                               (list _from _to)))))))
        (when lower-obj
          (send this lower lower-obj))))

    (define/public from
      (case-lambda
        (() _from)
        ((obj) (detach _from) (set! _from obj) (attach _from) (drop) (update))))
    (define/public to
      (case-lambda
        (() _to)
        ((obj) (detach _to) (set! _to obj) (attach _to) (drop) (update))))


    (define/public fromto
      (case-lambda
        (() (list _from _to))
        ((f t) (from f)
               (to t))))

    (define/public trunc
      (case-lambda
        (() _trunc)
        ((val) (set! _trunc val)
               (update))))

    (define/override (delete)
      (detach _from)
      (detach _to)
      (super delete))

    (define/override (writeoptions)
      `(,@(super writeoptions)
        (trunc ,_trunc)
        ;(refer-when-ready 'fromto ,(send _from id) ,(send _to id))
        (defer-send '(fromto ,(export-objs _from) ,(export-objs _to)))
        ))

    (when _from (from _from))
    (when _to (to _to))
    ))

(define graph-arc%
  (class graph-edge%
    (init _dynapad (_from-arg #f) (_to-arg #f))
    (init-field (_head-type arrowhead%))
    (inherit-field _from _to)
    (inherit dynaclass)
    (field (_arrowhead (ic (make-object _head-type _dynapad)
                           (update))))

    (define/public (arrowhead) _arrowhead)

    (define/override (update . args)
      (super update)
      (when (and _from _to)
        (begin
          (send _arrowhead tip (send this to-tip #t))
          (send _arrowhead tail (xycenter _from))
          (send _arrowhead pen (send this pen))
          (send _arrowhead raise this)
          (send _arrowhead update))))

    (define/override (delete)
      (send _arrowhead delete)
      (super delete))

    (super-instantiate (_dynapad _from-arg _to-arg))
    (dynaclass 'graph-arc%)
    ))

;        c
;   ____b|\a   width = b->c (lateral)
;        |/    len   = a->b (along)   sweep = a->c (along)
;

(define vect->anchor
  ;returns the closest non-center anchor corresponding to vector (dx dy)
  (case-lambda
    ((dx dy) (vect->anchor dx dy 2))
    ((dx dy bias) ;bias (always >1) is max slope still considered diagonal
     ; i.e. bias > 2 --> favor diag; bias < 2 --> favor vert/horiz
     (let* ((ax (abs dx))
            (ay (abs dy))
            (horizontalish? (> ax ay))
            (diagonalish?   (if horizontalish?
                                (> (* bias ay) ax)
                                (> (* bias ax) ay))))
       (if diagonalish?
           (if (positive? dx)
               (if (positive? dy) "ne" "se")
               (if (positive? dy) "nw" "sw"))
           (if horizontalish?
               (if (positive? dx) "e" "w")
               (if (positive? dy) "n" "s")))))
    ))

(define arrowhead%
  (class polygon%
    (init _dynapad)
    (init-field (_tip #f) (_radius 10))
    (field (_tail #f))
    (inherit dynaclass)
    (super-instantiate (_dynapad))
    (dynaclass 'arrowhead%)
    (send this findable #f)
    (send this penwidth 0)
    (send this fill "black")
    (slowgrow this .3 2 .5)

    (define/public (update . args)
      ;      (send this renderitem)  ;Waiting for renderitem bug fix...
      (let ((rad (* (send this z) (radius))))
        (when _tip
          (let*((dx (- (car  _tip) (car  _tail)))
                (dy (- (cadr _tip) (cadr _tail)))
                (dd (/ 1.0 (sqrt (+ (* dx dx) (* dy dy)))))
                )
            (send this re-anchor (vect->anchor dx dy 3))
            (set! dx (* dx dd))
            (set! dy (* dy dd))

            (send this coords
                  (list
                   (car  _tip)
                   (cadr _tip)
                   (- (car  _tip) (* dx rad) (*  -0.4 dy  rad))
                   (- (cadr _tip) (* dy rad) (*   0.4 dx  rad))
                   (- (car  _tip) (* 0.8 dx rad))
                   (- (cadr _tip) (* 0.8 dy rad))
                   (- (car  _tip) (* dx rad) (*   0.4 dy  rad))
                   (- (cadr _tip) (* dy rad) (*  -0.4 dx  rad))
                   )))
          )))

    (define/public tip (get/set _tip))
    (define/public tail (get/set _tail))
    (define/public radius
      (case-lambda
        (() _radius)
        ((rad) (set! _radius rad)
               (update))))
    ))

(define diamondhead%
  ;always oriented upright
  (class polygon%
    (init _dynapad)
    (init-field (_tip #f) (_radius 5))
    (inherit dynaclass)
    (super-instantiate (_dynapad))
    (dynaclass 'diamondhead%)
    (send this findable #f)
    (send this penwidth 0)
    (send this fill "black")
    (slowgrow this .3 2 .5)

    (define/public (update . args)
      (let ((rad (* (send this z) (radius))))
        (when _tip
          (send this coords
                (list
                 (car  _tip)          (+ (cadr _tip) rad)
                 (+ (car  _tip) rad)  (cadr _tip)
                 (car  _tip)          (- (cadr _tip) rad)
                 (- (car  _tip) rad)  (cadr _tip)
                 )))))

    (define/public tip (get/set _tip))
    (define/public (tail . args) #f) ;unneeded
    (define/public radius
      (case-lambda
        (() _radius)
        ((rad) (set! _radius rad)
               (update))))
    ))

(define roundhead%
  (class oval%
    (init _dynapad)
    (init-field (_tip #f) (_radius 4))
    (inherit dynaclass)
    (super-instantiate (_dynapad))
    (dynaclass 'roundhead%)
    (send this findable #f)
    (send this penwidth 0)
    (send this fill "black")
    (slowgrow this .3 2 .5)

    (define/public (update . args)
      (let ((rad (* (send this z) (radius))))
        (when _tip
          (send this coords
                (list (- (car _tip)  rad)
                      (- (cadr _tip) rad)
                      (+ (car _tip)  rad)
                      (+ (cadr _tip) rad))))))

    (define/public tip (get/set _tip))
    (define/public (tail . args) #f) ;unneeded
    (define/public radius
      (case-lambda
        (() _radius)
        ((rad) (set! _radius rad)
               (update))))
    ))

;(define arc-hilight%
;  (class hilight%
;    (init dynapad-arg object-arg (label-arg #f))
;    (inherit-field _dynapad _object _cptr _deleted?)
;    (super-instantiate (dynapad-arg object-arg label-arg))
;    (define-override
