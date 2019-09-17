; This module handles all forms of highlights
;  (C-level objects which are not dynaobject%s)
; including select% borders
; Every hilight is tied to a single dynaobject%,
;  although a dynaobject% may have multiple hilights.
; A hilight may be associated with a unique label (e.g. 'select)
;  Labels are compared using eq?; they may be symbols or objects
;  but not strings.

;Should these go here?
;(define *selection-layer* #f)
; if set to a layer, objs move to that layer while selected
;(define *selection-layer-filter-fn* (lambda (o) #t))
; used to block certain objs from moving to selection layer

; hilight base class...
(define hilight%
  (class object%
    (init-field _dynapad _object (_label #f))
    (field (_cptr #f) (_writable #f) (_deleted? #f))
    (public (mybind bind) base-bb delete get-cptr update writable?)

    (define (get-cptr) _cptr)
    
;    (define/public (lower target)
;      (sch_lower _cptr (send target get-cptr)))
    
    (define (delete)
      (get-and-rem-from-malist! assq remq this _object hilights)
;      (send _object hilights 'remove this)
      (set! _deleted? #t)
      (sch_delete _cptr))

    (define (base-bb . args) ;ignore args; may be overridden by subclass
      (send _object bbox))

    (define (update . args) ;ignore args
      (when (not _deleted?)
        (sch_coords _cptr (send/apply this base-bb args))))
;    (sch_raise _cptr (send _object get-cptr))
;        (if (send _object layer) ; only copy layer value if not #f
;          (when (not (eq? (sch_layer _cptr) (send _object layer)))
;            (sch_layer _cptr (send (send _object layer) get-cptr))))))

    (define (mybind . args)
      (apply bind this args))

    (define writable?
      (case-lambda
        (() _writable)
    ((bool) (set! _writable bool))))
    
    (define sticky
      (case-lambda
        (() (sch_sticky _cptr))
    ((newsticky) (sch_sticky _cptr newsticky))))

    (define/public (sch-makeobj-fn) sch_makerect) ;may be overrridden

    (super-instantiate ())
    (set! _cptr ((sch-makeobj-fn) (send _dynapad get-cptr) this))
    (sch_findable _cptr #f)
    (sch_events _cptr #f)
    (sch_penwidth _cptr 0)
    (sticky (send _object sticky))
    (if _label
    (send _object hilights 'add this _label)
    (send _object hilights 'add this))
    ; put all hilights on select-layer:
    ; RECONSIDER THIS...
    (sch_layer _cptr (send (send _dynapad getvar 'select-layer) get-cptr))

    (update)))
    
;(define rect-hilight%
;  (class hilight%
;     (init dynapad-arg object-arg (label-arg #f))
;     (inherit-field _dynapad _object _cptr)
;     (inherit-field _sch-makeobj-fn)
;     (set! _sch-makeobj-fn sch_makerect)
;     (super-instantiate (dynapad-arg object-arg label-arg))
;    ))

(define concentric-hilight%
; wraps around other hilights so all remain visible
  (class hilight%
     (init dynapad-arg object-arg (label-arg #f))
     (inherit-field _dynapad _object _cptr _deleted?)
     (field (_last-n 0) (_spacing 2))
     (super-instantiate (dynapad-arg object-arg label-arg))

     (define/override (base-bb . n)
 ; optional n means this is nth hilight of _object
       (set! n (if (null? n) _last-n (cset! _last-n (car n))))
       (bbwiden (send _object bbox) (/ (* _spacing n)
                       (send _dynapad getzoom))))

     (send _object update-any-hilights)
))

(define oval-hilight%
  (class concentric-hilight% ; hilight%
     (init dynapad-arg object-arg (label-arg #f))
     (inherit-field _dynapad _object _cptr _deleted?)
     (super-instantiate (dynapad-arg object-arg label-arg))

     (define/override (sch-makeobj-fn) sch_makeoval)

))

(define (hilight-diagonal-fn bb)
;  (bb-diag bb) ;nicer for rects, but slower
;OR
   (max (bbheight bb) (bbwidth bb)) ;faster, also nicer for ovals
)

(define ring-hilight%
  (class oval-hilight%
    (init dynapad-arg object-arg (label-arg #f))
    (inherit-field _dynapad _object _cptr _deleted?)
    ; cache radius to avoid recomputing when _object doesnt resize
    (field (_dx 0) (_dy 0) (_r 0))

    (define/public (update-cache . n)
      (let* ((bb (send/apply this base-bb n))
         (dx (bbwidth bb))
         (dy (bbheight bb)))
    (unless (and (= dx _dx) (= dy _dy))
        (set! _dx dx)
        (set! _dy dy)
        (set! _r (* .5 (hilight-diagonal-fn bb))))
        ;(let ((dx2 (* dx .5))
        ;      (dy2 (* dy .5)))
        ;  (set! _r (sqrt (+ (* dx2 dx2) (* dy2 dy2))))))
    bb ;return bb for caller to reuse
    ))

    (define/override (update . n)
      (when (not _deleted?)
        (let* ((bb (send/apply this update-cache n))
           (cx (bxc bb))
           (cy (byc bb))
           (crds (list (- cx _r) (- cy _r) (+ cx _r) (+ cy _r))))
          (sch_coords _cptr crds))))

    (super-instantiate (dynapad-arg object-arg label-arg))

))

(define halo-hilight%
;a ring that grows to keep one edge in view always
  (class ring-hilight%
    (init dynapad-arg object-arg (label-arg #f))
    (inherit-field _dynapad _object _cptr _deleted?)
    (inherit-field _r)

    (field (_last-caller #f))

    (define (inner-update . n)
      (let* ((obj-bb (send/apply this update-cache n))
         (vbb (send _dynapad bbox))
         (cx (bxc obj-bb))
         (cy (byc obj-bb))
         (R  (if (bbenclosedoron cx cy vbb)
            _r
            (let* ((vc (bbcenter vbb))
               (axle (append (list cx cy) vc))
               (fract (bb-crossing-fract axle vbb))
               (dist-from-vc (bb-diag axle))
               (dist-from-edge (* (- 1 fract) dist-from-vc))
               )
              (+ (* fract _r) dist-from-edge)))))
;               (r (/ _r (send dynapad getzoom))))
;              (+ (min r 20) dist-from-edge)))))
    (sch_coords _cptr
            (list (- cx R) (- cy R) (+ cx R) (+ cy R)))))

    (super-instantiate (dynapad-arg object-arg label-arg))

    (sch_renderscript _cptr this
       (lambda (halo)
     ;avoid invoking inner-update again if update already did
     (unless (eq? _last-caller 'update)
         (inner-update))
     (set! _last-caller 'renderscript)
     (sch_renderitem _cptr)))

    (define/override (update . n)
    (apply inner-update n)
    (set! _last-caller 'update))
))

(define select%
  (class ring-hilight%
;  (class halo-hilight%
    (init dynapad-arg object-arg)
    (inherit-field _dynapad _object _cptr)

    (define/override (delete)
      (send _dynapad del-selected _object)
      (super delete))

    (super-instantiate (dynapad-arg object-arg 'select))
    ((send _dynapad select-render-script) _cptr _object)
    ;(sch_layer _cptr (send (send _dynapad getvar 'select-layer) get-cptr)) ;moved to hilight% superclass
    (send _dynapad add-selected _object)
    ))
