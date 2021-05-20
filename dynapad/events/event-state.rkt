#lang racket/base

(require racket/class
         (only-in dynapad/base
                  dynapad%
                  line%
                  polygon%
                  rect%)
         (only-in dynapad/pad-state
                  dynapad
                  event-sx
                  event-sy
                  event-x
                  event-y
                  )
         (only-in dynapad/container
                  get-container
                  )
         (only-in dynapad/misc/misc
                  public-field
                  get/set
                  sendf
                  list-intersect
                  def
                  ic
                  )
         (only-in dynapad/layout/bbox
                  bbcenter
                  )
         (only-in dynapad/menu/wxmenu
                  add-submenu
                  add-checkable-menu-item
                  )
         (only-in dynapad/libdynapad-wrapper
                  sch_xy_in_poly)
         )

(provide event-state%)

(define *default-use-lasso?* #t)

(define event-state%
  (class object%
    (init-field _pad)
    (super-instantiate ())
    (send _pad evs this)

    (public-field lasso? _lasso?)
    (lasso? *default-use-lasso?*)
    ; lasso? and box? are exclusive
    (define/public box?
      (case-lambda
        (() (not (lasso?)))
        ((bool) (lasso? (not bool)))))

    (public-field obj0 _obj0) ;object receiving B1-down

    (public-field x0 _x0)
    (public-field y0 _y0)

    (public-field lastx _lastx)
    (public-field lasty _lasty)

    (define/public (pad) _pad)

    (define/public set-last-xy (case-lambda
                                 ((xy) (set-last-xy (car xy) (cadr xy)))
                                 ((x y) (set! _lastx x) (set! _lasty y))
                                 ))

    (public-field sx0 _sx0)
    (public-field sy0 _sy0)

    ;    (public-field lastsx _lastsx)
    ;    (public-field lastsy _lastsy)

    ;    (define/public set-last-sxy (case-lambda
    ;      ((sxy) (set-last-sxy (car sxy) (cadr sxy)))
    ;      ((sx sy) (set! _lastsx sx) (set! _lastsy sy))
    ;    ))

    (define/public set-sx0sy0 (case-lambda
                                ((sxsy) (set-sx0sy0 (car sxsy) (cadr sxsy)))
                                ((sx sy) (set! _sx0 sx) (set! _sy0 sy))
                                ))

    (define/public (moved-far-enough? e)
      (or (> (abs (- _sx0 (event-sx e))) 20)
          (> (abs (- _sy0 (event-sy e))) 20)))

    ;--- some select rect utilities --------------

    (public-field selector _selector) ;rect or lasso

    (define/public (update-selector e)
      (when _selector
        (send _selector update e))
      _selector)

    (define/public (selector-contains)
      (and _selector
           (send _selector contained-objects _obj0)))

    (define/public (selector-delete)
      (when _selector (send _selector delete))
      (set! _selector #f))

    (define/public (if-not-exists-make-selector-from-lastxy)
      (when (not _selector)
        (set! _selector
              (if (lasso?)
                  (make-object lasso%      _pad _lastx _lasty _sx0 _sy0)
                  (make-object selectrect% _pad _lastx _lasty)))
        ))

    (public-field selector-color _selector-color "white")
    ))

(define selectrect%
  (class rect%
    (init _dp)
    (init-field _x0 _y0) ;initial corner
    (super-instantiate(_dp))
    (inherit-field _dynapad)
    (send this findable #f)
    (send this pen (or (sendf _dynapad evs selector-color) "white"))
    (send this fill "none")

    (send this save-coords (list _x0 _y0))

    (field (_lastdx 0) (_lastdy 0))
    (field (_switched? #f))
    (define/public (switched?) _switched?)

    (define/public (update evnt)
      (let* ((x (event-x evnt))
             (y (event-y evnt))
             (dx (abs (- x _x0)))
             (dy (abs (- y _y0))))
        (unless _switched? ;if not already switched, check
          (set! _switched? (or (< dx (* .5 _lastdx))
                               (< dy (* .5 _lastdy)))))
        (set! _lastdx (max _lastdx dx))
        (set! _lastdy (max _lastdy dy))

        (send this coords (append (send this recall-coords) (list x y)))
        ))

    (define (default-containment-fn backgnd-obj)
      (if (is-a? backgnd-obj dynapad%)
          (send _dynapad find 'enclosed (send this bbox))
          ;else maybe container
          (let* ((outer (get-container backgnd-obj))
                 (locals (and outer (send outer contents)))
                 (bounded (and locals (send _dynapad find 'groupmembers 'enclosed (send this bbox)))))
            (if bounded
                (list-intersect locals bounded memq)
                null))))

    (public-field containment-fn _containment-fn)
    (containment-fn default-containment-fn)

    (define/public (contained-objects obj)
      (and _containment-fn
           (_containment-fn obj)))

    ;(if *selection-layer* (send this layer *selection-layer*))
    (send this dynaclass 'selectrect%)))

(define lasso%
  (class line%
    (init _dp _x0 _y0)
    (init-field _lastsx _lastsy)
    (super-instantiate(_dp))
    (inherit-field _dynapad)
    (send this findable #f)
    (send this pen (or (sendf _dynapad evs selector-color) "white"))

    (field (_minjump 5)) ;min spacing between points (reduces vtx density)

    (send this save-coords (list _x0 _y0))
    (define/public (update evnt)
      (let ((sx (event-sx evnt))
            (sy (event-sy evnt))
            (x  (event-x evnt))
            (y  (event-y evnt)))
        (when ;moved far enough?
            (or (> (abs (- sx _lastsx)) _minjump)
                (> (abs (- sy _lastsy)) _minjump))
          (set! _lastsx sx)
          (set! _lastsy sy)
          (send this save-coords
                (append (send this recall-coords) (list x y))))
        ; always:
        (send this coords
              (append (send this recall-coords) (list x y)))))

    (define (in-poly? obj poly)
      (def bbc (bbcenter (send obj bbox)))
      (sch_xy_in_poly (car bbc) (cadr bbc) (send poly get-cptr)))

    (define (objects-in-poly backgnd-obj)
      (let* ((poly (ic (make-object polygon% _dynapad (send this recall-coords))
                       (fill "none")
                       (pen (send this pen))
                       (penwidth (send this penwidth))
                       (findable #f)))
             (bb (send poly bbox))
             (keep
              (if (is-a? backgnd-obj dynapad%)
                  (let ((bounded (send _dynapad find 'overlapping bb)))
                    (filter (lambda (it) (in-poly? it poly)) bounded))
                  ;else other object-- perhaps container?
                  (let* ((outer (get-container backgnd-obj))
                         (locals (and outer (send outer contents)))
                         (bounded (and locals (send _dynapad find 'groupmembers 'overlapping bb)))
                         (polybounded (and bounded (filter (lambda (it) (in-poly? it poly)) bounded))))
                    (if polybounded
                        (list-intersect locals polybounded memq)
                        null)))))
        (send poly delete)
        keep))

    (define default-containment-fn
      (lambda (obj) (objects-in-poly obj)))

    (public-field containment-fn _containment-fn)
    (containment-fn default-containment-fn)

    (define/public (contained-objects obj)
      (and _containment-fn
           (_containment-fn obj)))

    ;(if *selection-layer* (send this layer *selection-layer*))
    (send this dynaclass 'lasso%)))

;
; File selector
;
(define (make-submenu-Selector mb object)
  (unless object
    (let* ((sb (add-submenu mb "Selector")))
      (add-checkable-menu-item sb "Box"
                               (lambda (i) (sendf dynapad evs box? (send i is-checked?)))
                               (sendf dynapad evs box?))
      (add-checkable-menu-item sb "Lasso"
                               (lambda (i) (sendf dynapad evs lasso? (send i is-checked?)))
                               (sendf dynapad evs lasso?))
      )))

(define combo-event-state% ;combines bbox and lasso
  (class event-state%
    (init _dp)
    (super-instantiate (_dp))
    (inherit-field _pad _obj0 _lastx _lasty _sx0 _sy0)

    ; here, lasso? and box? are independent, but one must be #t
    (field (_box? #t))
    (define/override box?
      (case-lambda
        (() _box?)
        ((bool) (set! _box? bool)
                (unless bool (lasso? #t)))))

    (inherit-field _lasso?)
    (define/override lasso?
      (case-lambda
        (() _lasso?)
        ((bool) (set! _lasso? bool)
                (unless bool (box? #t)))))
    (lasso? #t)

    (define/override (selector . args) #f)
    ;(public-field selector _selector) ;rect or lasso
    (field (_box #f)
           (_lasso #f))
    (field (_box-gone? #f))

    (define/override (update-selector e)
      (when _box
        (send _box update e))
      (when _lasso
        (send _lasso update e))
      (when (and _lasso _box (send _box switched?))
        (send _box delete)
        (set! _box-gone? #t)
        (set! _box #f))
      (or _box _lasso))

    (define/override (selector-contains)
      (cond ;_box gets priority
        (_box   (send _box contained-objects _obj0))
        (_lasso (send _lasso contained-objects _obj0))
        (else #f)))

    (define/override (selector-delete)
      (when _box (send _box delete) (set! _box #f))
      (set! _box-gone? #f)
      (when _lasso (send _lasso delete) (set! _lasso #f))
      )

    (define/override (if-not-exists-make-selector-from-lastxy)
      (unless _box-gone?
        (when (and _box? (not _box))
          (set! _box (make-object selectrect% _pad _lastx _lasty))))
      (when (and _lasso? (not _lasso))
        (set! _lasso (make-object lasso%    _pad _lastx _lasty _sx0 _sy0)))
      )
    ))
