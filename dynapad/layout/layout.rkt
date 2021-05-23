

(dynaload "tools-cmp.rkt")
(dynaload "tools-misc.rkt")
(dynaload "tools-lists.rkt")
(dynaload "animate.rkt")
(dynaload "lerp.rkt")
(require (lib "etc.rkt")) ;needs (identity...)

;==================== Padded BBoxes =========

(define padded-bbox%
  (class object%
    (init-field _outer-bb)
    (init (_x-pad-arg 0)
          (_y-pad-arg _x-pad-arg))
    ; treat args as fixed sizes if numeric,
    ;  or fractions of outer-bb if strings (e.g. ".xx" or "xx%")
    ;  neither margin may exceed ".5"
    (field (_inner-bb #f))
    (field (_x-margin-fract #f)
           (_y-margin-fract #f)
           (_x-margin #f)
           (_y-margin #f))
    (super-instantiate ())
    (send this x-margin _x-pad-arg)
    (send this y-margin _y-pad-arg)

    (define (refresh)
      (when _x-margin-fract
        (set! _x-margin (* _x-margin-fract (bbwidth _outer-bb))))
      (when _y-margin-fract
        (set! _y-margin (* _y-margin-fract (bbheight _outer-bb))))
      (set! _inner-bb
            (list (min (+ (b0 _outer-bb) _x-margin) (bxc _outer-bb))
                  (min (+ (b1 _outer-bb) _y-margin) (byc _outer-bb))
                  (max (- (b2 _outer-bb) _x-margin) (bxc _outer-bb))
                  (max (- (b3 _outer-bb) _y-margin) (byc _outer-bb)) )))

    (define/public (inner-bb)
      (when (not _inner-bb) (refresh))
      _inner-bb)
    (define/public outer-bb
      (case-lambda
        (() _outer-bb)
        ((newbb) (set! _inner-bb #f) ;forces refresh
                 (cset! _outer-bb newbb))))

    (define/public x-margin
      (case-lambda
        (() (when _x-margin-fract (number->string _x-margin-fract)))
        ((newval)
         (if (string? newval)
             (set! _x-margin-fract (min .5 (pct-string->number newval)))
             (begin
               (set! _x-margin newval)
               (set! _x-margin-fract #f)))
         (set! _inner-bb #f)) ))

    (define/public y-margin
      (case-lambda
        (() (when _y-margin-fract (number->string _y-margin-fract)))
        ((newval)
         (if (string? newval)
             (set! _y-margin-fract (min .5 (pct-string->number newval)))
             (begin
               (set! _y-margin newval)
               (set! _y-margin-fract #f)))
         (set! _inner-bb #f)) ))

    (define/public margins
      (case-lambda
        (()    (list (x-margin) (y-margin)))
        ((val) (margins val val))
        ((x y) (x-margin x)
               (y-margin y))))

    (define/public (fit-to-obj obj)
      (if obj
          (send this outer-bb (send obj bbox))
          #f))
    ))

;================= PICKY STACKS =============
(define picky-stack%
  ; a stack which pushes new cells only if (accept-fn new stack) is met
  ; returns (non-#f) _reply if accepted, #f otherwise
  (class object%
    (init-field _reply)
    (init-field _accept-fn)
    (field (_list null))
    (super-instantiate ())

    (define/public (maybe-push item)
      (if (_accept-fn item _list)
          (begin
            (push! item _list)
            _reply)
          #f))
    ))

(define picky-cascade%
  ; Series of picky-stacks, where failed pushes flow to next stack:
  ; |  0:XXXX
  ; |  1:XXX
  ; V  2:XXXXXX
  ; ...
  ; When a newcell (X) is added, it goes onto first stack which
  ;  accepts newcell (as per [_accept-fn newcell car])
  ; If no existing stack accepts, create new stack and try one last time
  (class object%
    (init-field _accept-fn) ;(lambda (new lst)...
    (field (_stacks null))
    (super-instantiate ())

    (define/public (accomodate cell) ;returns idx of accepting stack
      (define (try-push) ;returns list (cascade-tail result)
        (first-valid _stacks
                     (lambda (stack)
                       (send stack maybe-push cell))))
      (let* ((stack+idx (try-push)))
        (if (not stack+idx)
            (let ((new-stack (make-object picky-stack%
                                          (length _stacks) ;_reply is stack's idx
                                          _accept-fn)))
              (set-append! _stacks (list new-stack))
              (send new-stack maybe-push cell))
            (cadr stack+idx)))) ;return reply of accepting stack
    ))


;========= Layout Cells =======
(define-macro (coord-n var)
  `(case-lambda
     ((n) (car (auto-extend-list-tail ,var n #f)))
     ((n val) (set-car! (auto-extend-list-tail ,var n #f) val))))

(define layout-cell%
  ;a layout cell is essentially an annotatable cons cell--
  ;  one member of a linked list (or mesh, if >1 dimens.)
  ;  which locally caches not only the cell content but
  ;  metadata about the cell's relation to the mesh (e.g. coordinates)
  (class object%
    (init-field _obj)               ; cell contents
    (field (_i-coords null))      ; relative coords; set via (renumber)...
    (field (_v-coords null))      ; obj's val; set via parameter access-fn
    (field (_q-coords null))      ; obj's quant; set via parameter quantify-fn
    (field (_x-coords null))      ; obj's actual layout position

    (super-instantiate ())

    (define/public i-coords (get/set _i-coords))
    (define/public i-coord-n (coord-n _i-coords))

    (define/public v-coords (get/set _v-coords))
    (define/public v-coord-n (coord-n _v-coords))

    (define/public q-coords (get/set _q-coords))
    (define/public q-coord-n (coord-n _q-coords))

    (define/public x-coords (get/set _x-coords))
    (define/public x-coord-n (coord-n _x-coords))

    (define/public obj (get/set _obj))
    ))

(define cell-row%
  ;1-dimensional cell mesh-- essentially a list which can sort and index itself
  (class object%
    (init-field _param)    ; parameter which characterizes row sorting
    (init-field (_idx #f)) ;may be used as row or col #, if part of a multi-row mesh
    (init-field (_axis 0)) ;which coord (offset of cells' coord list)
    ;corresponds to this row
    ;in 2D: 0 for row, 1 for column
    (field (_cells null))
    (field (_idx-fn  ;fn to get/set the intra-row coord of a cell
            (lambda (cell . args)
              (send/apply cell v-coord-n _axis args))))
    (super-instantiate ())

    (define/public index (get/set _idx))

    (define/public (assimilate cell)  ;obj could be content or layout-cell
      (when (not (is-a? cell layout-cell%))
        ;no cell provided, assume cell is obj, mesh is 1-D; generate single coord
        (let ((obj cell))
          (set! cell (make-object layout-cell% obj))
          (send cell v-coords (list ((send _param access-fn) obj)))))
      (let ((cmp-fn (send _param compare-fn)))
        (insert-into-sorted-list _idx-fn cmp-fn cell _cells)))

    (define/public param (get/set _param))
    (define/public (map-cells fn)
      (map fn _cells))
    (define/public (contents)
      (map (lambda (cell) (send cell obj)) _cells))
    (define/public (for-each-cell fn)
      (for-each fn _cells))
    (define/public (remove-cell cell)
      (set! _cells (remv cell _cells)))
    (define/public (find-cells fn)
      ;returns list of all _cells for which (fn cel) is #t
      (filter fn _cells))

    (define/public (remove obj)
      ;removes all objects which contain obj
      (let ((cells (find-cells (lambda (cel) (eq? (send cel obj) obj)))))
        (for-each (lambda (cel) (send this remove-cell cel)) cells)))

    (define/public (renumber . crds)
      ;renumbers relative coords of cells
      ; if crds provided, uses it as template for all coords, iterating only
      ; on this axis
      (let* ((count 0)
             (renumber-fn (lambda (cell)
                            (when (not (null? crds))
                              (send cell i-coords (car crds))) ;set all coords using template
                            (send cell i-coord-n _axis (_++ count)))))
        (for-each renumber-fn _cells)
        count))

    (define/public (resolve) #t)
    ; (renumber)
    ))


; ================= Layout Projectors ==============
(define (attach-projector-to-obj obj proj-class . params)
  (let* ((rgn  (get-rgn obj))
         (proj (get-actor-named obj projector-actor-name))
         (reuse-objs (send rgn contents))
         (reuse-targets (and proj (send (send proj cells) contents)))
         (new-proj (apply make-object proj-class rgn params))
         (new-cells (send new-proj cells))
         (new-filter (send new-proj filter-fn)))
    (when proj (send proj delete))
    (when reuse-targets
      (foreach reuse-targets (lambda (o) (send new-cells assimilate o))))
    ;now reject any region contents which don't pass new filter,
    ;  which should also prevent their reentry
    (let ((rejects (filter (lambda (o) (not (new-filter o)))
                           reuse-objs)))
      (send rgn abandon rejects)
      (send rgn finish)) ;should be same as (send new-proj render)

    new-proj
    ))

(define projector-actor-name 'projector)

(define projector%
  ; Responsible for mapping cells onto actual positions within region (or bbox)
  (class named-actor%
    (init _rgn)
    (init-field _cells)     ; a cell-mesh object
    (field (_dynaclass 'projector%))
    (super-instantiate ())

    (when _rgn
      (let ((obj (send _rgn object)))
        (when (and obj
                   (is-a? obj dynaobject%))
          (send this attach-to obj projector-actor-name)
          (send obj post-build-ops 'add
                (lambda (o label) `(attach-projector-to-obj ,label
                                                            ,(send this dynaclass)
                                                            ,@(map (lambda (p) (send p name))
                                                                   (send this all-params))))
                this))
        (send _rgn enter-actions 'add
              (lambda (obj) (send _cells assimilate obj)) this)
        (send _rgn leave-actions 'add
              (lambda (obj) (send _cells remove obj)) this)
        (send _rgn finish-actions 'add
              (lambda () (send this render)) this)))

    (define/public dynaclass (get/set _dynaclass))

    (define/override (delete)
      (let* ((obj (send this object))
             (rgn (and obj (get-rgn obj))))
        (send obj post-build-ops 'remove #f this)
        (when rgn
          (send rgn enter-actions 'remove #f this)
          (send rgn leave-actions 'remove #f this)
          (send rgn finish-actions 'remove #f this))
        (super delete)))

    (define/public (cells) _cells)

    (define (add-one obj)
      (send _cells assimilate obj))
    (define/public (add obj)
      (if (list? obj)
          (for-each add-one obj)
          (add-one obj))
      (send this render))

    (define (remove-one obj)
      (send _cells remove obj))
    (define/public (remove obj)
      (if (list? obj)
          (for-each remove-one obj)
          (remove-one obj))
      (send this render))

    (define/public (render)
      (error "projector% subclass must overwrite method: render"))
    ))

(define rectangular-projector%
  ;includes tools for projecting into a padded rectangle
  (class projector%
    (def _bbox)
    (init _rgn _cells) ;_rgn may be a bbox instead
    (field (_cell-width 1) (_cell-height 1)) ;init vals should be non-zero
    (if (list? _rgn)
        (begin  ;given bbox instead of region
          (set! _bbox _rgn)
          (set! _rgn #f))
        (let ((bound (send _rgn pane)))
          (set! _bbox (if (list? bound) bound (send bound bbox)))))
    (super-instantiate (_rgn _cells))
    (field (_padded-bbox (make-object padded-bbox% _bbox)))
    (send this dynaclass 'rectangular-projector%)

    (define/public bbox
      (case-lambda
        (() (send _padded-bbox inner-bb))
        ((bb) (send _padded-bbox outer-bb bb))))
    (define/public (refresh-bb obj)
      (send _padded-bbox fit-to-obj obj))
    (define/public (margins . args) (send/apply _padded-bbox margins args))

    (define/public (set-cell-dimensions cell) ;assumes all contents same size
      (let ((obj (send cell obj)))
        (let* ((w (send obj width))
               (h (send obj height))
               (d (max w h)))
          (set! _cell-width d)
          (set! _cell-height d))))
    (define/public cell-width  (get/set _cell-width))
    (define/public cell-height (get/set _cell-height))
    ))

;weight of interpolation between 0 (i=ordinal) and 1 (q=quantitative)
(define *default-iq-weight* .3)

;Animated version:
(define set-obj-xy
  (case-lambda
    ;no target; x,y are absolute coords
    ((obj x y) (set-obj-xy obj x y #f))
    ; x,y are target-relative
    ((obj x y target)
     (def animation-time 1500) ;1.5 seconds
     ;(send obj anchor "center")
     (let ((stick (send obj sticky)))
       ;workaround for sticky-bug:
       ; must unstick during move to prevent reversion to old stuck posn
       (when stick
         (begin
           (send obj sticky #f)
           (send obj xy x y)
           (send obj sticky stick)))
       (if (not target)
           (animate-to obj (list x y) animation-time)
           ; if target obj, animation must track it
           (let* ((xy (send target position))
                  (dx (- x (car xy)))
                  (dy (- y (cadr xy))))
             (animate-to obj
                         (lambda () (let ((xy (send target position)))
                                      (list (+ (car xy) dx)
                                            (+ (cadr xy) dy))))
                         animation-time)
             ))))
    ))

; Simple, non-animated version:
;(define (set-obj-xy obj x y)
;  (send obj xy x y))

;(define (refresh-if-linear-proj obj)
;  (let ((proj (get-actor-named obj 'linear-projector)))
;    (if proj
;    (send proj render))))
(define (SetSelectedLinearProjectorWeight wt)
  (set! *default-iq-weight* wt)
  (let ((linear-projs (map (lambda (o) (get-actor-named o 'linear-projector))
                           (send dynapad selected))))
    (foreach linear-projs
             (lambda (p) (when p
                           (send p iq-weight wt)
                           (send p render))))))

(define (SetProjectorForSelected proj-class)
  (let ((objs (filter (lambda (o) (get-actor-named o projector-actor-name))
                      (send dynapad selected))))
    (foreach objs (lambda (o)
                    (let* ((proj (get-actor-named o projector-actor-name))
                           (param (send proj param)))
                      (attach-projector-to-obj o proj-class param))))))

(define (SetParamForSelectedProjectors param)
  (let ((projs (map (lambda (o) (get-actor-named o projector-actor-name))
                    (send dynapad selected))))
    (foreach projs (lambda (p) (when p (send p param param))))))


(define 1D-projector%
  (class rectangular-projector%
    (init _rgn)
    (init-field _param)
    (inherit-field _cells)
    (super-instantiate (_rgn (make-object cell-row% _param)))
    (send this dynaclass '1D-projector%)

    (when _rgn
      (send _rgn filter-fn 'add-last (send _param access-fn) this))
    (let ((obj (send this object)))
      (when obj (add-object-menu make-popup-menu-for-1D-projector obj)))

    (define/public (filter-fn)
      (send _param access-fn))

    (define/public (all-params) (list _param))
    #|
    (define/public param
    (case-lambda
    (() _param)
    ((newparam)
    (let* ((rgn (get-rgn (send this object)))
    (curr-objs (send rgn contents))
    (newfilter (send newparam access-fn)))
    (send rgn abandon curr-objs) ;clear out region
    (send rgn filter-fn 'remove #f this)
    (send rgn filter-fn 'add-last newfilter this)
    (set! _cells (make-object cell-row% newparam))
    (set! _param newparam)
    (send rgn contents curr-objs) ;replace former objects
    (send rgn finish)
    ))))
    |#
    (define/public param
      (case-lambda
        (() _param)
        ((newparam)
         (let* ((rgn (get-rgn (send this object)))
                (curr-objs (send rgn contents))
                (newfilter (send newparam access-fn))
                (reuse (send _cells contents)))
           (send rgn filter-fn 'remove #f this)
           (send rgn filter-fn 'add-last newfilter this)
           (set! _cells  (make-object cell-row% newparam))
           (set! _param newparam)
           (foreach reuse (lambda (o) (send _cells assimilate o)))
           ;now reject any region contents which don't pass new filter,
           ;  which should also prevent their reentry
           (let ((rejects (filter (lambda (o) (not (newfilter o)))
                                  curr-objs)))
             (send rgn abandon rejects)
             (send rgn finish)) ;should be same as (send this render)
           ))))
    ))


(define linear-projector%
  (class 1D-projector%
    (init _rgn)
    (init _param)
    (inherit-field _cells)
    ;linear functions to map
    ; quantified param (q) OR relative idx (i) -> position (x)
    (field (_q-slope 1) (_q-intercept 0))
    (field (_i-slope 1) (_i-intercept 0))
    (field (_y-base 0) (_y-step 0))
    (field (_max-j 0))
    (field (_y-stacks #f)) ; each y-stack corresponds to a row (some y val)
    ; cells "cascade" from bottom row upward until no collision
    (field (_iq-weight *default-iq-weight*))

    (super-instantiate (_rgn _param))
    (send this dynaclass 'linear-projector%)
    (when (is-a? _rgn region%)
      (send this attach-to (send _rgn object) 'linear-projector))
    ;additional handle for regionized projs
    (send this margins "10%")

    ; _cells are indexed by i (relative/ordinal) and q (quantified)
    ;  both scales are mapped onto the region's x-range
    ;     (encoded with i/q-slope, i/q-intercept)
    ;  and then interpolated
    (define (compute-cell-x cell)
      (let ((x (send cell x-coord-n 0)))
        (when (not x)
          (let ((i (send cell i-coord-n 0))
                (q (send cell q-coord-n 0)))
            (when (not q)
              (let* ((qnt-fn (send (send this param) quantify-fn))
                     (v    (send cell v-coord-n 0)))
                (set! q (qnt-fn v))
                (send cell q-coord-n 0 q)))
            (let* ((xq   (+ (* _q-slope q) _q-intercept))
                   (xi   (+ (* _i-slope i) _i-intercept)))
              (set! x (lerp _iq-weight xi xq))
              (send cell x-coord-n 0 x))))
        x))

    (define/public iq-weight (get/set _iq-weight))

    (define (prepare)
      (set! _y-stacks #f)
      (let ((len   (send _cells renumber))
            (obj   (send this object)))
        (send this refresh-bb (send obj frame))  ;refresh bbox if regionized
        (when (positive? len)
          (let* ((bb  (send this bbox))
                 (dx  (bbwidth bb))
                 (x0  (b0 bb))
                 (i1  (- len 1))
                 (cells (send _cells map-cells identity))
                 (first-cell (car cells))
                 (last-cell  (list-ref cells i1))
                 (q-fn (send (send this param) quantify-fn))
                 (q0 (q-fn (send first-cell v-coord-n 0)))
                 (q1 (q-fn (send last-cell  v-coord-n 0)))
                 (dq (- q1 q0)))
            (send this set-cell-dimensions first-cell)
            (if (zero? dq)
                (begin
                  (set! _q-slope 0)
                  (set! _q-intercept (+ x0 (/ dx 2))))
                (begin
                  (set! _q-slope (/ dx dq))
                  (set! _q-intercept (- x0 (* q0 _q-slope)))))
            (if (zero? i1)
                (begin
                  (set! _i-slope 0)
                  (set! _i-intercept (+ x0 (/ dx 2))))
                (begin
                  (set! _i-slope (/ dx i1))
                  (set! _i-intercept x0))) ;since i0=0
            (set! _y-stacks
                  (make-object picky-cascade%
                               (lambda (cell lst)
                                 (if (null? lst)
                                     #t ;no previous cells, always accept
                                     (let* ((my-x (compute-cell-x cell))
                                            (last-x (compute-cell-x (car lst)))
                                            (diff (- my-x last-x)))
                                       (>= diff (send this cell-width)))))))
            (set! _max-j 0)
            ))))

    (define (locate-cell cell)
      (send cell x-coord-n 0 #f) ;reset x coord
      (compute-cell-x cell)  ;recompute x coord
      (let ((j (send _y-stacks accomodate cell)))
        (send cell i-coord-n 1 j)
        (when (< _max-j j)
          (set! _max-j j))))

    (define (render-cell cell)
      (let* ((x (send cell x-coord-n 0))
             (j (send cell i-coord-n 1))
             (y (+ _y-base (* j _y-step)))
             (obj (send cell obj))
             (target (sendf this object frame)))
        (if target
            (set-obj-xy obj x y target)
            (set-obj-xy obj x y))))

    (define/override (render)
      (prepare) ;also refreshes _bbox
      ;(let ((stks (getfield _y-stacks picky-cascade% _stacks)))
      ;(say  (if (null? stks) null (getfield (car stks) picky-stack% _list))))
      (send _cells for-each-cell locate-cell)
      (let* ((bb     (send this bbox))
             (y-size (bbheight bb)))
        (set! _y-step (min (send this cell-height) (/ y-size (+ _max-j 1))))
        (set! _y-base (- (cadr (bbcenter bb)) (* _y-step _max-j .5))))
      (send _cells for-each-cell render-cell))
    ))

; UNFINISHED:
(define labeled-linear-projector%
  (class linear-projector%
    (init _rgn)
    (init _param)
    (super-instantiate (_rgn _param))
    (send this dynaclass 'labeled-linear-projector%)

    (field (_labels null)) ; list of text objects
    (inherit-field _y-stacks)
    (inherit-field _y-base _y-step)

    (define/public (cells-on-bottom-row)
      (if _y-stacks
          (let* ((stacks (getfield _y-stacks picky-cascade% _stacks))
                 (stack0 (if (null? stacks) #f (car stacks)))
                 (cells (and stack0 (getfield stack0 picky-stack% _list))))
            (or (and cells (reverse cells)) null))
          ;else
          null))

    (define/public (vals-on-bottom-row)
      ;      (map (lambda (cel) (send cel v-coords 0)) (cells-on-bottom-row)))
      ; in future: try to recycle v-coord in cell, but for now...
      (let ((access-fn (send (send this param) access-fn)))
        (map (lambda (cel) (access-fn (send cel obj))) (cells-on-bottom-row))))

    (define (make-one-label text x y)
      (push! (ic (make-object text% dynapad text (list x y 2.5))
                 (findable #f)
                 (pen "grey")
                 (anchor "center")
                 (takegroupevents #f))
             _labels))

    (define (make-label-row text-list y cells)
      (if (string? text-list)
          ;singleton; center label
          (make-one-label text-list (car (send (sendf this object frame) xy)) y)
          ;else series which maps onto cell row
          (let ((xs (map (lambda (c) (send c x-coord-n 0)) cells)))
            (for-each (lambda (txt x)
                        (make-one-label txt x y)) text-list xs))))

    (define/public (generate-labels)
      (let* ((param (send this param))
             (val-fn (send param access-fn))
             (labels-fn (send param label-fn))
             (cells (cells-on-bottom-row))
             (objs (map (lambda (c) (send c obj)) cells))
             (vals (map (lambda (o) (val-fn o)) objs))
             (label-rows (labels-fn vals))
             ;(frame-btm-y (b1 (send (sendf this object frame) bbox)))
             ;(label-y (- _y-base _y-step))
             (my-grp (sendf this object container)))
        (for-each (lambda (txts rownum)
                    (let ((y (max (- _y-base (* .6 (+ rownum 1) _y-step))
                                  (+  (b1 (sendf this object bbox))
                                      (* .6 (+ rownum .5) _y-step)))))
                      (make-label-row txts y cells)))
                  label-rows
                  (counting-list (length label-rows) 1))
        (send my-grp add _labels)
        ))

    (define/override (render)
      (foreach _labels (lambda (l) (send l delete)))
      (set! _labels null)
      (super render)
      (generate-labels))

    (define/override (delete)
      (foreach _labels (lambda (l) (send l delete)))
      (super delete))
    ))

(define raster-projector%
  (class 1D-projector%
    (init _rgn)
    (init _param)
    (inherit-field _cells)
    (super-instantiate (_rgn _param))
    (send this dynaclass 'raster-projector%)
    (field (_row-len 0) (_col-len 0) (_x-spacing 0) (_y-spacing 0))
    (field (_x-base 0) (_y-base 0))

    (when (is-a? _rgn region%)
      (send this attach-to (send _rgn object) 'raster-projector))
    (define (prepare)
      (let* ((obj (send this object))
             (cells (send _cells map-cells identity))
             (first-cell (if (null? cells)
                             #f
                             (car cells))))
        (send this refresh-bb (send obj frame))  ;refresh bbox if regionized
        (when first-cell
          (send this set-cell-dimensions first-cell))
        (set! _x-spacing (* 1.25 (send this cell-width)))
        (set! _y-spacing (* 1.25 (send this cell-height)))
        (send this margins (* .5 _x-spacing))
        (let* ((len  (send _cells renumber))
               (bb  (send this bbox))
               (bbw (bbwidth bb))
               (bbh (bbheight bb)))
          (unless (zero? len)
            (let* ((ideal-row-len (floor (/ bbw _x-spacing)))
                   (ideal-col-len (floor (/ bbh _y-spacing)))
                   (ideal-len  (* (+ 1 ideal-row-len) (+ 1 ideal-col-len))) )
              (cond
                ((= 0.0 ideal-row-len)
                 (set! _row-len 1.0)
                 (set! _col-len len)
                 (set! _y-spacing (/ bbh (- _col-len 1))))

                ((= 0.0 ideal-col-len)
                 (set! _col-len 1.0)
                 (set! _row-len len)
                 (set! _x-spacing (/ bbw (- _row-len 1))))
                (else
                 (when (<= len ideal-len)
                   ;underfull, go ahead
                   (begin
                     (set! _row-len (+ 1 ideal-row-len))
                     (set! _col-len (floor (/ len (max _row-len 1))))
                     ;(set! _y-spacing (/ bbh _col-len))
                     ;(set! _x-spacing (/ bbw _row-len))
                     )
                   ;overfull; must squash
                   (begin
                     (set! _row-len (ceiling (sqrt (* len (/ bbw bbh)))))
                     (set! _col-len (ceiling (/ len (max _row-len 1))))
                     (when (> _row-len 1) (set! _x-spacing (/ bbw (- _row-len 1))))
                     (when (> _col-len 1) (set! _y-spacing (/ bbh (- _col-len 1)))) ))))))

          (set! _x-base (b0 bb))
          (set! _y-base (b3 bb)) )))

    (define (render-cell cell)
      (let* ((obj (send cell obj))
             (i (send cell i-coord-n 0))
             (r (modulo i _row-len))
             (c (truncate (/ i (max 1 _row-len))))
             (x (+ _x-base (* r _x-spacing)))
             (y (- _y-base (* c _y-spacing)))
             (target (sendf this object frame)))
        ; animation must track frame component, not compound object
        ;  b/c moving members cause "drift" of obj center
        (if target
            (set-obj-xy obj x y target)
            (set-obj-xy obj x y)) ))

    (define/override (render)
      (prepare) ;also refreshes _bbox
      (send _cells for-each-cell render-cell))
    ))
