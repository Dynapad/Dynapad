(announce-module-loading "pile features...")
(dynaload "hulls.ss")
(update-progress .5)

(dynaload "regions.ss")
(update-progress .75)

(dynaload "colors.ss")
(dynaload "get-user-bbox.ss")

(define obj-point%
  (class geo-point%
    (init-field _obj)
    (init _x _y)
    (super-instantiate (_x _y))

    (define/public obj (get/set _obj))
))

;(define (make-fencepost-pair o)
;  (list o 'blahblah ));(apply make-object fencepost% o (send o xy))))

;(define (generate-fenceposts-from-objects object-list)
;  object-list)
;  (map make-fencepost-pair object-list))

(define (generate-convex-hull-from-fenceposts objs)
  (convex-hull
   (map (lambda (obj)
      (apply make-object obj-point% obj (send obj xy)))
    objs)))

(define (generate-convex-hull-from-fenceposts objs)
  (convex-hull
   (apply append
      (map (lambda (obj)
         (let* ((rgn  (get-actor-named obj region-actor-name))
            (frame (and rgn (send rgn pane)))
            (crds  (cond ((is-a? rgn pile-region%)
                      (send frame coords)) ;subpile
                     (frame (coords-from-bbox (send frame bbox))) ;other region
                     (else (coords-from-bbox (send obj bbox))))) ;rect etc
            (lazypairs (make-lazy-pairs crds)))
           (map (lambda (pr)
              (make-object obj-point% obj (car pr) (cadr pr)))
            lazypairs)))
           objs))))

#|
(define (make-fencepost-pair o)
  (list o (apply make-object fencepost% o (bbnw (send o bbox)))
          (apply make-object fencepost% o (bbne (send o bbox)))
          (apply make-object fencepost% o (bbse (send o bbox)))
          (apply make-object fencepost% o (bbsw (send o bbox)))))

(define (generate-fenceposts-from-objects object-list)
  (map make-fencepost-pair object-list))

(define (generate-convex-hull-from-fenceposts fenceposts)
  (convex-hull (apply append (map cdr fenceposts))))
|#

;(define *pile-containment-fn* intersects-but-not-surrounds?)
(define *pile-containment-fn* contained-in?)

(define pile-region%
  (class fusing-container%
    (init _obj)
    (super-instantiate (_obj))
    (send this dynaclass 'pile-region%)
    (send this containment-fn *pile-containment-fn*)
    ;(send this retentive #t)

    (field (_fenceposts null))
    ; a list of _contents which
    ; are current (or potential) vertices of convex hull frame
    (define/public (fenceposts) _fenceposts) ;temp for debugging
    (field (_checkall #f))

    (field (_margin #f)) ;manual setting if number, auto-margin if #f
    (field (_min-margin 150)) ;margin for empty piles
    (inherit-field _mean-item-dim)

    (define/public min-margin
      (case-lambda
       (() _min-margin)
       ((m) (set! _min-margin m)
        (when (null? (send this contents))
        (send this finish)))))
    (define/public margin
      (case-lambda
       (() _margin)
       ((m) (set! _margin m)
        (send this finish))))

    (define/public (geon)
      (get-actor-named (send this pane)  geo-actor-name))

    (sendf this formation alternate-build-fn
       (lambda (obj)
         (let* ((rgn (get-rgn obj))
            ;(cnts (send rgn contents))
            (crds (sendf rgn pane coords)))
           `(ic (make-pile ;-from-coords
             ,(send rgn dynaclass)
             ,(send obj dynaclass)
             null ;(list ,@cnts)
             (list ,@crds)
        ;(list ,@(map obj->IDexpr cnts))
             )
            (id ,(send obj id))
            (title ,(send obj title)))
           )))

;    (send (send this formation) post-build-ops 'remove #f 'enumerate-contents)

    (send this enter-actions 'add
      (lambda (o)
        ;(display (format "adding ~a~%" o))
        (push! o _fenceposts)
        ;(let ((dim (max (send o width) (send o height))))
        ;  (increment-mean _mean-item-dim _num-items dim))
        ))
;          (if (> dim _item-max-dim)
;          (set! _item-max-dim dim)))))

    (send this update-actions 'add
      (lambda (o)
        (if (memq o _fenceposts) ;if current post,
        (set! _checkall #t)  ; need to check all
          (push! o _fenceposts)))) ;else just check this

    (send this leave-actions 'add
      (lambda (o)
        ;(let ((dim (max (send o width) (send o height))))
        ;  (decrement-mean _mean-item-dim _num-items dim)
        ;  (sendf this object use-item-size _mean-item-dim)
        ;  )
        (when (memq o _fenceposts)
         ;removed fencepost; need to recompute w. all pts
        (set! _checkall #t))))

    (define/override (decorate)
      (let* ((owner (get-object-keyval (send this formation) 'region-owner))
         (owner-color (if owner
                  (send (send owner pane) fill)
                  (send dynapad background)))
         (dynacolor (make-object dynacolor% owner-color))
         (mildcolor (send dynacolor blend .2 128 128 128)) ;interpolate toward grey
;         (mildcolor (send dynacolor blend .5 200 200 200)) ;interpolate toward grey
         (frame (send this pane)))
    (send frame pen "#808080"); (send dynapad background))
    (send frame penwidth 0)
    (send frame transparency .85)
    (send frame fill (send mildcolor tcl-color))
    mildcolor))

    (define (rewrap)
      (when (not (null? (send this visible-contents)))
        (when _checkall
          (set! _checkall #f)
          (set! _fenceposts (send this visible-contents)))
        (let* ((margin (if _margin
                   _margin
                   (/ _mean-item-dim 3)))
           (circuit (generate-convex-hull-from-fenceposts _fenceposts))
           (len (length circuit))
           (crds
            (if (<= len 1)
          ;special case: 0 or 1 items
            (let ((xy (if (zero? len)
                      (send (send (geon) anchor) xy)
                      (send (car circuit) xy))))
;                      (send (caar _fenceposts) xy)
              (when (zero? len)
                  (set! margin _min-margin))
              (coords-from-bbox
               (make-bb (car xy) (cadr xy) (* 2 margin))))
          ;normal case...
            (let* ((inner-poly (make-object geo-polygon% circuit))
        ;(num (max (length (send this contents)) 1))
        ;(area (send inner-poly area))
        ;(margin-width (* 1.22 (sqrt (/ area num))))
                   (outer-poly (send inner-poly
                         smoothed-margin margin)))
              (set! _fenceposts
                (map (lambda (p) (send p obj)) circuit))
              (send outer-poly coords)))))
          (send (geon) coords crds)
          (send (send this pane) coords crds)

          (send this decorate)
          )))

  (send this finish-actions 'add rewrap)

))

;(define nesting-pile%
;specific support for nested piles
;  (class pile-region%
;     (init _obj)
;     (super-instantiate (_obj))
;     (send this dynaclass 'nesting-pile%)


(define *pile-item-fusing-size* 25)
(define fusing-pile%
  (class pile-region%
    (init _obj)
    (super-instantiate (_obj))
    (send this dynaclass 'fusing-pile%)

;    (inherit-field _mean-item-dim) ;maximum dimension of content items
;    (field (_item-fadesize *pile-item-fusing-size*)) ;min pixel size of item before fusing

;    (define (refresh-fadesize)
;      (let* ((frame (send this pane))
;         (dim (max (send frame width) (send frame height)))
;         (newfadesize (/ (* dim _item-fadesize) (max _mean-item-dim 1))))
;    (send (send this formation) fusing-size newfadesize)))

;    (let ((frame (send this pane)))
;      (send frame aftercoords-callbacks 'add ;recompute fadesize after re-wrap
;        (lambda (o crds)
;          (send (send (send this formation) cover) coords crds)
;          (refresh-fadesize))))

    (define/override (decorate)
      (let* ((framecolor (super decorate)) ;returns fill (dynacolor) of frame
         (cover (sendf this formation cover))
         (mycolor  ;shift color -> 35% away from bg
          (send framecolor blend -.35 (send dynapad background))))
    (send cover pen "#808080") ;(send dynapad background))
    (send cover penwidth 0)
    ;(send cover penwidth 5 #t)
    (send cover fill (send mycolor tcl-color))
    ;(send cover transparency .01)
    ))

))

(define shadowed-fusing-pile%
  (class fusing-pile%
    (init _obj)
    (super-instantiate (_obj))
    (send this dynaclass 'fusing-pile%)

    (let ((frame (send this pane)))
      (send frame aftercoords-callbacks 'add ;recompute fadesize after re-wrap
        (lambda (o crds)
          (send (send (send this formation) shadow) coords crds))))

    (define/override (decorate)
      (super decorate)
      (let ((shadow (sendf this formation shadow))
        (frame (sendf this formation frame)))
    (send shadow fill (send dynapad background))
    (send shadow transparency (send frame transparency))
    (send shadow penwidth (send frame penwidth))
    (send shadow pen (send frame pen))))
    ))

#|
(define fusing-pile%
  (class pile-region%
    (init _obj)
    (super-instantiate (_obj))
    (send this dynaclass 'fusing-pile%)

    (inherit-field _mean-item-dim) ;maximum dimension of content items
    (field (_item-fadesize 15)) ;min pixel size of item before fusing
    (field (_fadesize 1)) ;min pixel size of wrapper before fusing (recomputed)

    (define/public (fadesize) _fadesize)

    (define (refresh-fadesize)
      (let* ((frame (send this pane))
         (dim (max (send frame width) (send frame height)))
         (newfadesize (/ (* dim _item-fadesize) (max _mean-item-dim 1))))
    (send frame zoomaction _fadesize #f #f) ;clear old threshold
    (set! _fadesize newfadesize)
    (send frame zoomaction _fadesize
          (lambda (o sz) ;bigger than threshold
        (let ((grp (get-top-group o)))
          (send (get-top-group o) contents-on-top? #t)
          (send o renderitem)))  ;this line...
          (lambda (o sz) ;smaller than threshold
        (let ((grp (get-top-group o)))
          (send (get-top-group o) contents-on-top? #f)
          (send o renderitem)))))) ;..and this line are arbitrary hacks
    ;to try to force the buggy renderer to refresh

;    (send this refresh-contents)
;    (send this finish)
    (refresh-fadesize)

    (let ((frame (send this pane)))
    ;ensure correct starting fusing
      (send (send this formation) contents-on-top?
        (> _fadesize (max (screen-width (send frame bbox))
                  (screen-height (send frame bbox)))))

      (send frame pen (send dynapad background))
;      (send frame fill "#dddddd")
      (send frame transparency .4)
      (send frame aftercoords-callbacks 'add  ;recompute fadesize after re-wrap
        (lambda (o crds) (refresh-fadesize))))
))
|#

(define highlight-pile%
  (class pile-region%
    (init _obj)
    (super-instantiate (_obj))
    (send this dynaclass 'highlight-pile%)

    (send this enter-actions 'add
      (lambda (o) (bind o "<Enter>"
                (lambda (o e) (send this hilight)))
                      (bind o "<Leave>"
                (lambda (o e) (send this unhilight))))
      'highlight)
    (send this leave-actions 'add
      (lambda (o) (bind o "<Enter>" #f)
              (bind o "<Leave>" #f)))

    (send (send this pane) bind "<Enter>"
      (lambda (o e) (send this hilight)))
    (send (send this pane) bind "<Leave>"
      (lambda (o e) (send this unhilight)))

    (define/public (hilight)
      (send (send this pane) pen "grey"))
    (define/public (unhilight)
      (send (send this pane) pen "none"))


))

;(define (make-pile-from-poly p)
;  (when p
;     (undoify-fresh-obj
;       (let* ((geon (geodize p))
;          (cnts (contained-objects p))
;          (form (regionize p fusing-pile% fusing-frame-container%))
;          (reg (get-actor-named form region-actor-name)))
;     ;(respond-to-pickup dynapad #f cnts)
;     (send reg contents cnts)
;     (send reg finish)
;     form))))

;(define (make-pile-with-objs pile-class form-class . objs)
;  (let* ((poly (ic (make-object polygon% dynapad '(0 0 1 1))
;           (pen "none")
;           (fill "none")))
;     (form (regionize poly pile-class form-class))
;     (reg  (get-actor-named form region-actor-name)))
;    (foreach objs (lambda (o) (send o unselect)))
;    (send reg contents objs)
;    (send reg finish)
;    form))

;(define (make-pile-from-coords crds)
;  (when (and crds (not (null? crds)))
;     (undoify-fresh-obj
;       (let* ((p (make-object polygon% dynapad crds))
;          (form (regionize p fusing-pile% fusing-frame-container%))
;          (cnts (contained-objects p *pile-containment-fn*))
;          (reg (get-actor-named form region-actor-name)))
;     ;(respond-to-pickup dynapad #f cnts)
;     (send reg contents cnts)
;     (send reg finish)
;     form))))

;(define nowrap-pile-from-coords
;  (case-lambda
;   ((pile-class form-class . crds)
;    (let* ((poly (ic (make-object polygon% dynapad crds)
;             (pen "none")
;             (fill "none")))
;       (form (regionize poly pile-class form-class))
;       (reg  (get-actor-named form region-actor-name)))
;      (send reg finish)
;      form))
;   ((crds) ;old-style default
;    (apply nowrap-pile-from-coords fusing-pile% fusing-frame-container% crds))
;))

(define (make-pile-from-selection regn-type form-type cnts)
  (unless (null? cnts)
    (begin
    (Start-Changing-Select--undoable dynapad)
;      (let ((form (apply make-pile-with-objs fusing-pile% fusing-frame-container% cnts)))
      (let ((form (make-pile regn-type form-type cnts)))
    (Done-Changing-Select--undoable dynapad)
    (undoify-fresh-obj form)
    ;(send form select)
    form))))


(define *default-pile-regn* shadowed-fusing-pile%)
(define *default-pile-form* dissolving-fusing-frame-container%)
(define *default-pile-title* #f) ;may be #f

(define make-pile
  (case-lambda
; 4 args, Fully specified:
   ((pile-class form-class objs crds)
; (... objs crds) --> take coords initially, take objs, no wrap
; (...  #f  crds) --> take coords initially and wrap if possible
; (... objs  #f ) --> wrap abound objs (non-null)
    (let* ((no-wrap (and objs crds))
       (use-crds (or crds (list 0 0 1 1)))
       (poly (ic (make-object polygon% dynapad use-crds)
             (pen "none")
             (fill "none")))
       (use-objs (or objs (lasso-contained-objects poly))); *pile-containment-fn*)))
       (form (regionize poly pile-class form-class))
       (reg  (get-actor-named form region-actor-name)))
      (send form title *default-pile-title*)
      (send reg contents use-objs)
      (unless (or no-wrap (null? use-objs)) ; empty, don't wrap
          (send reg finish))
      form))
; remaining cases allow polymorphic syntax
; 3
   ((pile-class form-class lst)
; (... crds) --> (... #f crds)
; (... objs) --> (... objs #f)
    (cond ((null? lst)
       (make-pile pile-class form-class lst #f)); null, treat as objs
      ((number? (car lst))
       (make-pile pile-class form-class #f lst)); coords?
      ((is-a? (car lst) dynaobject%)
       (make-pile pile-class form-class lst #f)); objs?
      (else (error "unknown list type for pile"))))
; 2
   ((arg1 arg2)
    (cond
    ;(rgn-class form-class)-- generic GUI entry
     ((not (list? arg2))
      (make-pile-from-gui arg1 arg2))
    ;(list list)
     ((list? arg1)
      (make-pile *default-pile-regn* *default-pile-form* arg1 arg2))
    ;(form-class list)
     ((is-a? arg1 base-formation%)
      (make-pile *default-pile-regn* arg1 arg2))
    ;(rgn-class list)
     ((is-a? arg2 region%)
      (make-pile arg1 *default-pile-form* arg2))
     (else (error "No pattern matching args to make-pile"))))
; 1
   ((lst) ; assume list for now
    (make-pile *default-pile-regn* *default-pile-form* lst))
; 0
   (()
    (make-pile *default-pile-regn* *default-pile-form*))
   ))

(define make-pile-from-gui
  (case-lambda
   ((regn-type form-type)
    (if (not (any-selected?))
    ;(ask-user-for-lasso dynapad make-pile-from-coords)
    (ask-user-for-lasso dynapad
        (lambda (crds) (undoify-fresh-obj
                (make-pile regn-type form-type crds))))
    ;(ask-user-for-lasso dynapad
    ;  (lambda (crds)
    ;    (make-pile fusing-pile% fusing-frame-container% #f crds)))
    (make-pile-from-selection regn-type form-type
                  (send dynapad selected))))
   (() (make-pile-from-gui *default-pile-regn* *default-pile-form*))
   ))

; Needed for back-compatibility:
(define (make-pile-from-coords crds)
  (make-pile crds))
(define (make-pile-with-objs regn-type form-type . objs)
  (make-pile regn-type form-type objs))


(when *popup-menus-enabled?*
    (append-mainmenu-constructor
     (lambda (mb obj)
       (add-menu-separator mb)
       (add-menu-item mb (if (any-selected?)
                 "Pile Selected"
                 "Make Pile...")
              make-pile)
       )))


(update-progress 1)
