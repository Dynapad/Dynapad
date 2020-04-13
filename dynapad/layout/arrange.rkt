#lang racket/base

(require (only-in racket/class send)
         (only-in dynapad/pad-state
                  dynapad)
         dynapad/misc/misc
         (only-in dynapad/misc/tools-lists
                  counting-list)
         (only-in dynapad/layout/bbox
                  bbcenter
                  bbheight
                  bbunion
                  bbwidth
                  bbunion-objects
                  b0
                  b1
                  b2
                  b3
                  bxc
                  byc
                  )
         )

(provide arrange-in-spiral-in-current-view
         arrange-in-grid-in-current-view
         )
;;Basic routines

(define (anchor-quadrant anc)
  ; returns pair (x y) for quandrant relative to center
  (cond ((equal? anc "nw") '(-1 1))
        ((equal? anc "n")  '(0 1))
        ((equal? anc "ne") '(1 1))
        ((equal? anc "w")  '(-1 0))
        ((equal? anc "e")  '(1 0))
        ((equal? anc "sw") '(-1 -1))
        ((equal? anc "s")  '(0 -1))
        ((equal? anc "se") '(1 -1))
        (else '(0 0)) ;center
        ))

(define (precompute-NxM-grid-coords-at-xyz cols rows dx dy x0 y0 z)
  (map (lambda (n)
         (let ((col (modulo n cols))
               (row (floor (/ n cols))))
           (list (+ x0 (* col dx))
                 (- y0 (* row dy))
                 z)))
       (counting-list (* cols rows))))

(define precompute-grid-coords
  ;generates list of len triplets (x y z)
  ; which scan l->r, top->btm in square grid, with anchor at xyz
  ; default anchor is center
  (case-lambda
    ((len dx dy)     (precompute-grid-coords len dx dy '(0 0 1)))
    ((len dx dy xyz) (precompute-grid-coords len dx dy xyz "center"))
    ((len dx dy xyz anchor)
     (let* ((cols (round (sqrt len)))
            (rows (ceiling (/ len cols)))
            (halfwidth (* 0.5 dx (- cols 1)))
            (halfheight (* 0.5 dy (- rows 1)))
            (quad (anchor-quadrant anchor))
            (x-quad (car quad))
            (y-quad (cadr quad))
            (x0 (- (car xyz)  (* (+ x-quad 1) halfwidth)))
            (y0 (- (cadr xyz) (* (- y-quad 1) halfheight)))
            (z  (caddr xyz)))
       (precompute-NxM-grid-coords-at-xyz cols rows dx dy x0 y0 z)))
    ))

(define (arrange-objs-in-current-view lst)
  (arrange-objs-in-bbox  lst (send dynapad bbox)))

(define (arrange-objs-in-bbox lst target_bbox . bbox_fraction)
  (when (pair? lst)
    (let* ((oldbbox (send (car lst) bbox))
           (oldbbox_center 0)
           (target_center 0)
           (dx 0)(dy 0)
           (rx 0)(ry 0)(rr 1)
           (squeeze (if (null? bbox_fraction) 1.0 (car bbox_fraction))))
      (for-each (lambda (obj )
                  (set! oldbbox (bbunion oldbbox (send obj bbox))))
                lst)
      (set! oldbbox_center (bbcenter oldbbox))
      (set! target_center (bbcenter target_bbox))
      (set! dx (- (car  target_center) (car  oldbbox_center)))
      (set! dy (- (cadr target_center) (cadr oldbbox_center)))

      (set! rx (/ (bbwidth  oldbbox) (bbwidth  target_bbox) 0.5))
      (set! ry (/ (bbheight oldbbox) (bbheight target_bbox) 0.5))
      (set! rr (/ squeeze (max rx ry)))
      (foreach lst (lambda (obj)
                     (send obj scale 2.0 (- (car oldbbox_center) dx) (- (cadr oldbbox_center) dy))
                     (send obj scale rr  (car target_center) (cadr target_center)))) )))

;;Flat arrangements
(define (arrange-objects-in-grid lst . spread)
  (let ((z0 (send (car lst) z))(dz 1)(bb 0))
    (arrange-objs-in-grid  lst '() (if (null? spread) spread (car spread)) "nw")
    (set! dz (/ z0 (send (car lst) z)))
    (set! bb (bbunion-objects lst))
    (foreach lst (lambda (o) (send o scale dz (b0 bb) (b3 bb))))))

(define (arrange-in-grid-in-current-view lst . spread)
  (arrange-objs-in-grid  lst '() (if (null? spread) spread (car spread)) "nw")
  (arrange-objs-in-current-view lst))

(define (arrange-in-spiral-in-current-view lst . spread)
  (arrange-objs-in-spiral lst '() (if (null? spread) spread (car spread)) "center")
  (arrange-objs-in-current-view lst))

(define (arrange-in-pile-in-current-view lst . spread)
  (arrange-objs-in-pile lst '() (if (null? spread) spread (car spread)) "center")
  (arrange-objs-in-current-view lst))

;; =======================
;; ToDo: set consistent anchorpoint to ensure corrent placement in grid
;; or spiral (rotations may goof it up a little)

(define (arrange-objs-in-grid lst targetbbox spread . dynapad-anchor)
  (when (not (null? lst))
    (let* ((width 0) (height 0)
                     (irow 0) (icol 0)
                     (xstart 0) (ystart 0)
                     (dx 0) (dy 0)
                     (bb_scale 1)
                     (item_scale (if (null? spread) .9 (/ 1 spread)))
                     (tbbox (get-target-bbox targetbbox lst)))
      (set! xstart (car tbbox))
      (set! ystart (cadddr tbbox))
      ;; pick a width and height that are proportional to the bounding box width and height
      (set! width (ceiling
                   (sqrt (* (length lst) (/ (bbwidth tbbox) (bbheight tbbox))))))
      (set! height (ceiling (/ (length lst) width)))
      (set! dx (if (< (bbwidth tbbox) (bbheight tbbox))
                   (/ (bbwidth tbbox) width)
                   (/ (bbheight tbbox) height)))
      (set! dy dx)
      (set! bb_scale (* item_scale dx))
      (for-each
       (lambda (obj)
         (let* ((curr-bbox (send obj bbox))
                (curr-bbs (max (bbwidth curr-bbox) (bbheight curr-bbox)))
                (curr-z (caddr (send obj position)))
                (panchor (if (null? dynapad-anchor)
                             (send obj anchor)
                             (car dynapad-anchor))))
           (send obj position
                 (list (+ xstart (* dx icol))
                       (- ystart (* dy irow))
                       (* curr-z (/ bb_scale curr-bbs))))
           (send obj anchor panchor)
           (set! icol (+ 1 icol))
           (when (= icol width)
             (set! icol 0)
             (set! irow (+ 1 irow)))))
       lst)
      dynapad-anchor)))

(define (arrange-objs-in-row lst targetbbox spread)
  (def tbbox (get-target-bbox targetbbox lst))
  (def xcurr (b0 tbbox))
  (def ycurr (b3 tbbox))
  (def curr-bbox)
  (def frc (/ (bbwidth tbbox)
              (apply + (map (lambda (i) (send i width)) lst))))
  (when (null? spread) (set! spread .9))

  (foreach lst
           (lambda (obj)
             (send obj scale frc)
             (set! curr-bbox (send obj bbox))
             (send obj slide (- xcurr (b0 curr-bbox)) (- ycurr (b3 curr-bbox)))
             (set! xcurr (+ xcurr (bbwidth curr-bbox)))
             (send obj scale spread)
             ))
  )

(define (array-in-col lst)
  (let ((curr (car lst))(dx 0)(dy 0))
    (foreach (cdr lst) (lambda (obj)
                         ;(set! dx (- (b0 (send curr bbox)) (b0 (send obj bbox))))
                         (set! dy (- (b1 (send curr bbox)) (b3 (send obj bbox))))
                         (send obj slide dx dy)
                         (set! curr obj)))))

(define (array-in-row lst)
  (let ((curr (car lst))(dx 0)(dy 0))
    (foreach (cdr lst) (lambda (obj)
                         (set! dx (- (b2 (send curr bbox)) (b0 (send obj bbox))))
                         ;(set! dy (- (b3 (send curr bbox)) (b3 (send obj bbox))))
                         (send obj slide dx dy)
                         (set! curr obj)))))


(define (align-left lst leftx)
  (for-each (lambda (o) (send o slide (- leftx (b0 (send o bbox))) 0)) lst))

(define (align-right lst rightx)
  (for-each (lambda (o) (send o slide (- rightx (b2 (send o bbox))) 0)) lst))

(define (align-bottom lst bottomx)
  (for-each (lambda (o) (send o slide 0 (- bottomx (b1 (send o bbox))))) lst))

(define (align-top lst topx)
  (for-each (lambda (o) (send o slide 0 (- topx (b3 (send o bbox))))) lst))

(define (min-left-objects   lst) (apply min (map (lambda (o) (b0 (send o bbox))) lst)))
(define (min-bottom-objects lst) (apply min (map (lambda (o) (b1 (send o bbox))) lst)))
(define (max-right-objects  lst) (apply max (map (lambda (o) (b2 (send o bbox))) lst)))
(define (max-top-objects    lst) (apply max (map (lambda (o) (b3 (send o bbox))) lst)))



(define (find-fnc-min-max objectlist fnc)
  (define vmin (fnc (car objectlist)))
  (define vmax vmin)
  (foreach (cdr objectlist)
           (lambda (obj) (let ((v (fnc obj)))
                           (when (> v vmax) (set! vmax v))
                           (when (< v vmin) (set! vmin v)))
                   )
           )
  (list vmin vmax)
  )

(define (arrange-objs-by-function lst targetbbox xfnc . yfnc)
  (define xsize-bbox (bbwidth targetbbox))
  (define ysize-bbox (bbheight targetbbox))
  (define xmin-bbox (b0 targetbbox))
  (define ymin-bbox (b1 targetbbox))
  (define xmap (lambda (obj) (bxc targetbbox)))
  (define ymap (lambda (obj) (byc targetbbox)))
  (define xmin 0)
  (define xsize-fnc 1)
  (define xrange 1)
  (define ymin 0)
  (define ysize-fnc 1)
  (define yrange 1)

  (when xfnc
    (set! xrange (find-fnc-min-max lst xfnc))
    (set! xmin (car xrange))
    (set! xsize-fnc (- (cadr xrange) xmin))
    (when (not (zero? xsize-fnc))
      (set! xmap
            (lambda (obj)
              (+ xmin-bbox (* xsize-bbox (/ (- (xfnc obj) xmin) xsize-fnc))))))
    )
  (when (pair? yfnc)
    (set! yfnc (car yfnc))
    (set! yrange (find-fnc-min-max lst yfnc))
    (set! ymin (car yrange))
    (set! ysize-fnc (- (cadr yrange) ymin))
    (when (not (zero? ysize-fnc))
      (set! ymap
            (lambda (obj)
              (+ ymin-bbox (* ysize-bbox (/ (- (yfnc obj) ymin) ysize-fnc))))))
    )
  (foreach lst (lambda (obj) (send obj xy (xmap obj) (ymap obj))))
  )

(define (object-sort lst fnc cmp-fnc)
  (sort lst (lambda (a b) (cmp-fnc (fnc a)(fnc b)))))

(define pi (* 4 (atan 1)))

(define (arrange-objs-in-spiral lst targetbbox spread . dynapad-anchor)
  (let* (
         (item_scale (if (null? spread) 0.9 spread))
         (radius 0)
         (theta (* 2 pi))
         (t_zero (length lst))
         (tbbox (get-target-bbox targetbbox lst))
         (bb_radius (/ (min (bbwidth tbbox) (bbheight tbbox)) 2))
         (item_radius (/ (* bb_radius pi)
                         (* (sqrt (+ (* 4 pi (- t_zero 1)) (* pi pi)))
                            item_scale)))
         (bb_scale (round (* (sqrt 2) item_radius)))
         (vx (car (bbcenter tbbox)))
         (vy (cadr (bbcenter tbbox)))
         (d_theta 0))
    (for-each
     (lambda (obj)
       (let* ((curr-bbox (send obj bbox))
              (curr-bbs (max (bbwidth curr-bbox) (bbheight curr-bbox)))
              (curr-z (caddr (send obj position)))
              (panchor (if (null? dynapad-anchor)
                           (send obj anchor)
                           (car dynapad-anchor))))
         (send obj position (list
                             (+ vx (* radius (cos theta)))
                             (+ vy (* radius (sin theta)))
                             (* curr-z (/ bb_scale curr-bbs))))
         (send obj anchor panchor)

         (set! d_theta (/ 2 (- (/ theta pi) 1)))
         (set! theta (+ theta d_theta))
         (set! radius (* item_radius
                         item_scale
                         (- (/ theta pi) 1)))))
     lst)))
;  (apply pad_unselect .dynapad lst)
;  (selectMode .dynapad "Pan"))

(define (arrange-objs-in-pile lst targetbbox spread . dynapad-anchor)
  ;; possible options:
  ;;   (targetbbox pad_list_anchor)
  (let* ((xcenter 0) (ycenter 0)
                     (dx 0) (dy 0)
                     (dynapad-length (length lst))
                     (bb_scale 1)
                     (item_scale (if (null? spread) 0 spread))
                     (tbbox (get-target-bbox targetbbox lst)))
    (set! xcenter (car (bbcenter tbbox)))
    (set! ycenter (cadr (bbcenter tbbox)))
    (set! bb_scale (/ (min (bbwidth tbbox) (bbheight tbbox)) (+ 1 item_scale)))
    (for-each
     (lambda (obj)
       (let* ((curr-bbox (send obj bbox))
              (curr-bbs (max (bbwidth curr-bbox) (bbheight curr-bbox)))
              (curr-z (caddr (send obj position)))
              (panchor (if (null? dynapad-anchor)
                           (send obj anchor)
                           (car dynapad-anchor))))
         (set! dx (* (- (/ (* 2 (random dynapad-length)) dynapad-length) 1) item_scale (/ bb_scale 2)))
         (set! dy (* (- (/ (* 2 (random dynapad-length)) dynapad-length) 1) item_scale (/ bb_scale 2)))
         (send obj position (list (+ xcenter dx)
                                  (+ ycenter dy)
                                  (* curr-z (/ bb_scale curr-bbs))))
         (send obj anchor panchor)))
     lst)
    ;    (apply pad_unselect .dynapad lst)
    ;    (selectMode .dynapad "Pan")
    dynapad-anchor))

(define (get-target-bbox possible-bbox lst)
  (let ((targetbbox
         (if (null? possible-bbox)
             ;; generate a reasonable targetbbox from the union of all bboxes
             ;; in lst
             (bbunion-objects lst)
             possible-bbox)))
    targetbbox))

; option arg specifies aspect ratio for each item (as a bbox)
; default is a square aspect
(define (best-m-n-to-fit target_bbox num . args )
  (define item_bbox (if (null? args) '(0 0 1 1) (car args)))
  (define mcolumns 1)
  (define nrows 1)
  (if (not target_bbox)
      (set! nrows (ceiling (/ num 1.0 (cset! mcolumns (round (sqrt num))))))
      (let ()
        (define w (bbwidth target_bbox))
        (define h (bbheight target_bbox))
        (define ww (bbwidth item_bbox))
        (define hh (bbheight item_bbox))
        (define scaled_aspect_ratio (/ (/ w ww) (/ h hh)))
        (if (>= scaled_aspect_ratio 1.0)
            (begin
              (set! mcolumns (round (sqrt (* scaled_aspect_ratio num))))
              (set! nrows (ceiling (/ num 1.0 mcolumns))))
            (begin
              (set! nrows (round (sqrt (/ num scaled_aspect_ratio))))
              (set! mcolumns (ceiling (/ num 1.0 nrows)))) )
        )
      )
  (list (exact->inexact mcolumns) (exact->inexact nrows))
  )

; mn are a pair of integers
; xd_yd are a pair of distance deltas, center point to center point.
(define (array-objs-in-m-by-n-grid lst mn xd_yd)
  (when (pair? lst)
    (let* ((bbox0 (send (car lst) bbox))(dx 0)(dy 0)(i 0)
                                        (m (car mn)) (n (cadr mn)) (xstep (car xd_yd)) (ystep (cadr xd_yd))
                                        (xcurr (bxc bbox0)) (ycurr (byc bbox0)) )
      ; uses first object as reference point for layout
      ; only remainder of list is moved
      (foreach (cdr lst) (lambda (obj)
                           (set! xcurr (+ xcurr xstep))
                           (set! i (+ i 1))
                           (when (>= i m)
                             (set! i 0)
                             (set! xcurr (bxc bbox0))
                             (set! ycurr (- ycurr ystep))
                             )
                           (send obj slide
                                 (- xcurr (bxc (send obj bbox)))
                                 (- ycurr (byc (send obj bbox)))) )))))

; example
;(array-objs-in-m-by-n-grid (fs)
;   (best-m-n-to-fit '(0 0 762 692) (length (fs)))
;   (list 150 150))


;;Multiscale arrangements

(define (arrange-behind-object lst obj)
  (arrange-objs-in-bbox  lst (send obj bbox) 0.95)
  (for-each
   (lambda (id)
     (send id lower obj))
   lst)
  (send obj maxsize (list 50 #t)))

(define (arrange-in-grid-behind-object lst obj)
  (arrange-objs-in-grid lst (send obj bbox) '() "nw")
  (arrange-objs-in-bbox  lst (send obj bbox) 0.8)
  (for-each
   (lambda (id)
     (send id lower obj))
   lst)
  (send obj maxsize (list 50 #t)))

(define (arrange-in-spiral-behind-object lst obj)
  (arrange-objs-in-spiral lst (send obj bbox) '() "center")
  (arrange-objs-in-bbox  lst (send obj bbox) 0.95)
  (for-each
   (lambda (id)
     (send id lower obj))
   lst)
  (send obj maxsize (list 50 #t)))

(define (arrange-onto-object lst obj)
  (arrange-objs-in-bbox  lst (send obj bbox) 0.8)
  (for-each
   (lambda (id)
     (send id raise obj))
   lst)
  (send obj maxsize (list 0.95 #t)))

#; ; overwritten
(define (arrange-in-grid-onto-object lst obj)
  (arrange-objs-in-grid lst (send obj bbox) '() "nw")
  (arrange-objs-in-bbox  lst (send obj bbox) 0.8)
  (for-each
   (lambda (id)
     (send id raise obj))
   lst)
  (send obj maxsize (list 0.95 #t)))

(define (arrange-in-grid-onto-object lst target_obj . opts)
  (begin
    (define spread 1.05)
    (define spacing 0.95)
    (when (pair? opts) (set! spread (car opts))(set! opts (cdr opts)))
    (when (pair? opts) (set! spacing (car opts))(set! opts (cdr opts)))
    )
  (arrange-objs-in-grid lst (send target_obj bbox) spread "nw")
  (arrange-objs-in-bbox  lst (send target_obj bbox) spacing)
  (foreach lst
           (lambda (id) (send id raise target_obj)))
  (send target_obj maxsize (list 0.95 #t)))

(define (arrange-in-spiral-onto-object lst obj)
  (arrange-objs-in-spiral lst (send obj bbox) '() "center")
  (arrange-objs-in-bbox  lst (send obj bbox) 0.8)
  (for-each
   (lambda (id)
     (send id raise obj))
   lst)
  (send obj maxsize (list 0.95 #t)))

; this can be redefined (to do animation, for example)
(define (slide-center-to obj x y)
  (def bb (send obj bbox))
  (send obj slide (- x (bxc bb)) (- y (byc bb))))

(define (arrange-in-spiral-no-scaling objects xy)
  (def x0 (car xy))
  (def y0 (cadr xy))
  (def bblist)
  (def maxw)
  (def maxh)
  (def theta (* 2 pi))
  (def radius 0)
  (def d_theta 0)
  (def curr-bbox)
  (def item_radius)

  (when (pair? objects)
    (set! bblist (map (lambda (o) (send o bbox)) objects))
    (set! maxw (sort (map bbwidth bblist) >))
    (set! maxh (sort (map bbheight bblist) >))
    ;skip the first outliers
    (when (> (length maxw) 10) (set! maxw (cdddr maxw)))
    (when (> (length maxh) 10) (set! maxh (cdddr maxh)))
    (set! item_radius (max (car maxw) (car maxh)))
    (set! item_radius (* item_radius 0.55)) ;fudge factor

    (cond
      ((> (length objects) 4)
       (foreach objects (lambda (obj)
                          (slide-center-to obj (- x0 (* radius (cos theta)))
                                           (+ y0 (* radius (sin theta))) )
                          (set! d_theta (/ 2 (- (/ theta pi) 1)))
                          (set! theta (+ theta d_theta))
                          (set! radius (* item_radius (- (/ theta pi) 1)))
                          ))
       ;aethetic adjustment of first five objects
       (slide-center-to (list-ref objects 4)
                        (+ x0 (* item_radius -0.96))
                        (+ y0 (* item_radius -2.32)) )
       (slide-center-to (cadddr objects)
                        (+ x0 (* item_radius 0.77))
                        (+ y0 (* item_radius -2.11)) )
       (slide-center-to (caddr objects)
                        (+ x0 (* item_radius 1.78))
                        (+ y0 (* item_radius -0.21)) )
       (slide-center-to (cadr objects)
                        (+ x0 (* item_radius 0.55))
                        (+ y0 (* item_radius 1.43)) )
       (slide-center-to (car objects)
                        (+ x0 (* item_radius -0.79))
                        (+ y0 (* item_radius -0.0)) )
       )
      ;;; special cases
      ((= 1 (length objects))
       (slide-center-to (car objects) x0 y0))

      ((= 2 (length objects))
       (slide-center-to (cadr objects) (+ x0 item_radius) y0)
       (slide-center-to (car objects)  (- x0 item_radius) y0))

      ((= 3 (length objects))
       (slide-center-to (caddr objects) (+ x0 item_radius) (- y0 (* item_radius 0.4)) )
       (slide-center-to (cadr objects) x0 (+ y0 (* item_radius 0.8)))
       (slide-center-to (car objects) (- x0 item_radius) (- y0 (* item_radius 0.4)) ))

      ((= 4 (length objects))
       (slide-center-to (cadddr objects) x0 (- y0 (* item_radius 0.8)))
       (slide-center-to (caddr objects) (+ x0 (* item_radius 1.5)) y0)
       (slide-center-to (cadr objects) x0 (+ y0 (* item_radius 0.8)))
       (slide-center-to (car objects) (- x0 (* item_radius 1.5)) y0) )
      )
    )
  )

(define (arrange-in-spiral-onto-object-no-scaling objects targetobj)
  (arrange-in-spiral-no-scaling objects (bbcenter (send targetobj bbox)))
  (foreach objects (lambda (obj) (send obj raise targetobj))) )

