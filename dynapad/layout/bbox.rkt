#lang racket/base
(require (only-in racket/math
                  sqr
                  )
         (only-in racket/class
                  send)
         dynapad/pad-state
         dynapad/misc/misc
         dynapad/misc/tools-misc
         dynapad/utils/lerp
         )

(provide bbwidth
         bbheight
         b0
         b1
         b2
         b3
         bbenclosed
         bbenclosedoron
         bbcenter
         bbwiden
         bbstretch
         bxc
         byc
         bb-crossing
         bb-crossing-fract
         bb-diag
         )

; many of these functions should be pushed down into a C implementation

; coord splitting
(define (list-evens lst)
  (cond ((null? lst) null)
        ((null? (cdr lst)) null)
        (else
         (cons (cadr lst) (list-evens (cddr lst))))))
(define (list-odds lst)
  (cond ((null? lst) null)
        ((null? (cdr lst)) lst)
        (else
         (cons (car lst) (list-odds (cddr lst))))))

; most/all of these bbox functions assume standard ordering of elements:
; bbox := (x-min y-min x-max y-max)

(define (make-bb ctr-x ctr-y dx . dy)
  (set! dy (if (null? dy) dx (car dy)))
  (let ((rx (/ dx 2))
        (ry (/ dy 2)))
    (list (- ctr-x rx)
          (- ctr-y ry)
          (+ ctr-x rx)
          (+ ctr-y ry))))

(define (normalize-bb bb)
  ;ensures that bb is in lo->hi form
  (list (b0 bb) (b1 bb) (b2 bb) (b3 bb)))

(define (bb-diag bbox) ;returns length of diagonal/hypotenuse
  (sqrt (+ (sqr (bbwidth bbox)) (sqr (bbheight bbox)))))

;
; 9 functions to return points on the bbox
;
(define (bbsw bbox)
  (list [list-ref bbox 0] [list-ref bbox 1]))

(define (bbw bbox)
  (list [list-ref bbox 0] [* 0.5 (+ [list-ref bbox 1] [list-ref bbox 3])] ))

(define (bbnw bbox)
  (list [list-ref bbox 0] [list-ref bbox 3]))

(define (bbn bbox)
  (list [* 0.5 (+ [list-ref bbox 0] [list-ref bbox 2])] [list-ref bbox 3]))

(define (bbne bbox)
  (list [list-ref bbox 2] [list-ref bbox 3]))

(define (bbe bbox)
  (list [list-ref bbox 2] [* 0.5 (+ [list-ref bbox 1] [list-ref bbox 3])]))

(define (bbse bbox)
  (list [list-ref bbox 2] [list-ref bbox 1]))

(define (bbs bbox)
  (list [* 0.5 (+ [list-ref bbox 0] [list-ref bbox 2])] [list-ref bbox 1]))

; bbcenter: Returns the center (x y) of a bounding box
(define (bbcenter bbox)
  (list [* 0.5 (+ [list-ref bbox 0] [list-ref bbox 2])]
        [* 0.5 (+ [list-ref bbox 1] [list-ref bbox 3])]))

;slightly safer access to bbox coords
;(define (b0 bbox) (min (list-ref bbox 0) (list-ref bbox 2)))
;(define (b2 bbox) (max (list-ref bbox 0) (list-ref bbox 2)))
;(define (b1 bbox) (min (list-ref bbox 1) (list-ref bbox 3)))
;(define (b3 bbox) (max (list-ref bbox 1) (list-ref bbox 3)))

; new #f-friendly versions:
(define (b0 bbox) (if (and (car bbox) (caddr bbox))
                      (min (car bbox) (caddr bbox))
                      (car bbox)))
(define (b2 bbox) (if (and (car bbox) (caddr bbox))
                      (max (car bbox) (caddr bbox))
                      (caddr bbox)))
(define (b1 bbox) (if (and (cadr bbox) (cadddr bbox))
                      (min (cadr bbox) (cadddr bbox))
                      (cadr bbox)))
(define (b3 bbox) (if (and (cadr bbox) (cadddr bbox))
                      (max (cadr bbox) (cadddr bbox))
                      (cadddr bbox)))

(define (bxc bbox) (* 0.5 (+ (list-ref bbox 0) (list-ref bbox 2))))
(define (byc bbox) (* 0.5 (+ (list-ref bbox 1) (list-ref bbox 3))))

; bbheight: Returns the height of a bounding box
(define (bbheight bbox) (- (b3 bbox) (b1 bbox)))

; bbwidth: Returns the width of a bounding box
(define (bbwidth bbox) (- (b2 bbox) (b0 bbox)))

; maxdim: Returns the maximum dimension of a bounding box
(define (maxdim bbox) (max (bbwidth bbox) (bbheight bbox)))

;--- bbox predicates ---------------------------------------------

; bbenclosed: Decides if the point {x,y} is enclosed by the bbox.
(define (bbenclosed x y bbox)
  (and
   (> x [list-ref bbox 0])
   (> y [list-ref bbox 1])
   (< x [list-ref bbox 2])
   (< y [list-ref bbox 3])))

; bbenclosedoron: Decides if the point {x,y} is enclosed by or on the bbox.
(define (bbenclosedoron x y bbox)
  (and
   (>= x (list-ref bbox 0))
   (>= y (list-ref bbox 1))
   (<= x (list-ref bbox 2))
   (<= y (list-ref bbox 3))))

;
; bbsurrounds
;
(define (bbsurrounds? bb1 bb2)
  (and (<= (car bb1) (car bb2))
       (<= (cadr bb1) (cadr bb2))
       (>= (caddr bb1) (caddr bb2))
       (>= (cadddr bb1) (cadddr bb2))))
;
; bboverlap? is fast boolean test for overlap of 2 bbs
;
(define (bboverlap? bb1 bb2)
  (and (<= (b0 bb1) (b2 bb2))
       (<= (b0 bb2) (b2 bb1))
       (<= (b1 bb1) (b3 bb2))
       (<= (b1 bb2) (b3 bb1))))

;--- bbox generation and manipulation functions ------------------

;
; bbunion: Returns the bounding box that contains all bbox arguments
; syntax:  (bbunion bbox_list) or (bbunion bbox1 bbox2 ... bboxn)
;                                ...but not (bbunion bbox1)
;
(define (bbunion bboxes . args)
  (if (null? args)
      (list (apply min (map (lambda (box) (car box)) bboxes))
            (apply min (map (lambda (box) (cadr box)) bboxes))
            (apply max (map (lambda (box) (caddr box)) bboxes))
            (apply max (map (lambda (box) (cadddr box)) bboxes)))
      (list
       (apply min (car bboxes) (map (lambda (box) (car box)) args))
       (apply min (cadr bboxes) (map (lambda (box) (cadr box)) args))
       (apply max (caddr bboxes) (map (lambda (box) (caddr box)) args))
       (apply max (cadddr bboxes) (map (lambda (box) (cadddr box)) args)))))

;
; bbunion-objects: same as bbunion but argument is list of objects
; [does less list building]
;
(define (bbunion-objects objectlist)
  (if (not (null? objectlist))
      (let* ((bb (send (car objectlist) bbox))
             (x0 (car  bb))
             (y1 (cadr  bb))
             (x2 (caddr  bb))
             (y3 (cadddr  bb))
             (bbo 0)
             )
        (for-each
         (lambda (obj)
           (set! bbo (send obj bbox))
           (set! x0 (min x0 (car bbo)))
           (set! y1 (min y1 (cadr bbo)))
           (set! x2 (max x2 (caddr bbo)))
           (set! y3 (max y3 (cadddr bbo)))
           )
         (cdr objectlist)
         )
        (list x0 y1 x2 y3)
        )
      '(0 0 1 1)
      )
  )

;
; bbintersection: Returns the bbox of overlap of bbox1, bbox2, or #f if none
;
(define (bbintersection bboxes . args)
  (let* ((result
          (if (null? args)
              (list (apply max (map (lambda (box) (car box)) bboxes))
                    (apply max (map (lambda (box) (cadr box)) bboxes))
                    (apply min (map (lambda (box) (caddr box)) bboxes))
                    (apply min (map (lambda (box) (cadddr box)) bboxes)))
              (list
               (apply max (car bboxes) (map (lambda (box) (car box)) args))
               (apply max (cadr bboxes) (map (lambda (box) (cadr box)) args))
               (apply min (caddr bboxes) (map (lambda (box) (caddr box)) args))
               (apply min (cadddr bboxes) (map (lambda (box) (cadddr box)) args))))))
    (if (and (<= (car result) (caddr result))
             (<= (cadr result) (cadddr result)))
        result
        #f))) ;no intersection

;
; bboutermost: If any bbox completely encloses (inclusive) all others, returns that,
; else #f
(define (bboutermost . bboxes)
  (let* ((union (bbunion bboxes))
         (result (filter (lambda (outer) (if (bbsurrounds? outer union)
                                             outer
                                             #f))
                         bboxes)))
    (if (null? result)
        #f
        (car result))))
;
; bbslide: returns new bbox = bb displaced by dx, dy
;
(define (bbslide bb dx dy)
  (list
   (+ (car bb) dx)
   (+ (cadr bb) dy)
   (+ (caddr bb) dx)
   (+ (cadddr bb) dy)))

; converts bbox to polygon-like coordinates
(define (coords-from-bbox bb)
  (list (car bb) (cadr bb)
        (caddr bb) (cadr bb)
        (caddr bb) (cadddr bb)
        (car bb) (cadddr bb)))

; a "cbox" is the rectangle bounding a set of coords
; differs from some objects' bbox because of pen width
(define (cbox-from-coords c)
  (let ((evens (list-evens c))
        (odds (list-odds c)))
    (list (apply safemin odds)
          (apply safemin evens)
          (apply safemax odds)
          (apply safemax evens))))

; bbstretch: returns a new bbox that is slightly bigger (or smaller).
; ex: (bbstretch bb 0.2)  20% bigger in x and y directions
;     (bbstretch bb 0.3 0.4)  30% bigger in x, 40% bigger in x,
;     (bbstretch bb -0.05)  5% _smaller_ in x and y
;     (bbstretch bb 0 0.1)  no change in x, 10% bigger in y
(define (bbstretch bbox f . args)
  (let  ((x0 (car bbox)) (y0 (cadr bbox)) (x1 (caddr bbox)) (y1 (cadddr bbox))(fy f))
    (when (not (null? args)) (set! fy (car args)))
    (list (- (* x0 (+ 1 f )) (* x1 f ))
          (- (* y0 (+ 1 fy)) (* y1 fy))
          (- (* x1 (+ 1 f )) (* x0 f ))
          (- (* y1 (+ 1 fy)) (* y0 fy)))))

(define (bbwiden bb f . args)
  (let  ((fy f))
    (when (not (null? args)) (set! fy (car args)))
    (list (- (b0 bb) f)
          (- (b1 bb) fy)
          (+ (b2 bb) f)
          (+ (b3 bb) fy))))

;
;bb-visible: bounding box containing all visible objects
;bb-all-objects; bounding box containing all objects
;
(define (bb-visible pad)
  (bbunion-objects (send pad visible)))

(define (bb-all-objects pad)
  (bbunion-objects (send pad objects)))


;-----------------------------------------------------------------
; some utilities for sticky objects
; screen coordinates <--> pad coordinates

(define (screen-height bb)
  (* (bbheight bb) (send dynapad getzoom)))

(define (screen-width bb)
  (* (bbwidth bb) (send dynapad getzoom)))

; speed warning: don't use these for hundreds of objects in real-time.
(define (screen-xy padx pady)
  (let ((pos (send dynapad view)))
    (list (* (caddr pos) (- padx (car  pos)))
          (* (caddr pos) (- pady (cadr pos))) )))

(define (screen-xy-from-corner padx pady corner)
  (let* ((sxy (screen-xy padx pady))
         (sx (car  sxy))
         (sy (cadr sxy))
         (bb (send dynapad bbox))
         (halfwidth  (* 0.5 (screen-width  bb)))
         (halfheight (* 0.5 (screen-height bb))) )
    (cond ((equal? corner "sw") (+= sx halfwidth) (+= sy halfheight))
          ((equal? corner "nw") (+= sx halfwidth) (-= sy halfheight))
          ((equal? corner "ne") (-= sx halfwidth) (-= sy halfheight))
          ((equal? corner "se") (-= sx halfwidth) (+= sy halfheight)))
    (list sx sy) ))


(define (pad-xy screenx screeny)
  (let ((pos (send dynapad view)))
    (list (+ (/ screenx (caddr pos)) (car pos))
          (+ (/ screeny (caddr pos)) (cadr pos)))))

(define (pad-xy-from-corner screenx screeny corner)
  (let* ((pxy (pad-xy screenx screeny))
         (px (car  pxy))
         (py (cadr pxy))
         (bb (send dynapad bbox))
         (halfwidth  (* 0.5 (bbwidth  bb)))
         (halfheight (* 0.5 (bbheight bb))) )

    (cond ((equal? corner "sw") (-= px halfwidth) (-= py halfheight))
          ((equal? corner "nw") (-= px halfwidth) (+= py halfheight))
          ((equal? corner "ne") (+= px halfwidth) (+= py halfheight))
          ((equal? corner "se") (+= px halfwidth) (-= py halfheight)))
    (list px py) ))

;--------- Relative BBs / Transformations ---------

; +---from---+
; | ?   +----+
; | =>  | to |
; +-----+----+
(define (make-bb-transform from-bb to-bb)
  ; computes a transformation T=(t0 t1 t2 t3) which
  ;  encodes to-bb relative to from-bb;
  ; i.e. to-bb = T(from-bb)
  ; Applying transformation to a new-bb
  ;  produces the corresponding bbox relative to new-bb
  ;  that to-bb is relative to from-bb
  (let* ((x0 (b0 from-bb))
         (y0 (b1 from-bb))
         (dx (- (b2 from-bb) x0))
         (dy (- (b3 from-bb) y0)))
    (map (lambda (x x0 d)
           (and x (/ (- x x0) d))) ;edge of to-bb may be #f (unconstrained)
         to-bb
         (list x0 y0 x0 y0)
         (list dx dy dx dy))
    ))

; +--new---+
; | T  +---+
; | => | ? |
; +----+---+
(define (apply-bb-transform T new-bb)
  (let* ((x0  (b0 new-bb))
         (y0  (b1 new-bb))
         (dx (- (b2 new-bb) x0))
         (dy (- (b3 new-bb) y0)))
    (map (lambda (t x0 d)
           (and t (+ (* t d) x0))) ;edge of T may be #f (unconstrained)
         T
         (list x0 y0 x0 y0)
         (list dx dy dx dy))))

; I.E: (set! T (make-bb-transform from to))
;      (apply-bb-transform T from) ==> to

;======== BB-Geometry (for EPS, im/convert, etc) =========

;Given a coord system defined by
; y-dir, scale, and a ref. bbox,
; return (W H X Y) of target in that coord system.
;Target may be either an obj or bbox.

; y-dir is one of:  (default is 'up)
;  'up      'down      'up-down
;  +-----    +--v--    +-----
;  |         |  v      |
;  |   +-+   |   +-+   | - +-+
;  |   L_|   |   L_|   | ^ L_|
;  |  ^      |         | ^
;  L__^__    L_____    L_^__

#; ; this doesn't seem to be used anywhere and obj->geometry-list is not defined anywhere
(define bb-geometry
  (case-lambda
    (            (ref-bb tgt)  (obj->geometry-list 'up     1 ref-bb tgt))
    (      (scale ref-bb tgt)  (obj->geometry-list 'up scale ref-bb tgt))
    ((y-dir scale ref-bb tgt)
     (let* ((tgt-bb (if (list? tgt)
                        tgt
                        (send tgt bbox)))
            (xoffset (- (b0 tgt-bb) (b0 ref-bb)))
            (yoffset
             (case y-dir
               ((down)    (- (b3 ref-bb) (b3 tgt-bb)))
               ((up)      (- (b1 tgt-bb) (b1 ref-bb)))
               ((up-down) (- (b3 tgt-bb) (b1 ref-bb)))
               (else (error "y-dir must be 'up, 'down, or 'up-down"))
               )))
       (map (lambda (n) (* scale n))
            (list (bbwidth tgt-bb) (bbheight tgt-bb)
                  xoffset yoffset))))
    ))


(define format-geometry-str
  ; generates an X-Windows-compatible string "WxH{+-}X{+-}Y"
  (case-lambda
    ((w h)
     (format "~ax~a" (round-to-int w) (round-to-int h)))
    ((w h x y) (let ((xint (round-to-int x))
                     (yint (round-to-int y)))
                 (string-append (format-geometry-str w h)
                                (format "~a~a~a~a"
                                        (if (negative? xint) "" "+")
                                        xint
                                        (if (negative? yint) "" "+")
                                        yint))))
    ))

(define bb-geometry->string
  ;geom may be (W H) or (W H X Y);
  ; suffix may be one of "!" "<" ">"
  (case-lambda
    ((geom)        (bb-geometry->string geom ""))
    ((geom sffx)
     (format "~a~a" (apply format-geometry-str geom) sffx))))
;   ((arg0 arg1 . more)
;    (obj->geometry-string
;     (apply obj->geometry-list arg0 arg1 more)))))


;; why the heck were these in edges !??!

(define (bb-crossing-fract segment bbox)
  (let* ((xfract (abs (/ (bbwidth bbox)
                         (* 2 (bbwidth segment)))))
         (yfract (abs (/ (bbheight bbox)
                         (* 2 (bbheight segment))))))
    (min xfract yfract)))

(define (bb-crossing segment bbox)
  ;segment is (ax ay bx by), where (bx by) is center of bbox
  ;returns (x y) point where segment crosses edge of bbox
  (let* ((fract (bb-crossing-fract segment bbox))
         ;use car-cadddr instead of b0-b3 to override auto-correct
         (tipx  (lerp fract (caddr segment) (car segment)))
         (tipy  (lerp fract (cadddr segment) (cadr segment))))
    (list tipx tipy)))
