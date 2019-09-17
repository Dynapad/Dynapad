(announce-module-loading "graph tools...")
; Originally adapted from 
; /home/Share/high/dyna/toys/dyna_constraints/dyna_constraints.ss
;-----------------------------------------------------------------

(define (make-nub x y)
  (ic (make-object oval% dynapad (list (- x 2)(- y 2)(+ x 2)(+ y 2)))
      (fill "white")(pen "black")(penwidth 2)))

(define (make-label xy text)
  (ic (make-object text% dynapad text (append xy '(1.0))) (pen "black")))

;---------

(define (center-of-line l)
  (let ((crds (send l coords)))
    (list (* 0.5 (+ (b0 crds) (b2 crds)))
          (* 0.5 (+ (b1 crds) (b3 crds))))))

(define (xycenter obj) (bbcenter (send obj bbox)))

(define (make-connect a b)
  (let ((l (ic (make-object line% dynapad (append (xycenter a) (xycenter b))) (pen "black"))))
    (send l lower)
    (send a slide-callbacks (cons
      (list
      (lambda (o dx dy) (send l coords (append (xycenter a) (xycenter b))))
      )
      (send a slide-callbacks)))
    (send b slide-callbacks (cons
      (list
      (lambda (o dx dy) (send l coords (append (xycenter a) (xycenter b))))
      )
      (send b slide-callbacks)))
    l
  )
)


(define (fancy-connect a b)
  (let ((l (ic (make-object line% dynapad (append (xycenter a) (xycenter b))) (pen "black")))
        (na (apply make-nub (append (xycenter a))))
        (nb (apply make-nub (append (xycenter b))))
        (fnc #f)
        )
    (send l lower)
    (send na raise)
    (send nb raise)
    (set! fnc
      (lambda (o dx dy)
        (let*((pa #f)
              (pb #f)
              (bb_a (bbwiden (send a bbox) 3))
              (bb_b (bbwiden (send b bbox) 3))
              (a_xc (car (bbcenter bb_a)))
              (a_yc (cadr (bbcenter bb_a)))
              (b_xc (car (bbcenter bb_b)))
              (b_yc (cadr (bbcenter bb_b)))
              (a_nearxy
            (cond
                  ((and (> (b0 bb_b) (b2 bb_a)) (> (b1 bb_b) (b3 bb_a))) (list (b2 bb_a) (b3 bb_a)))
                  ((and (< (b2 bb_b) (b0 bb_a)) (< (b3 bb_b) (b1 bb_a))) (list (b0 bb_a) (b1 bb_a)))
                  ((and (> (b0 bb_b) (b2 bb_a)) (< (b3 bb_b) (b1 bb_a))) (list (b2 bb_a) (b1 bb_a)))
                  ((and (< (b2 bb_b) (b0 bb_a)) (> (b1 bb_b) (b3 bb_a))) (list (b0 bb_a) (b3 bb_a)))
                  ((>= (b0 bb_b) (b2 bb_a)) (list (b2 bb_a) a_yc))
                  ((<= (b2 bb_b) (b0 bb_a)) (list (b0 bb_a) a_yc))
                  ((>= (b1 bb_b) (b3 bb_a)) (list a_xc (b3 bb_a)))
                  ((<= (b3 bb_b) (b1 bb_a)) (list a_xc (b1 bb_a)))
                  (else (list a_xc a_yc))))
              (b_nearxy
            (cond
                  ((and (> (b0 bb_a) (b2 bb_b)) (> (b1 bb_a) (b3 bb_b))) (list (b2 bb_b) (b3 bb_b)))
                  ((and (< (b2 bb_a) (b0 bb_b)) (< (b3 bb_a) (b1 bb_b))) (list (b0 bb_b) (b1 bb_b)))
                  ((and (> (b0 bb_a) (b2 bb_b)) (< (b3 bb_a) (b1 bb_b))) (list (b2 bb_b) (b1 bb_b)))
                  ((and (< (b2 bb_a) (b0 bb_b)) (> (b1 bb_a) (b3 bb_b))) (list (b0 bb_b) (b3 bb_b)))
                  ((>= (b0 bb_a) (b2 bb_b)) (list (b2 bb_b) b_yc))
                  ((<= (b2 bb_a) (b0 bb_b)) (list (b0 bb_b) b_yc))
                  ((>= (b1 bb_a) (b3 bb_b)) (list b_xc (b3 bb_b)))
                  ((<= (b3 bb_a) (b1 bb_b)) (list b_xc (b1 bb_b)))
                  (else (list b_xc b_yc))))
               )
          
          (send l coords (append a_nearxy b_nearxy))
          (send na xy (car a_nearxy) (cadr a_nearxy))
          (send nb xy (car b_nearxy) (cadr b_nearxy))
        )
      )
    )
    (send a slide-callbacks (cons (list fnc) (send a slide-callbacks)))
    (send b slide-callbacks (cons (list fnc) (send b slide-callbacks)))
    l
  )
)


(define (label-connect l txt)
  (let ((na (apply make-nub (append (xycenter l))))
        (txt (make-label (xycenter l) txt))
        (fnc #f)
        )
    (send na raise)
    (send txt anchor "sw")

    (set! fnc
      (lambda (o a)
        (let ((xy (center-of-line l)))
          (send na xy (car xy) (cadr xy))
          (send txt xy (car xy) (cadr xy))
        )
      )
    )
    (send l coords-callbacks (list (list fnc)))
    na
  )
)

;================
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

;(define arc-hilight%
;  (class hilight%
;    (init dynapad-arg object-arg (label-arg #f))
;    (inherit-field _dynapad _object _cptr _deleted?)
;    (super-instantiate (dynapad-arg object-arg label-arg))
;    (define-override 

(define (Make-Tie-Between type from to)
  (let ((edge (make-object type (send from dynapad))))
    (send edge from from)
    (send edge to to)
    edge))

; ---- Menubar Widgets -----
; supplement to events.ss and menubar.ss
(define (initCreateEdge argPAD)
  (send argPAD focus)
  (set! *enact-link-fn*
    (lambda (source target)
      (unless (list? target)
         (Make-Tie-Between graph-edge% source target))))
  (changemode argPAD "CreateLink"))

(define (initCreateArc argPAD)
  (send argPAD focus)
  (set! *enact-link-fn*
    (lambda (source target)
      (unless (list? target)
         (Make-Tie-Between graph-arc% source target))))
  (changemode argPAD "CreateLink"))

(define off_grapharc  (use-bitmap-or-string "pad/bitmaps/grapharc.xbm"  "Graph Arc"))
(define off_graphedge (use-bitmap-or-string "pad/bitmaps/graphedge.xbm" "Graph Edge"))
(define on_grapharc   (use-bitmap-or-string "pad/bitmaps/On_grapharc.xpm"  "Graph Arc"))
(define on_graphedge  (use-bitmap-or-string "pad/bitmaps/On_graphedge.xpm" "Graph Edge"))

(define btn_edge
  (make-object button%
     off_graphedge
     arcpane
     (lambda (button event)
       (clear-all-menu-buttons)
       (send button set-label on_graphedge)
       (set! Draw-multiple (button-double-clicked?))
       (for-all-pads (lambda (argPAD)
        (initCreateEdge argPAD)))
       ))
  )
(define btn_arc
  (make-object button%
     off_grapharc
     arcpane
     (lambda (button event)
       (clear-all-menu-buttons)
       (send button set-label on_grapharc)
       (set! Draw-multiple (button-double-clicked?))
       (for-all-pads (lambda (argPAD)
        (initCreateArc argPAD)))
       ))
  )

(push! (list btn_edge off_graphedge)
       *guest-button-off-labels*)
(push! (list btn_arc off_grapharc)
       *guest-button-off-labels*)

(update-progress 1)
