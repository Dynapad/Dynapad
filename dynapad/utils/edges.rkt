#lang racket/base

(provide Make-Tie-Between)

(require racket/class
         (only-in racket/gui/base button%)
         dynapad/misc/progress
         dynapad/misc/misc ; ic item config
         dynapad/layout/bbox
         ; cycle inducing
         ; dynapad/dynapad-c-api
         ; oval%
         (only-in dynapad/pad-state dynapad)
         (only-in dynapad/objects oval% line% polygon%)
         (only-in dynapad/events/text text%)
         (only-in dynapad/events/hyperlink set-*enact-link-fn*!)
         (only-in dynapad/events/mode
                  changemode
                  )
         (only-in dynapad/menu/menubar
                  use-bitmap-or-string
                  arcpane
                  clear-all-menu-buttons
                  button-double-clicked?
                  for-all-pads
                  push-*guest-button-off-labels*!
                  )
         (only-in dynapad/events/draw
                  set-Draw-multiple!
                  )
         dynapad/utils/graph
         )

#; ; no longer needed
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

(define (Make-Tie-Between type from to)
  (let ((edge (make-object type (send from dynapad))))
    (send edge from from)
    (send edge to to)
    edge))

; ---- Menubar Widgets -----
; supplement to events.ss and menubar.ss
(define (initCreateEdge argPAD)
  (send argPAD focus)
  (set-*enact-link-fn*!
        (lambda (source target)
          (unless (list? target)
            (Make-Tie-Between graph-edge% source target))))
  (changemode argPAD "CreateLink"))

(define (initCreateArc argPAD)
  (send argPAD focus)
  (set-*enact-link-fn*!
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
                 (set-Draw-multiple! (button-double-clicked?))
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
                 (set-Draw-multiple! (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initCreateArc argPAD)))
                 ))
  )

(push-*guest-button-off-labels*! (list btn_edge off_graphedge))
(push-*guest-button-off-labels*! (list btn_arc off_grapharc))

(update-progress 1)
