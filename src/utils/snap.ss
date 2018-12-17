; $Id: snap.ss,v 1.1.1.1 2005/10/09 04:28:49 stanonik Exp $

; Makes an object snap to the edges of the view when dragged in run mode. An
; optional second parameter sets the snap range (in pixels from the edge).
(define (make-snappy obj . rest)
  (let* ((pix-range
           (if (and (not (null? rest)) (real? (car rest))) (car rest) 25))

         (grabbed-at-x #f)
         (grabbed-at-y #f)
         (orig-x #f)
         (orig-y #f)
         (orig-bbox #f)

         (bbl (lambda (bbox) (first bbox)))
         (bbb (lambda (bbox) (second bbox)))
         (bbr (lambda (bbox) (third bbox)))
         (bbt (lambda (bbox) (fourth bbox)))

         (snap-adjust
           (lambda (thing place range)
             (if (<= (abs (- thing place)) range) (- place thing) #f)))

         (snappy-grab
           (lambda (p e)
             (set! grabbed-at-x (event-x e))
             (set! grabbed-at-y (event-y e))
             (let* ((eo (event-obj e))
                    (xy (send eo xy)))
               (set! orig-x (first xy))
               (set! orig-y (second xy))
               (set! orig-bbox (send eo bbox)))
             #f))

         (snappy-drag
           (lambda (p e)
             (let* ((pdx (- (event-x e) grabbed-at-x))
                    (pdy (- (event-y e) grabbed-at-y))
                    (eo (event-obj e))
                    (view-bbox (send dynapad bbox))
                    (range (/ pix-range (send dynapad getzoom)))
                    (x-snap
                      (or (snap-adjust
                            (+ pdx (bbl orig-bbox)) (bbl view-bbox) range)
                          (snap-adjust
                            (+ pdx (bbr orig-bbox)) (bbr view-bbox) range)
                          0))
                    (y-snap
                      (or (snap-adjust
                            (+ pdy (bbt orig-bbox)) (bbt view-bbox) range)
                          (snap-adjust
                            (+ pdy (bbb orig-bbox)) (bbb view-bbox) range)
                          0)))
               (send eo xy (+ orig-x pdx x-snap) (+ orig-y pdy y-snap)))
             #f))
        )
    (send obj bind "<Run-ButtonPress-1>" snappy-grab)
    (send obj bind "<Run-B1-Motion>" snappy-drag)
  )
)
