(send dynapad modifier 'create "Zoom")

(define zoomtype 0)
(define sxsy #f)
(define x0y0 #f)
(define x1y1 #f)
(define press? #f)

(send dynapad bind "<Zoom-ButtonPress-1>"
  (lambda(eventPAD e)
    (set! sxsy (list (event-sx e) (event-sy e)))
    (set! x0y0 (list (event-sx e) (event-sy e)))
    (set! x1y1 #f)
    (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
    (set! press? #t)))

(send dynapad bind "<Zoom-ButtonRelease-1>"
  (lambda(eventPAD e)
    (set! press? #f)))

(send dynapad bind "<Zoom-Motion>"
  (lambda(eventPAD e)
    (when press?
      (let
	((x0 (car x0y0))
	 (y0 (cadr x0y0))
         (x2 (event-sx e))
	 (y2 (event-sy e)))

	(if (not x1y1)
	  (when (> (+ (abs (- x2 x0)) (abs (- y2 y0))) 10)
	    (set! x1y1 (list x2 y2)))
	  (let
	    ((x1 (car x1y1))
	     (y1 (cadr x1y1)))
	    (when (> (+ (abs (- x2 x1)) (abs (- y2 y1))) 10)
	      (let*
	        ((ax (- x1 x0))
	         (ay (- y1 y0))
	         (bx (- x2 x1))
	         (by (- y2 y1))
	         (cp (- (* ax by) (* ay bx)))
		 (zfac (+ 1 (/ cp 100000))))

	        (cond
	          ((= zoomtype 1)
		    (let
		      ((sgn (if (positive? cp) 1 -1))
		       (dst (sqrt
		         (+
		           (expt (- x2 (car sxsy)) 2)
		           (expt (- y2 (cadr sxsy)) 2)))))
		      (set! zfac (if (zero? cp) 1 (+ 1 (/ (* sgn dst) 10000)))))
	            (set! x0y0 x1y1)))
	        (set! x1y1 (list x2 y2))
	        (send dynapad zoom zfac 0
		  (sendf eventPAD evs lastx)
		  (sendf eventPAD evs lasty))))))))))

(gui-mode-cursor "Zoom" 14)
