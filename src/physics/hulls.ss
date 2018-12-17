(dynaload "geometry.ss")

; The following is a 2D implementation of the QuickHull algorithm
;
;           p  (left-pts)         p
;                p             
;                        p     ^ lnormal
;  line ....a________b.....i...!
;               seg        |
;                    p     | offset
;         p   (right-pts)  p

(define (partition-points-with-seg a b pts)
;a, b are start, end of segment
; pts are set of points to partition (may include a, b)
; returns (left-pts rgt-pts)
; where left-pts and rgt-pts are ((pt dist)...)
;  where dist is normal distance from pt to line of seg
  (let* ((seg  (make-object geo-segment% a b))
	 (line (send seg copy-as-line))
	 (lnormal (send seg l-normal-angle))
	 (lpts null)
	 (rpts null))
    (for-each
     (lambda (p)
       (if (not (or (eq? p a) (eq? p b)))
	   (let* ((intersect (send line closest-pt-to p))
		  (offset (send intersect vector-to p))
		  (len (send offset length))
		  (pair (list p len)))
	     (if (positive? len) ;eliminate colinear (len=0) pts
		 (if (positive? (send lnormal unit-dot offset))
		     (push! pair lpts)  ;left-side point
		     (push! pair rpts)  ;right-side point
		 )))))
     pts)
    (list lpts rpts)))

(define (partition-points-with-seg a b pts)
; faster version using cross-prod
  (let* ((ab   (send a vector-to b))
	 (lpts null)
	 (rpts null))
    (for-each
     (lambda (c)
       (if (not (or (eq? c a) (eq? c b)))
	   (let* ((ac (send a vector-to c))
		  (len (send ab cross ac)))
	     (unless (zero? len) ;eliminate colinear (len=0) pts
		     (if (positive? len)
			 (push! (list c len) lpts)  ;left-side point
			 (push! (list c (- len)) rpts)  ;right-side point
			 )))))
     pts)
    (list lpts rpts)))


(define (hull-circuit pt-a pt-b pts outermost?)
  (let* ((partition (partition-points-with-seg pt-a pt-b pts))
	 (left-prs (car partition))
	 (right-prs (cadr partition))
	 (circuit (half-circuit pt-a pt-b left-prs)))
    (if outermost?
	(set! circuit
	      (append (list pt-a)
		      (half-circuit pt-b pt-a right-prs)
		      (list pt-b)
		      circuit)))
    circuit))


(define (half-circuit pt-a pt-b pairs)
  (let* ((pts (map car pairs))
	 (maxdist -1)
	 (pt-c #f))
    (foreach pairs
	     (lambda (pr)
	       (let ((mydist (cadr pr)))
		 (if (> mydist maxdist)
		     (begin
		       (set! pt-c (car pr))
		       (set! maxdist mydist))))))
    (if pt-c
	(begin
;	  (display (format "a:~a b:~a c:~a~%"
;			   (send pt-a xy)
;			   (send pt-b xy)
;			   (send pt-c xy)))
	  (append (hull-circuit pt-c pt-b pts #f)
		  (list pt-c)
		  (hull-circuit pt-a pt-c pts #f)))
	  null)))

(define (sidemost-points pts)
  (let* ((first (car pts))
	 (minx (send first x))
	 (maxx minx)
	 (left-pt first)
	 (right-pt first))
    (foreach pts
	     (lambda (pt)
	       (let ((myx (send pt x)))
		 (if (< myx minx)
		     (begin
		       (set! left-pt pt)
		       (set! minx myx)))
		 (if (> myx maxx)
		     (begin
		       (set! right-pt pt)
		       (set! maxx myx))))))
    (list left-pt right-pt)))

(define (convex-hull pts)
; returns subset of pts, in CCW order, that form convex hull of pts
  (if (and (not (null? pts)) (list? (car pts))) ;given raw coords?
      (set! pts (map (lambda (xy) (make-object geo-point% xy)) pts))) ;convert to geo-points
  (let ((len (length pts)))
    (if (<= len 2)
	pts  ;if 2 or fewer points, return those
	; note: cannot be 3 or fewer, b/c must ensure 3 pts are in CCW order
	;else return hull
	(let* ((sides (sidemost-points pts))
	       (left (car sides))
	       (right (cadr sides)))
	  (hull-circuit left right pts #t))) ;left/right could also be reversed
      ))

(define (convex-hull-polygon pts)
  (make-object geo-polygon% (convex-hull pts)))

;DEBUGGING TOOLS----------------
  
(define (random-coord lo hi bins)
  (let ((x (+ (* (random bins) (/ (- hi lo) bins)) lo))
        (y (+ (* (random bins) (/ (- hi lo) bins)) lo)))
    (list x y)))
 
(define (random-points n lo hi bins)
  (if (zero? n)
      null
      (cons (make-object geo-point% (random-coord lo hi bins))
            (random-points (- n 1) lo hi bins))))

(define (render-points pts)
  (map 
   (lambda (pt)
     (ic (make-object oval% dynapad '(0 0 1 1))
	 (anchor "center")
	 (xy (send pt xy))))
   pts))

(define (render-circuit cir)
  (make-object polygon% dynapad
	       (apply append (map (lambda (pt) (send pt xy)) cir))))