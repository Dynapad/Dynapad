
;------------------- misc polygon routines -----------------------

(define (polygon-contains-pt? pp pt)
  (define bb (send pp bbox))
  (if (not (bbenclosedoron (car pt) (cadr pt) bb))
    #f
    ;else
    (let ()
      (define ray (list (car pt) (cadr pt) (car pt) (+ (b3 bb) (bbheight bb))))
      (define relevant-edges (make-lazy-line-segs (send pp coords)))
      (define crossed-edges
        (filter
          (lambda (edge) (vertical-ray-intersect-linesegs? ray edge))
          relevant-edges))
      (odd? (length crossed-edges)))))

;-----

(define (make-lazy-line-segs crds)
  (define len (length crds))
  (cons
    (list (list-ref crds (- len 2))
          (list-ref crds (- len 1))
          (car crds)
          (cadr crds) )
    (make-lazy-line-segs-aux crds)))

(define (make-lazy-line-segs-aux crds)
  (if (< (length crds) 4)
    null
    (cons crds (make-lazy-line-segs-aux (cddr crds)))))

;-----
; ray is assumed to travel vertically upward from given xy point
(define (vertical-ray-intersect-linesegs? ray segB)
  (cond ((< (max (car  segB)(caddr  segB)) (car ray)) #f)
        ((> (min (car  segB)(caddr  segB)) (car ray)) #f)
        ((< (max (cadr segB)(cadddr segB)) (cadr ray)) #f)
        (else (let* ((f (/ (- (car ray) (car segB)) (- (caddr segB) (car segB))))
                     (y (+ (cadr segB) (* f (- (cadddr segB) (cadr segB))))))
                (> y (cadr ray)) ))))

(define (intersection-of-line-segments segA segB)
  (cond ((> (min (car segA)(caddr segA)) (max (car segB)(caddr segB))) #f)
        ((< (max (car segA)(caddr segA)) (min (car segB)(caddr segB))) #f)
        ((> (min (cadr segA)(cadddr segA)) (max (cadr segB)(cadddr segB))) #f)
        ((< (max (cadr segA)(cadddr segA)) (min (cadr segB)(cadddr segB))) #f)
          ; from http://www.faqs.org/faqs/graphics/algorithms-faq/  
          ; find: "intersections of 2 2D line segments"
        (else (let* ((A.dx (- (caddr segA) (car segA)))
                     (A.dy (- (cadddr segA) (cadr segA)))
                     (B.dx (- (caddr segB) (car segB)))
                     (B.dy (- (cadddr segB) (cadr segB)))
                     (AB.dx (- (car segA) (car segB)))
                     (AB.dy (- (cadr segA) (cadr segB)))
                     (denom (- (* A.dx B.dy) (* A.dy B.dx)))
                     (r (/ (- (* AB.dy B.dx) (* AB.dx B.dy)) denom))
                    )
                (cond ((zero? denom) (if (zero? r)
                                         ; parallel lines
                                         null
                                         ; colinear lines
                                         (list (car segA) (cadr segA)) ))
                      ((< r 0.0) #f)
                      ((> r 1.0) #f)
                      (else (list (+ (car segA) (* r A.dx))
                                  (+ (cadr segA) (* r A.dy)))))))))

;-----
;(dynaload "tools-lists.ss")

(define geo-actor-name 'geon)
(define (geodize it)  ;creates dummy actor
  (define geo-obj (make-object actor%))
  geo-obj
)

(define contains-pt?
  (case-lambda
   ((container xy)  (if (list? xy)
			(apply contains-pt? container xy)
			(error ("Expects xy-pair or x, y args\n"))))
   ((container x y)  (and
		      (bbenclosedoron x y (send container bbox))
		      ;(send-actor-named container geo-actor-name contains-pt? (list x y))
                      ))))

;--- Containment -------------------------------------------------
(define (centroid-in? obj container)
  (contains-pt? container (bbcenter (send obj bbox))))

; 1) centroid of obj is inside boundary of container, and
; 2) bbox of container not strictly within bbox of obj
; (this excludes objs which dwarf container)
(define (contained-in? obj container)
  (and (centroid-in? obj container)
       (not (bbsurrounds? (send obj bbox) (send container bbox)))))

; simple version
;(define (contained-in? obj1 obj2)
;  (bbsurrounds? (send obj2 bbox) (send obj1 bbox)))

;--- Containment end ---------------------------------------------

(define (intersection? obj1 obj2)
  (bboverlap? (send obj1 bbox) (send obj2 bbox)))

(define (contained-objects container)
  (let* ((bbox (send container bbox))
	 (intruders (send dynapad find 'groupmembers 'overlapping bbox)))
    (filter (lambda (it) (and (not (eq? it container))
			      (contained-in? it container)))
	    intruders)))


(define (add-objects-to-greedy-region region-box imagelist)
  (let ((region-actor (get-region-actor region-box)))
    (foreach imagelist (lambda (img) (send region-actor include-obj img)))
    (send region-actor trigger-post-action)
  )
)

;--- dummy calls ---

;(define (cross-map fn lst1 lst2) ())
;(define (filtered-cross-map filter-fn map-fn l1 l2) ())
;(define (cross-filter fn lst1 lst2) ())
;(define (list-intersect+diff l1 l2 mem-fn) ())

(define cross-map
;returns list of results of fn applied to every pair in
; lst1 X lst2
  (case-lambda
   ((fn lst1 lst2)
    (apply append
	   (map (lambda (c)
		  (map (lambda (l) (fn c l)) lst2))
		lst1)))))

(define (filtered-cross-map filter-fn map-fn l1 l2)
  (filter filter-fn (cross-map map-fn l1 l2)))

(define cross-filter
  (case-lambda
   ((fn lst1 lst2)
    (apply append
	   (map (lambda (c)
		  (filter (lambda (l) (fn c l)) lst2))
		lst1)))))

(define (list-intersect+diff l1 l2 mem-fn)
  (if (null? l1)
      (list null null)
      (let* ((this (car l1))
	     (rest (list-intersect+diff (cdr l1) l2 mem-fn))
	     (ints (car rest))
	     (diffs (cadr rest)))
	(if (mem-fn this l2)
	    (list (cons this ints)
		  diffs)
	    (list ints
		  (cons this diffs))))))
