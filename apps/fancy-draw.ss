;TOOLS
(define (car2 lst)
  (list (car lst) (cadr lst)))

(define (cdr2 lst)
  (list-tail lst 2))

(define (make-pairs lst)
; takes list (x1 y1 x2 y2...) and returns ((x1 y1) (x2 y2)...)
  (if (null? lst)
      '()
      (cons (car2 lst) (make-pairs (cdr2 lst)))))

;== Dan's fancy draw mode
;== Following defs augment/replace drawing-related defs in events.ss
(define (changemode dynapad mode)
  (let ((curmode (send dynapad modifier 'get)))
    (send dynapad focus)

    (cond
     ((equal? curmode "Draw")
      (begin
	(if (and (equal? mode "Select") Draw-object)
	    (send Draw-object select))
	(unhandleize-draw-object)))
     ((equal? curmode "Text")
      (let ((obj (send dynapad getfocus)))
	(if obj (send obj unfocus)))))

    (if (string=? mode "Draw")
	(let ((selected (send dynapad selected)))
	  (if (= 1 (length selected))
	      (maybe-handleize-draw-object (car selected)))))

    (if (and (string=? curmode "Select")
           (or (string=? mode "Run")
	       (string=? mode "Draw")
	       (string=? mode "")))
      (map (lambda (o) (send o unselect)) (send dynapad selected)))

    (send dynapad modifier 'set mode))
    (gui-update-mode)
    )

(define *new-handle* #f)
(define *vtx-handles* null)
(define vertex-handle%
  (class rect%
	 (init-field i)   ;index of handle (0-#vtxs)
	 (init	     pt   ;(x y) for handle
		     (sz 2) ;box radius in pixels
		     (color "white")) ;box color
	 (field (prev-edge #f)
		(next-edge #f)
		(2i #f))
	 (super-instantiate(dynapad))

	 (define/public (refresh newi)
	   (set! i newi)
	   (when (not (null? *edge-handles*))
		 (set! prev-edge (list-ref *edge-handles*
					   (if (positive? i)
					       (- i 1)
					       (- (length *edge-handles*) 1)))) ;wrap
		 (set! next-edge (list-ref *edge-handles*
					   (if (< i (length *edge-handles*))
					       i
					       0)))) ;wrap
	   (set! 2i (* 2 i)))

	 (define/public (increment)
	   (refresh (+ i 1)))

	 (define/public (decrement)
	   (refresh (- i 1)))

	 (define/public (drag-handle-to x y)
	   (let* ((z (send this z))
		  (c (send Draw-object coords)))
	     (send this position (list x y z))
	     (set-car! (list-tail c 2i) x)
	     (set-car! (list-tail c (+ 2i 1)) y)
	     (if prev-edge
		 (send prev-edge move-tail-to x y))
	     (if next-edge
		 (send next-edge move-head-to x y))
	     (send Draw-object coords c)))
	 
	 (let* ((zoom (caddr (send dynapad view)))
		(radius (/ sz zoom))
		(x (car pt))
		(y (cadr pt)))
	   (send this coords (list (- x radius) (- y radius) (+ x radius) (+ y radius)))
	   (send this sticky "z")
	   (send this pen color)
	   (send this fill "black")
	   (send this findable #f)
	   (send this bind "<Draw-B1-Motion>"   ;move this vtx
		 (lambda (dynapad e)
		   (let ((x (event-x e))
			 (y (event-y e)))
		     (send this drag-handle-to x y))))

 	   (send this bind "<Draw-Shift-ButtonPress-1>"  ;cut this vtx
 		 (lambda (dynapad e)
		   (when *edge-handles* ;if no edges, can't cut vtx
			 (let* ((c (send Draw-object coords)))
			   (when (> (length c) 4) ; at least 3 points
				 ;close edge ring
				 (send/apply prev-edge move-tail-to ;next vtx
					     (let ((c-tail
						    (list-tail c
						     (if (< (+ 2i 2) (length c))
							 (+ 2i 2)
							 0)))) ;wrap
					       (list (car c-tail) (cadr c-tail))))
				 ; kill extra edge
				 (send (list-ref *edge-handles* i) delete)
			         ;cut item i from coords, *vtx-hndls*, and *edge-hndls*
				 (if (zero? i)
				     (begin
				       (send Draw-object coords (cddr c))
				       (set! *vtx-handles* (cdr *vtx-handles*))
				       (set! *edge-handles* (cdr *edge-handles*)))
				     (let ((c-tail (list-tail c (- 2i 1)))
					   (v-tail (list-tail *vtx-handles* (- i 1)))
					   (e-tail (list-tail *edge-handles* (- i 1))))
				       (set-mcdr! c-tail (cdddr c-tail))
				       (send Draw-object coords c)
				       (set-mcdr! v-tail (cddr v-tail))
				       (set-mcdr! e-tail (cddr e-tail))))
				 ;renumber remaining edges/vtxs
				 (for-each (lambda (edg) (send edg decrement))
					   (list-tail *edge-handles* i))
				 (for-each (lambda (vtx) (send vtx decrement))
					   (list-tail *vtx-handles* i))
				 ;kill this vtx
				 (send this delete))))))

	 (refresh i)
	 (set! *new-handle* this))))

(define *edge-handles* null)
(define edge-handle%  ;invisible edge, used to insert vtxs
  (class line%
	 (init-field i)
	 (init pt1 pt2)

	 (define/public (move-head-to x y)
	   (let ((c (send this coords)))
	     (send this coords (list x y (caddr c) (cadddr c)))))
	 (define/public (move-tail-to x y)
	   (let ((c (send this coords)))
	     (send this coords (list (car c) (cadr c) x y))))
	 (define/public (increment)
	   (set! i (+ i 1)))
	 (define/public (decrement)
	   (set! i (- i 1)))
	 (define/public (insert-new-vtx x y)
	   (let* ((my-c (send this coords))
			(edges-tail (list-tail *edge-handles* i))
			(vtxs-tail (list-tail *vtx-handles* i))
			(c (send Draw-object coords))
			(c-tail (list-tail c (+ 1 (* 2 i)))))
		   (set-mcdr! edges-tail
			     (cons (make-object edge-handle% (+ i 1) (list x y) (list-tail my-c 2))
				   (cdr edges-tail)))
		   (for-each (lambda (edg) (send edg increment))
			     (cddr edges-tail))
		   (set-mcdr! vtxs-tail
			     (cons (make-object vertex-handle% (+ i 1) (list x y))
				   (cdr vtxs-tail)))
		   (for-each (lambda (vtx) (send vtx increment))
			     (cddr vtxs-tail))
		   (send (car *vtx-handles*) refresh 0)
		   (for-each (lambda (vtx) (send vtx raise))
			     *vtx-handles*)
		   (send this move-tail-to x y)
		   (set-mcdr! c-tail (append (list x y) (cdr c-tail)))
		   (send Draw-object coords c)))

	 (super-instantiate (dynapad))
	 (send this pen (send Draw-object pen))
	 (send this findable #f)
	 (send this transparency .2)
	 (send this coords (list (car pt1) (cadr pt1) (car pt2) (cadr pt2)))
	 (send this bind "<Draw-ButtonPress-1>" ;insert new vtx on this edge
	       (lambda (dynapad e)
		 (let* ((x (event-x e))
			(y (event-y e)))
		   (insert-new-vtx x y))))
	 ))

(define (unhandleize-draw-object)
  (for-each (lambda (it) (send it delete)) *vtx-handles*)
  (set! *vtx-handles* null)
  (for-each (lambda (it) (send it delete)) *edge-handles*)
  (set! *edge-handles* null)
  (set! Draw-object #f)
  (set! *new-handle* #f))


(define (handleize-draw-object obj)
      (let ((c (make-pairs (send obj coords)))) 
	(unhandleize-draw-object)
	(set! Draw-object obj)
	(when (or (is-a? obj polygon%)
		  (is-a? obj line%))
	      (set! *edge-handles*
		    (let ((i -1))
		      (map (lambda (pt1 pt2)
			     (set! i (+ i 1))
			     (make-object edge-handle% i pt1 pt2))
			   c
			   (append (cdr c) (list (car c)))))))
	(set! *vtx-handles* ;also sets *new-handle* to last-generated vtx
	      (let ((i -1))
		(map (lambda (pt)
		       (set! i (+ i 1))
		       (make-object vertex-handle% i pt))
		     c)))
	))

(define (maybe-handleize-draw-object obj)
  (when (and obj
	   (send obj findable)
	   (not (eq? obj Draw-object)))
	(handleize-draw-object obj)
	(set! *new-handle* #f)))

(define *freehanding* #f)
(send dynapad bind "<Draw-ButtonPress-1>"
      (lambda(dynapad e)
	(let ((x (event-x e))
	      (y (event-y e))
	      (obj (event-obj e)))
	  (if (eq? obj dynapad)
	      ;click on pad surface
	      (if (not Draw-object)
		  (begin  ;no selection; draw new object
		    (set! Draw-object (make-object Draw-class dynapad))
		    (set! obj Draw-object)
		    (if (eq? Draw-class freehand%)
			(begin ;freehand drawing
			  (set! *freehanding* #t)
			  (send Draw-object coords (list x y)))
			(begin ;else
			  (if (has-method? Draw-object 'fill)
			      (let ((thefill (if (send dynapad fill?) (send dynapad defaultfill) "none")))
				(send Draw-object fill thefill)))
			  (if (has-method? Draw-object 'pen)
			      (send Draw-object pen (send dynapad defaultpen)))
		    
			  (send Draw-object coords (list x y x y)) ;make 2 points, initially the same
			  (handleize-draw-object Draw-object))))
		  
		  (unhandleize-draw-object)) ;unselect object
	      ;click on existing object
		(maybe-handleize-draw-object obj)))))
      

(send dynapad bind "<Draw-Double-ButtonPress-1>"
  (lambda(dynapad e)
    (changemode dynapad "Select")
    (gui-update-mode)))

(send dynapad bind "<Draw-KeyPress-Escape>"
  (lambda(dynapad e)
    (changemode dynapad "Select")
    (gui-update-mode)))
	
  (send dynapad bind "<Draw-B1-Motion>"
     (lambda(dynapad e)
       (if *freehanding*
	   (send Draw-object coords (append (send Draw-object coords)
					    (list (event-x e) (event-y e))))
	   (if *new-handle*
	       (send *new-handle* drag-handle-to (event-x e) (event-y e))))))

  (send dynapad bind "<Draw-ButtonRelease-1>"
    (lambda(dynapad e)
      (set! *new-handle* #f)
      (if *freehanding*
	  (set! *freehanding* #f))))

