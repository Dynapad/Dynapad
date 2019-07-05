;=== Reshape and Resize ==========================================

; general handles provide a dragable object with configurable callback
(define general-handle%
  (class rect%
    (init (argPAD currentPAD))
    (init (x 0) (y 0))
    (init-field (_drag_callback #f))
    (init-field (_itemdelete_callback #f))

    (super-instantiate (argPAD))

    (let* ((zoom (send argPAD getzoom))
           (radius (/ 3 zoom)))
      (send this coords (list (- x radius) (- y radius) (+ x radius) (+ y radius)))
      (send this sticky "z")
      (send this pen "white")
      (send this fill "black")
      (send this findable #f))

    ;--- methods ---

    (define/public drag-callback
      (case-lambda 
        (() _drag_callback)
        ((new_fnc) (set! _drag_callback new_fnc))
      ))

    (send this bind "<Enter>" (lambda (w e) (send this fill "red") #f))
    (send this bind "<Leave>" (lambda (w e) (send this fill "black") #f))

    (send this bind "<Select-ButtonPress-1>"   (list (lambda (w e) #f)))
    (send this bind "<Select-ButtonRelease-1>" (list (lambda (w e) #f)))
    (send this bind "<Run-ButtonPress-1>"      (list (lambda (w e) #f)))
    (send this bind "<Run-ButtonRelease-1>"    (list (lambda (w e) #f)))

    (send this bind "<Select-B1-Motion>" 
      (list
      (lambda (eventPAD e) (set! currentPAD eventPAD)
        (let ((x (event-x e)) (y (event-y e)))
          (send this xy x y)
          (when _drag_callback (_drag_callback x y))
        )
        #f
      )
      )
    )
    (send this bind "<Run-B1-Motion>" (send this bind "<Select-B1-Motion>"))
))

(define (make-lazy-pairs plst)
  (if (null? plst)
    plst
    (cons plst (make-lazy-pairs (cddr plst)))))

(define (reshape-polygon argPAD poly)
  (def hlist '())
  (def reshape-savvy-actors #f) ;DSB added
  (def addbindings)
 ;(changemode argPAD "Select")
  (send poly select)
;UNDO HERE?

  ; create a list of handles
  (set! hlist
    (map
      (lambda (pnt) (make-object general-handle% argPAD (car pnt) (cadr pnt)))
      (make-lazy-pairs (send poly coords))))

  ; identify any reshape-sensitive actors on object (DSB added)
  (let ((named-actors-list (assq 'reshape-sensitive (send poly alist))))
    (when (and named-actors-list (not (null? (cdr named-actors-list))))
          (set! reshape-savvy-actors (cdr named-actors-list))))

  ; create a function to add bindings
  (set! addbindings (lambda (h i)  ; h is handle, i is coord index
    (let ((actor-drag-callbacks
	   (if reshape-savvy-actors
	       (filter (lambda (fn) fn)
		       (map (lambda (actr)
			      (send actr provide-reshape-handle-drag-callback i))
			    reshape-savvy-actors))
	       null))
	  (actor-delete-callbacks
	   (if reshape-savvy-actors
	       (filter (lambda (fn) fn)
		       (map (lambda (actr)
			      (send actr provide-reshape-handle-delete-callback i))
			    reshape-savvy-actors))
	       null)))
      (send h bind "<ButtonPress-2>" (lambda (w e) #f))
      (send h bind "<ButtonRelease-2>"
	    (lambda (w e)
	      (set! hlist (remove h hlist))
	      (for-each (lambda (fn) (fn)) actor-delete-callbacks)
	      (send poly coords (apply append (map (lambda (h) (send h xy)) hlist)))
	      (send h delete)
	      #f))
      (send h drag-callback
	    (lambda (x y)
	      (let ((my-xy (send h xy)))
		(for-each (lambda (fn) (fn my-xy)) actor-drag-callbacks))
	      (send poly select)
	      ;UNDO HERE?
	      (send poly coords (apply append (map (lambda (h) (send h xy)) hlist))))))
  ))

  ; apply the function to the list of handles
  (let ((i 0))
    (foreach hlist (lambda (h) (addbindings h i) (set! i (+ i 1)))))
  
  ; when object slides, reposition handles
  (send poly afterslide-callbacks
    'add
    (lambda (w dx dy) (foreach hlist (lambda (o) (send o slide dx dy))))
    'reshape)

  ; when dynaobject is unselected, delete all handles
  (send poly select-callbacks
    'add
    (lambda (w on?) (when (not on?)
      (send poly afterslide-callbacks 'remove #t 'reshape)
      (send poly select-callbacks 'remove #t 'reshape)
      (foreach hlist (lambda (h) (send h delete)))))
    'reshape)

  ; add some bindings to the dynaobject being reshaped
  (send poly bind "<ButtonPress-2>" (lambda (w e) #f))
  (send poly bind "<ButtonRelease-2>"
    (lambda (w e)
      (let ((i (find-nearest-line-segment hlist (event-x e)(event-y e)))
            (newh (make-object general-handle% argPAD (event-x e)(event-y e)))
            (head '()))
	(when reshape-savvy-actors
        (for-each (lambda (actr)
        (send actr add-vertex-n i (send newh xy)))
            reshape-savvy-actors))
          (addbindings newh (+ i 1))
          (set! head (list-tail hlist i))
          (set-mcdr! head (cons newh (cdr head)))
          (send poly coords (apply append (map (lambda (h) (send h xy)) hlist)))))))

(define (find-nearest-line-segment hlist ex ey)
  (if (<= (length hlist) 1)
    0
    (let* ((len (length hlist))
           (ibest 0)
           (done? #f))
      (dotimes (i len)
        (when (not done?)
              (let ((xy0 (send (list-ref hlist i) xy))
                    (xy1 (send (list-ref hlist (modulo (add1 i) len)) xy)))
                (when  ;a curious calculation to "nearest segment", from ben
                  (and
                    (or (and (>= ex (car xy0)) (<= ex (car xy1)))
                        (and (<= ex (car xy0)) (>= ex (car xy1))))
                    (or (and (>= ey (cadr xy0)) (<= ey (cadr xy1)))
                        (and (<= ey (cadr xy0)) (>= ey (cadr xy1)))))
                  (set! ibest i)
                  (set! done? #t)))))
      ibest)))

