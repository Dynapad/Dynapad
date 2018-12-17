(dynaload "menu-draw.ss")
;======= Draw Rect, Oval, Line, and Polygon from Menubar ========

(define Draw-mode #f)
(define Draw-class #f)
(define Draw-object #f)
(define Draw-preview #f)
(define Draw-multiple #f)

(define (resetDrawPreview)
  (when Draw-preview
	(send Draw-preview delete)
	(set! Draw-preview #f)))

(define (cancelDrawObject argPAD)
  (if Draw-object
      (send Draw-object delete))
  (set! Draw-object #f)
  (resetDrawPreview)
  )

(define (resetDrawObject argPAD)
  (if Draw-object
      (undoify-fresh-obj Draw-object)) ;save draw object for undo
  (set! Draw-object #f)
  (resetDrawPreview)
  )

(define (initDraw argPAD drawclass drawmode)
  (resetDrawObject argPAD)
  (set! Draw-class drawclass)
  (set! Draw-mode drawmode)
  (changemode argPAD drawmode)
  )

(define (initDrawPreview argPAD)
  (when (not Draw-preview)
	(set! Draw-preview (clone-object Draw-object))
	(send Draw-preview transparency .3))
  (let ((c (send Draw-object coords)))
    (send Draw-preview coords c)
    (send Draw-preview save-coords c))
)

(define (degenerate-object? obj)
  (or (not obj)
      (if (is-a? obj text%)
	  (equal? "" (send obj text))
	  (let ((c (send obj coords)))
	    (cond ((is-a? obj polyline%) (< (length c) 4))
		  ((is-a? obj polygon%)  (< (length c) 6))
		  (else (null? c)))))))

(define (finishDraw argPAD)
  (resetDrawObject argPAD)
  (changemode--no-gui argPAD (default-mode argPAD))
  ;(Set-Select--undoable argPAD Draw-object)
  (gui-update-mode argPAD))

;=== Bound events ===
(define (start-shape-event eventPAD e) ;start new shape (rect%, poly%, text%, etc)
  (set! currentPAD eventPAD)
  (let* ((x (event-x e))
	 (y (event-y e))
	 (objs-here (reverse (send eventPAD find 'overlapping (list x y x y)))))
    (set! Draw-object (if (null? objs-here) #f (car objs-here)))
    (cond
	 ; maybe edit existing obj (e.g. text%)
     ((and Draw-object
	   (is-a? Draw-object Draw-class)
	   (eq? Draw-class text%))
      (edit-text-at-xy Draw-object x y))
     
     (else ; make new obj
      (set! Draw-object (make-object Draw-class eventPAD))
      
      (if (has-method? Draw-object 'fill)
	  (send Draw-object fill
		(if (send eventPAD fill?) (send eventPAD defaultfill) "none")))
      
      (if (has-method? Draw-object 'pen)
	  (send Draw-object pen (send eventPAD defaultpen)))
      
      (cond
	    ;edit new text
       ((subclass? Draw-class text%)
	(edit-text-at-xy Draw-object x y))
	    ;new polygon/polyline
       ((or (subclass? Draw-class polygon%)
	    (subclass? Draw-class polyline%))
	(send Draw-object coords (list x y))
	(send Draw-object save-coords (list x y))
	(initDrawPreview eventPAD)
	(changemode eventPAD "DrawAdd")
	)
	   ;new rect/oval/line/etc
       (else 
	(send Draw-object save-coords (list x y)))
       )))
    ))

(define (drag-shape-vertex-event eventPAD e)  ;move unfixed vertex
  (set! currentPAD eventPAD)
  (if Draw-object ;should always be #t
      (let*
          ((c (or (send Draw-object recall-coords) null))
           (x (event-x e))
           (y (event-y e))
	   (newc (append c (list x y))))
	(send Draw-object coords newc)
	(when Draw-preview
	      (send Draw-preview coords newc)
	      (send Draw-preview save-coords newc))
	(if (is-a? Draw-object freehand%)
            (send Draw-object save-coords newc)))))

(define (update-shape-preview-event eventPAD e) ;rubber-band to cursor
  (set! currentPAD eventPAD)
  (let ((c (or (send Draw-preview recall-coords) null))
	(x (event-x e))
	(y (event-y e)))
    (send Draw-preview coords (append c (list x y)))))

(define (fix-shape-vertex-event eventPAD e) ;fix vertex
  (set! currentPAD eventPAD)
  (send Draw-object save-coords
	(send Draw-object coords)))

(define (add-shape-vertex-event eventPAD e) ;add unfixed vertex
  (set! currentPAD eventPAD)
  (let ((c (send Draw-preview coords)))
    (send Draw-preview save-coords c)
    (send Draw-object coords c)
    ))

(define (esc-shape-event eventPAD e) ; exit object or mode
  (set! currentPAD eventPAD)
  (cond
   ((not Draw-object)
    (finishDraw eventPAD))
   ((degenerate-object? Draw-object)
    (cancelDrawObject eventPAD)
    (changemode eventPAD Draw-mode))
   (Draw-multiple
    (resetDrawObject eventPAD)
    (changemode eventPAD Draw-mode))
   (else
    (finishDraw eventPAD))))

(define (bindDrawMode argPAD)

  (send argPAD bind "<Draw-ButtonPress-1>" start-shape-event)
  
  (send argPAD bind "<DrawAdd-B1-Motion>" drag-shape-vertex-event)
  (send argPAD bind "<Draw-B1-Motion>" drag-shape-vertex-event)

  (send argPAD bind "<DrawAdd-Motion>" update-shape-preview-event)

  (send argPAD bind "<DrawAdd-ButtonRelease-1>" fix-shape-vertex-event)

  (send argPAD bind "<DrawAdd-ButtonPress-1>" add-shape-vertex-event)

  (send argPAD bind "<Draw-ButtonRelease-1>" esc-shape-event)

  (send argPAD bind "<DrawAdd-Double-ButtonPress-1>" esc-shape-event)

  (send argPAD bind "<Draw-KeyPress-Escape>"    esc-shape-event)
  (send argPAD bind "<DrawAdd-KeyPress-Escape>" esc-shape-event) 
    
  ; allow zooming (oldschool) while drawing
  ;(send argPAD bind "<Draw-ButtonPress-2>"   Zoom-In-lambda)
  ;(send argPAD bind "<Draw-ButtonRelease-2>" Zoom-In-Stop-lambda)
  ;(send argPAD bind "<Draw-ButtonPress-3>"   Zoom-Out-lambda)
  ;(send argPAD bind "<Draw-ButtonRelease-3>" Zoom-Out-Stop-lambda)

)