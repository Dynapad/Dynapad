(require (lib "port.ss"))

(define (rel-x x xy0 z)
  (* z (- x (car xy0))))
(define (rel-y y xy0 z)
  (* z (- y (cadr xy0))))

(define (writeps-line . args)
  (apply writeps-polything #f args))
(define (writeps-polygon . args)
  (apply writeps-polything #t args))

(define (writeps-color color)
  (cond ((number? color) (format "~a setgray" color)) 
	((list? color) (apply format "~a ~a ~a setrgbcolor" color))
	((string? color) ;assume tcl/dynapad color string, e.g. "#000000"
	   (let ((temp-color (make-object dynacolor% color)))
	     (writeps-color (send temp-color eps-color))))
	(else (error "unknown color format: " color))))

(define (writeps-fill color)
  (if color
      (format "gsave ~a fill grestore~%" (writeps-color color))
      ""))
(define (writeps-stroke color)
  (if color
      (format "gsave ~a stroke grestore~%" (writeps-color color))
      (format "stroke~%")))

(define writeps-string 
  (case-lambda
   ((bb crds font size str) (writeps-text bb crds font size str #f))
   ((bb crds font size str pen)
    (let ((xy (bbsw bb))
	  (z (or *eps-export-scale* (send dynapad getzoom))))
    (format "/~a findfont ~a scalefont setfont~% newpath ~a ~a moveto~% (~a) show~%"
	    font size
	    (rel-x (pop! crds) xy z)
	    (rel-y (pop! crds) xy z)
	    str)))
   ))
(define writeps-textobj
  (case-lambda
   ((bb text) (writeps-textobj bb text #f))
   ((bb text pen)
    (when text
     (let ((str (send text text))
	   (crds (bbsw (send text bbox)))
	   (font (send text font))
	   (sz (* (or *eps-export-scale* (send dynapad getzoom))
		  (send text height))))
       (writeps-string bb crds font sz str pen))))
   ))


(define writeps-polything
  (case-lambda
   ((close? bb crds)          (writeps-polything close? bb crds #f #f))
   ((close? bb crds pen)      (writeps-polything close? bb crds pen #f))
   ((close? bb crds pen fill)
    (let ((xy (bbsw bb))
	  (z (or *eps-export-scale* (send dynapad getzoom))))
    (if (null? crds)
	""
	(format "newpath~%~a~a~a~a~%"
		(apply string-append
		       (format " ~a ~a moveto~%"
			       (rel-x (pop! crds) xy z)
			       (rel-y (pop! crds) xy z))
		       (mmap 2 0 2 (lambda (x y)
				     (format " ~a ~a lineto~%"
					     (rel-x x xy z)
					     (rel-y y xy z)))
			     crds))
		(if close?" closepath~%" "")
		(writeps-fill fill)
		(writeps-stroke pen)
		))))
))

(define writeps-circle
  (case-lambda
   ((bb cxy rad)          (writeps-circle bb cxy rad #f #f))
   ((bb cxy rad pen)      (writeps-circle bb cxy rad pen #f))
   ((bb cxy rad pen fill)
    (let ((xy0 (bbsw bb))
	  (z (or *eps-export-scale* (send dynapad getzoom))))
      (format "newpath~% ~a ~a ~a ~a ~a arc closepath~% ~a~a~%"
	      (rel-x (car cxy) xy0 z)
	      (rel-y (cadr cxy) xy0 z)
	      (* rad z)
	      0 360
	      (writeps-fill fill)
	      (writeps-stroke pen))))
))

(define *epsimage-offset-rexp* (regexp "DisplayImage\n0 0\n"))
(define (writeps-image bb img)
  (let* ((epsfile (path->string (make-temporary-file "mztmp~a.eps")))
	 (geom (bb-geometry
		'up ;y-direction
		(or *eps-export-scale* (send dynapad getzoom))
		bb
		img)) ; --> (W H X Y)
	 (x (caddr geom))
	 (y (cadddr geom))
	 (cmd-args (list (send img hirespath)
			 "-geometry"
			 (bb-geometry->string geom "!")
			 epsfile)) ;--> "WxH+X+Y!"
	 ;im/convert uses only WxH component of geometry and ignores
	 ; offset +X+Y, so insert these later directly into eps code
	 (do-it (apply im/convert cmd-args)) ;make epsfile from img hirespath
	 (in-port (open-input-file epsfile))
	 (out-port (open-output-string)))
    (copy-port in-port out-port) ;copy epsfile to string
    (close-input-port in-port)
    (delete-file epsfile)
    ; now out-port is a string containing the eps code.
    ; replace the image's offset in that string:
    (regexp-replace *epsimage-offset-rexp*
		    (get-output-string out-port)
		    (format "DisplayImage\n~a ~a\n" x y))
    ; FUTURE WORK: strip out redundant headers/footers with each image string
))


(define writeps-obj 
  (case-lambda
   ((bb obj) (writeps-obj bb obj #f))
   ((bb obj lighten)
    (if lighten (set! lighten 
		      (max lighten
			   (if (send obj findable) .5 .8))))
    (cond 
     ((is-a? obj text%)  (list (writeps-textobj bb obj)))
     ((is-a? obj line%)  (list (writeps-line bb
				  (send obj coords) .9)))
;     ((is-a? obj permalight%)
;      (list (writeps-polygon bb
;	       (coords-from-bbox (send obj coords))
;	       #f (list 0 1 0))))
     ((is-a? obj rect%)  (list (writeps-polygon bb 
				  (coords-from-bbox (send obj coords))
				  (send obj pen)
				  (send obj fill))))
     ((is-a? obj oval%)
      (list (writeps-circle bb (bbcenter (send obj bbox))
			    (* .5 (bbwidth (send obj bbox)))
			    (send obj pen)
			    (send obj fill))))
     ((is-a? obj polygon%) (list (writeps-polygon bb (send obj coords)
						  ;(send obj pen)
						  ;(send obj fill)
						  )))
     ((is-a? obj image%)
      (if *include-eps-images?*
	  (list (writeps-image bb obj)
		(writeps-polygon bb
		 (coords-from-bbox (send obj bbox)) #f #f)) ;border
	  (list (writeps-polygon bb
		 (coords-from-bbox (send obj bbox)) #f lighten))))
     ((is-a? obj group%)   (writeps-set bb (send obj members)))
     ((is-a? obj pdf-portrait%)   (writeps-obj bb (send obj graphic)
				    (if (send obj findable) .5 .8)))
     ((is-a? obj container-form%)
      (append (writeps-obj bb (send obj frame))
	      (writeps-set bb (send obj contents))
	      (if (is-a? obj titled-frame-container%)
		  (writeps-obj bb (send obj titlebox))
		  null)))
     ((is-a? obj base-group%)
      (writeps-obj bb (send obj frame)))
     (else null))
    )))

(define (writeps-set bb objs)
  (apply append (map (lambda (o) (writeps-obj bb o)) objs))
)

(define writeps-batch 
  (case-lambda
   ((objs)
    (if (list? objs)
	(let* ((bb (bbunion-objects objs))
	       ;(xy0 (list (b0 bb) (b1 bb)))
	       (scale (or *eps-export-scale*
			  (send dynapad getzoom))))
	  (format "%!PS-Adobe-2.0 EPSF-3.0~%~a~a%%BoundingBox: 0 0 ~a ~a~%~a~ashowpage"
		  (format "%padbb: ~a~%" bb)
		  (format "%scale: ~a~%" scale)
		  (* scale (bbwidth bb))
		  (* scale (bbheight bb))
		  (apply string-append (writeps-set bb objs))
		  (if *eps-viewbb* ;view rect on top, so last
		      (writeps-viewbb bb)
		      "")
		  ))
	(writeps-batch (list objs))))
   ((objs path)
    (if (and path (not (string? path)))
	(set! path (Select-File-Dialog 'save *eps-default-path*)))
    (if path
	(set! *eps-default-path* path))
    (let ((str (writeps-batch objs))
	  (port (and path
		     (open-output-file path 'text 'truncate))))
      (and port
	   (fprintf port str)
	   (close-output-port port)
	   #t)))
   ))

(define *eps-default-path* #f)
(define *eps-export-scale* #f) ;#f means apparent size (zoom-dependent)
(define *eps-viewbb* #f)
(define *include-eps-images?* #f)

;overrides defn in lockdown.ss:
(define *last-view-outline* #f)

(define (make-submenu-ExportEPS mb object)
  (let* ((sb (add-submenu mb "Export to EPS...")))
    (add-menu-item sb "Export Selected..."
		   (lambda () (writeps-batch (fs) #t)))
    (add-menu-separator sb)
    (add-checkable-menu-item sb "Apparent Size"
	   (lambda (i) (set! *eps-export-scale* #f))
	   (not *eps-export-scale*))
    (add-checkable-menu-item sb "20% Scale"
	   (lambda (i) (set! *eps-export-scale* .2))
	   (equal? .2 *eps-export-scale*))
    (add-checkable-menu-item sb "100% Scale"
	   (lambda (i) (set! *eps-export-scale* 1))
	   (equal? 1 *eps-export-scale*))
    (add-checkable-menu-item sb "500% Scale"
	   (lambda (i) (set! *eps-export-scale* 5))
	   (equal? 5 *eps-export-scale*))
    (add-menu-separator sb)
    (add-checkable-menu-item sb "Include view bbox"
	   (lambda (i) (set! *eps-viewbb*
			     (if (send i is-checked?)
				 *last-view-outline*
				 #f
				 )))
	   (and *eps-viewbb* *last-view-outline*)
	   *last-view-outline*)
    (add-checkable-menu-item sb "Include images"
	   (lambda (i) (set! *include-eps-images?*
			     (send i is-checked?)))
	   *include-eps-images?*)
))

(if *popup-menus-enabled?*
    (append-mainmenu-constructor
     (lambda (mb obj)
       (add-menu-separator mb)
       (make-submenu-ExportEPS mb obj)
       )))

;==== taped on later ...
;(define (writeps-posthoc-viewbb zoom viewbb)

(define writeps-viewbb
  (case-lambda
   ((bb) (writeps-viewbb bb *eps-viewbb*))
   ((bb viewbb)
    (let ((crds (coords-from-bbox (send viewbb coords)))
	  (pen (list 1 0.2 0.2)))  ;light red
      (writeps-polygon bb crds pen)))
   ))

(define (cache-eps-viewbb viewbb)
  (set! *eps-viewbb* viewbb))