(announce-module-loading "PDF features...")
(require (lib "process.ss"))
(require (lib "imagemagick.ss" "imagemagick"))
(require (lib "thumbify.ss" "thumbify"))
(require (lib "misc.ss" "pdfrip"))
(dynaload "handle.ss")
(dynaload "metadata.ss")

(define *max-pdf-images* 8)
(define *max-pdf-images-shown* 1)



; never mind; use (make-object imagedata%..)  instead
#|
(define *image-size-regexp* (regexp "^[^ ]+ [A-Z]+ ([0-9]+)x([0-9]+) "))
(define (find-image-size imagepath)
  (let* ((strng (read-line (system (format "identify ~a" imagepath))))
	 (match (regexp-match *image-size-regexp* strng)))
    (and match (list (string->number (cadr match))
		     (string->number (caddr match))))))
|#


(define (build-composite-cmd bbox scale img old-file new-file)
; constructs system command to composite two images together:
;  superimposes img atop old-file, assuming old-file fills bbox
;  saves merged image as new-file
; (list "-geometry" "<NXN>" "<filename>" ...)
  (list "-geometry"
	(bb-geometry->string
	 (bb-geometry 'down scale bbox img) "!")
	;potentially insert borders/etc here
	(send img hirespath)
	old-file
	new-file))

(define (limit-drag-to-bbox obj bbox-fn union?)
;prevents obj from being dragged outside of bbox
;  bbox-fn is (lambda (obj) ...) which returns bbox enclosure
;  (can change during drag if needed)
  (let ((n-fn (if union? b1 b3))
	(s-fn (if union? b3 b1))
	(e-fn (if union? b0 b2))
	(w-fn (if union? b2 b0)))
    (send obj bind "<B1-Motion>"
	  (lambda (eventPAD e)
	    (let* ((last_x (sendf eventPAD evs lastx))
		   (last_y (sendf eventPAD evs lasty))
		   (x (event-x e))
		   (y (event-y e)))
	      (if (and last_x last_y)
		  (let* ((bb-now (send obj bbox))
			 (bb-limit (bbox-fn obj))
			 (max-up (- (b3 bb-limit) (n-fn bb-now)))
			 (max-lf (- (b0 bb-limit) (w-fn bb-now)))
			 (max-rt (- (b2 bb-limit) (e-fn bb-now)))
			 (max-dn (- (b1 bb-limit) (s-fn bb-now)))
			 (dx (- x last_x))
			 (dy (- y last_y)))
		    (if (negative? dx)
			(set! dx (max max-lf dx))
			(set! dx (min max-rt dx)))
		    (if (negative? dy)
			(set! dy (max max-dn dy))
			(set! dy (min max-up dy)))
		    (send eventPAD do-duringdrag e)
		    (for-each
		     (lambda (o) (send o slide dx dy))
		     (send eventPAD getvar 'dragset))
		    (sendf eventPAD evs set-last-xy (+ last_x dx) (+ last_y dy)) )
		  (sendf eventPAD evs set-last-xy x y))
	      #f ;override existing <Drag-B1-Motion> binding
	      )))
    (send obj afterposition-callbacks 'add
	  (lambda (obj xyz)
	    (let* ((bb (send obj bbox))
		   (cage (bbox-fn obj))
		   (n (b3 cage))
		   (s (b1 cage))
		   (e (b2 cage))
		   (w (b0 cage)))
	      (if (< n (n-fn bb)) (send obj bbox (list #f n #f #f))) ;snap to n
	      (if (< e (e-fn bb)) (send obj bbox (list e #f #f #f))) ;snap to e
	      (if (> s (s-fn bb)) (send obj bbox (list #f #f #f s))) ;snap to s
	      (if (> w (w-fn bb)) (send obj bbox (list #f #f w #f))) ;snap to w
	      ))
	  )))

#|
(define (make-object-resizable obj)
  (let ((border (ic (make-object rect% dynapad (send obj bbox))
		    (fill "none")
		    (pen "white")
		    (transparency .5))))
    (send obj afterslide-callbacks 'add
	  (lambda (o dx dy) (send border slide dx dy)))
    (resizer-bindings (send obj dynapad)
		      border
		      (lambda (frame bb) (send obj bbox bb)))))

(define draglimit-container%
  (class frame-container%
    (init dynaptr)
    (init (frame-arg #f))
    (inherit dynaclass)
    (super-instantiate (dynaptr frame-arg))
    (dynaclass 'draglimit-container%)

    (define (get-my-frame-bbox obj)  ;doesnt use obj arg
      (send (send this frame) bbox))
    
    (define/override (add obj)
      (limit-drag-to-bbox obj get-my-frame-bbox)
      (if (is-a? obj base-formation%)
	  (foreach (send obj named-parts)
		   (lambda (prt)
		     (limit-drag-to-bbox prt get-my-frame-bbox))))
      (super add obj))

    (define/override (remove obj)
      (send obj bind "<Drag-B1-Motion>" #f)
      (super remove obj))

    (define/override contents
      (case-lambda
       (() (super contents))
       ((lst) (foreach lst (lambda (o) (limit-drag-to-bbox o get-my-frame-bbox))))))
))
|#

(define (compose-images base-bb mult dir basename img-list outname)
;(say "composing " (send (car img-list) hirespath))
  (let* ((basefilepath (build-path->string dir basename))
	 (outfilepath  (build-path->string dir outname)))
    (cond ((null? img-list) basefilepath)
	  ((let* ((framed-img (car img-list))
		  (img-part   (send framed-img image))
		  (frame-part (send framed-img border))
		  (linewidth  (send frame-part penwidth))
		  (bbint (bbintersection base-bb (send img-part bbox)))
		  (wd (and bbint (<= linewidth (bbwidth bbint)))) ;linewidth gives margin of error for slight overlap
		  (ht (and bbint (<= linewidth (bbheight bbint)))))
	     (and wd ht)) ; non-zero intersection? --> do composite
	   (let* ((cmd (build-composite-cmd base-bb mult (send (car img-list) image)
					    basefilepath outfilepath))
		  (nextname (insert-filenamebase-suffix outname "#"))
		  (result (apply im/composite cmd))) ;make the next image
	     (unless result
		     (error "Uh-oh"))
	     (compose-images base-bb mult dir outname (cdr img-list) nextname)))
	  (else		;skip this image
	   (compose-images base-bb mult dir basename (cdr img-list) outname)))))

(define image-composite%
  (class frame-container%
     (init dynaptr)
     (init (frame-arg #f))
     (inherit dynaclass)
     (super-instantiate (dynaptr frame-arg))
     (dynaclass 'image-composite%)
     (send (send this container) boundable #t)

     (name-part _crop-box crop-box)
     (crop-box (ic (make-object rect% dynaptr (send frame-arg bbox))
		   (fill "none")
		   (pen "black")
		   (findable #f)
		   (penwidth -1)))

    (define (get-my-frame-bbox obj)  ;doesnt use obj arg
      (send (send this frame) bbox))

     (define (prepare-image img)
       (let ((wrapper (make-object resizable-image% (send img dynapad) img)))
	 (limit-drag-to-bbox img get-my-frame-bbox #t) ;last #t means loose constraint
	 (limit-drag-to-bbox wrapper get-my-frame-bbox #t) ;(i.e. union not intersection)
;this may be wrong:
(send wrapper findable #f) ; prevents selection, menu options,
			   ; membership in piles, lenses, etc
	 wrapper))

    (define/override (add obj)
      (super add (prepare-image obj)))

    (define/override (remove obj) #f) ;can't remove, only delete
    (define/override contents
      (case-lambda
       (() (super contents))
       ((lst) #f))) ;can't set; use add instead

    (define/public (place-image img bb)
      (unless (is-a? img image%)
	      (error "Only image%s may be added to image-composite%"))
      (send img bbox bb)
      (send this add img))
    
;    (define/public (toggle)
;      ((image-toggle-hires-thumb

    (define/public flatten
      ;construct single image from set; returns filename of merged image
      (case-lambda
       ;-0-----------
       (() (flatten *home-directory*))
       ;-1-----------
       ((dir) (flatten dir ".composite.jpg"))
       ;-2-----------
       ((dir name)
	(let* ((pad (send (send this frame) dynapad))
	       (my-imgs (map (lambda (grp) (send grp image)) (send this contents)))
	       (sorted-imgs (send pad order my-imgs))) ;bottom-up ordering of content images
	  (flatten dir name sorted-imgs)))
       ;-3-----------
       ((dir name imgs)
	; could take time; show delay cursor
(say "flattening...")
	 (show-possible-delay dynapad
           (let* ((base (send this frame)) ;background image
		  (base-dim (send (send base hires) dimensions))
		  (basepath (build-path->string dir name))
		  (base-bb (send base bbox))
		  (base-bbw (bbwidth base-bb))
		  (base-bbh (bbheight base-bb))
		  (base-rez-ratio (max (/ (car base-dim) base-bbw)
				       (/ (cadr base-dim) base-bbh)))
		  (resize-base-cmd
		   (format "mogrify -format jpg -geometry ~a! ~a"
			   (format-geometry-str (* base-rez-ratio base-bbw)
						(* base-rez-ratio base-bbh))
			   basepath)))
	; duplicate base image file
	     (copy-file-replace  (send base hirespath) basepath)
	; resize base image file copy
	   ;(say resize-base-cmd)
	   ;(say base-rez-ratio)
	   ;(say resize-base-cmd)
	   ;(system resize-base-cmd)
	     (let ((lastfile (compose-images base-bb
					     base-rez-ratio
					     dir
					     name
					     imgs
					     name))
		   (destfile (build-path->string dir name)))
	  ; copy end result to specified filename
	       (if (not (equal? lastfile destfile))
		   (copy-file-replace  lastfile destfile))
	     ;thumbify
	       (thumbify/force #t)
	       (thumbify destfile)
	       destfile
	       )
	     ))
       )))
))

#|
(define thumbify-via-mogrify
;assumes dir/thumbs/size exists already
  (case-lambda
   ((filepath) (thumbify-via-mogrify filepath 125))
   ((filepath maxsize)
      (set! maxsize (round-to-int maxsize))
      (let-values (((dir name junk) (split-path->string filepath)))
        (let* ((newpath (format "~athumbs/~a/~a" dir maxsize 
				(insert-filenamebase-suffix
				   name (format "-~a" maxsize))))
	       (copy-cmd (format "cp ~a ~a" filepath newpath))
	       (mogrify-cmd (format "mogrify -format jpg -geometry ~ax~a ~a"
				    maxsize maxsize newpath)))
	  (system copy-cmd)
	  (system mogrify-cmd))))))
     
|#

; no need for above; use build-in split path
; (or: replace with (file-name-from-path...)?)
(define (end-of-path path)
  (let-values (((path end result) (split-path->string path)))
	      end))

(define *pdf-subimage-base* "image")
(define *pdf-subimage-rexp* (regexp (format "^(~a.*)" *pdf-subimage-base*)))

(define (imagefile-aspect-ratio file)
  (let* ((w+h (im/identify file))
	 (w (car w+h))
	 (h (cadr w+h))
	 (aspect (if (zero? h)
		     0
		     (/ w h))))
    aspect))

(define (reasonable-aspect? file)
  (let ((aspect (imagefile-aspect-ratio file)))
    (and (< aspect 5)
	 (> aspect .2))))

(define (gather-pdf-subimages dir)
  (and (directory-exists? dir)
       (filter (lambda (filename) (regexp-match *pdf-subimage-rexp* filename))
	       (map
		path->string
		(filter-and-sort-directory dir file-size >
					  sch_imagep
					  (lambda (f) (>= (file-size f) 5000)) ;file must be >5k
					  reasonable-aspect?
					  )))
	  ;(sort-image-files-by-descending-size dir)))
))

(define 1cell-layout (list '(0.2 #f 0.8 0.75))) ;centered below 3/4 hline
(define 2cell-layout (list '(0.3 0.5 0.7 #f) '(0.3 #f 0.7 0.45))) ;N,S
(define 3cell-layout (list '(0 0.4 #f 0.75) '(#f 0.2 1 0.6) '(0 0 #f 0.4))) ;NW,E,SW
(define 4cell-layout (list '(0 0.4 #f 0.75) '(#f 0.4 1 0.75) ;NW,NE,
			   '(0 0 #f 0.4)   '(#f 0 1 0.4)))   ;SW,SE
(define 5cell-layout (cons '(0.3 #f 0.7 0.7) 4cell-layout))
;(define 6cell-layout (list-append (list '() '())
;				  4cell-layout))
(define 1cell-surplus (list '(#f 1 0 1.5)))
(define 2cell-surplus (cons '(1 1 #f 1.5) 1cell-surplus))
;(define 3cell-surplus (cons '(-0.5 1 #f 0) 2cell-surplus))
(define 3cell-surplus (cons '(1 -0.5 #f 0) 2cell-surplus))
(define 4cell-surplus (cons '(#f -0.5 0 0) 3cell-surplus))

(define *subimg-grab* #f)

(define (ensure-pdf obj)
  (cond ((is-a? obj pdf-portrait%) obj)
	((is-a? obj resizable-image%) (send (get-container obj) getgroup))
	(else #f)))
      

(define (save-pdf-config pdf) ;pdf is a pdf-portrait
  (let ((dir (send pdf dir)))
    (and (directory-exists? dir)
	 (let* ((path (build-path->string dir "config.ss"))
		(file (open-output-file path 'text 'truncate))
		(cmd `(ic (make-object pdf-portrait% dynapad
				       (relpath ,(build-path->string "../"
							     (file-name-from-path (send pdf url)))))
			  (subimages (list ,@(map (lambda (lst) (cons 'list lst))
						  (send pdf subimages)))))))
	   (write cmd file)
	   (close-output-port file)))))

(define make-metadata-menu-for-pdf
  (case-lambda
   ((obj) (make-metadata-menu-for-pdf (new-popup "Document Details")))
   ((obj menu)
    (date-display-format (pad-date-format))
    (make-submenu-DateFormat menu obj)
    (add-menu-item menu (format "File: ~a" (send obj url)) void #f)
    (add-menu-item menu (format "Created: ~a"
				(let* ((pair (get-pdf-date obj))
				       (date (pair->date pair)))
				  (if date
				      (date->string date (show-metadata-time?))
				      "(unknown)"))) void #f)
    (add-menu-item menu (format "Acquired: ~a"
				(let* ((pair (get-pdf-filedate obj))
				       (date (pair->date pair)))
				  (if date
				      (date->string date (show-metadata-time?))
				      "(unknown)"))) void #f)
    (add-menu-item menu "Author" void #f)
    (add-menu-item menu "Title" void #f)
    menu)))

(define (make-menu-for-pdf obj)
  (let ((menu (new-popup "Document Details"))
	(pdf  (ensure-pdf obj))) ;obj itself or pdf containing it
;    (when (send obj findable)
    (send pdf select)
	  (make-submenu-Edit menu pdf)
	  ;(make-submenu-Arrange menu obj) ;dubious...
	  (unless (eq? obj pdf)
		  (add-menu-item menu "Raise" (lambda () (send obj raise)))
		  (add-menu-item menu "Lower" (lambda () (send obj lower))))
	  (add-checkable-menu-item menu "Lock Arrangement"
	     (lambda (i) (if (send pdf expanded?)
			     (send pdf condense)
			     (send pdf expand)))
	     (not (send pdf expanded?)))
;	  (add-menu-item menu "Rearrange..."
;			 (lambda () (send obj expand))
;			 (not (send obj expanded?)))
;	  (add-menu-item menu "Flatten..."
;			 (lambda () (send obj condense))
;			 (send obj expanded?)))
	  ;)
    (add-menu-item menu "View Document..." (lambda () (send pdf view-document)))
    (make-submenu-Select-Highlighted menu pdf)
    (add-menu-separator menu) ;------------------------------
    (make-metadata-menu-for-pdf pdf menu)
    menu))

(define *pdf-aftermake-callbacks* null) ;list of (lambda (img)...)
(define pdf-aftermake-callbacks
  (callback-accessor-functions *pdf-aftermake-callbacks*))

(define (blank-baseimage-file)
   (build-path->string *dynapad-directory* "pad/bitmaps/blank.jpg"))

(define pdf-portrait%
  (class base-formation%
    (init dynaptr)
    (inherit dynaclass)
    (super-instantiate (dynaptr))
    (dynaclass 'pdf-portrait%)
    ;(inherit-field cptr)

    ; link to a handle shared by other portraits of same pdf/url
    (init _url)
    (init (_xyz '(0 0 1))) ;initial position
    (field (_handle #f))
    (set! _handle (assign-handle _url car pdf-handle%))
    (send _handle add this)

    (name-part _graphic graphic) ; may be image-composite% (subimage-group)
				 ; or merged/flattened image%
    ;(name-part _titlebox titlebox)
    ;(name-part _authorbox authorbox)

    (init-image-bindings this)

    (define/override (delete)
      (when _handle (send _handle remove this))
      (super delete))

    (define/public (view-document)
      (send _handle start-viewer))
    (send this bind "<Double-ButtonPress-2>"
	  (lambda (eventPAD e) (view-document) #f))

#|
    (send this bind "dt-down" ;will happen only if dt tools are loaded
          (cons
            (lambda (eventPAD e)
              (when (permit-lock-toggle?) ;defined in dt tools
                (if (expanded?) (condense) (expand))
                (image-hires-and-center eventPAD e)
                (finish-lock-toggle) ;defined in dt tools
                #f)) ;stop event cascade
            (send this bind "dt-down"))) ;don't clobber other bindings
|#
    
    (define/public (hires?)
      (and (is-a? _graphic image%)
	   (send _graphic hires?)))
    (define/public (hires)
      (and (is-a? _graphic image%)
	   (send _graphic hires)))
    (define/public (thumb)
      (and (hires?)
	   (send _graphic thumb)))

    (define/public (subimages . args) (send/apply _handle subimages args))
    (define/public (baseimage-file . args) (send/apply _handle baseimage-file args))
    (define/public (composite-file . args) (send/apply _handle composite-file args))
    (define/public (dir . args) (send/apply _handle dir args))
    (define/public (url) (send _handle url))
    (define/public (year . args) #f);(send/apply _handle year args))
    ; year is only her for backward-compat; should be del'd asap

    (define/override (export dir)
      (let ((myname (file-name-from-path (url))))
	(export-link (url) (build-path->string dir myname))))

    (define/override (writeoptions)
      `(,@(super writeoptions)
	(subimages (list ,@(map (lambda (lst) (cons 'list lst))
				(subimages))))))

    (define/public (refresh-graphic . xyz-list)
      (let* ((basepath (build-path->string (dir) (baseimage-file)))
	     (comppath (build-path->string (dir) (composite-file)))
	     (old-graphic (graphic))
	     (pad (send this dynapad))
	     (init-pos (if old-graphic
			   (list (send old-graphic position))
			   xyz-list)))
	(if old-graphic
	    (send old-graphic delete))
	(graphic ;set new graphic
	 (cond ((file-exists? comppath)
		(apply make-object image% pad comppath init-pos))
	       ((file-exists? basepath)
		(apply make-object image% pad basepath init-pos))
	       (else
		(apply make-object image% pad (blank-baseimage-file) init-pos)))
		;(make-object rect% pad (apply make-bb (if (null? xyz-list)
		;					  '(0 0 1)
		;					  (car xyz-list)))))))
	 )))

    (define (disable-me)
      (let ((savebindings
	     (send this bind "<ButtonPress-1>")))
	(send this bind "<ButtonPress-1>" 
	      (cons (lambda (e o) #f) savebindings))))

    (define (reenable-me)
      (send this bind "<ButtonPress-1>"
	    (cdr (send this bind "<ButtonPress-1>"))))
	

    (define/public (expanded?) (is-a? _graphic image-composite%))
    (define/public (expand)
(say "expanding")
      ;while expanding, temporarily make disable
      (disable-me)
      ;replace flat image with subimage-group (for manual rearrangement)
      (let* ((pad (send this dynapad))
	     (old-composite (graphic))
	     (basefile (build-path->string (dir) (baseimage-file)))
	     (usebasefile (if (file-exists? basefile)
			      basefile
			      (blank-baseimage-file)))
	     (baseimage (ic (make-object image% pad
					 usebasefile
					 (send old-composite position))
			    (bbox (send old-composite bbox))))
	     (new-bb (send baseimage bbox)))
	;replace simple image with image-composite%
	(graphic (make-object image-composite% pad baseimage))
	;(send (graphic) lower) ; <-NEEDED?
	;install subimages in composite
	(foreach (subimages)
		 (lambda (pair)
		   (let* ((filename (car pair))
			  (rel-bb (cdr pair))
			  (path (build-path->string (dir) filename))
			  (thumb (ensure-thumb path))
			  (img (make-object image% pad
					    path
					    (send baseimage position)))
			  (wrapped-img
			   (send (graphic) place-image img
				 (apply-bb-transform rel-bb new-bb))))
		         (add-object-menu make-menu-for-pdf wrapped-img))))
	;when all is ready, delete old image to expose composite
	(send old-composite delete)
	;reawaken
	(image-hires-and-center (list this))
	(reenable-me)

	(send this divisible #t) ;was #t; why? If #t, bindings fail!
))

    (define/public (condense)
      (display (format "Building ~a" (send this url)))
    ; record subimage configuration, then replace subimage-group with flat image;
      ;save subimage configs
      (send this divisible #f)
      (let* ((pad (send this dynapad))
	     (subimgs (graphic))
	     (imgs (send pad order (send subimgs contents))))
	(subimages
	      (map (lambda (img)
		     (cons (end-of-path (send img hirespath)) ;subimage filename
			   (make-bb-transform (sendf subimgs frame bbox)
					      (let ((bb (sendf img image bbox)))
						; substitute one #f in bbox
						; so as to preserve aspect ratio
						(cons #f (cdr bb))))))
		   imgs))

	;save current subimage config
	(save-pdf-config this)

	;create composite image
	(send subimgs flatten (dir) (composite-file) imgs)
	(send _handle refresh-all-instances-except this)

	;(say "load my composite...")
	;load composite image to replace subimages
	(graphic (ic (make-object image% pad (build-path->string (dir) (composite-file))
				  (sendf subimgs frame position))))
		     ;(bbox (send subimgs bbox))))
	;delete image-comp group
	(send subimgs delete)
	(when (send this selected?)
	      (send this unselect) (send this select)) ;refresh selection box
	(send this hires)
	(add-images-to-hires-list (list this))
	(send this divisible #f)
	(newline)
	))

    (define/public (gather-all)
(say "gathering..." (send this url))
      (let* ((curr-subimages (map car (subimages)))
	     (all-subimages (or (gather-pdf-subimages (dir)) null))
	     (new-subimages
	      (list-diff all-subimages curr-subimages member))
	;all new names to _subimages with initial ref-bbox for each
	     (bboxes (case (min (length new-subimages) *max-pdf-images-shown*)
		       ((0) null)
		       ((1) 1cell-layout)
		       ((2) 2cell-layout)
		       ((3) 3cell-layout)
		       ((4) 4cell-layout)
		       (else 5cell-layout)))
	     (num-shown (length bboxes))
	     (show-subimages (list-head new-subimages num-shown))
	     (surplus-subimages (list-tail new-subimages num-shown))
	     (surplus-bboxes (case (min (length surplus-subimages) (- *max-pdf-images* num-shown))
			       ((0) null)
			       ((1) 1cell-surplus)
			       ((2) 2cell-surplus)
			       ((3) 3cell-surplus)
			       (else 4cell-surplus)))
	     (new-subimage-configs
	      (append (reverse (map (lambda (name bb) (cons name bb))
				    show-subimages
				    bboxes))
		      (map cons
			   (list-head surplus-subimages (length surplus-bboxes))
			   surplus-bboxes))))
	(subimages (append (subimages) new-subimage-configs))
;	(send this expand)
	))

    (send this alternate-build-fn
	  (lambda (o) (let* ((xy (send o xy))
			     (x  (car xy))
			     (y  (cadr xy))
			     (zfac (send o z)))
			`(ic (make-pdf-at-position ,(send o url) ,x ,y ,zfac)
			     (id ,(send o id))
			     ,@(if (null? (send o alist))
				   null
				   `((loadalist ',(send o savealist))))))))

    (add-object-menu make-menu-for-pdf this)
;    (let ((act (make-object popup-provider%)))
;      (send act attach-to this)
;      (send act use-this-popup-fnc pdf-popup-menu))

    (refresh-graphic _xyz)     ; set initial graphic
    (send this divisible #f)

    (if *pdf-aftermake-callbacks*
	(exec-any-callbacks *pdf-aftermake-callbacks* this))
))

  
(define (make-pdf-portrait dynapad pdfpath)
  (let ((dir (get-pdf-metadata-dir pdfpath)))
    (unless (directory-exists? dir)
	    (make-directory dir))
    (or (let ((config-file (build-path->string dir "config.ss")))
	  (and (file-exists? config-file)
	       (load config-file)))
	(ic (make-object pdf-portrait% dynapad pdfpath)
			;force new composite
		       (gather-all)
		       (expand)
		       (condense)
		       (refresh-graphic)
		       ))))

;this is an override of (make-pdf...) in arrangeimages.ss
(define (make-pdf-at-position file x y zfac)
  (ic (make-pdf-portrait dynapad file) 
      (position (list x y zfac))))


;(define pdf-menu-provider%
;  (class actor%
;    (init (_obj #f))
;    (super-instantiate ())
;    
;    (if _obj (send this attach-to _obj));;;

;    (define/public 


; ---- bootstrapping:
;(define test (make-pdf-portrait dynapad
;				"/home/dsbauer/dynapad/pdfs/irys.pdf"))
;(send test graphic
;      (make-object image% dynapad (build-path->string (send test dir) "firstpage.jpg")
;		   (send dynapad view)))

;(send test gather-all)


(update-progress 1)
