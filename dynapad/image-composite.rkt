#lang racket/base

(require racket/class
         dynapad/spd
         dynapad/image
         dynapad/base
         dynapad/container
         dynapad/pad-state
         dynapad/misc/misc
         dynapad/misc/filenames
         dynapad/misc/user-preferences ; *home-directory*
         dynapad/layout/bbox
         dynapad/utils/formation
         collects/misc/pathhack
         collects/pdfrip/misc
         collects/imagemagick/imagemagick
         collects/thumbify/thumbify
         )

(provide image-composite%
         )

(define *subimg-grab* #f)

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
          (else        ;skip this image
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
                                  (when (not (equal? lastfile destfile))
                                    (copy-file-replace  lastfile destfile))
                                  ;thumbify
                                  (thumbify/force #t)
                                  (thumbify destfile)
                                  destfile
                                  )
                                ))
         )))
    ))
