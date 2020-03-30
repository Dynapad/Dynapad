#lang racket/base

(require racket/class
         dynapad/pad-state
         dynapad/image
         dynapad/misc/misc
         dynapad/image-composite
         ;dynapad/layout/composite
         dynapad/utils/formation ; for name-part otherwise a cryptic error occurs
         collects/misc/pathhack
         )

(provide pdf-portrait%)

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
        (when old-graphic
          (send old-graphic delete))
        (graphic ;set new graphic
         (cond ((file-exists? comppath)
                (apply make-object image% pad comppath init-pos))
               ((file-exists? basepath)
                (apply make-object image% pad basepath init-pos))
               (else
                (apply make-object image% pad (blank-baseimage-file) init-pos)))
         ;(make-object rect% pad (apply make-bb (if (null? xyz-list)
         ;                      '(0 0 1)
         ;                      (car xyz-list)))))))
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
        ;    (send this expand)
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

    (when *pdf-aftermake-callbacks*
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
;                "/home/dsbauer/dynapad/pdfs/irys.pdf"))
;(send test graphic
;      (make-object image% dynapad (build-path->string (send test dir) "firstpage.jpg")
;           (send dynapad view)))

;(send test gather-all)

