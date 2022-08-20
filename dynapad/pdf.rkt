#lang racket/base

(provide pdf-portrait%
         pad-date-format
         make-pdf-at-position
         make-submenu-DateFormat
         show-metadata-time?
         )

(require racket/class
         (only-in racket/date
                  date->string
                  date-display-format)
         (only-in racket/path
                  file-name-from-path)
         compatibility/defmacro
         (for-syntax racket/base)
         dynapad/pad-state
         (only-in dynapad/image
                  image%
                  ensure-thumb
                  )
         (only-in dynapad/container
                  get-container
                  resizable-image%
                  )
         dynapad/misc/misc
         (only-in dynapad/misc/tools-lists
                  list-head
                  )
         (only-in dynapad/misc/filenames
                  export-link
                  filter-and-sort-directory)
         (only-in dynapad/image-composite
                  image-composite%
                  )
         ;dynapad/layout/composite
         dynapad/utils/formation ; for name-part otherwise a cryptic error occurs
         dynapad-collects/misc/pathhack
         (only-in dynapad-collects/imagemagick/imagemagick
                  im/identify
                  )
         (only-in dynapad/libdynapad-wrapper
                  sch_imagep)
         (only-in dynapad/layout/bbox
                  apply-bb-transform
                  make-bb-transform
                  )
         (only-in dynapad/menu/popup-provider
                  add-object-menu
                  )
         (only-in dynapad/menu/menu_functions
                  make-submenu-Select-Highlighted
                  make-submenu-Edit
                  )
         (only-in dynapad/menu/wxmenu
                  add-menu-item
                  new-popup
                  add-submenu
                  add-checkable-menu-item
                  add-menu-separator
                  )
         #;
         (only-in dynapad/menu/menu_functions
                  make-menu-for-pdf
                  )
         (only-in dynapad/events/image-events
                  add-images-to-hires-list
                  image-hires-and-center
                  init-image-bindings
                  )
         (only-in dynapad/utils/handle
                  assign-handle
                  pdf-handle%
                  get-pdf-metadata-dir
                  )
         (only-in dynapad/image-utils/metadata-base
                  get-pdf-date
                  get-pdf-filedate
                  )
         (only-in dynapad/utils/parsedate
                  pair->date
                  )
         )

(define *max-pdf-images* 8)
(define *max-pdf-images-shown* 1)

(define *pdf-aftermake-callbacks* null) ;list of (lambda (img)...)
(define pdf-aftermake-callbacks
  (callback-accessor-functions *pdf-aftermake-callbacks*))

(define *pdf-subimage-base* "image")
(define *pdf-subimage-rexp* (regexp (format "^(~a.*)" *pdf-subimage-base*)))

; no need for above; use build-in split path
; (or: replace with (file-name-from-path...)?)

(define 1cell-layout (list '(0.2 #f 0.8 0.75))) ;centered below 3/4 hline
(define 2cell-layout (list '(0.3 0.5 0.7 #f) '(0.3 #f 0.7 0.45))) ;N,S
(define 3cell-layout (list '(0 0.4 #f 0.75) '(#f 0.2 1 0.6) '(0 0 #f 0.4))) ;NW,E,SW
(define 4cell-layout (list '(0 0.4 #f 0.75) '(#f 0.4 1 0.75) ;NW,NE,
                           '(0 0 #f 0.4)   '(#f 0 1 0.4)))   ;SW,SE
(define 5cell-layout (cons '(0.3 #f 0.7 0.7) 4cell-layout))
;(define 6cell-layout (list-append (list '() '())
;                  4cell-layout))
(define 1cell-surplus (list '(#f 1 0 1.5)))
(define 2cell-surplus (cons '(1 1 #f 1.5) 1cell-surplus))
;(define 3cell-surplus (cons '(-0.5 1 #f 0) 2cell-surplus))
(define 3cell-surplus (cons '(1 -0.5 #f 0) 2cell-surplus))
(define 4cell-surplus (cons '(#f -0.5 0 0) 3cell-surplus))

;;; from dynapad/image-utils/metadata
; ====== Metadata menu =========
(define *show-metadata-time?* #t)
(define (show-metadata-time?) *show-metadata-time?*)

(define *pad-date-format* 'american)
(define pad-date-format
  ;(date-display-format ...) needs this wrapper for some reason
  ;  or menu doesnt refresh
  (case-lambda
    (() *pad-date-format*)
    ((frmt) (set! *pad-date-format* frmt)
            (date-display-format frmt))))

(define-macro (time-format-item format)
  `(add-checkable-menu-item sb ,(symbol->string format)
                            (lambda (i)
                              (pad-date-format ',format))
                            (eq? ',format (pad-date-format))))

(define (make-submenu-DateFormat mb obj)
  (let* ((sb (add-submenu mb "Date Format...")))
    (add-checkable-menu-item sb "Show time"
                             (lambda (i) (set! *show-metadata-time?*
                                               (not (show-metadata-time?))))
                             (show-metadata-time?))
    (add-menu-separator sb) ;------------------------------
    (time-format-item american)
    (time-format-item chinese)
    (time-format-item german)
    (time-format-item indian)
    (time-format-item irish)
    (time-format-item julian)
    (time-format-item iso-8601)
    (time-format-item rfc2822)
    ))

;;;
(define (blank-baseimage-file)
  (build-path->string *dynapad-directory* "pad/bitmaps/blank.jpg"))

(define (end-of-path path)
  (let-values (((path end result) (split-path->string path)))
    end))

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

(define (ensure-pdf obj)
  (cond ((is-a? obj pdf-portrait%) obj)
        ((is-a? obj resizable-image%) (send (get-container obj) getgroup))
        (else #f)))


(define (save-pdf-config pdf) ;pdf is a pdf-portrait
  (let ((dir (send pdf dir)))
    (and (directory-exists? dir)
         (let* ((path (build-path->string dir "config.rkt"))
                (file (open-output-file path #:mode 'text #:exists 'truncate))
                (cmd `(ic (make-object pdf-portrait% dynapad
                                       (relpath ,(build-path->string "../"
                                                                     (file-name-from-path (send pdf url)))))
                          (subimages (list ,@(map (lambda (lst) (cons 'list lst))
                                                  (send pdf subimages)))))))
           (write cmd file)
           (close-output-port file)))))

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
    (or (let ((config-file (build-path->string dir "config.rkt")))
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

;; pdf menu functions from composite

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
    ;      (add-menu-item menu "Rearrange..."
    ;             (lambda () (send obj expand))
    ;             (not (send obj expanded?)))
    ;      (add-menu-item menu "Flatten..."
    ;             (lambda () (send obj condense))
    ;             (send obj expanded?)))
    ;)
    (add-menu-item menu "View Document..." (lambda () (send pdf view-document)))
    (make-submenu-Select-Highlighted menu pdf)
    (add-menu-separator menu) ;------------------------------
    (make-metadata-menu-for-pdf pdf menu)
    menu))

