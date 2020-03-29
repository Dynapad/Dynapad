#lang racket/base

(require racket/class
         racket/list
         racket/file
         (only-in racket/path file-name-from-path)
         compatibility/mlist
         compatibility/defmacro
         dynapad/libdynapad
         dynapad/ffs
         dynapad/base ; dynapad%
         dynapad/bind
         dynapad/pad-state
         dynapad/misc/misc
         dynapad/misc/alist
         dynapad/layout/bbox  ; sigh of course this wasn't in the original spec
         ;dynapad/history/ids  ; see if we really need this ... cycles
         ;                     ; export-objs
         dynapad/utils/hilights
         collects/misc/pathhack  ; build-path->string
         dynapad/events/image-events
         )

;;; (load-relative-extension (path-replace-suffix "build/libdynapad" (system-type 'so-suffix)))

(define padthread
  (let
      ((new-es (make-eventspace)))

    (parameterize
        ((current-eventspace new-es))
      (thread
       (lambda ()
         (wish)
         (display "bye"))))))

(define (describe-all-callbacks obj)
  (map (lambda (name mth) (say name (eval `(send ,obj ,mth))))
       (list "delete" "afterdelete"
             "slide" "afterslide"
             "position" "afterposition"
             "scale" "afterscale"
             "width" "afterwidth"
             "height" "afterheight"
             "select"
             "reposition" "resize"
             )
       (list 'delete-callbacks 'afterdelete-callbacks
             'slide-callbacks 'afterslide-callbacks
             'position-callbacks 'afterposition-callbacks
             'scale-callbacks 'afterscale-callbacks
             'width-callbacks 'afterwidth-callbacks
             'height-callbacks 'afterheight-callbacks
             'select-callbacks
             'reposition-callbacks 'resize-callbacks
             )))

(define slowgrow
  (case-lambda
    ((obj slowness)
     ;slowness 0--> sticky z (constant size)
     ;slowness 1--> normal zooming
     ;        >1--> faster than view
     (slowgrow obj slowness
               (or (not slowness)
                   (let ((padz (send (send obj dynapad) getzoom)))
                     (* (send obj z)
                        (expt padz slowness))))))
    ((obj slowness home-sz)
     (slowgrow obj slowness home-sz 0))
    ;      (Obj-z = relative size; Apparent size = obj-z * pad-z)
    ; Need 3 params to describe a line plus threshold.
    ;
    ;
    ; obj-z   `
    ;          `
    ;           `       /
    ;            `     / - - slowness (slope)
    ;             `.  /
    ;               `/  <- - home-sz (y-intercept):
    ;  obj          /:`.      = rel. sz = apparent sz at zoom=1
    ;apparent      / :  ` .
    ;  size  _____/  :  <- -`-.- - - min-sz
    ;                :          `  .
    ;   0            1                 `  -   _
    ;      out  <--zoom-->  in                    "    -    .
    ;
    ((obj slowness home-sz min-sz)
     ;home-sz is obj's z when dynapad zoom=1
     ; min-sz sets minimum size (fraction of home-sz)
     (if (not slowness)
         (send obj renderscript #f) ;normal zooming
         ;else slowed zooming:
         (let* ((mypad (send obj dynapad))
                (set-z-script
                 (lambda (o)
                   (let* ((padz (send mypad getzoom))
                          (newz (* home-sz (expt padz (- slowness 1))))
                          (minz (/ min-sz padz)))
                     (send o z (max minz newz))))))
           ;(send obj z (/ home-sz (send mypad getzoom)))
           (send obj renderscript
                 (lambda (o)
                   (set-z-script o)
                   (send o renderitem)))
           ;run set-z-script now, to set the correct intial z
           (set-z-script obj)
           )))))


; for text labels, try (slowgrow label .3 2 .5)


(define panel%
  (class dynaobject%
    (inherit-field _dynapad)
    (init dynaptr (initcoords '(0 0 100 100)) (initmembers #f))
    (set! _dynapad dynaptr)
    (inherit-field cptr selected)
    (inherit dynaclass)
    (override writeoptions fix delete anchor re-anchor)
    (public members add remove save divisible coords fill)
    (field (savemembers null))

    (define anchor (case-lambda
                     (() (super anchor))
                     ((newanchor) (super anchor "sw")) ))

    (define (re-anchor newanchor) (super re-anchor "sw"))

    (define (fill color) (sch_panelsetfill (send this get-cptr) color))

    (define coords (case-lambda
                     (() (send this bbox))
                     ((crds)
                      (send this width (bbwidth crds))
                      (send this height (bbheight crds))
                      (send this xy (car crds) (cadr crds)) )))

    (define members
      (case-lambda
        (() (sch_members cptr))
        ((newmembers)
         (sch_members cptr (map (lambda (o) (send o get-cptr)) newmembers))
         (send this update-any-hilights))))

    (define save
      (case-lambda
        (() (map (lambda (o) (send o padid)) (members)))
        ((newmembers) (set! savemembers newmembers))))

    (define (fix alist)
      (for-each (lambda (o) (add (cadr (assq o alist)))) savemembers)
      (super fix alist))

    (define (add l)
      (let ((l (if (list? l) l (list l))))
        (for-each (lambda (o) (sch_addmember cptr (send o get-cptr))) l))
      (send this update-any-hilights))

    (define (remove l)
      (let ((l (if (list? l) l (list l))))
        (for-each (lambda (o) (sch_removemember cptr (send o get-cptr))) l))
      (send this update-any-hilights))

    (define (writeoptions)
      `(,@(super writeoptions)
        (save ',(save)) ;;; Should be same as group%
        (coords ',(coords))
        (divisible ,(divisible))))

    (define (delete)
      (for-each (lambda (o) (remove o) (send o delete)) (members))
      (super delete))

    (define divisible
      (case-lambda
        (() (sch_divisible cptr))
        ((bool) (sch_divisible cptr bool))))

    (define/override (select . args)
      (if (null? args) (super select)
          (when (not selected)
            (set! selected (make-object select% _dynapad this (car args))))))

    (super-instantiate (_dynapad (sch_makepanel (send _dynapad get-cptr) this)))
    (dynaclass 'panel%)
    (coords initcoords)
    (when initmembers (members initmembers))))

(define imagedata%
  (class object%
    (init-field _dynapad (initimagepath #f))
    (field (cptr #f) (_dynaclass #f))
    (public imagepath free get-cptr dimensions dynaclass)

    (define (get-cptr)
      cptr)

    (define (imagepath)
      (sch_imagepath cptr))

    (define (free)
      (sch_freeimagedata (send _dynapad get-cptr) cptr))

    (define (dimensions)
      (sch_imagedim cptr))

    (define dynaclass
      (case-lambda
        (() _dynaclass)
        ((newclass) (set! _dynaclass newclass))))

    (super-instantiate ())
    (dynaclass 'imagedata%)
    (when initimagepath
      (set! cptr (sch_makeimagedata (send _dynapad get-cptr) this initimagepath)))))

(define grabdata%
  (class imagedata%
    (init _dynapad (winid #f) (xywh #f))
    (inherit-field cptr)
    (inherit dynaclass)
    (super-instantiate (_dynapad))
    (set! cptr (sch_grab (send _dynapad get-cptr) this winid xywh))
    (dynaclass 'grabdata%)))

(define baseimage%
  (class dynaobject%
    (init-field (initdynapad #f))
    (init (initimagedata #f) (initposition #f))
    (inherit position unselect dynaclass)
    (inherit-field cptr selected)
    (field (idata #f))
    (public imagedata saveimagepath)
    (override writeoptions)

    (define saveimagepath
      (case-lambda
        (() (if idata (send idata imagepath) #f))
        ((newimagepath)
         (when newimagepath
           (imagedata (make-object imagedata% initdynapad newimagepath))))))

    (define/override (export dir)
      (let ((myname (file-name-from-path (saveimagepath))))
        (export-link (saveimagepath) (build-path->string dir myname))))

    (define (writeoptions)
      `(,@(super writeoptions)))

    #|
    (define/override (write)
    `(let ((obj (make-object ,(dynaclass) dynapad)))
    (send* obj
    ,@(writeoptions)
    (saveimagepath ,(saveimagepath)))
    obj))
    |#
    ;    (define/override (write)
    ;      `(ic (make-object ,(dynaclass) dynapad)
    ;       ,@(writeoptions)
    ;       (saveimagepath)))

    (define imagedata
      (case-lambda
        (() idata)
        ((data)
         (cond
           ((is-a? data imagedata%)
            (sch_imagedata cptr (send data get-cptr))
            (send this update-any-hilights))
           ((eq? data #f)
            (unselect)
            (sch_imagedata cptr #f))
           (else
            (error "image% imagedata expects imagedata, given " data)))
         (set! idata data))))

    (when (not (is-a? initdynapad dynapad%))
      (error "instantiate: image% expects type dynapad% as 1st argument, given " initdynapad))

    (super-instantiate (initdynapad (sch_makeimage (send initdynapad get-cptr) this)))
    (dynaclass 'baseimage%)

    (when initposition (send this position initposition))
    (when initimagedata (imagedata initimagedata))))

(define *image-aftermake-callbacks* null) ;list of (lambda (img)...)
(define image-aftermake-callbacks (callback-accessor-functions *image-aftermake-callbacks*))
(define image%
  (class baseimage%
    (init initpad)
    (init-field (_hirespath #f))
    (init (initposition #f))
    (field (_hiresdata #f) (_thumbdata #f))
    (field (_use_bbox #f))
    (inherit-field _dynapad)
    (inherit imagedata bind anchor scale dynaclass bbox)
    (override delete)
    (override writeoptions center)
    (public hires thumb hires? hirespath free free-hiresdata)

    (define (hires)
      (when (not _hiresdata)
        (set! _hiresdata (make-object imagedata% _dynapad _hirespath))
        (if _thumbdata
            ; has thumb; restore curr size after loading hiresdata
            (let ((w (send this width))
                  (h (send this height)))
              (imagedata _hiresdata)
              (send this widthheight w h))
            ;else no thumb; don't worry about size
            (imagedata _hiresdata)) )
      _hiresdata)

    (define (free-hiresdata)
      ; works only when thumb already in view; call (thumb) first
      (when (and _thumbdata (eq? _thumbdata (imagedata)))
        (when _hiresdata
          (send _hiresdata free))
        (set! _hiresdata #f)))

    (define (thumb)
      (when (and _thumbdata (eq? (imagedata) _hiresdata))
        (let ((w (send this width))
              (h (send this height)))
          (imagedata _thumbdata)
          (send this widthheight w h)
          (unless (send _dynapad getvar 'cache-image-hiresdata)
            (free-hiresdata))))
      _thumbdata)

    (define/override width (case-lambda
                             (() (super width))
                             ((new) (set! _use_bbox #t) (super width new))
                             ))

    (define/override height (case-lambda
                              (() (super height))
                              ((new) (set! _use_bbox #t) (super height new))
                              ))

    (define/override (export dir)
      (let ((myname (file-name-from-path (hirespath))))
        (export-link (hirespath) (build-path->string dir myname))))

    ; write hirespath, but thumb position
    ; so super-writeoptions must be last
    (define (writeoptions)
      (filter (lambda (opt) opt)
              `(,@(super writeoptions)
                (hirespath (string->path ,(path->string (hirespath))))
                ,(if (not _use_bbox) #f `(bbox ',(bbox)))
                )))
    (define/override (writeinits)
      (list `(string->path ,(path->string (hirespath)))))

    #|
    (define/override (write)
    `(let ((obj (make-object ,(dynaclass) dynapad)))
    (send* obj
    ,@(writeoptions))
    obj))
    |#
    ;    (define/override (write)
    ;      `(ic (make-object ,(dynaclass) dynapad ,(hirespath))
    ;       ,@(writeoptions)))
    ;      (let* ((suffixes (map (lambda (fn) ((car fn) this)) _post-build-ops))
    ;         (alter-bld (and _alternate-build-fn (_alternate-build-fn this)))
    ;         (build-expr (or alter-bld
    ;                 `(ic (make-object ,(dynaclass) dynapad)
    ;                  ,@(writeoptions)))))
    ;    (if (null? suffixes)
    ;        build-expr
    ;        `(with obj ,build-expr ,@suffixes))))

    (define hirespath
      (case-lambda
        (() _hirespath)
        ((newhirespath)
         (set! _hirespath newhirespath)
         (let ((thumbpath (findthumb _hirespath)))
           (when thumbpath
             (set! _thumbdata (make-object imagedata% _dynapad thumbpath))))
         (if _thumbdata
             (imagedata _thumbdata)
             (hires)))))

    (define (delete)
      (imagedata #f)
      (when _hiresdata (send _hiresdata free))
      (when _thumbdata (send _thumbdata free))
      (bind "<Run-Shift-ButtonPress-1>" '())  ; #f causes fatal error on delete-key
      (super delete))

    (define (free)
      (imagedata #f)
      (when _hiresdata (send _hiresdata free))
      (when _thumbdata (send _thumbdata free)))

    (define (hires?)
      _hiresdata)

    (define (center . args)
      (let ((lc (send _dynapad lastcentered)))
        (when (and lc (is-a? lc image%)) (send lc thumb)))
      (super center . args)
      (hires))

    (super-instantiate (initpad))
    (dynaclass 'image%)
    (when initposition (send this position initposition))
    (when _hirespath (hirespath _hirespath))

    ;(init-image-bindings this)  ; see events.ss
    (when *image-aftermake-callbacks*
      (exec-any-callbacks *image-aftermake-callbacks* this))
    ))

(define (addbinding PAD evnt fnc)
  (send PAD bind evnt (cons fnc (send PAD bind evnt))))

(define bind_to_tag
  (case-lambda
    ((pad tag) (sch_bind_to_tag (send pad get-cptr) tag))
    ((pad tag event) (sch_bind_to_tag (send pad get-cptr) tag event))
    ((pad tag event callback)
     (let ((bindings (bind_to_tag pad tag event)))
       (cond
         ((not callback) (set! bindings null)) ; grandfather #f to mean null
         ((list? callback) (set! bindings callback))
         (else (set! bindings (list callback))))
       (sch_bind_to_tag
        (send pad get-cptr)
        tag
        event
        bindings)))))
