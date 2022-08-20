#lang racket/base

(provide image%
         baseimage%
         imagedata%
         findthumb
         pdf%
         ensure-thumb
         image-aftermake-callbacks
         image-toggle-hires-thumb
         gifjpg_rexp
         )

(require racket/class
         (only-in racket/path file-name-from-path)
         dynapad/objects
         dynapad/pad-state
         dynapad/misc/misc
         dynapad/misc/filenames
         ;dynapad/events/image-events ; induces a cycle
         (only-in dynapad/libdynapad-wrapper
                  sch_imagepath
                  sch_imagedim
                  sch_makeimage
                  sch_imagedata
                  sch_makeimagedata
                  sch_freeimagedata
                  sch_grab
                  )
         dynapad-collects/misc/pathhack
         dynapad-collects/thumbify/thumbify
         )


;; thumbs

(define gifjpg_rexp (regexp "(.*[/\\]|)(.*)(\\.[^\\.]*)$"))
(define thumb_rexp (regexp "(.*[/\\]|)(.+)(\\.|-)(125|150)(\\.([gG][iI][fF]|[jJ][pP][gG]|[pP][pP][mM]|[pP][nN][gG]|[tT][iI][fF]|[bB][mM][pP]))$"))

(define (image-toggle-hires-thumb img)
  (lambda (eventPAD e) (set-currentPAD! eventPAD)
          (if (send img hires?)
              (send img thumb)
              (send img hires))))

; List of places to look for corresponding thumbnail.
; (findthumb) no longer supports thumbnails in same directory
; with images.  This can be adjusted by changing the following
; list to include ("" "-125"), for example.
(define *thumbnail-variants*
  '( ("thumbs/125/" "-125") ; <-- try default first
     ("thumbs/"     "-125")
     ("thumbs/"     "-150")
     ("thumbs/150/" "-150")
     ("thumbs/"     ".125")
     ("thumbs/"     ".150")
     ("thumbs/125/" ".125")
     ("thumbs/150/" ".150")))

(define (findthumb hires)
  (call/cc (lambda (return)
             (let* ((s (if (path? hires) (path->string hires) hires))
                    (l (regexp-match gifjpg_rexp s))
                    (path   (list-ref l 1))
                    (base   (list-ref l 2))
                    (suffix (list-ref l 3)))
               (foreach *thumbnail-variants* (lambda (dir_sz)
                                               ; try thumbnail with same suffix as hires image
                                               (let ((thumbfile (string-append path (car dir_sz) base (cadr dir_sz) suffix)))
                                                 (when (file-exists? thumbfile) (return thumbfile)))
                                               ; try thumbnail with .jpg suffix
                                               (let ((thumbfile (string-append path (car dir_sz) base (cadr dir_sz) ".jpg")))
                                                 (when (file-exists? thumbfile) (return thumbfile))) ))
               (return #f)))))

(define (ensure-thumb hirespath)
  (let ((thumbpath (findthumb hirespath)))
    (unless (and thumbpath (file-exists? thumbpath))
      (thumbify hirespath))
    thumbpath))

;; classes

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
         (set! _hirespath
               (cond
                 ((path? newhirespath) newhirespath)
                 ((string? newhirespath) (string->path newhirespath))
                 (else (error 'bad-path-type-error "unsupported type for path ~a" newhirespath))))
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

;; from dynapad/image-utils/arrangeimages
(define pdf%
  (class image%
    (init initpad initpath (initposition #f))
    (inherit hirespath dynaclass position writeoptions)
    (field (_path #f) (_name #f) (_dotdir #f) (_author #f) (_title #f)
           (_firstpage #f) (_images '()) (_largest #f)
           (_composite #f) (_composites '()) (_text #f))

    (override write)

    (define/public (path)
      _path)
    (define/public (name)
      _name)
    (define/public (dotdir)
      _dotdir)
    (define/public (author)
      _author)
    (define/public (title)
      _title)
    (define/public (firstpage)
      _firstpage)
    (define/public (images)
      _images)
    (define/public (largest)
      _largest)
    (define/public (composite)
      _composite)
    (define/public (composites)
      _composites)
    (define/public (text)
      _text)

    (define (write)
      `(let ((obj (make-object ,(dynaclass) dynapad ,(path))))
         (send* obj
           ,@(writeoptions))
         obj))

    (set! _path (path->complete-path initpath))
    (let-values (((base name dir?) (split-path->string _path)))
      (set! _name name)
      (set! _dotdir (build-path->string base (string-append "." name))))

    (define (dotpath name)
      (let ((x (build-path->string _dotdir name)))
        (if (file-exists? x) x #f)))

    (set! _firstpage (dotpath "firstpage.jpg"))
    (set! _largest (dotpath "largest.jpg"))
    (set! _composite (dotpath "composite.jpg"))
    (set! _text (dotpath "text.jpg"))

    (define image_regexp (regexp "image-[0-9][0-9][0-9].[jJ][pP][gG]"))

    (when (directory-exists? _dotdir)
      (set! _images
            (map (lambda(x) (dotpath x))
                 (filter (lambda(file) (regexp-match image_regexp file))
                         (sort (directory-list->string _dotdir) string<?))))
      )

    (define composite_regexp (regexp "composite-[0-9][0-9][0-9].[jJ][pP][gG]"))

    (when (directory-exists? _dotdir)
      (set! _composites
            (map (lambda(x) (dotpath x))
                 (filter (lambda(file) (regexp-match composite_regexp file))
                         (sort (directory-list->string _dotdir) string<?))))
      )

    (super-instantiate(initpad))
    (when initposition
      (position initposition))
    (when _composite (hirespath _composite))
    (dynaclass 'pdf%)))
