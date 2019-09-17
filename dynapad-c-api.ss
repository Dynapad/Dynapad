(require compatibility/mlist)

(require (lib "class.ss"))
(require (lib "list.ss")) ; because mred doesn't load it by default
(require (lib "defmacro.ss"))
(require (lib "file.ss"))

; macros and utilities for code development
(load-relative "src/misc/misc.ss")
(load-relative "src/misc/alist.ss")
(load-relative "src/history/ids.ss") ;move this elsewhere?
(load-relative "src/utils/hilights.ss")

;;; (load-relative-extension (path-replace-suffix "build/libdynapad" (system-type 'so-suffix)))

(let
    ([libdynapad-path (getenv "LIBDYNAPAD_PATH")])
  (if libdynapad-path
      (load-relative-extension libdynapad-path)
      (load-relative-extension "build/libdynapad.so")))

(define padthread
  (let
      ((new-es (make-eventspace)))

    (parameterize
        ((current-eventspace new-es))
      (thread
       (lambda ()
         (wish)
         (display "bye"))))))

(define-struct event (x y type key obj sx sy))
(define-struct (tablet-event event) (xid p tiltx tilty state button))

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

(define dynaobject%
  (class linkable-obj%
    (init-field _dynapad cptr)
    (init (initposition #f) (initanchor #f))
    (init-field (initlink #f))
    (field
     (prev_coords #f)
     (selected #f)
     (_hilights null) ;includes selected; list of (...(hl-obj name)...)
     ;                 where hl-obj is hilight% and name is optional
     (_dynaclass #f)
     ;(_oldid #f) (_id #f)  ;now in superclass
     (actor_list null)
     (_alist null)
     (_taglist null)
     (_deleted? #f)
     (_writable #t)
     (_deletable #t)
     (_takegroupevents #f)
     (_layer-stack '())
     (_select_highlight_func #f)
     (_post-build-ops null)
     (_onetime-writeoptions null)
     (_alternate-build-fn #f)
     (_dependents-fn #f)

     (_delete-callbacks   '()) (_afterdelete-callbacks   '())
     (_slide-callbacks    '()) (_afterslide-callbacks    '())
     (_position-callbacks '()) (_afterposition-callbacks '())
     (_scale-callbacks    '()) (_afterscale-callbacks    '())
     (_width-callbacks    '()) (_afterwidth-callbacks    '())
     (_height-callbacks   '()) (_afterheight-callbacks   '())
     (_select-callbacks   '())
     (_reposition-callbacks '())   (_resize-callbacks '())
     (_physics? #f)
     (_mass 1)
     (_force (list 0 0))
     (_velocity (list 0 0))
     (_prevxy (list 0 0))
     )

    (public (mybind bind)
            above actors add-actor addtag alist dependents-fn alternate-build-fn
            alwaysrender anchor
            bbox below center post-build-ops delete-all ;delete now in superclass
            deletable? deleted? deletetag dtag dynaclass dynapad
            events export faderange findable fix focus get-cptr getgroup
            gettags hastag hilights height boundable
            layer layer-stack link loadalist lower
            maxsize minsize padid pixeldim position
            raise re-anchor recall-coords rendercolor renderimage
            renderitem renderline renderpolygon renderscript rendertext
            save-coords savealist savelink scale select selected?
            select-highlight-func settags
            slide sticky takegroupevents transparency unfocus unselect
            update-any-hilights visible?
            width widthheight writable? write write-all writeinits writeoptions
            onetime-writeoptions
            xy z zoomaction

            select-callbacks      ;currently no after-select callbacks needed
            delete-callbacks    afterdelete-callbacks
            slide-callbacks     afterslide-callbacks
            position-callbacks  afterposition-callbacks
            scale-callbacks     afterscale-callbacks
            width-callbacks     afterwidth-callbacks
            height-callbacks    afterheight-callbacks

            ; all callbacks, if provided, use syntax (callback-fn object args...)
            ; where args are the arguments to the original fn (i.e. slide, scale, etc)
            )

    (define/public physics?
      (case-lambda
        (() _physics?)
        ((v) (set! _physics? v))))

    (define (mass? v) (number? v))

    (define/public mass
      (case-lambda
        (() _mass)
        ((v)
         (if (mass? v)
             (set! _mass v)
             (error "not a mass" v)))))

    (define (force? v)
      (and
       (list? v)
       (= (length v) 2)
       (number? (list-ref v 0))
       (number? (list-ref v 1))))

    (define/public force
      (case-lambda
        (() _force)
        ((v)
         (if (force? v)
             (set! _force v)
             (error "not a force" v)))))

    (define/public fx
      (case-lambda
        (() (car _force))
        ((v) (force (list v (cadr _force))))))

    (define/public fy
      (case-lambda
        (() (cadr _force))
        ((v) (force (list (car _force) v)))))

    (define (velocity? v)
      (and
       (list? v)
       (= (length v) 2)
       (number? (list-ref v 0))
       (number? (list-ref v 1))))

    (define/public velocity
      (case-lambda
        (() _velocity)
        ((v)
         (if (velocity? v)
             (set! _velocity v)
             (error "not a velocity" v)))))

    (define/public vx
      (case-lambda
        (() (car _velocity))
        ((v) (velocity (list v (cadr _velocity))))))

    (define/public vy
      (case-lambda
        (() (cadr _velocity))
        ((v) (velocity (list (car _velocity) v)))))

    (define (prevxy? v)
      (and
       (list? v)
       (= (length v) 2)
       (number? (list-ref v 0))
       (number? (list-ref v 1))))

    (define/public prevxy
      (case-lambda
        (() _prevxy)
        ((v)
         (if (prevxy? v)
             (set! _prevxy v)
             (error "not a prevxy" v)))))

    (define/public prevx
      (case-lambda
        (() (car _prevxy))
        ((v) (prevxy (list v (cadr _prevxy))))))

    (define/public prevy
      (case-lambda
        (() (cadr _prevxy))
        ((v) (prevxy (list (car _prevxy) v)))))

    (define alist
      (case-lambda
        (() _alist)
        ((newalist) (set! _alist newalist))))

    (define (savealist)
      (filter (lambda (p) (dynapad-printable? p))
              (filter-alist this (alist))
              ))

    (define (loadalist newalist)
      ;newlist is (...(key v0 v1 ...) ...)
      ;  each v is eval'd (which may include side effects)
      ; if key is #f, entire tuple is omitted (after possible side effects from vs)
      ; else replaced w. (key v0' v1'...)
      (alist (append _alist
                     (filter (lambda (term) term)
                             (map (lambda (keyval)
                                    (let* ((key (car keyval))
                                           (vals (cdr keyval))
                                           (results (map (lambda (val)
                                                           (if (list? val) (eval val) val))
                                                         vals)))
                                      (and key
                                           (cons key results))))
                                  newalist)))))


    (define (settags newtaglist) (set! _taglist newtaglist))
    (define (gettags) _taglist)
    (define (addtag tag) (set! _taglist (append _taglist (list tag))))
    (define (dtag tag) (set! _taglist (remq tag _taglist)))
    (define (deletetag tag) (dtag tag))
    (define (hastag tag) (memq tag _taglist))

    (define (mybind . args)
      (apply bind this args))

    (define deletable?
      (case-lambda
        (() _deletable)
        ((bool) (set! _deletable bool))))

    (define/override (delete)
      (set! _deleted? #t)
      (super delete)
      (when cptr
        (exec-any-callbacks _delete-callbacks this)
        (sch_delete cptr)
        (set! cptr #f))
      (when (eq? this (send _dynapad lastcentered)) (send _dynapad lastcentered #f))
      (for-each (lambda (actor) (send actor delete)) actor_list)
      (for-each (lambda (tuple) (send (car tuple) delete)) (send this hilights)) ;includes select, if any
      (exec-any-callbacks _afterdelete-callbacks this))
    (define delete-callbacks (callback-accessor-functions _delete-callbacks))
    (define afterdelete-callbacks (callback-accessor-functions _afterdelete-callbacks))

    (define deleted?
      (case-lambda
        (() _deleted?)
        ((bool) (set! _deleted? bool))))

    (define writable?
      (case-lambda
        (() _writable)
        ((bool) (set! _writable bool))))

    (define dependents-fn (get/set _dependents-fn))
    ; lambda which returns list of objects written
    ;  by write-all method
    ; Example (lambda (obj) (send obj members))  includes group members in write-all

    (define post-build-ops (callback-accessor-functions _post-build-ops))
    ;
    ; lambda (this label)
    ; which returns list of commands, appended to write expression,
    ;  to apply to this after build.
    ; The result of build will be bound to label,
    ;  which can then be included in each expr to refer to that future copy
    ; Example: (... (lambda (this 'me) (rgstr-obj N me)) ...)

    (define alternate-build-fn (get/set _alternate-build-fn))
    ;
    ; This fn can be set to (lambda (obj) ...) which
    ;  returns a build-expr which replaces its default (ic (make-object...))
    ;   or #f, which keeps the default.
    ; Example: (lambda (obj) (if (is-a? obj image%)
    ;                            `(ic (make-object rect% dynapad) ...)
    ;                            #f))
    ;   This saves all images as rectangles.
    ; In all cases, the build-expr is suffixed with operations
    ;   returned by the _post-build-ops

    (define (write)
      (define my-label 'obj) ; any _post-build-ops use my-label to refer to
      ; the future clone of this
      (onetime-writeoptions null)
      (let* ((suffixes (map (lambda (fn) ((car fn) this my-label))
                            _post-build-ops))
             (alter-bld (and _alternate-build-fn (_alternate-build-fn this)))
             (build-expr (or alter-bld
                             `(ic (make-object ,(dynaclass) dynapad ,@(send this writeinits))
                                  ,@(writeoptions)
                                  ,@(map car (onetime-writeoptions))))))
        (if (null? suffixes)
            build-expr
            `(with ,my-label ,build-expr ,@suffixes))))
    ;      (error "dynaobject% write: need write method in " _dynaclass)


    ;hopefully, this is now obsolete:
    (define onetime-writeoptions (callback-accessor-functions _onetime-writeoptions))
    ;items that may be generated *during* a write;
    ;  cleared at the start of each write.
    ;  Allows e.g. alist filters to add items outside alist boundary

    ; position anchor oldid
    (define (writeoptions)
      (filter (lambda (opt) opt)
              `(
                (id ,(send this id)) ;generates id if not one already
                ,(if (string=? (anchor) "center")     #f `(anchor ,(anchor)))
                (position ,@(position))
                ,(if (equal? (minsize) '( 0.0 #f)) #f `(minsize ',(minsize)))
                ,(if (equal? (maxsize) '(-1.0 #f)) #f `(maxsize ',(maxsize)))  ; maxsize isn't 10000
                ,(if (and (> (faderange) 0.3) (< (faderange) 0.3001))
                     #f `(faderange ,(faderange)))
                ,(if (= (transparency) 1.0)           #f `(transparency ,(transparency)))
                ,(if (not (sticky))                   #f `(sticky ,(sticky)))
                ;(oldid ,(padid))
                ,(if (null? (alist))                  #f `(loadalist ',(savealist)))
                ,(if (null? (gettags))                #f `(settags ',(gettags)))
                ,(and (savelink))
                )))

    ; args to include on (make-object...) line of write string
    (define (writeinits) null) ;overridden in subclasses

    (define (write-all)
      (cons (write)
            (if (not _dependents-fn)
                null
                (write-set (_dependents-fn this)))))

    (define (delete-all)
      (let ((others (if _dependents-fn
                        (_dependents-fn this)
                        null)))
        (foreach others (lambda (o) (send o delete-all)))
        (send this delete)))

    (define (update-any-hilights)
      (when (not (null? _hilights))
        (let ((n 0))
          (foreach _hilights (lambda (hl) (send (car hl) update (_++ n)))))))

    (define (get-cptr)
      cptr)

    (define (dynapad) _dynapad)

    (define (getgroup)
      (sch_getgroup cptr))

    (define takegroupevents (case-lambda
                              (() _takegroupevents)
                              ((bool) (set! _takegroupevents bool)) ))

    (define actors (case-lambda (() actor_list) ((l) (set! actor_list l))))
    (define (add-actor a) (set! actor_list (append actor_list (list a))))

    (define (save-coords c)
      (set! prev_coords c))

    (define (recall-coords)
      prev_coords)

    (define/public (update-position dx dy)
      (exec-any-callbacks _reposition-callbacks this dx dy))
    (define/public reposition-callbacks (callback-accessor-functions _reposition-callbacks))
    (define/public update-size
      (case-lambda
        ;       ((zfac) (exec-any-callbacks _resize-callbacks this zfac))
        ;       ((wfac hfac) (exec-any-callbacks _resize-callbacks this wfac hfac))
        ((zfac x y)
         (exec-any-callbacks _resize-callbacks this zfac x y))
        ((xfac yfac x y)
         (exec-any-callbacks _resize-callbacks this xfac yfac x y))))
    (define/public resize-callbacks (callback-accessor-functions _resize-callbacks))

    (define position
      (case-lambda
        (() (sch_position cptr))
        ((x y s) (position (list x y s)))
        ((l)
         (let* ((xyz (sch_position cptr))
                (z (caddr xyz))
                (zfac 1) ;unless changed below...
                )
           (if (null? (cddr l)) ;l is xy only?
               (set-append! l (list z))
               (set! zfac (/ (caddr l) z)))
           (exec-any-callbacks _position-callbacks this l)
           (update-position (- (car l) (car xyz)) (- (cadr l) (cadr xyz)))
           (unless (= zfac 1)
             (update-size zfac (car l) (cadr l)))
           (sch_position cptr l)
           (exec-any-callbacks _afterposition-callbacks this l)
           (send this update-any-hilights)))))

    (define position-callbacks (callback-accessor-functions _position-callbacks))
    (define afterposition-callbacks (callback-accessor-functions _afterposition-callbacks))

    (define xy
      (case-lambda
        (() (let ((xyz (sch_position cptr)))
              (list (car xyz) (cadr xyz))))
        ((x y)  (xy (list x y)))
        ((new-xy)
         (send/apply this update-position (map - new-xy (xy)))
         (sch_position cptr (append new-xy (list (z))))
         (send this update-any-hilights)
         )))

    (define z
      (case-lambda
        (() (caddr (position)))
        ((new-z) (let ((xy (xy))
                       (z  (z)))
                   (send/apply this update-size (/ new-z z) xy)
                   (sch_position cptr (append xy (list new-z)))
                   (send this update-any-hilights)))))

    (define (slide dx dy)
      (exec-any-callbacks _slide-callbacks this dx dy)
      (update-position dx dy)
      (sch_slide cptr dx dy)
      (exec-any-callbacks _afterslide-callbacks this dx dy)
      (send this update-any-hilights))
    (define slide-callbacks (callback-accessor-functions _slide-callbacks))
    (define afterslide-callbacks (callback-accessor-functions _afterslide-callbacks))

    (define scale
      (case-lambda
        ((fac) (send/apply this scale fac (xy)))
        ((fac x y)
         ;(exec-any-callbacks _scale-callbacks this fac x y)
         (update-size fac x y)
         (sch_scale cptr fac x y)
         ;(exec-any-callbacks _afterscale-callbacks this fac x y)
         (send this update-any-hilights))
        ; x,y may be scaled independently:
        ((xfac yfac) (send/apply this scale xfac yfac (xy)))
        ((xfac yfac x y)
         (if (= xfac yfac)
             (scale xfac x y) ;use normal scaling
             (let* ((myxy (xy))
                    (myx  (car myxy))
                    (myy  (cadr myxy))
                    (w    (width))
                    (h    (height)))
               (send this slide (* (- xfac 1) (- myx x))
                     (* (- yfac 1) (- myy y)))
               (send this widthheight (* w xfac) (* h yfac)))))
        ))

    (define scale-callbacks (callback-accessor-functions _scale-callbacks))
    (define afterscale-callbacks (callback-accessor-functions _afterscale-callbacks))

    (define width
      (case-lambda
        (() (sch_width cptr))
        ((w)
         (exec-any-callbacks _width-callbacks this w)
         (send/apply this update-size (/ w (width)) 1 (xy))
         (sch_width cptr w)
         (exec-any-callbacks _afterwidth-callbacks this w)
         (send this update-any-hilights))))
    (define width-callbacks (callback-accessor-functions _width-callbacks))
    (define afterwidth-callbacks (callback-accessor-functions _afterwidth-callbacks))

    (define height
      (case-lambda
        (() (sch_height cptr))
        ((h)
         (exec-any-callbacks _height-callbacks this h)
         (send/apply this update-size 1 (/ h (height)) (xy))
         (sch_height cptr h)
         (exec-any-callbacks _afterheight-callbacks this h)
         (send this update-any-hilights))))
    (define height-callbacks (callback-accessor-functions _height-callbacks))
    (define afterheight-callbacks (callback-accessor-functions _afterheight-callbacks))

    ; get the value used in {max,min}size calculations
    ; the C code that computes this value can only run during a render, so it's duplicated here
    (define (pixeldim)
      (max
       (* (send _dynapad getzoom) (sch_height cptr))
       (* (send _dynapad getzoom) (sch_width cptr))))

    (define (widthheight arg1 . args)
      (let ((w 1)(h 1))
        (if (null? args)
            (begin (set! w (car arg1)) (set! h (cadr arg1)))
            ;else
            (begin (set! w arg1) (set! h (car args)))
            )
        (exec-any-callbacks _width-callbacks this w)
        (exec-any-callbacks _height-callbacks this h)
        (send/apply this update-size (/ w (width)) (/ h (height)) (xy))
        (sch_width cptr w)
        (sch_height cptr h)
        (exec-any-callbacks _afterwidth-callbacks this w)
        (exec-any-callbacks _afterheight-callbacks this h)
        (send this update-any-hilights)
        ))
    (define layer-stack
      (case-lambda
        (() _layer-stack)
        ((new-stck) (set! _layer-stack new-stck))))

    (define select
      (case-lambda
        (()
         (when (not _deleted?)
           (exec-any-callbacks _select-callbacks this #t)
           (when (not selected)
             (if _select_highlight_func
                 (set! selected (_select_highlight_func _dynapad this))
                 (set! selected (make-object select% _dynapad this)) )
             )))
        ;            (when (and *selection-layer* (*selection-layer-filter-fn* this))
        ;              (push! (send this layer) _layer-stack)
        ;              (send this layer *selection-layer*)))))
        ;allows syntax (send this select #t/#f)
        ((val)
         (if val (select)
             (unselect)))))

    (define select-highlight-func (get/set _select_highlight_func))

    (define (unselect)
      (exec-any-callbacks _select-callbacks this #f)
      (when selected
        (let ((old-lyr (if (null? _layer-stack)
                           #f
                           (car _layer-stack))))
          (when old-lyr
            (send this layer old-lyr)
            (set! _layer-stack (cdr _layer-stack))))
        (send selected delete)
        (set! selected #f)))

    (define select-callbacks (callback-accessor-functions _select-callbacks))

    (define (selected?) selected)

    (define hilights (callback-accessor-functions _hilights))

    (define (padid)
      (if cptr (sch_padid cptr) #f))

    ;    (define id
    ;      (case-lambda
    ;       (() _id)
    ;       ((new) (set! new (import-id new))
    ;          ;(if _id (error "ID overwrite: " _id new)
    ;          (begin
    ;            (set! _id new)
    ;            (push! (list new this) *id->obj-index*)))));)
    ;    (define id
    ;      (case-lambda
    ;       (() _id)
    ;       ((x) #t)))  ;dummy funct during debug

    (define (focus)
      (sch_focus cptr #t)
      (send this update-any-hilights))

    ; Pad_Text object deletes itself if "" and loses focus
    (define (unfocus)
      (when (not (sch_focus cptr #f))
        (set! cptr #f)
        (delete))
      (send this update-any-hilights))

    (define anchor
      (case-lambda
        (() (sch_anchor cptr))
        ((newanchor)
         (sch_anchor cptr newanchor)
         (send this update-any-hilights))))

    (define (re-anchor newanchor)
      (let ((oldbb (send this bbox)))
        (sch_anchor cptr newanchor)
        ;use sch_slide instead of (send this slide...) to bypass callbks
        (sch_slide cptr (- (b0 oldbb) (b0 (send this bbox)))
                   (- (b1 oldbb) (b1 (send this bbox))))
        (send this update-any-hilights)
        )
      )

    (define zoomaction
      (case-lambda
        (() (sch_zoomaction cptr this))
        ((size grow shrink) (sch_zoomaction cptr this size grow shrink))))

    (define (renderscript proc)
      (sch_renderscript cptr this proc))

    (define (renderitem)
      (sch_renderitem cptr))

    (define (renderscale dz)
      (error "renderscale: not implemented"))

    (define (rendertranslate dx dy)
      (error "rendertranslate: not implemented"))

    (define (renderimage imagedata x y)
      (sch_renderimage cptr (send imagedata get-cptr) x y))

    (define (renderline xys)
      (sch_renderline cptr xys))

    (define (renderpolygon xys)
      (sch_renderpolygon cptr xys))

    (define (rendertext string x y)
      (sch_rendertext cptr string x y))

    (define (rendercolor color)
      (sch_rendercolor cptr color))

    (define link
      (case-lambda
        (()
         (and
          initlink
          (is-a? initlink dynaobject%)
          (send initlink deleted?)
          (set! initlink #f))
         initlink)
        ((newlink)
         (cond
           ((eq? newlink #f)
            (set! initlink #f))
           ((and (list? newlink) (eq? (length newlink) 3)
                 (andmap (lambda (x) (if (number? x) x #f)) newlink))
            (set! initlink newlink))
           ((is-a? newlink dynaobject%)
            (set! initlink newlink))
           (else
            (error "link expects dynaobject% or view, given " newlink)))
         (link-hook this))))

    (define savelink
      (case-lambda
        (()
         (cond
           ((eq? initlink #f) #f)
           ((list? initlink) `(savelink ',initlink))
           ((is-a? initlink dynaobject%) ;(send initlink padid))
            ;           `(refer-when-ready 'savelink ,(obj->id initlink)))
            `(defer-send (savelink ,(export-objs initlink))))
           (else
            (error "savelink expects dynaobject% or view, given " initlink))))
        ((newlink) (link newlink))))

    (define (fix alist)
      (when (number? initlink) (link (cadr (assq initlink alist)))))

    (define (center . args)
      (send/apply (dynapad) center this args)
      (center-hook this))

    (define dynaclass
      (case-lambda
        (() _dynaclass)
        ((newclass) (set! _dynaclass newclass))))

    ;    (define oldid
    ;      (case-lambda
    ;        (() _oldid)
    ;    ((new_oldid) (set! _oldid new_oldid))))

    (define bbox (case-lambda
                   (() (sch_bbox cptr))
                   ((bb)
                    ; any subset of bb elements may be #f (i.e. "free" side)
                    (cond ((andmap (lambda (b) b) bb)
                           ;all sides constrained; must set width/height
                           (send this width (bbwidth bb))
                           (send this height (bbheight bb)))
                          ;else at least one #f (free side), so use scale instead:
                          ((and (b0 bb) (b2 bb)) ;fixed x's; scale by width
                           (send this scale (/ (bbwidth bb) (send this width))))
                          ((and (b1 bb) (b3 bb)) ;fixed y's; scale by height
                           (send this scale (/ (bbheight bb) (send this height)))))
                    ; may fall through w/o meeting any cond if multiple #f's
                    (slide
                     (cond ((b0 bb) (- (b0 bb) (b0 (send this bbox))))
                           ((b2 bb) (- (b2 bb) (b2 (send this bbox))))
                           (else 0))
                     (cond ((b1 bb) (- (b1 bb) (b1 (send this bbox))))
                           ((b3 bb) (- (b3 bb) (b3 (send this bbox))))
                           (else 0)))
                    )
                   ))


    ; minsize is (number fraction?)
    (define minsize
      (case-lambda
        (() (sch_minsize cptr))
        ((size)
         (if (pair? size)
             (sch_minsize cptr size)
             (sch_minsize cptr (list size #f))))
        ((size flag) (sch_minsize cptr (list size flag)))))

    ; maxsize is (number fraction?)
    (define maxsize
      (case-lambda
        (() (sch_maxsize cptr))
        ((size)
         (if (pair? size)
             (sch_maxsize cptr size)
             (sch_maxsize cptr (list size #f))))
        ((size flag) (maxsize (list size flag)))))

    (define faderange
      (case-lambda
        (() (sch_faderange cptr))
        ((faderange) (sch_faderange cptr faderange))))

    (define raise
      (case-lambda
        (() (sch_raise cptr))
        ((abovethis)
         (if (or (equal? abovethis "one") (equal? abovethis 'one))
             (sch_raise cptr "one")
             (sch_raise cptr (send abovethis get-cptr))))))

    (define lower
      (case-lambda
        (() (sch_lower cptr))
        ((belowthis)
         (if (or (equal? belowthis "one") (equal? belowthis 'one))
             (sch_lower cptr "one")
             (sch_lower cptr (send belowthis get-cptr))))))

    (define (visible?)
      (sch_visiblep cptr))

    (define transparency
      (case-lambda
        (() (sch_transparency cptr))
        ((transparency) (sch_transparency cptr transparency))))

    (define sticky
      (case-lambda
        (() (sch_sticky cptr))
        ((newsticky) (sch_sticky cptr newsticky)
                     (when selected
                       (send selected sticky newsticky)))))

    (define findable
      (case-lambda
        (() (sch_findable cptr))
        ((bool) (sch_findable cptr bool))))

    (define boundable
      (case-lambda
        (() (sch_boundable cptr))
        ((bool)
         (sch_boundable cptr bool)
         ; should update select box here
         )))

    (define (export dir) (export-stub dir (dynaclass)))
    ; In general: creates a file-system analog of the object
    ;  to be overridden by some subclasses (eg. group%, base-image%...)

    (define events
      (case-lambda
        (() (sch_events cptr))
        ((bool) (sch_events cptr bool))))

    (define alwaysrender
      (case-lambda
        (() (sch_alwaysrender cptr))
        ((bool) (sch_alwaysrender cptr bool))))

    (define (below)
      (sch_below cptr))

    (define (above)
      (sch_above cptr))

    (define layer
      (case-lambda
        (() (sch_layer cptr))
        ((newlayer)
         (sch_layer cptr (send newlayer get-cptr))
         (send this update-any-hilights))))

    (super-instantiate ())
    ;(findable #t); CHANGED default to #t; see line 346 in pad/generic/object.cpp
    (when initposition (position initposition))
    (when initanchor (anchor initanchor))

    )) ;end dynaobject%

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


;----- some help for writing the alist option -----

(define (dynapad-printable? x)
  (or (symbol? x)
      (string? x)
      (boolean? x)
      (char? x)
      (null? x)
      (number? x)
      (and (pair? x)
           (dynapad-printable? (car x))
           (dynapad-printable? (cdr x))) ))

; global list for functions that want to filter alists before saving.
; applications can push filters onto the list using:
;   (alist-filters 'add func)
; where func is (lambda (object alist-element) body)
;               which returns the element, or a modified version,
;                             or () in order prevent writing.

(define *filter-alist-functions* '())
(define alist-filters (callback-accessor-functions *filter-alist-functions*))

(define (filter-alist object alist)
  (if (null? *filter-alist-functions*)
      alist
      ;else
      (begin
        (foreach *filter-alist-functions* (lambda (fncpair)
                                            (set! alist
                                                  (apply append (map (lambda (elt) ((car fncpair) object elt)) alist)))
                                            ))
        alist
        )
      )
  )

; a little utility for simple alist modifers
; usage:  (alist-filters 'add (make-alist-modifier-function 'keyname foo))
;         where foo is (lambda (val) body) --> newval
(define (make-alist-modifier-function key value-modifier-func)
  (lambda (object elt)
    (if (eq? (car elt) key)
        (list (cons (car elt) (value-modifier-func (cdr elt))))
        (list elt))))

;Another version which takes not a single key but a lambda on key-vals tuple:
(define (make-alist-modifier-function-general match-fn value-modifier-func)
  (lambda (object elt)
    (if (match-fn elt)
        (list (cons (car elt) (value-modifier-func (cdr elt))))
        (list elt))))
;EX:
;Replace any alist-val which is an object with its ID:
;(make-alist-modifier-function-general
;  (lambda (tuple) (any-vals-are-objects? (cdr tuple)))
;  (lambda (tuple) (replace-objects-w-IDs (cdr tuple))))
;-----

(define base-group%
  ;this level relays reposition/resize callbacks to group members
  (class dynaobject%
    (inherit-field _dynapad cptr)
    (init dynaptr)
    (super-instantiate (dynaptr (sch_makegroup (send dynaptr get-cptr) this)))

    (define/override (update-position dx dy)
      (super update-position dx dy)
      (foreach (sch_members cptr)
               (lambda (m) (send m update-position dx dy))))

    (define/public case-super-update-size
      (case-lambda
        ;       ((arg) (super update-size arg))
        ;       ((arg1 arg2) (super update-size arg1 arg2))
        ((arg1 arg2 arg3) (super update-size arg1 arg2 arg3))
        ((arg1 arg2 arg3 arg4) (super update-size arg1 arg2 arg3))
        ))

    (define/override (update-size . args)
      (send/apply this case-super-update-size args)
      (foreach (sch_members cptr)
               (lambda (m) (send/apply m update-size args))))
    ))

(define group%
  (class base-group%
    (inherit-field _dynapad cptr selected)
    (init dynaptr (initmembers #f))
    (inherit dynaclass)
    (override writeoptions fix delete)
    (public members member? add remove save divisible ungroup)
    (field (savemembers null))

    (define members
      (case-lambda
        (() (sch_members cptr))
        ((some . more)
         (let ((newmembers (if (list? some)
                               (append some more)
                               (cons some more))))
           (sch_members cptr (map (lambda (o) (send o get-cptr)) newmembers))
           (send this update-any-hilights)))))

    (define (member? o)
      (eq? this (send o getgroup)))

    (define save
      (case-lambda
        (() `(quote ,(map (lambda (o) (send o padid)) (members))))
        ((newmembers) (set! savemembers newmembers))))

    (define/override (export dir)
      (let ((subdir (export-container-generic-name dir "group")))
        (foreach (send this members) (lambda (o) (send o export subdir)))))

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
        (defer-send (members ,(export-objs (members))))
        ;(refer-when-ready 'members ,@(map obj->id (members)))
        ;(members (list ,@(map (lambda (o) (send o write)) (reverse (members)))))
        (divisible ,(divisible))))

    (define (delete)
      ;delete callbacks may remove members before they're deleted,
      ; so need to apply callbacks (normally triggered in (super-delete))
      ; BEFORE deleting members:
      (for-each (lambda (cb-fn-pair) ((car cb-fn-pair) this))
                (send this delete-callbacks))
      ;already done, so clear them
      (send this delete-callbacks null)
      ;finally, delete remaining members + this
      (for-each (lambda (o) (remove o) (send o delete)) (members))
      (super delete))

    (define (ungroup)
      (send this members '())
      (delete)
      #t)

    (define divisible
      (case-lambda
        (() (sch_divisible cptr))
        ((bool) (sch_divisible cptr bool))))

    (define/override (select . args)
      (if (null? args) (super select)
          (when (not selected)
            (set! selected (make-object select% _dynapad this (car args))))
          ))

    (super-instantiate (dynaptr)); (sch_makegroup (send _dynapad get-cptr) this)))
    (dynaclass 'group%)
    (send this dependents-fn (lambda (me) (send me members)))
    (when initmembers (members initmembers))))

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

(define dynapad%
  (class frame% ;object%
    (init (name ".dynapad"))
    (field (cptr #f) (_selectlist '()) (_event-state-object #f) (path #f)
           (_ztimer #f) (_stimer #f) (_zinterval 10)
           (_dynaclass #f) (_defaultpen "white") (_defaultpenwidth 1) (_defaultfill "none")
           (_defaultfont "Times") (_fill? #f)
           (_select_render_script #f) (_lastcentered #f)
           (_main-layer #f) (_status #f) (_hidden #f) (_writable #t)
           (_afterzoom-callbacks '())
           (_afterdrag-callbacks '()) (_beforedrag-callbacks '())
           ; [LIST->MLIST] was: (_varlist `((this ,this)) )
           (_varlist (mlist (mcons 'this this)))
           (_eventModeStack null) ;(list "Run"))
           (_drag_and_drop_object #f)
           (_reject-save-filters '())
           )
    (override
     center focus warp-pointer)
    (public
     moveto view getzoom setzoom zoom
     xinputdevices (mybind bind) bindtags modifier get-cptr find pick
     write get-path set!-path
     evs zinterval ztimer stimer
     add-selected del-selected selected
     objects order unfindable visible idle?
     cpanzoom unfocus getfocus padid dynaclass fontnames
     defaultpen defaultpenwidth defaultfill fill? defaultfont background
     select-render-script cursor
     bbox centerbbox lastcentered layers main-layer status hidden winid
     rendertime viewrendertime dissolvespeed refinedissolvespeed desiredframerate
     refinementdelay doublebuffer fastpan
     update writable? winfo
     afterzoom-callbacks
     afterdrag-callbacks do-afterdrag-callbacks init-drag do-duringdrag
     beforedrag-callbacks do-beforedrag-callbacks
     eventModeStack
     getvar setvar
     reject-save-filters
     xftenable
     debugevent
     )

    (define getvar
      (case-lambda
        ((varname) (cond ((massq varname _varlist) => (λ (pair) (mcar (mcdr pair)))) (else #f)))
        ((varname fail) (cond ((massq varname _varlist) => (λ (pair) (mcar (mcdr pair)))) (else fail)))))

    (define (setvar varname val)
      (let ((elmt (massq varname _varlist)))
        (if elmt
            (set-mcdr! elmt (mlist val))
            (set-mcdr! _varlist (mcons (mlist varname val) (mcdr _varlist))))))

    (define moveto
      (case-lambda
        (() (printf "should be: (send _dynapad moveto (x y zoom) [animtime [twostep?]])~%"))
        ((xyzoom) (sch_moveto cptr xyzoom 0 #f))
        ((xyzoom animtime) (sch_moveto cptr xyzoom animtime #f))
        ((xyzoom animtime twostep) (sch_moveto cptr xyzoom animtime twostep))))

    (define view (case-lambda
                   (() (sch_getview cptr))
                   ((x y z) (moveto (list x y z) 200 #t))))

    (define (getzoom) (caddr (view)))

    (define setzoom
      (case-lambda
        ((z) (moveto (list (car (view)) (cadr (view)) z)))
        ((z animtime) (moveto (list (car (view)) (cadr (view)) z) animtime))))

    (define zoom
      (case-lambda
        (() (printf "should be: (send _dynapad zoom zfac [animtime [x y]])~%"))
        ((zfac) (zoom zfac 0))
        ((zfac animtime)
         (let*
             ((v (view))
              (x (car v))
              (y (cadr v)))
           (zoom zfac animtime x y)))
        ((zfac animtime x y)
         (sch_zoom cptr zfac animtime x y)
         (exec-any-callbacks _afterzoom-callbacks this x y zfac))
        ))
    (define afterzoom-callbacks (callback-accessor-functions _afterzoom-callbacks))

    (define (do-afterdrag-callbacks evnt obj-list)
      (when _drag_and_drop_object
        (set! obj-list
              (send _drag_and_drop_object respond-to-drop evnt obj-list)))
      (when _afterdrag-callbacks
        (exec-any-callbacks _afterdrag-callbacks this evnt obj-list)))

    (define afterdrag-callbacks (callback-accessor-functions _afterdrag-callbacks))

    (define (do-beforedrag-callbacks evnt obj-list)
      ;(when _drag_and_drop_object
      ;  (set! obj-list
      ;    (send _drag_and_drop_object respond-to-drop evnt obj-list)))
      (when _beforedrag-callbacks
        (exec-any-callbacks _beforedrag-callbacks this evnt obj-list)))

    (define beforedrag-callbacks (callback-accessor-functions _beforedrag-callbacks))

    (define/public drag_and_drop_object (get/set _drag_and_drop_object))

    (define (init-drag evnt objlist)
      (when _drag_and_drop_object
        (send _drag_and_drop_object init-drag evnt objlist)))

    (define (do-duringdrag evnt)
      (when _drag_and_drop_object
        (send _drag_and_drop_object respond-to-drag evnt)))

    (define eventModeStack
      (case-lambda
        (() _eventModeStack)
        ((newstack) (set! _eventModeStack newstack))))

    (define (xinputdevices)
      (sch_xinputdevices cptr))

    (define (mybind . args)
      (apply bind this args))

    (define bindtags
      (case-lambda
        (() (sch_bindtags cptr))
        ((order) (sch_bindtags cptr order))))

    (define modifier
      (case-lambda
        ((op) (sch_modifier cptr op))
        ((op modifier) (sch_modifier cptr op modifier))))

    (define (get-cptr)
      cptr)

    (define (find . args) ; usually (find op xyxy)
      (cond ((null? args) #f) ; print useful hint message
            ((eq? (car args) 'withtype) (filter (lambda (o) (is-a? o (cadr args))) (objects)))
            ((eq? (car args) 'withtag) (filter (lambda (o) (send o hastag (cadr args))) (objects)))
            (else (apply sch_find (cons cptr args)))))

    (define pick
      (case-lambda
        ((xy) (send/apply this pick xy))
        ((x y) (sch_pick cptr x y))))

    (define (write) #f)

    (define reject-save-filters (get/set _reject-save-filters))

    (define evs (get/set _event-state-object))

    (define zinterval (get/set _zinterval))
    (define ztimer (get/set _ztimer))
    (define stimer (get/set _stimer))

    (define (get-path)
      path)

    (define (set!-path p)
      (set! path p))

    (define objects
      (case-lambda
        (() (objects #f))
        ((groupmembers?) (sch_objectlist cptr groupmembers?))))

    (define (order objects)
      (sch_order cptr (map (lambda (o) (send o get-cptr)) objects)))

    (define unfindable
      (case-lambda
        (() (unfindable #f))
        ((groupmembers?) (sch_unfindable cptr groupmembers?))))

    (define (visible)
      (sch_visible cptr))

    (define (add-selected o)
      (if (list? o)
          (set! _selectlist (append _selectlist o))
          (set! _selectlist (append _selectlist (list o)))))

    (define (del-selected o)
      (if (list? o)
          (set! _selectlist (remq* o _selectlist))
          (set! _selectlist (remq o _selectlist))))

    (define selected
      (case-lambda
        (() _selectlist)
        ((newset)
         (for-each (lambda (o) (send o unselect)) _selectlist)
         (for-each (lambda (o) (send o select)) newset))))
    ;This version may or may not be faster:
    ;     (let* ((now _selectlist)
    ;        (outs (list-diffq now newset))
    ;        (ins  (list-diffq newset now)))
    ;       (for-each (lambda (o) (send o unselect)) outs)
    ;       (for-each (lambda (o) (send o select)) ins))

    (define cpanzoom
      (case-lambda
        (() (sch_cpanzoom cptr))
        ((flag) (sch_cpanzoom cptr flag))))

    (define (focus)
      (super focus)
      (sch_focus cptr #t))

    (define (unfocus)
      (sch_focus cptr #f))

    (define (getfocus)
      (sch_getfocus cptr))

    (define (padid)
      (sch_padid cptr))

    (define dynaclass
      (case-lambda
        (() _dynaclass)
        ((newclass) (set! _dynaclass newclass))))

    (define (fontnames)
      (let loop
          ((l (quicksort (cons "Times" (sch_fontnames cptr)) string<?)))
        (cond
          ((null? l) null)
          ((null? (cdr l)) null)
          ((string=? (car l) (cadr l)) (cons (car l) (loop (cddr l))))
          (else (cons (car l) (loop (cdr l)))))))

    (define xftenable
      (case-lambda
        (() (sch_xftenable cptr))
        ((val) (sch_xftenable cptr val))))

    (define debugevent
      (case-lambda
        (() (sch_debugevent cptr))
        ((val) (sch_debugevent cptr val))))

    (define fill? (case-lambda (() _fill?) ((bool) (set! _fill? bool))))

    (define defaultfill
      (case-lambda
        (() _defaultfill)
        ((newdefaultfill) (set! _defaultfill newdefaultfill))))

    (define defaultpen
      (case-lambda
        (() _defaultpen)
        ((newdefaultpen) (set! _defaultpen newdefaultpen))))

    (define defaultpenwidth
      (case-lambda
        (()
         (let ((zoom (caddr (view))))
           (/ _defaultpenwidth zoom)))
        ((newdefaultpenwidth) (set! _defaultpenwidth newdefaultpenwidth))))

    (define defaultfont
      (case-lambda
        (() _defaultfont)
        ((newdefaultfont) (set! _defaultfont newdefaultfont))))

    (define background
      (case-lambda
        (() (sch_background cptr))
        ((newbackground) (sch_background cptr newbackground))))

    (define (update) (sch_update cptr))

    (define (cursor id)
      (if (send this getvar 'delay-cursor?)
          (sch_cursor cptr 3)
          (sch_cursor cptr id)))

    (define select-render-script
      (case-lambda
        (() _select_render_script)
        ((new_script) (set! _select_render_script new_script))))

    (define (bbox)
      (sch_bbox cptr))

    (define winfo
      (case-lambda
        (() (sch_winfo cptr))
        ((geometry_string) (sch_winfo cptr geometry_string))))

    (define center
      (case-lambda
        (() (printf "should be: (send _dynapad center object [animtime [twostep? [xcenter ycenter [viewscale]]]])~%"))
        ((l) (center l 0))
        ((l anim) (center l anim #f))
        ((l anim twostep?) (center l anim twostep? 0.5 0.5))
        ((l anim twostep? x y) (center l anim twostep? x y 0.85))
        ((l anim twostep? x y zoom)
         (when (eq? l 'all) (set! l (send this objects)))
         (set! _lastcentered (if (list? l) #f l))
         (let*
             ((l (if (list? l) l (list l)))
              (cptrlist (map (lambda (o)(send o get-cptr)) l)))
           (sch_center cptr cptrlist anim twostep? x y zoom)))))

    (define centerbbox
      (case-lambda
        ((l) (centerbbox l 0))
        ((l anim) (centerbbox l anim #f))
        ((l anim twostep?) (centerbbox l anim twostep? 0.5 0.5))
        ((l anim twostep? x y) (centerbbox l anim twostep? x y 0.85))
        ((l anim twostep? x y zoom)
         (sch_centerbbox cptr l anim twostep? x y zoom))))

    (define lastcentered
      (case-lambda
        (() _lastcentered)
        ((value) (set! _lastcentered value))))

    (define (layers)
      (sch_layers cptr))

    (define (main-layer)
      (when (not _main-layer) (set! _main-layer (make-object layer% this "main")))
      _main-layer)

    (define (status)
      (when (not _status) (set! _status (make-object layer% this "status")))
      _status)

    (define (hidden)
      (when (not _hidden)
        (set! _hidden (make-object layer% this "hidden"))
        (send _hidden visible #f))
      _hidden)

    (define (winid)
      (sch_winid cptr))

    (define writable?
      (case-lambda
        (() _writable)
        ((bool) (set! _writable bool))))

    (define (idle?)
      (sch_idle cptr))

    (define (rendertime)
      (sch_rendertime cptr))

    (define (viewrendertime)
      (sch_viewrendertime cptr))

    (define dissolvespeed
      (case-lambda
        (() (sch_dissolvespeed cptr))
        ((x) (sch_dissolvespeed cptr x))))

    (define refinedissolvespeed
      (case-lambda
        (() (sch_refinedissolvespeed cptr))
        ((x) (sch_refinedissolvespeed cptr x))))

    (define desiredframerate
      (case-lambda
        (() (sch_desiredframerate cptr))
        ((x) (sch_desiredframerate cptr x))))

    (define refinementdelay
      (case-lambda
        (() (sch_refinementdelay cptr))
        ((x) (sch_refinementdelay cptr x))))

    (define doublebuffer
      (case-lambda
        (() (sch_doublebuffer cptr))
        ((x) (sch_doublebuffer cptr x))))

    (define fastpan
      (case-lambda
        (() (sch_fastpan cptr))
        ((x) (sch_fastpan cptr x))))

    (define (warp-pointer dx dy)
      ;dx, dy are changes in screen coords, not dynapad coords
      (unless (and (exact? dx) (integer? dx)) (set! dx (inexact->exact (round dx))))
      (unless (and (exact? dy) (integer? dy)) (set! dy (inexact->exact (round dy))))
      (sch_warp_pointer cptr dx dy))

    (define/public (makepup pum x y)
      (send this popup-menu pum (inexact->exact x) (inexact->exact y)))

    (super-instantiate ("Dynapad-frame"))
    (set! cptr (sch_makedynapad name this))
    (background "black")
    (select-render-script ;default select-box appearance:
     (lambda (cptr obj) (sch_pen cptr "red") (sch_penwidth cptr 0)))
    (dynaclass 'dynapad%)
    (main-layer)
    (status)
    (setvar 'select-layer (make-object layer% this "select"))
    (set! _selectlist '())
    ))

(define baseline%
  (class dynaobject%
    (init _dynapad makeproc)
    (inherit-field cptr selected)
    (inherit dynaclass position)
    (override writeoptions)
    (field (_coords-callbacks '())  (_aftercoords-callbacks '()))
    (public coords coords-callbacks aftercoords-callbacks
            pen penwidth abslinestyle? savepenwidth)

    (define (coords . args)
      (if (null? args)
          (sch_coords cptr)
          (let ((points (if (null? (cdr args)) (car args) args)))
            (exec-any-callbacks _coords-callbacks this points)
            (sch_coords cptr points)
            (exec-any-callbacks _aftercoords-callbacks this points)
            (send this update-any-hilights))))
    (define coords-callbacks (callback-accessor-functions _coords-callbacks))
    (define aftercoords-callbacks (callback-accessor-functions _aftercoords-callbacks))

    (define pen
      (case-lambda
        (() (sch_pen cptr))
        ((color) (sch_pen cptr color))))

    (define savepenwidth
      (case-lambda
        (()
         (let*
             ((pw (penwidth))
              (abs? (abslinestyle?))
              (s (caddr (position)))
              (savepw (if abs? pw (* pw s))))
           (list savepw abs?)))))

    (define (abslinestyle?)
      (sch_abslinestyle cptr))

    (define penwidth
      (case-lambda
        (() (sch_penwidth cptr))
        ((width) (sch_penwidth cptr width))
        ((width abs?) (sch_penwidth cptr width abs?))))

    (define (writeoptions)
      (filter (lambda (opt) opt)
              `(,@(super writeoptions)
                (coords ,@(coords))
                (pen ,(pen))
                ,(if
                  (and
                   (= (penwidth) 1.0)
                   (not (abslinestyle?)))
                  #f
                  `(penwidth ,@(savepenwidth)))
                )))

    (super-instantiate (_dynapad (makeproc (send _dynapad get-cptr) this)))
    (when (send _dynapad defaultpen) (pen (send _dynapad defaultpen)))
    (when (send _dynapad defaultpenwidth) (penwidth (send _dynapad defaultpenwidth)))))

(define line%
  (class baseline%
    (init _dynapad (initcoords #f))
    (inherit dynaclass coords)
    (super-instantiate (_dynapad sch_makeline))
    (dynaclass 'line%)
    (when initcoords (coords initcoords))))

; two dummy classes, just to clean up Draw event handling
(define freehand% (class line%
                    (super-instantiate())
                    (send this dynaclass 'freehand%)))
(define polyline% (class line%
                    (super-instantiate())
                    (send this dynaclass 'polyline%)))

(define baserect%
  (class baseline%
    (init _dynapad makeproc)
    (inherit dynaclass)
    (inherit-field cptr)
    (override writeoptions)
    (public fill)

    (define fill
      (case-lambda
        (() (sch_fill cptr))
        ((color) (sch_fill cptr color))))

    (define (writeoptions)
      `(,@(super writeoptions)
        (fill ,(fill))))

    (super-instantiate (_dynapad makeproc))
    (when (send _dynapad defaultfill) (fill (send _dynapad defaultfill)))))

(define rect%
  (class baserect%
    (init _dynapad (initcoords #f))
    (inherit dynaclass coords)
    (super-instantiate (_dynapad sch_makerect))
    (dynaclass 'rect%)
    (when initcoords (coords initcoords))))

(define oval%
  (class baserect%
    (init _dynapad (initcoords #f))
    (inherit dynaclass coords)
    (super-instantiate (_dynapad sch_makeoval))
    (dynaclass 'oval%)
    (when initcoords (coords initcoords))))

(define polygon%
  (class baserect%
    (init _dynapad (initcoords #f))
    (inherit dynaclass coords)
    (super-instantiate (_dynapad sch_makepolygon))
    (dynaclass 'polygon%)
    (when initcoords (coords initcoords))))

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
                (hirespath ,(hirespath))
                ,(if (not _use_bbox) #f `(bbox ',(bbox)))
                )))
    (define/override (writeinits)
      (list (hirespath)))

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

(define basetext%
  (class dynaobject%
    (init _dynapad (inittext #f) (initposition #f) (initfont #f) (initanchor #f))
    (inherit anchor delete position dynaclass)
    (inherit-field cptr selected)
    (override writeoptions)
    (public text insert forward backward next previous
            setpoint delete-backward delete-forward font pen)

    (define text
      (case-lambda
        (() (sch_gettext cptr))
        ((newtext) (sch_settext cptr newtext))))

    (define/override (export dir)
      (export-stub dir (text)))

    (define (writeoptions)
      `(,@(super writeoptions)
        (text ,(text))
        (font ,(font))
        (pen ,(pen))))

    (define insert
      (case-lambda
        ((str)
         (sch_inserttext cptr "point" str)
         (send this update-any-hilights))
        ((index str)
         (sch_inserttext cptr index str)
         (send this update-any-hilights))))

    (define (setpoint pt)
      (when (number? pt) (set! pt (number->string pt)))
      (sch_marktext cptr "set" "point" pt)
      (send this update-any-hilights))

    (define (point-end)
      (sch_marktext cptr "set" "point" "end")
      (send this update-any-hilights))

    (define (forward)
      (sch_marktext cptr "set" "point" "point + 1 char")
      (send this update-any-hilights))

    (define (backward)
      (sch_marktext cptr "set" "point" "point - 1 char")
      (send this update-any-hilights))

    (define (next)
      (sch_marktext cptr "set" "point" "point + 1 line")
      (send this update-any-hilights))

    (define (previous)
      (sch_marktext cptr "set" "point" "point - 1 line")
      (send this update-any-hilights))

    (define (delete-backward)
      (sch_deletetext cptr "point - 1 char")
      (send this update-any-hilights))

    (define (delete-forward)
      (sch_deletetext cptr "point")
      (send this update-any-hilights))

    (define font
      (case-lambda
        (() (sch_font cptr))
        ((newfont)
         (sch_font cptr newfont)
         (send this update-any-hilights))))

    (define pen
      (case-lambda
        (() (sch_pen cptr))
        ((color) (sch_pen cptr color))))

    (super-instantiate (_dynapad (sch_maketext (send _dynapad get-cptr) this)))
    (when (send _dynapad defaultfont) (font (send _dynapad defaultfont)))
    (when (send _dynapad defaultpen) (pen (send _dynapad defaultpen)))
    (dynaclass 'basetext%)
    (anchor "nw")
    (when initposition (position initposition))
    (when initfont (font initfont))
    (when initanchor (anchor initanchor))
    (when inittext (text inittext))))

(define layer%
  (class object%
    (init _dynapad initname)
    (field (cptr #f) (_dynaclass #f) (savemembers null) (_writable #t))
    (public get-cptr delete name members dynaclass
            lower raise save write writeoptions visible writable?)

    (define (get-cptr)
      cptr)

    (define (delete)
      (for-each (lambda (o)(send o delete)) (members))
      (sch_deletelayer cptr))

    (define (name)
      (sch_layername cptr))

    (define (members)
      (sch_layermembers cptr))

    (define visible
      (case-lambda
        (() (sch_visiblelayer cptr))
        ((bool) (sch_visiblelayer cptr bool))))

    (define dynaclass
      (case-lambda
        (() _dynaclass)
        ((newclass) (set! _dynaclass newclass))))

    (define (fix alist)
      (for-each
       (lambda (oldid)
         (let ((o (assq oldid alist)))
           (send o layer this)))
       savemembers))

    (define save
      (case-lambda
        (() (map (lambda (o) (send o padid)) (members)))
        ((newmembers) (set! savemembers newmembers))))

    (define (writeoptions)
      `((save ',(save))))

    (define (write)
      `(let ((obj (make-object ,(dynaclass) dynapad ,(name))))
         (send* obj
           ,@(writeoptions))
         obj))

    (define writable?
      (case-lambda
        (() _writable)
        ((bool) (set! _writable bool))))

    (define raise
      (case-lambda
        (() (sch_raiselayer cptr))
        ((abovethis)
         (if (or (equal? abovethis "one") (equal? abovethis 'one))
             (sch_raiselayer cptr 1)
             (sch_raiselayer cptr (send abovethis get-cptr))))))

    (define lower
      (case-lambda
        (() (sch_lowerlayer cptr))
        ((belowthis)
         (if (or (equal? belowthis "one") (equal? belowthis 'one))
             (sch_lowerlayer cptr 1)
             (sch_lowerlayer cptr (send belowthis get-cptr))))))

    (super-instantiate ())
    (dynaclass 'layer%)
    (set! cptr (sch_makelayer (send _dynapad get-cptr) this initname))
    ; This is hideous: because of a long-time bug (empty layers drift upward)
    ;  use a permanent, hidden object as a "paperweight" on each layer
    ; to ensure that it is never empty.
    (field (_ballast (ic (make-object group% _dynapad)
                         (findable #f)
                         (layer this))))
    ))

(define bind
  (case-lambda
    ((obj) (sch_bind (send obj get-cptr)))
    ((obj event) (sch_bind (send obj get-cptr) event))
    ((obj event callback) (bind obj event callback #f))
    ((obj event callback save?)
     (let ((bindings (bind obj event)))
       (cond
         ; don't forget to update *writelist* when removing bindings
         ((not callback) (set! bindings null)) ; grandfather #f to mean null
         ((list? callback) (set! bindings callback))
         (else
          (set! bindings (list callback))
          (when save? (make-object bind/write% obj event callback))))
       (sch_bind (send obj get-cptr) event bindings)))))

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

(define *writelist* null)

(define bind/write%
  (class object%
    (init-field obj event callback)

    (define/public (write)
      (cond
        ((annotated-proc? callback)
         (list 'make-object 'bind/write% (send obj padid) event
               (apply list 'lambda (procedure-args callback)
                      (procedure-body callback))))
        (else
         (list 'make-object 'bind/write% (send obj padid) event
               (object-name callback)))))

    (define/public (fix alist)
      (set! obj (cadr (assq obj alist)))
      (bind obj event callback))

    (define/public (delete)
      (set! *writelist* (remq *writelist* this)))

    (super-instantiate ())
    (set! *writelist* (cons this *writelist*))))

(define (link-hook o) #t)

(define (center-hook o) #t)
