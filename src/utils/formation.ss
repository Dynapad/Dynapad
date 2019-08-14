(dynaload "alist.ss")
(dynaload "filenames.ss")

(define-macro (name-part field-name method-name)
    `(begin
       (field (,field-name #f))
       (define/public ,method-name
         (case-lambda
          (() ,field-name)
          ((new) (when ,field-name (send this remove-part ,field-name))
                 (when new (send this add-part new))
                 (set! ,field-name new))))
       (send this part-names
             (append (send this part-names)
                     (list ',method-name))) ))

(define (part? obj)
  (assq 'part (send obj alist)))

(define base-formation%
  (class base-group%
;    (inherit-field _dynapad)
    (init dynaptr);  (set! _dynapad dynaptr)
    (inherit dynaclass)
    (super-instantiate (dynaptr)) ; (_dynapad (sch_makegroup (send _dynapad get-cptr) this)))
    (sch_divisible cptr #t)
    (dynaclass 'base-formation%)
    (inherit-field cptr)
    (field (_part-names null))
    (field (_highlight_object #f))
    (override writeoptions delete)

    (define/public (highlight bool) #f) ; override this

    (field (_within_fnc #f))
    (define/public set-within-fnc (get/set _within_fnc))
    (define/public (xy-within? x y)
      (if _within_fnc
        (_within_fnc x y)
        (bbenclosed x y (send this bbox))))
    
    (define/public part-names (get/set _part-names))
    (define/public (named-parts)
      (map (lambda (meth) (eval `(send ,this ,meth))) _part-names))

    (define (mark-part obj)
      (get-else-push-onto-malist! assq (list 'part) obj alist))
    (define (unmark-part obj)
      (get-and-rem-from-malist! assq remq 'part obj alist))

    (define/public (add-part obj)
      (mark-part obj)
      (sch_addmember cptr (send obj get-cptr)))
    (define/public (remove-part obj)
      (unmark-part obj)
      (when (not (send obj deleted?))
          (sch_removemember cptr (send obj get-cptr))))    
    (define delete
      (case-lambda
       (()
        ;delete callbacks may remove members before they're deleted,
        ; so need to apply callbacks (normally triggered in (super-delete))
        ; BEFORE deleting members:
        (for-each (lambda (cb-fn-pair) ((car cb-fn-pair) this))
                  (send this delete-callbacks))
        ;already done, so clear them
        (send this delete-callbacks null)
        ;finally, delete remaining members + this
        (for-each (lambda (o) (send o delete)) (sch_members cptr))
        (super delete))
       ((partname)
          (let ((part (send this partname)))
            (send this partname #f)
            (send part delete)))))

    (define/public (disband)
      (sch_members cptr null)
      (super-delete)
      #t)

    (define (writeoptions)
      `(,@(super writeoptions)
        ,@(map (lambda (o) (send this write-part o)) (send this part-names))))

    (define/public (write-part name)
      (let ((obj (eval `(send ,this ,name))))
        `(,name ,(and obj (send obj write)))))

    (define/public (member? o)
      (eq? this (send o getgroup)))

    (define/public divisible
      (case-lambda
        (() (sch_divisible cptr))
        ((bool) (sch_divisible cptr bool))))

    (define/public separable
      (case-lambda
       ((memb) (and (send this member? memb)
                    (send memb takegroupevents)))
       ((memb bool) (when (send this member? memb)
                        (send memb takegroupevents bool)))))

    (field (_rigid-parts null)) ;subset of parts which ignore reshape: bbox, width, height, etc
    (define/public rigid
      (case-lambda
       ((part) (memq part _rigid-parts))
       ((part bool)
          (let ((already (rigid part)))
            (if bool
                (unless already
                        (push! part _rigid-parts))
                (when already
                      (set! _rigid-parts (remq part _rigid-parts))))))))

    ; THIS MAY BE TOO BOLD...
    ;redefs of reshaping methods:
    (define/override bbox
      (case-lambda
       (() (super bbox))
       ((bb)
        (let ((nonrigids (list-diff (named-parts) _rigid-parts memq)))
          (foreach nonrigids
                   (lambda (part) (and part (send part bbox bb))))))))
    (define/override width
      (case-lambda
       (() (super width))
       ((w)
        (let ((nonrigids (list-diff (named-parts) _rigid-parts memq)))
          (foreach nonrigids
                   (lambda (part) (and part (send part width w))))))))
    (define/override height
      (case-lambda
       (() (super height))
       ((h)
        (let ((nonrigids (list-diff (named-parts) _rigid-parts memq)))
          (foreach nonrigids
                   (lambda (part) (and part (send part height h))))))))
))

(define container-form%
  (class base-formation%
    (init dynaptr)
    (inherit dynaclass)
    (super-instantiate (dynaptr))
    (dynaclass 'container-form%)
    ;(inherit-field cptr)

    (name-part _container container)
    (container (make-object group% dynaptr))

    ; questionable: always exclude container from bbox of formation:
    ;(send (container) boundable #f)
    ; Used alternative: exclude only when container is empty

    (define/public contents
      (case-lambda
       (() (send _container members))
       ((lst) 
        (let ((new-cnts (list-diff lst (send _container members) memq)))
          (for-each (lambda (o) (send o takegroupevents #t)) new-cnts)
          (send _container members lst)
          (when (null? lst)
                (send _container boundable #f))
          ))))

    (define/override (writeoptions)
      `(,@(super writeoptions)
        (contents (list ,@(map (lambda (o) (send o write)) (send this contents))))))

    (define/public (add obj)
      (send _container add obj)
      (send obj takegroupevents #t)
      (send _container boundable #t) ;no longer empty
      obj)

    (define/public (remove obj)
      (send _container remove obj)
      (when (null? (send _container members))
            (send _container boundable #f))
      )

    (define/override (export dir)
      (let ((subdir (export-container-generic-name dir "clump")))
        (foreach (send this contents) (lambda (o) (send o export subdir)))))     

    (send this rigid (container) #t) ;don't resize container to avoid distorting contents
 ))

(define (get-container obj)
;returns first container-form enclosing obj (excluding obj itself) or #f if none
  (let ((wrapper (send obj getgroup)))
    (cond ((not wrapper) #f)
          ((is-a? wrapper container-form%) wrapper)
          (else (get-container wrapper)))))

(define frame-container%
  (class container-form%
    (init dynaptr)
    (inherit dynaclass)
    (init (frame-arg #f))
    (super-instantiate (dynaptr))
    (dynaclass 'frame-container%)

   ;(name-part _frame frame)  ;unwrapped so that frame is lowered by default
    (field (_frame #f))
    (send this part-names '(frame))
    (define/public frame (case-lambda
      (() _frame)
      ((new-frame)
        (when _frame (send this remove-part _frame))
        (when new-frame (send this add-part new-frame))
        (send new-frame lower)  ; <-- lower frame
        (set! _frame new-frame))))
    (define/public (pane) _frame) ; for back-compatibility

    (when frame-arg
      (frame frame-arg)
      (send (send this container) bbox (send frame-arg bbox)))
))

(define titled-frame-container%
  (class frame-container%
    (init dynaptr)
    (inherit dynaclass)
    (init (frame-arg #f))
    (super-instantiate (dynaptr frame-arg))
    (dynaclass 'titled-frame-container%)

    (name-part _titlebox titlebox)

    (define/public (refresh-titlebox)
      (when _titlebox
            (send _titlebox xy (bbs (send (send this frame) bbox)))))

    (define (make-titlebox)
      (let* ((frm (send this frame))
             (tb (ic (make-object text% (send frm dynapad))
                     (anchor "n"))))
        (send tb delete-callbacks 'add (lambda (tb) (say "clearing title")
                                               (send this titlebox #f)))
        (titlebox tb)
        (send this rigid tb #t)
        (slowgrow tb .3 2 .5)
        (refresh-titlebox)
        (send frm aftercoords-callbacks 'add
              (lambda (obj crds) (send this refresh-titlebox)))
        (send frm afterwidth-callbacks 'add
              (lambda (obj w) (send this refresh-titlebox)))
        (send frm afterheight-callbacks 'add
              (lambda (obj h) (send this refresh-titlebox)))
        ))
        

    (define/public title
      (case-lambda
       (() (and (titlebox) (send (titlebox) text)))
       ((txt)
        (unless (send this frame)
                (error "Needs frame before title"))
        (if txt
            (begin
              (when (not (titlebox)) (make-titlebox))
              (send (titlebox) text txt))
            (when (titlebox)
                  (send (titlebox) delete)
                  (titlebox #f)))
        (send this update-any-hilights)
        )))

    (define/override (export dir)
      (let* ((name (send this title))
             (subdir 
              (if name
                  (export-container-custom-name dir name)
                  (export-container-generic-name dir "clump"))))
        (foreach (send this contents) (lambda (o) (send o export subdir)))))

    ;add menu item for title
    (add-custom-popup-items
     (lambda (me menu)
       (unless (send me titlebox)
               (add-menu-item menu "Add title..."
                              (lambda ()
                                (send me title "")
                                (let* ((my-tb (send me titlebox))
                                       (xy (bbcenter (send my-tb bbox))))
                                  (apply edit-text-at-xy (send me dynapad)
                                         my-tb
                                         xy)))
                              #t)))
     this)

))

(define resizable-frame-container%
  (class titled-frame-container%
    (init dynaptr)
    (init (frame-arg #f))
    (inherit dynaclass)
    (super-instantiate (dynaptr frame-arg))
    (dynaclass 'resizable-frame-container%)

    (name-part _border border)
    (when (send this frame)
          (make-resize-border this))

    (define/override (writeoptions)
      `(,@(super writeoptions)
        (make-resize-border this)))
))

;
(define (mean-max-dim-of-objs objs)
  (if (null? objs)
      #f
      (apply mean
             (map (lambda (o) (max (send o width) (send o height)))
                  objs))))

(define fusing-frame-container%
  (class titled-frame-container%
;  (class frame-container%
    (init dynaptr)
    (init (frame-arg #f))
    (inherit dynaclass)
    (super-instantiate (dynaptr frame-arg))
    (dynaclass 'fusing-frame-container%)

    (name-part _cover cover)
    (cover (ic (clone-object (send this frame))
               (transparency .4) ;cover should be opaque enough to be seen
               (faderange .02)))
    (send (cover) raise (send this container))

    (define/public (fusing-size pixels)
      (send (send this cover) maxsize pixels))
      ;(send (send this frame) minsize pixels))
    ; might want to swap out frame (as above)
    ; but then low transparency of cover won't subdue foreign object behind frame

    ;auto-adjustment of fusing-size:
    ; fusing-size adjusts to fuse when item-size is item-fuse-dim
    (field (_item-fuse-dim 25)) ;min pixel-dim (max w,h) of one item
                                ; to start fusing
    (define/public item-fuse-dim (get/set _item-fuse-dim))

    (field (_use-item-size ;may be number or lambda ()
            ; default lambda:
            (lambda () (mean-max-dim-of-objs (send this contents)))))
    (define/public use-item-size (get/set _use-item-size))

    (define/public refresh-fusing-size
      (case-lambda
       (() (refresh-fusing-size (if (procedure? _use-item-size)
                                    (_use-item-size)
                                    _use-item-size)))
       ((itemsize)
          (let* ((cover (cover))
                 (mysize (max (send cover width) (send cover height)))
                 (fusesize (/ (* mysize (item-fuse-dim))
                              (max (or itemsize 0) 1))))
            (fusing-size fusesize)))))

    (let ((frame (send this frame))
          (cover (send this cover)))
      (send frame aftercoords-callbacks 'add
            (lambda (o crds) 
              (send cover coords crds)
              (send this refresh-fusing-size)))
      (send frame afterwidth-callbacks 'add
            (lambda (o w)
              ;(send cover width w)
              (send this refresh-fusing-size)) 'refresh-fusing-size)
      (send frame afterheight-callbacks 'add
            (lambda (o h)
              ;(send cover height h)
              (send this refresh-fusing-size)) 'refresh-fusing-size)
      )

;    (define/override (export dir)
;      (let ((subdir (export-container-generic-name dir "pile")))
;        (foreach (send this contents) (lambda (o) (send o export subdir)))))     
))

;Usage-example:
; (def ffc (make-object fusing-frame-container% ...))
; (send ffc refresh-fusing-size) ==> computes object sizes, adjusts accordingly
; OR cache object sizes:
; (send ffc use-item-size 20)
; (send ffc refresh-fusing-size) ==> adjusts for contents of size 20
; OR provide size:
; (send ffc refresh-fusing-size 20) ==> adjusts for contents of size 20
; OR adjust manually:
; (send ffc fusing-size 500) ==> fuses at 500 pixels, regardless of contents

(define dissolving-fusing-frame-container%
  (class fusing-frame-container%
    (init dynaptr)
    (init (frame-arg #f))
    (inherit dynaclass)
    (super-instantiate (dynaptr frame-arg))
    (dynaclass 'dissolving-fusing-frame-container%)

    (name-part _shadow shadow)
    (let ((frame (send this frame)))
      (shadow (ic (clone-object frame)
                  (fill (sendf this dynapad background))
                  (minsize .98 #t) ;trade off with frame, but start earlier
                  (faderange 0) ;snap in instantly; hidden behind frame
                  (bind "<Run-ButtonPress-1>" Start-Shift-Select-Event)))
      (send (shadow) lower) ;must happen after added to formation above
      (ic frame
          (maxsize 1 #t) ;make frame dissolve when fills 100% view
          (faderange .02)) ;begins to fade just as shadow snaps in
      )
))

(define resizable-image%
  (class base-formation%
    (init dynaptr)
    (init image-arg)
    (inherit dynaclass)
    (super-instantiate (dynaptr))
    (dynaclass 'resizable-image%)
    
    (name-part _image image)
    (image image-arg)
    (name-part _border border)

    (define/public (resize-image bb)
      (send (send this image) bbox bb))
    (define/public (make-border)
      (when (not (border))
            (border (ic (make-object rect% (send this dynapad))
                      (penwidth 2)
                      (fill "none"))))
      (send (border) anchor (send (image) anchor))
      (send (border) coords (send (image) bbox))

      (resizer-bindings (send this dynapad)
                            _border
                            (lambda (brdr bbox) (send this resize-image bbox))
                            ; release callback: no effect unless position-callbacks
                            (lambda (brdr) (send this position
                                                 (send this position)))
                            
                            ;#f ;no release callback
                            (is-a? (image) image%))
      (send _border takegroupevents #t)
      )

    (when (image)
          (make-border))
    (define/public (hirespath)
      (send (image) hirespath))

    (define/override (writeoptions)
      `(,@(super writeoptions)
        (make-border)))
))

(define (make-resize-border form)
  (let ((frame (send form frame))
        (brdr  (send form border)))
    (when (not brdr)
          (set! brdr (ic (make-object rect% (send form dynapad) (send frame coords))
                         (anchor (send frame anchor))
                         (fill "none")
                         (penwidth 10)))
          (send form border brdr) ; add to formation
          (send brdr raise) ;must follow addition to form
          (undoable-resizer-bindings (send form dynapad)
                                     brdr
                                     (lambda (junk bb)
                                       (send form bbox bb)
                                       (send form update-any-hilights))
                                     #f ;no release callback
                                     (is-a? frame image%)
                                     (lambda ()
                                       `(send ,(obj->IDexpr form) bbox
                                              (list ,@(send form bbox))))
                                     150 ;min. size
                                     )
          ;(send brdr takegroupevents #t)
          )
    ))

(define resizable-dissolving-fusing-frame-container%
  (class dissolving-fusing-frame-container%
    (init dynaptr)
    (init (frame-arg #f))
    (inherit dynaclass)
    (super-instantiate (dynaptr frame-arg))
    (dynaclass 'resizable-dissolving-fusing-frame-container%)

    (name-part _border border)
    (when (send this frame)
          (make-resize-border this))

    (define/override (writeoptions)
      `(,@(super writeoptions)
        (make-resize-border this)))
))

; not used currently:
#|
(define labeled-resizable-frame-container%
  (class resizable-frame-container%
    (init dynaptr)
    (init (frame-arg #f))
    (inherit dynaclass)
    (super-instantiate (dynaptr frame-arg))
    (dynaclass 'labeled-resizable-frame-container%)

    (name-part _labels labels)

    (define/override (resize-frame bbox)
      (super resize-frame bbox)
      (when (titlebox)
            (send (titlebox) xy (bbs (send (send this frame) bbox)))))
))

(define titled-resizable-frame-container%
  (class resizable-frame-container%
    (init dynaptr)
    (init (frame-arg #f))
    (inherit dynaclass)
    (super-instantiate (dynaptr frame-arg))
    (dynaclass 'titled-resizable-frame-container%)

    (name-part _titlebox titlebox)
    
    (define (make-titlebox)
      (let ((frm (send this frame)))
        (ic (make-object text% (send frm dynapad))
            (anchor "n")
            (xy (bbs (send frm bbox))))))

;     (define/override frame
;       (case-lambda
;        (() (super frame))
;        ((obj)
;         (super-frame obj)
;         (when obj
;               (if (not (titlebox))
;                   (make-titlebox))
;               (reanchor-titlebox)))))

;     (send this frame (send this frame))

    (define/public (title . args)
      (when (not (send this frame))
            (error "Needs frame before title"))
      (when (not (titlebox))
            (titlebox (make-titlebox)))
      (send/apply (titlebox) text args))

    (define/override (resize-frame bbox)
      (super resize-frame bbox)
      (when (titlebox)
            (send (titlebox) xy (bbs (send (send this frame) bbox)))))
))
|#

(define textblock%
  (class base-formation%
    (init dynaptr)
    (init (txt-arg ""))
    (init (xy-arg (list-head (send dynaptr view) 2)))
    (inherit dynaclass)
    (super-instantiate (dynaptr))
    (dynaclass 'textblock%)
    
    (name-part _block block)
    (block (make-object rect% dynaptr))
    (name-part _textpart textpart)
    (textpart (make-object text% dynaptr))
    (send (textpart) xy xy-arg)

    (define/public text
      (case-lambda
       (() (send _textpart text))
       ((newtext)
          (send _textpart text newtext)
          (send _block re-anchor (send _textpart anchor))
          (send _block coords (send _textpart bbox)))))

    (send this text txt-arg)
))
