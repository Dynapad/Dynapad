#lang racket/base

(require racket/class
         dynapad/base
         dynapad/misc/misc
         dynapad/misc/filenames
         dynapad/utils/formation
         dynapad/layout/bbox
         dynapad/events/text ; how this is not creating a cycle is a mystery
         )

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
