#lang racket/base

(provide get-container
         frame-container%
         resizable-image%
         container-form%
         make-resize-border
         )

(require racket/class
         dynapad/objects
         (only-in dynapad/image
                  image%)
         dynapad/misc/misc
         dynapad/misc/filenames
         dynapad/utils/formation
         (only-in dynapad/utils/resize
                  resizer-bindings
                  undoable-resizer-bindings
                  )
         dynapad/layout/bbox
         (only-in dynapad/history/ids
                  obj->IDexpr
                  )
         )

(define (get-container obj)
  ;returns first container-form enclosing obj (excluding obj itself) or #f if none
  (let ((wrapper (send obj getgroup)))
    (cond ((not wrapper) #f)
          ((is-a? wrapper container-form%) wrapper)
          (else (get-container wrapper)))))

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
|#
