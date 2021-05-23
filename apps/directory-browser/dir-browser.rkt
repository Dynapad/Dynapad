(require (lib "file.ss"))

(define titled-frame-container%
  (class frame-container%
    (init dynaptr frame-arg)
    (inherit dynaclass)
    (super-instantiate (dynaptr frame-arg))
    (dynaclass 'titledframe-container%)

    (name-part _titlebox titlebox)
    (titlebox (ic (make-object text% dynaptr)
                  (anchor "n")
                  (xy (bbs (send (send this frame) bbox)))))
    (define/public (title . args)
      (send/apply (titlebox) text args))
    ))


(define dir-window%
  (class titled-frame-container%
    (init dynaptr (initpath #f) (initframe #f))
    (init-field (_initmaxdepth 0)
                (_parwindow #f)) ;;ptr to parent window
    (field (_frameptr initframe)
           (_path initpath))
    (inherit dynaclass title titlebox)
    (inherit-field _dynapad)

    ;; If no frame is given, then use current dynpad view
    (if (not _frameptr)
        (set! _frameptr (make-object rect% dynaptr (send dynaptr bbox))))

    (super-instantiate (dynaptr _frameptr))
    (dynaclass 'dir-window%)

    ;; Flags
    (field (_centered? #f)
           (_opened? #f)) ;; set #t when icons and subdirs are drawn

    ;; Lists
    (field (_files '())
           (_fileicons '())
           (_rects '())
           (_subdirs '())
           (_subdirwindows '()))

    ;; Pointer to managing dir browser
    (field (_dirbrowserptr #f))

    ;; Store zfac of titlebox
    (field (_titleboxzfac 0))

    ;; Check for valid init path... if invalid, use current dir
    (when (or (not _path) (not (directory-exists? _path)))
      (set! _path (current-directory))
      (send _dirbrowserptr initpath _path) ;; give dirbrowser new path
      )

    ;; Get files and subdirs
    (set! _files (filter
                  (lambda (item)
                    (file-exists?
                     (build-path->string _path item)))
                  (directory-list-ignore-hidden _path)))

    (set! _subdirs (filter
                    (lambda (item)
                      (directory-exists?
                       (build-path->string _path item)))
                    (directory-list-ignore-hidden _path)))


    ;; Create a frame for subdirectories
    (name-part _dir_frame dir-frame)


    ;    (send _frameptr center 1 #f .5 .5 1.0)
    (let* ((framebb (send _frameptr bbox))
           (ycenter (byc framebb))
           (ytop (b3 framebb))
           (ybottom (b1 framebb))
           (xleft (b0 framebb))
           (xright (b2 framebb))
           (_dir_frame_rect '()))

      ;; Resize _frameptr to half of original size
      (send _frameptr bbox (list xleft ybottom xright ycenter))

      ;; Make _dir_frame_rect to other half
      (set! _dir_frame_rect
            (make-object rect% dynaptr (list xleft ycenter xright ytop)))
      (dir-frame _dir_frame_rect)

      ;; If there are no subdirectories, then shrink dir-frame, resize frameptr
      (when (null? _subdirs)
        (send _dir_frame bbox (list xleft ytop xright ytop))
        (send _frameptr bbox (list xleft ybottom xright ytop))
        )

      ;; If there are only subdirectories (no files), then resize dir-frame
      ;; and shrink frameptr
      (when (and (null? _files) (not (null? _subdirs)))
        (send _dir_frame bbox (list xleft ybottom xright ytop))
        (send _frameptr bbox (list xleft ybottom xright ybottom))) ;;hide hack
      )

    ;; -- Public Methods --
    (define/public centered?
      (case-lambda
        (() _centered?)
        ((arg) (set! _centered? arg)
               (cond
                 (arg (show-full-path))
                 ((not arg) (show-name-only)))
               (reconfigure-titlebox)
               )))

    (define/public (close)
      (for-each (lambda (o)
                  (send this remove o)
                  (send o delete))
                (send this contents))
      (send _frameptr fill "white")
      (send _dir_frame fill "white")
      (set! _subdirwindows '())
      (send _frameptr maxsize -1)  ; not 10000
      (send _dir_frame maxsize -1)  ; not 10000
      (set! _opened? #f))

    (define/public dirbrowser (get/set _dirbrowserptr))

    (define/public (files) _files)

    (define/public (fileicons) _fileicons)

    (define/public (hide-all-subdirs)
      (for-each (lambda(o) (send o delete)) _subdirwindows)
      (set! _subdirwindows '())
      ;(resize-frames) keep frame sizes static for now
      )

    (define/public (path) _path)

    (define/public (open)
      (define _save_view (send _dynapad view))
      ;; Configure frame and dir-frame
      (send _frameptr fill "#0000cc")
      (send _frameptr takegroupevents #f)
      (send _dir_frame fill "#100000")
      (send _dir_frame lower)

      (draw-icons-for-files)
      (draw-rects-for-subdirs)
      (draw-sub-windows)

      (if _dirbrowserptr
          (send _dirbrowserptr add-event-bindings _subdirwindows))

      (send _dynapad moveto _save_view)
      (send _frameptr maxsize -1)  ; not 10000
      (send _dir_frame maxsize -1)  ; not 10000
      (set! _opened? #t))

    (define/public (opened?) _opened?)

    (define/public (subdirs) _subdirs)

    (define/public (subdirwindows) _subdirwindows)

    (define/public titleboxzfac (get/set _titleboxzfac))

    ;; -- Private Methods --
    (define (draw-icons-for-files)
      (let ((_file_icon_list (map
                              (lambda (file)
                                (make-file-icon (build-path->string _path file)
                                                (generate-random-pos 1)))
                              _files)))
        (unless (null? _file_icon_list)
          (arrange-in-grid-onto-object _file_icon_list _frameptr))
        (set! _fileicons _file_icon_list)
        (for-each (lambda(o)
                    (send this add o)
                    (send o bind "<Double-ButtonPress-1>"
                          (lambda (eventPAD evnt)
                            (let* ((obj (event-obj evnt))
                                   (path (send obj path)))
                              (send *launcher* open-application-for path))))
                    (send thermometer set-temp o)
                    ) _fileicons)
        ))

    (define (draw-rects-for-subdirs)
      (let ((_rect_list (map
                         (lambda (subdir)
                           (ic (make-object rect% dynaptr
                                            (send dynapad bbox)) ;;hack for bbox
                               (fill "white")
                               (position (generate-random-pos))
                               ))
                         _subdirs)))
        (unless (null? _rect_list)
          (arrange-in-grid-onto-object _rect_list _dir_frame 1.10 0.90))
        (set! _rects _rect_list)))

    (define (draw-sub-windows)
      (let ((_subwinlist (map
                          (lambda (rect subdir)
                            (make-object dir-window% dynaptr
                                         (build-path->string _path subdir) rect
                                         (- _initmaxdepth 1) this))
                          _rects _subdirs)))
        (unless (null? _subwinlist)
          (arrange-in-grid-onto-object _subwinlist _dir_frame 1.10 0.90))
        (set! _subdirwindows _subwinlist)
        (set! _rects '())
        (for-each (lambda (win)
                    (send this add win)
                    ) _subdirwindows)
        ))

    (define (show-full-path)
      (title _path)
      (send (titlebox) z _titleboxzfac)
      )

    (define (show-name-only)
      (if (file-name-from-path _path)
          (title (file-name-from-path _path))
          (title _path))
      (if _parwindow
          (send (titlebox) z (send _parwindow titleboxzfac)))
      )

    (define (reconfigure-titlebox)
      (let ((titleboxbb (send (titlebox) bbox))
            (framebb (send _frameptr bbox)))
        (if (> (bbwidth titleboxbb) (bbwidth framebb))
            (send (titlebox) scale (/ (bbwidth framebb) (bbwidth titleboxbb)))
            )
        ))

    (define (resize-frames)
      (let* ((framebb (send _frameptr bbox))
             (dirframebb (send _dir_frame bbox))
             (ytop (b3 dirframebb))
             (ymid (b1 dirframebb))
             (ybot (b1 framebb))
             (xleft (b0 framebb))
             (xright (b2 framebb)))

        ;; If there are no subdirectories, then shrink dir-frame, resize frameptr
        (when (null? _subdirwindows)
          (send _dir_frame bbox (list xleft ytop xright ytop))
          (send _frameptr bbox (list xleft ybot xright ytop))
          (arrange-in-grid-onto-object _fileicons _frameptr 1.10 0.90)
          )

        ;; If there are only subdirectories (no files), then resize dir-frame
        ;; and shrink frameptr
        (when (and (null? _files) (not (null? _subdirs)))
          (send _dir_frame bbox (list xleft ybot xright ytop))
          (send _frameptr bbox (list xleft ybot xright ybot)) ;;hide hack
          (arrange-in-grid-onto-object _subdirwindows _dir_frame 1.10 0.90))
        )
      )

    ;;------------------------

    (send _dir_frame penwidth 0)
    (send _frameptr penwidth 0)
    ;    (send this center 1 #f .5 .5 1.0)
    (set! _titleboxzfac (send this z))
    (show-name-only)
    (reconfigure-titlebox)

    (if (>= _initmaxdepth 0)
        (open)
        (close))

    ;; Set maxsizes
    (send this maxsize -1)
    (send (send this container) maxsize -1)
    ))


(define dir-browser%
  (class base-formation%
    (init dynaptr)
    (init-field (_initpath #f) (_initdepth 9999) (_initframe #f))
    (inherit dynaclass)

    (super-instantiate (dynaptr))
    (dynaclass 'dir-browser%)

    ;; Create name part
    (name-part _parent parent)

    ;; Instantiate parent
    (parent (make-object dir-window% dynaptr _initpath _initframe _initdepth))

    ;; Current window on display
    (field (_currwin _parent))

    ;; -- Public Methods --
    (define/public (window) _currwin)

    (define/public initpath (get/set _initpath))

    (define/public (add-event-bindings winlist)
      (unless (null? winlist)
        (let* ((win (car winlist))
               (titlebox (send win titlebox))
               (frame (send win frame))
               (dirframe (send win dir-frame)))
          (send frame bind "<Run-ButtonRelease-1>"
                (lambda (o e) (centeron win)))
          (send dirframe bind "<Run-ButtonRelease-1>"
                (lambda (o e) (centeron win)))
          (send frame bind "<Shift-ButtonPress-1>"
                (lambda (o e)
                  (if (send win opened?)
                      (send win close)
                      (send win open))))
          (send dirframe bind "<Shift-ButtonPress-1>"
                (lambda (o e)
                  (if (send win opened?)
                      (send win close)
                      (send win open))))
          (send win dirbrowser this) ;;send each window a ptr to this dirbrowser
          (add-event-bindings (append (cdr winlist) (send win subdirwindows)))
          )))

    ;; -- Private Methods --
    (define (centeron win)
      (if (send win centered?)
          (send win center 1 #f 0.5 0.5 .99)
          (begin
            (let ((oldwin _currwin))
              (set! _currwin win)
              (send win centered? #t)
              (send win center 750 #f 0.5 0.5 0.99)
              (send oldwin centered? #f)))))

    ;; -------------------------------------

    (add-event-bindings (list _parent))
    (send _parent centered? #t)
    (centeron _parent)
    (send this maxsize -1)
    ))

(define (dir-browser . args)
  (apply make-object dir-browser% currentPAD args))
