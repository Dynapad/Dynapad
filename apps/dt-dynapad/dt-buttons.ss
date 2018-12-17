; I don't know if this is foolproof, but it makes the usual case easier.
(appload "dt-dynapad" "dt.ss")
(appload "dt-dynapad" "dt-events.ss")

; a layer for tools to live on above the drawing surface
(define *dt-tool-layer* (make-object layer% dynapad "dt-tool"))

(define *button-width-1* 40)
(define *button-width-2* 80)
(define *button-height* 40)
(define *current-x* (b0 (send dynapad bbox)))
(define *current-y* (b1 (send dynapad bbox)))

(define (make-dt-button-bank buttons anchor)
  (let ((dpbb (send dynapad bbox))
        (grp (ic (make-object group% dynapad buttons)
                 (anchor anchor)
                 (layer *dt-tool-layer*))))
    (send grp xy (cond
                   ((equal? anchor "se") (bbse dpbb))
                   ((equal? anchor "s")  (bbs  dpbb))
                   ((equal? anchor "sw") (bbsw dpbb))))
    (send grp ungroup)
    ))

(define dt-button-formation%
  (class base-formation%
    (init dynaptr
          label fill-color button-width button-height
          down-condition-lambda down-lambda
          up-condition-lambda up-lambda)
    (inherit dynaclass)
    (super-instantiate (dynaptr))
    (dynaclass 'dt-button-formation%)
    
    (name-part _label-obj label-obj)
    (name-part _rect rect)

  (define label-img
    (if (regexp-match (regexp "\\.png$") label)
      (let* ((path (build-path->string *dynapad-directory* (string-append "apps/dt-dynapad/" label)))
             (img (ic
                    (make-object image% dynapad path)
                    (anchor "sw")
                    (position (list *current-x* *current-y*))
                    (findable #f))))
        (set! button-width (send img width))
        (set! button-height (send img height))
        img)
      #f))

  (define rect-coords
    (list *current-x* *current-y*
          (+ *current-x* button-width)
          (+ *current-y* button-height)))

  (rect
    (ic
      (make-object rect% dynapad rect-coords)
      (fill fill-color)
      (findable #f)))

  (label-obj
    (or label-img
        (ic
          (make-object text% dynapad label (bbcenter (send (rect) bbox)))
          (z 0.75)
          (font "Helvetica")
          (anchor "center")
          (findable #f))))

  (send this layer *dt-tool-layer*)
  (send this findable #f)

  (when (is-a? (label-obj) image%)
    (set! fill-color "none")
    (send (rect) fill fill-color)
    (send (rect) pen "none")
    (send (rect) raise))

  (bind this "dt-down"
        (lambda (d e)
          (when (down-condition-lambda d e)
            (send (rect) fill "white") (down-lambda d e)) #f))

  (bind this "dt-move" (lambda (d e) #f))

  (bind this "dt-up"
        (lambda (d e)
          (when (up-condition-lambda d e) (up-lambda d e))
          (send (rect) fill fill-color) #f))

  (set! *current-x* (+ *current-x* button-width))
))

(define *dt-panzoom-button*
;  make-object dt-button-formation% dynapad "Pan/Zoom" "#444444" *button-width-2* *button-height*
  (make-object dt-button-formation% dynapad "panzoom.png" "#444444" *button-width-2* *button-height*
    (lambda (d e) (run-mode?))  ; down-condition-lambda
    (lambda (d e) (set! *dt-mouse-button* "2"))  ; down-lambda
    (lambda (d e) #t)  ; up-condition-lambda
    (lambda (d e) (set! *dt-mouse-button* "1"))  ; up-lambda
    ))

(define *dt-lens-button*
;  make-object dt-button-formation% dynapad "Lens" "#224422" *button-width-1* *button-height*
  (make-object dt-button-formation% dynapad "lens.png" "#224422" *button-width-1* *button-height*
    (lambda (d e) (and (run-mode?) (any-selected?)))  ; down-condition-lambda
    (lambda (d e) #f)  ; down-lambda
    (lambda (d e) (and (run-mode?) (any-selected?)))  ; up-condition-lambda
    (lambda (d e) (NewRegionTool lens%))  ; up-lambda
    ))


(define *dt-magnet-button*
;  make-object dt-button-formation% dynapad "Magnet" "#442222" *button-width-1* *button-height*
  (make-object dt-button-formation% dynapad "magnet.png" "#442222" *button-width-1* *button-height*
    (lambda (d e) (and (run-mode?) (any-selected?)))  ; down-condition-lambda
    (lambda (d e) #f)  ; down-lambda
    (lambda (d e) (and (run-mode?) (any-selected?)))  ; up-condition-lambda
    (lambda (d e) (NewRegionTool brush%))  ; up-lambda
    ))


(define *dt-tray-button*
;  make-object dt-button-formation% dynapad "Tray" "#222244" *button-width-1* *button-height*
  (make-object dt-button-formation% dynapad "tray.png" "#222244" *button-width-1* *button-height*
    (lambda (d e) (and (run-mode?) (any-selected?)))  ; down-condition-lambda
    (lambda (d e) #f)  ; down-lambda
    (lambda (d e) (and (run-mode?) (any-selected?)))  ; up-condition-lambda
    (lambda (d e) (NewRegionTool container%))  ; up-lambda
    ))

(define *dt-pile-button*
;  make-object dt-button-formation% dynapad "Pile" "#444444" *button-width-1* *button-height*
  (make-object dt-button-formation% dynapad "pile.png" "#444444" *button-width-1* *button-height*
    (lambda (d e) (and (run-mode?) (any-selected?)))  ; down-condition-lambda
    (lambda (d e) #f)  ; down-lambda
    (lambda (d e) (and (run-mode?) (any-selected?)))  ; up-condition-lambda
    (lambda (d e) (make-pile))  ; up-lambda
    ))

(define *dt-lock-button*
;  make-object dt-button-formation% dynapad (format " Lock/~%Unlock") "#444444" *button-width-1* *button-height*
  (make-object dt-button-formation% dynapad "lock.png" "#444444" *button-width-1* *button-height*
    (lambda (d e) (and (run-mode?) (one-pdf?)))  ; down-condition-lambda
    (lambda (d e) #f)  ; down-lambda
    (lambda (d e) (and (run-mode?) (one-pdf?)))  ; up-condition-lambda
    (lambda (d e) (if (send (it) expanded?)
                      (send (it) condense)
                      (send (it) expand)))  ; up-lambda
  ))

(define *dt-view-button*
;  make-object dt-button-formation% dynapad "View" "#444444" *button-width-1* *button-height*
  (make-object dt-button-formation% dynapad "view.png" "#444444" *button-width-1* *button-height*
    (lambda (d e) (and (run-mode?) (one-pdf?)))  ; down-condition-lambda
    (lambda (d e) #f)  ; down-lambda
    (lambda (d e) (and (run-mode?) (one-pdf?)))  ; up-condition-lambda
    (lambda (d e) (send (it) view-document))  ; up-lambda
    ))

(define *dt-copy-button*
;  make-object dt-button-formation% dynapad "Copy" "#444444" *button-width-1* *button-height*
  (make-object dt-button-formation% dynapad "copy.png" "#444444" *button-width-1* *button-height*
    (lambda (d e) (and (run-mode?) (any-selected?)))  ; down-condition-lambda
    (lambda (d e) #f)  ; down-lambda
    (lambda (d e) (and (run-mode?) (any-selected?)))  ; up-condition-lambda
    (lambda (d e) (copy-paste-reselect))  ; up-lambda
    ))

(define *dt-undo-button*
;  make-object dt-button-formation% dynapad "Undo" "#000000" *button-width-1* *button-height*
  (make-object dt-button-formation% dynapad "undo.png" "#000000" *button-width-1* *button-height*
    (lambda (d e) (run-mode?))  ; down-condition-lambda
    (lambda (d e) #f)  ; down-lambda
    (lambda (d e) (run-mode?))  ; up-condition-lambda
    (lambda (d e) (undo))  ; up-lambda
    ))

(define *dt-redo-button*
;  make-object dt-button-formation% dynapad "Redo" "#000000" *button-width-1* *button-height*
  (make-object dt-button-formation% dynapad "redo.png" "#000000" *button-width-1* *button-height*
    (lambda (d e) (and (run-mode?) (not (at-log-leaf?))))  ; down-condition-lambda
    (lambda (d e) #f)  ; down-lambda
    (lambda (d e) (and (run-mode?) (not (at-log-leaf?))))  ; up-condition-lambda
    (lambda (d e) (redo))  ; up-lambda
    ))

(make-dt-button-bank (list *dt-panzoom-button*) "sw")
(make-dt-button-bank (list *dt-lens-button*
                           *dt-magnet-button*
                           *dt-tray-button*
                           *dt-pile-button*) "s")
(make-dt-button-bank (list *dt-lock-button*
                           *dt-view-button*
                           *dt-copy-button*
                           *dt-undo-button*
                           *dt-redo-button*) "se")

; Do this after they're in position, otherwise they get homesick.
(send *dt-panzoom-button* sticky #t)
(send *dt-lens-button* sticky #t)
(send *dt-magnet-button* sticky #t)
(send *dt-tray-button* sticky #t)
(send *dt-pile-button* sticky #t)
(send *dt-undo-button* sticky #t)
(send *dt-redo-button* sticky #t)
(send *dt-view-button* sticky #t)
(send *dt-copy-button* sticky #t)
(send *dt-lock-button* sticky #t)

(changemode dynapad "Run")  ; The default mode in this setup.

(define (run-mode?) (equal? "Run" (send dynapad modifier 'get)))

(define (one-pdf?)
   (and (one-selected?) (ensure-pdf (it))))

;override generic version of spawn-process currently in misc.ss:
(define (spawn-process cmd . args)
  (system* (find-executable-path "killall" #f) "touchd")
  (system (string-append (build-path->string *dynapad-directory* "apps/dt/dt_xmouse/dt_xmouse") " 5" " &"))
  (apply system* cmd args)
  (system* (find-executable-path "killall" #f) "dt_xmouse")
  (sendf dynapad evs selector-delete) ;nasty hack: prevents spurious select rects upon restart of touchd
  (pop-event-mode dynapad "BBox")
  (system (string-append (build-path->string *dynapad-directory* "apps/dt/touchd/touchd") " &")))
