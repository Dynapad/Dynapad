#lang racket/base

(require racket/class
         dynapad/base
         dynapad/misc/misc
         dynapad/layout/bbox
         dynapad/events/mode
         dynapad/events/draw
         (only-in dynapad/libdynapad
                  sch_gettext
                  sch_settext
                  sch_inserttext
                  sch_deletetext
                  sch_maketext
                  sch_marktext
                  sch_pen
                  sch_font
                  )
         )

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

    (define (export-stub dir class) (error 'not-implemented))

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

;======= Text ========

(define text%
  (class basetext%
    (init argPAD (inittext #f) (initposition #f) (initfont #f) (initanchor #f))
    (inherit text position font anchor dynaclass)

    (super-instantiate(argPAD))
    (dynaclass 'text%)
    (when inittext (text inittext))
    (when initposition (position initposition))
    (when initfont (font initfont))
    (when initanchor (anchor initanchor))
    (add-Text-Bindings this) ))

#|
(define (bindText argPAD)
  (let ((enter-text-fn
         (lambda (eventPAD e) (set! currentPAD eventPAD)
                 (let*
                     ((x (event-x e))
                      (y (event-y e))
                      (pos (list x y (/ 1.0 (send eventPAD getzoom))))
                      (lst (reverse (send eventPAD find 'overlapping (list x y x y))))
                      (obj (if (null? lst) #f (car lst))))

                   (cond
                     ((is-a? obj text%)
                      (send obj focus)
                      (text-move-point-to-xy obj x y))
                     (else
                      (changemode eventPAD "EditText")
                      (set! obj (make-object text% eventPAD "" pos))
                      (send obj focus)))))))
    #f
    ;(send argPAD bind "<DrawText-ButtonPress-1>" enter-text-fn)
    ))
|#

; this version only works for single line text
; and then only approximately
(define (text-move-point-to-xy obj x y)
  (def bb (send obj bbox))
  (def f (/ (- x (b0 bb)) (bbwidth bb)))
  (def i (inexact->exact (round (* f (string-length (send obj text))))))
  (send obj setpoint i))

(define (edit-text-at-xy argPAD obj x y)
  ; assumes obj is text%
  (changemode argPAD "EditText")
  (set-Draw-object! obj)
  (send obj focus)
  (text-move-point-to-xy obj x y))

; This mimics start-shape-event in draw.ss:
(define (start-text-event eventPAD e)
  (set! currentPAD eventPAD)
  (let* ((x (event-x e))
         (y (event-y e))
         (pos (list x y (/ 1.0 (send eventPAD getzoom))))
         (objs-here (reverse (send eventPAD find 'overlapping (list x y x y))))
         (target (if (null? objs-here) #f (car objs-here))))
    (unless (and target
                 (is-a? target text%))
      ; make new text:
      (set! target (make-object text% eventPAD "" pos)))
    ;   (set-Draw-object! target))
    (edit-text-at-xy eventPAD target x y)))

(define (bindTextMode argPAD)
  (send argPAD bind "<DrawText-ButtonPress-1>" start-text-event)
  (send argPAD bind "<DrawText-KeyPress-Escape>" esc-shape-event)
  (send argPAD bind "<EditText-ButtonPress-1>"     esc-shape-event)
  )

(define (add-Text-Bindings o)
  (send o bind "<Double-ButtonPress-1>"
        (lambda (eventPAD e)
          (edit-text-at-xy eventPAD o (event-x e) (event-y e))
          #f
          ))

  (send o bind "<EditText-KeyPress>"
        (lambda (eventPAD e)
          (let ((str (event-key e)))
            (when (> (string-length str) 0)
              (send o insert str)))))

  (send o bind "<EditText-KeyPress-Escape>"
        esc-shape-event)

  (send o bind "<EditText-KeyPress-Right>"
        (lambda (eventPAD e) (send (event-obj e) forward) #f))

  (send o bind "<EditText-Control-KeyPress-f>"
        (lambda (eventPAD e) (send (event-obj e) forward) #f))

  (send o bind "<EditText-KeyPress-Left>"
        (lambda (eventPAD e) (send (event-obj e) backward) #f))

  (send o bind "<EditText-Control-KeyPress-b>"
        (lambda (eventPAD e) (send (event-obj e) backward) #f))

  (send o bind "<EditText-KeyPress-Down>"
        (lambda (eventPAD e) (send (event-obj e) next) #f))

  (send o bind "<EditText-Control-KeyPress-n>"
        (lambda (eventPAD e) (send (event-obj e) next) #f))

  (send o bind "<EditText-KeyPress-Up>"
        (lambda (eventPAD e) (send (event-obj e) previous) #f))

  (send o bind "<EditText-Control-KeyPress-p>"
        (lambda (eventPAD e) (send (event-obj e) previous) #f))

  (send o bind "<EditText-KeyPress-BackSpace>"
        (lambda (eventPAD e) (send (event-obj e) delete-backward)))

  (send o bind "<EditText-KeyPress-Delete>"
        (lambda (eventPAD e) (send (event-obj e) delete-forward)))

  (send o bind "<EditText-Control-KeyPress-d>"
        (lambda (eventPAD e) (send (event-obj e) delete-forward)))

  (send o bind "<EditText-Control-KeyPress-e>"
        (lambda (eventPAD e)
          (send (event-obj e) setpoint "point lineend")
          #f))

  (send o bind "<EditText-Control-KeyPress-a>"
        (lambda (eventPAD e)
          (send (event-obj e) setpoint "point linestart")
          #f))

  (send o bind "<EditText-Alt-KeyPress-f>"
        (lambda (eventPAD e)
          (send (event-obj e) setpoint "point + 1 char wordend")
          #f))

  (send o bind "<EditText-Alt-KeyPress-b>"
        (lambda (eventPAD e)
          (send (event-obj e) setpoint "point - 1 char wordstart")
          #f))


  (send o bind "<EditText-ButtonPress-1>"
        (lambda (eventPAD e)
          (let ((x (event-x e))
                (y (event-y e)))
            (text-move-point-to-xy o x y)
            #f)))
  ;       (if (bbenclosedoron x y (send (event-obj e) bbox))
  ;       (text-move-point-to-xy o x y)
  ;       (begin
  ;         (say "outside")
  ;         (send eventPAD focus))))
  ;     #f))


  )

;; what were you doing in formation.rkt silly?

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
