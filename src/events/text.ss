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
  (set! Draw-object obj)
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
     ;   (set! Draw-object target))
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

