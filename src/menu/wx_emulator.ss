
;-- "after" util ---
; execute lambda after wait of specified milliseconds
;  example:   (after 3000 (lambda () (say "hello from 3 seconds ago")))

(define (after msec proc)
  (make-object timer% proc msec #t))

(define (tail list) (car (reverse list)))

(define (currpixels n)
  (/ n (send currentPAD getzoom)))

(define *default-text-scale* 0.5)

(define dywx-frame%
  (class container-form%
    (init title dummy1 dummy2 dummy3 initx inity)
    (field (_object_sequence ()))
;;  (field (_backing #f))

    (super-instantiate (currentPAD))

    (send this re-anchor "nw")
    (let ((xy (bbnw (send currentPAD bbox))))
      (send this xy (car xy) (cadr xy)))
    (send this sticky #t)
    (send this findable #f)

    (define/override (add obj)
      (let ((targetXY 0))
        (if (null? _object_sequence)
          (set! targetXY (send this xy))
          (set! targetXY (bbsw (send (tail _object_sequence) bbox))))
        (send obj xy (+ (car targetXY) (currpixels 2)) (cadr targetXY)))
      (set! _object_sequence (append _object_sequence (list obj)))
      (super add obj)
    )

    (define/public (update)
      (if (not (null? _object_sequence))
        (let ((targetXY (send this xy)))
          (set! targetXY (list (car targetXY) (+ (cadr targetXY) (currpixels 2))))
          (foreach _object_sequence (lambda (obj)
            (send obj xy (car targetXY) (- (cadr targetXY) (currpixels 2)))
            (set! targetXY (bbsw (send obj bbox))) ))))

;;    (when (not _backing)
;;      (set! _backing (ic (make-object rect% (send this dynapad)
;;                           (bbunion-objects _object_sequence))
;;                         (penwidth 0)
;;                         (pen "none")
;;                         (fill "white")))
;;      (super-add _backing)
;;    )
;;    (send _backing coords (bbunion-objects _object_sequence))
;;    (send _backing lower)

    )

    (define/public (update-all)
      (if (not (null? _object_sequence))
        (foreach _object_sequence (lambda (pane)
          (send pane update-all))))
      (update)
    )

    (define/public (get-mems) _object_sequence)

    (define/public (show bool) #t)
  )
)

(define dywx-horizontal-pane%
  (class group%
    (init-field _parent)
    (field (_object_sequence ()))
    (super-instantiate ((send _parent dynapad)))
    (send this anchor "nw")
    (send this xy 0 0)
    (send _parent add this)
    (send this findable #f)

    (define/override (add obj)
      (let ((targetXY 0))
        (if (null? _object_sequence)
          (set! targetXY (send this xy))
          (set! targetXY (bbne (send (tail _object_sequence) bbox))))
        (send obj xy (+ (car targetXY) (currpixels 2)) (cadr targetXY)))
      (set! _object_sequence (append _object_sequence (list obj)))
      (super add obj)
      (send _parent update)
      )

    (define/public (update)
      (if (not (null? _object_sequence))
        (let ((targetXY (send this xy)))
          (foreach _object_sequence (lambda (obj)
            (send obj xy (+ (car targetXY) (currpixels 2)) (cadr targetXY))
            (set! targetXY (bbne (send obj bbox))) ))))
      (send _parent update))

    (define/public (update-all)
      (if (not (null? _object_sequence))
        (foreach _object_sequence (lambda (btn)
          (send btn update-all))))
      (update)
    )

    (define/public (get-mems) _object_sequence)

    (define/public (show bool) #t)
  )
)
    (def tl-list ())
(define dywx-button%
  (class group%
    (init init-label init-pane command-function) ; function args are (bttn evnt)
    (field (_pane init-pane))
    (field (_label #f))
    (field (_btn_rect #f))
    (super-instantiate ((send _pane dynapad)))
    (send this anchor "nw")
              (define/public (get-label) _label)
    (define/public (set-label new_label)
      (if _label (send _label delete))
      (if (string? new_label)
        ;make text%
        (begin
          (set! _label (ic (make-object text% (send this dynapad))
                           (pen "black")
                           (font "Helvetica")
                           (z (/ *default-text-scale* (send currentPAD getzoom)))
                           (text new_label)
                           ))
             (push! _label tl-list)
          (send _label anchor "nw")
          (send _label xy (send this xy))
          (send this add _label)
        )
        ;else make baseimage%
        (begin
          (set! _label (ic (make-object baseimage% (send this dynapad) new_label)
                           (z (/ 1.0 (send currentPAD getzoom)))
                           ))
          (send _label anchor "nw")
          (send _label xy (send this xy))
          (send this add _label)
        )
      )
      (when (not _btn_rect)
        (set! _btn_rect (ic (make-object rect% (send this dynapad))
                         (penwidth 0)
                         (pen "black")
                         (fill "#e0dfe3")
                         ))
        (send _btn_rect re-anchor "nw")
        (send _btn_rect xy (send this xy))
        (send _btn_rect coords (send _label bbox))
        (send this add _btn_rect)
      )
                        
      (send _label raise)
      (send this update-all)
      (send _pane update)
    )

    (define/public (update)
      (send this update-all)
      (send _pane update))

    (define/public (update-all)
      (let ((oldxy (bbnw (send this bbox))))
        (send _label z (* 1.001 (send _label z))) ; silliness to force bbox update
        (send _btn_rect coords
          (bbwiden (send _label bbox)
            (/ 2.0 (send currentPAD getzoom))
            (/ 2.0 (send currentPAD getzoom))))
        (send this xy oldxy)
      )
      (send _label raise))

    (define/public (show bool) #t)

    (if init-label (set-label init-label))
    (send _pane add this)
    (send this bind "<ButtonPress-1>"
      (lambda (d e) (command-function this e) #f))
    (send this bind "<Run-ButtonPress-1>"
      (lambda (d e) (command-function this e) #f))
    (send this bind "<Select-ButtonPress-1>"
      (lambda (d e) (command-function this e) #f))
    (send this bind "<Draw-ButtonPress-1>"
      (lambda (d e) (command-function this e) #f))
    (send this divisible #f)
    (send this findable #f)
    (send this takegroupevents #t)
    (if init-label
      (after 1000 (lambda () (send this set-label init-label))))
  )
)

(define color-ellipse%
  (class group%
    (init-field _parent)
    (field (_min_height 30))
    (field (_background #f))
    (field (_ellipse #f))

    (super-instantiate ((send _parent dynapad)))

    (define/public (pencolor newcolor) (send _ellipse pen newcolor))
    (define/public (fillcolor newcolor) (send _ellipse fill newcolor))

    (define/public (update-all)
      (let ((oldxy (bbnw (send this bbox))))
        (send _background coords
          (list (car oldxy)
                (- (cadr oldxy) (currpixels _min_height))
                (+ (car oldxy) (* 2 (currpixels _min_height)))
                (cadr oldxy)))
        (send _ellipse coords
          (list (car oldxy)
                (- (cadr oldxy) (* 0.75 (currpixels _min_height)))
                (+ (car oldxy) (currpixels _min_height))
                (cadr oldxy)))
        (send _ellipse xy (bbcenter (send _background bbox)))
        ))

    ; finish construction
    (set! _background (ic (make-object rect% (send this dynapad))
                     (penwidth 0)
                     (pen "none")
                     (fill "#e0dfe3")
                     (coords (list 0 0 (* 2 (currpixels _min_height)) (currpixels _min_height) ))
                     ))
    (send _background re-anchor "nw")
    (send _background xy (send this xy))
    (send this add _background)

    (set! _ellipse (ic (make-object oval% (send this dynapad))
                     (penwidth 5)
                     (pen "black")
                     (fill "white")
                     (coords (list 0 0 (currpixels _min_height) (* 0.75 (currpixels _min_height)) ))
                     ))
    (send _ellipse re-anchor "center")
    (send _ellipse xy (bbcenter (send _background bbox)))
    (send this add _ellipse)
    (send _ellipse raise)

    (send _parent add this)
  )
)

(define dywx-bitmap%
  (class imagedata%
    (init filename filetype)
    (super-instantiate (currentPAD filename))
  )
)


