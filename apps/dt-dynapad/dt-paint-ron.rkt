(load "dt.rkt")
(load "dt-events.rkt")

(define dt-user-ovals
  (let ((oval-list-v #4(())))
    (case-lambda
      (() oval-list-v)
      ((user-id) (vector-ref oval-list-v user-id))
      ((user-id new-list)
       (vector-set! oval-list-v user-id new-list))
      ((cmd user-id oval)  ; assume cmd is 'push
       (vector-set! oval-list-v user-id
                    (cons oval (vector-ref oval-list-v user-id)))))))

(define (mk-oval bbox)
  (make-object oval% dynapad bbox))

(define (mk-rect bbox)
  (make-object rect% dynapad bbox))

(define (mk-slash bbox)
  (let ((bb (bbstretch bbox 0.5))(crds ()))
    (set! crds
          (list
           (+ (* 0.9 (b0 bb)) (* 0.1 (b2 bb))) (b1  bb)
           (b2  bb) (+ (* 0.9 (b3 bb)) (* 0.1 (b1 bb)))
           (+ (* 0.9 (b2 bb)) (* 0.1 (b0 bb))) (b3  bb)
           (b0  bb) (+ (* 0.1 (b3 bb)) (* 0.9 (b1 bb)))
           )
          )
    (make-object polygon% dynapad crds)
    )
  )

(define (dist p1 p2)
  (define dx (- (car p1) (car p2)))
  (define dy (- (cadr p1) (cadr p2)))
  (sqrt (+ (* dx dx)(* dy dy)))
  )

(define (pen-modify-bb bbox)
  (def d (dist (list (b0 bbox)(b1 bbox)) (list (b2 bbox)(b3 bbox))))
  (def f (* 3 (- d 20)))
  (if (< f 1) (set! f 1))
  (set! f (/ (- f 100) 100.0))
  (bbstretch bbox f)
  )

(define *old-pen* #f)
(define (mk-pen bbox)
  (let ((bb
         (bbstretch bbox 0.5)
         ;(pen-modify-bb bbox)
         )
        (bbc (bbcenter bbox))
        (bll (list (b0 bbox)(b1 bbox)))
        (bur (list (b2 bbox)(b3 bbox)))
        (crds ()))
    (if *old-pen*
        (begin
          (if (< (dist (car *old-pen*) bbc) 100)
              (begin
                ;draw it
                (set! crds (apply append (cadr *old-pen*) (caddr *old-pen*) (list bur bll)))
                (set! *old-pen* (list bbc bll bur))
                (make-object polygon% dynapad crds)
                )
              (begin
                (set! *old-pen* #f)
                (make-object polygon% dynapad (apply append bll bur bll))
                )
              )
          )
        (begin
          (set! *old-pen* (list bbc bll bur))
          (make-object polygon% dynapad (apply append bll bur bll))
          )
        )
    )
  )

(def *brush-functions* (list mk-oval mk-pen mk-rect mk-slash))
(def *user-brush-list* (list->vector (list 0 0 0 0)))

(define (toggle-users-brush user-id)
  (define old-brush (vector-ref *user-brush-list* user-id))
  (vector-set! *user-brush-list* user-id
               (case old-brush
                 ((0) 1)
                 ((1) 2)
                 ((2) 3)
                 ((3) 0)
                 (else 0)
                 )
               )
  ;  (if (eq? (vector-ref *user-brush-list* user-id) mk-oval)
  ;    (vector-set! *user-brush-list* user-id mk-pen)
  ;    (vector-set! *user-brush-list* user-id mk-oval))
  )

(define (dt-draw-oval pad event)
  (let* ((user-id (dt-event-user-id event))
         (color (dt-user-color user-id)))
    (dt-user-ovals 'push user-id
                   (ic ((list-ref *brush-functions* (vector-ref *user-brush-list* user-id)) (dt-event-bbox event))
                       (pen color)
                       (fill color) ))))

(bind dynapad "dt-down" dt-draw-oval)
(bind dynapad "dt-move" dt-draw-oval)

(define (dt-delete-ovals user-id)
  (for-each (lambda (o) (send o delete)) (dt-user-ovals user-id))
  (dt-user-ovals user-id null))

;
; create the interface components
;

; a layer for tools to live on above the drawing surface
(define *dt-tool-layer* (make-object layer% dynapad "dt-tool"))

(define *dt-colormap-image*
  (ic
   (make-object image% dynapad "colormap.png")
   (re-anchor "sw")
   (position (first (send dynapad bbox)) (second (send dynapad bbox)) 1)
   (re-anchor "nw")
   (sticky #t)
   (layer *dt-tool-layer*)))

; make grey bars on left and top sides, and a round red delete button
(let* ((cibb (send *dt-colormap-image* bbox))
       (dbb (send dynapad bbox))
       (left-bar
        (ic
         (make-object rect% dynapad (list (first cibb) (fourth cibb)
                                          (third cibb) (fourth dbb)))
         (sticky #t)
         (layer *dt-tool-layer*)
         (fill "grey")))
       (bottom-bar
        (ic
         (make-object rect% dynapad (list (third cibb) (second cibb)
                                          (third dbb) (fourth cibb)))
         (sticky #t)
         (layer *dt-tool-layer*)
         (fill "grey")))
       (change-brush-button
        (ic
         (make-object oval% dynapad
                      (bbstretch (bbslide cibb 0 (bbheight cibb)) -0.25))
         (fill "blue")
         (sticky #t)
         (layer *dt-tool-layer*)
         (slide 0 200)
         )
        )
       (change-brush-touch-id #4(#f))
       (delete-button
        (ic
         (make-object oval% dynapad
                      (bbstretch (bbslide cibb 0 (bbheight cibb)) -0.25))
         (fill "red")
         (sticky #t)
         (layer *dt-tool-layer*)
         )
        )
       (delete-text
        (ic
         (make-object text% dynapad "Delete")
         (re-anchor "c")
         (position (append (bbcenter (bbstretch (bbslide cibb 0 (bbheight cibb)) -0.25)) (list 1)))
         (pen "black")
         (sticky #t)
         (layer *dt-tool-layer*))
        )
       (delete-touch-id #4(#f)))
  (bind delete-button "dt-down"
        (lambda (pad event)
          (vector-set! delete-touch-id (dt-event-user-id event)
                       (dt-event-touch-id event))
          #f))
  (bind delete-button "dt-move" (lambda (pad event) #f))
  (bind delete-button "dt-up"
        (lambda (pad event)
          (when (equal? (dt-event-touch-id event)
                        (vector-ref delete-touch-id (dt-event-user-id event)))
            (dt-delete-ovals (dt-event-user-id event)))
          #f))

  (bind change-brush-button "dt-down"
        (lambda (pad event)
          (vector-set! change-brush-touch-id (dt-event-user-id event)
                       (dt-event-touch-id event))
          #f))
  (bind change-brush-button "dt-move" (lambda (pad event) #f))
  (bind change-brush-button "dt-up"
        (lambda (pad event)
          (when (equal? (dt-event-touch-id event)
                        (vector-ref change-brush-touch-id (dt-event-user-id event)))
            (dt-change-brush (dt-event-user-id event) change-brush-button))
          #f))
  )

(define (dt-change-brush user-id bttn)
  (toggle-users-brush user-id)
  (if (string=? (send bttn fill) "blue")
      (send bttn fill "green")
      (send bttn fill "blue")))

(define (dt-color-from-object event)
  (dt-user-color (dt-event-user-id event)
                 (send (event-obj event) fill)))

(define *user-colors* (list "red" "green" "yellow" "blue"))

; make a swatch and bind appropriate procedures to it
(define (dt-make-swatch user-id)
  (let ((swatch
         (ic
          (make-object rect% dynapad (send *dt-colormap-image* bbox))
          (re-anchor "sw")
          (position (+ (first (send dynapad bbox))
                       (* 1.3 (+ 1 user-id) (send *dt-colormap-image* width)))
                    (second (send dynapad bbox))
                    1)
          (sticky #t)
          (layer *dt-tool-layer*)
          (fill (list-ref *user-colors* user-id))
          ))
        (swatch-touch-id #4(#f)))

    (bind swatch "dt-down"
          (lambda (pad event)
            (dt-color-from-object event)
            (vector-set! swatch-touch-id (dt-event-user-id event)
                         (dt-event-touch-id event))
            #f))
    (bind swatch "dt-move"
          (lambda (pad event)
            (when (equal? (dt-event-touch-id event)
                          (vector-ref swatch-touch-id (dt-event-user-id event)))
              (dt-color-from-object event))
            #f))
    swatch))

; return or change the color associated with each of four dt users
(define dt-user-color
  (let ((color-v (list->vector *user-colors*));#4("white")
        (swatch-v (list->vector (map dt-make-swatch '(0 1 2 3)))))
    (case-lambda
      (() color-v)
      ((user-id) (vector-ref color-v user-id))
      ((user-id new-color)
       (vector-set! color-v user-id new-color)
       (send (vector-ref swatch-v user-id) fill new-color)))))

(define *dt-colormap-imagedata*
  (send *dt-colormap-image* imagedata))

(define *dt-colormap-dimensions*
  (send *dt-colormap-imagedata* dimensions))

; procedure to update a user's color when they touch the colormap image
(define (dt-color-from-colormap event)
  (let ((x (event-x event))
        (y (event-y event))
        (pos (send *dt-colormap-image* position))
        (xpixel 0)
        (ypixel 0)
        (rgb #f))
    (when (bbenclosed x y (send *dt-colormap-image* bbox))
      (set! xpixel (- x (first pos)))
      (set! ypixel (- (second pos) y))
      (set! xpixel
            (* (/ xpixel (send *dt-colormap-image* width))
               (first *dt-colormap-dimensions*)))
      (set! ypixel
            (* (/ ypixel (send *dt-colormap-image* height))
               (second *dt-colormap-dimensions*)))
      (set! rgb
            (sch_imagedata_rgb (send *dt-colormap-imagedata* get-cptr)
                               (inexact->exact (floor ypixel))
                               (inexact->exact (floor xpixel))))

      (dt-user-color (dt-event-user-id event)
                     (format "#~a~x~a~x~a~x" (if (< (first rgb) 16) "0" "") (first rgb)
                             (if (< (second  rgb) 16) "0" "") (second rgb)
                             (if (< (third rgb) 16) "0" "") (third rgb)))
      )
    )
  )

; bind procedures to the colormap image
(let ((colormap-touch-id #4(#f)))
  (bind *dt-colormap-image* "dt-down"
        (lambda (pad event)
          (dt-color-from-colormap event)
          (vector-set! colormap-touch-id (dt-event-user-id event)
                       (dt-event-touch-id event))
          #f))
  (bind *dt-colormap-image* "dt-move"
        (lambda (pad event)
          (when (equal? (dt-event-touch-id event)
                        (vector-ref colormap-touch-id (dt-event-user-id event)))
            (dt-color-from-colormap event))
          #f)))
