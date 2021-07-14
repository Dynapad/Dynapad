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

(define (dt-draw-oval pad event)
  (let* ((user-id (dt-event-user-id event))
         (color (dt-user-color user-id)))
    (dt-user-ovals 'push user-id
                   (ic (make-object oval% dynapad (dt-event-bbox event))
                       (pen color)
                       (fill color)))))

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
       (delete-button
        (ic
         (make-object oval% dynapad
                      (bbstretch (bbslide cibb 0 (bbheight cibb)) -0.25))
         (sticky #t)
         (layer *dt-tool-layer*)
         (fill "red")))
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
          #f)))

(define (dt-color-from-object event)
  (dt-user-color (dt-event-user-id event)
                 (send (event-obj event) fill)))

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
          (fill "white")))
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
  (let ((color-v #4("white"))
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
