(require (lib "mysterx.ss" "mysterx"))

(define mediaplayer%
  (class object%
    (field (_mx #f) (_mxdoc #f) (_mp #f))

    (define/public (open avi)
      ;play it, sam
      (com-invoke _mp "Open" avi))

    (super-instantiate())
    ;make a browser (ie without controls) hosts the object
    (set! _mx (make-object mx-browser% "Movie" 325 375))

    ;get the current document (empty page)
    (set! _mxdoc (send _mx current-document))

    ;insert an object
    (set! _mp (send _mxdoc insert-object-from-progid
        "MediaPlayer.MediaPlayer" 300 300))
))

(define avi%
  (class oval%
    (init dynapad)
    (init-field _filename _mp)
    (inherit coords fill bind)

    (define/public (play) (send _mp open _filename))
    (super-instantiate(dynapad))
    (coords '(0 0 50 50))
    (fill "blue")
    (bind "<Run-Double-ButtonPress-1>"
      (lambda(o e) (send (event-obj e) play)))
))

(define mp (make-object mediaplayer%))
(define highway (make-object avi% dynapad "E:/stanonik/highway.avi" mp))
(define surf (make-object avi% dynapad "E:/stanonik/surf.avi" mp))


;another way to insert, then get the object
;(send mxdoc insert-html (progid->html "MediaPlayer.MediaPlayer" 300 300))
;(define mp (car (send mxdoc objects)))


;what methods
;(com-methods mp)

;what properties
;(com-get-properties mp)

;what events
;(com-events)

;get a property
;(com-get-property mp "CurrentPosition")

;set a property
;(com-set-property! mp "CurrentPosition" 3.3)
