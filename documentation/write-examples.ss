(dynaload "formation.ss")
(require (lib "pretty.ss"))
;(dynaload "actor.ss")

(define (make-bucket argPAD bbox cnts)
  (let* ((rect (make-object rect% argPAD bbox))
     (form (make-object titled-resizable-frame-container% argPAD rect)))
    (send form contents cnts)
    (send form title "hello")
    form
    ))

(define b (make-bucket dynapad '(0 0 100 100) (fs)))
(pretty-print (send b write))

(send b alternate-build-fn
      (lambda (obj)
    `(make-bucket dynapad
              ,(cons 'list (send obj bbox))
              ,(map (lambda (c) (send c write)) (send obj contents)))))
(pretty-print (send b write))


;;;;;;;;;;;;;;;;;;;;
(define (blue-square x y sz)
  (ic (make-object rect% dynapad)
      (coords (list x y (+ x sz) (+ y sz)))
      (fill "blue")
      (transparency .5)))

(define sq (blue-square 0 0 1000))
(pretty-print (send sq write))

; yet another way:
(send sq alternate-build-fn
      (lambda (o) `(blue-square ,@(send o xy) ,(send o width))))
(pretty-print (send sq write))

; PART II:
; distributing write reposibility:

(define enter-color-actor%
  (class named-actor%
    (init _obj)
    (init-field (_color "#00000"))
    (super-instantiate ())
    
    (send this attach-to _obj 'enter)
    (define/public color (get/set _color))
    (send _obj bind "<Enter>"
      (lambda (eventPAD e) (send (event-obj e) fill _color)))
    (send _obj post-build-ops 'add
      (lambda (o label)  `(make-object enter-color-actor% ,label ,_color)))
))
(define leave-color-actor%
  (class named-actor%
    (init _obj)
    (init-field (_color "#ffffff"))
    (super-instantiate ())
    
    (send this attach-to _obj 'leave)
    (define/public color (get/set _color))
    (send _obj bind "<Leave>"
      (lambda (eventPAD e) (send (event-obj e) fill _color)))
    (send _obj post-build-ops 'add
      (lambda (o label)  `(make-object leave-color-actor% ,label ,_color)))
))

(make-object leave-color-actor% sq "blue")
(make-object enter-color-actor% sq "red")

(pretty-print (send sq write))
