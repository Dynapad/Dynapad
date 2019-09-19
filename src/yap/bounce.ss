(define ball%
  (class oval%
    (init (bbox (list 0 0 50 50)))
    (inherit fill)
    (super-instantiate (dynapad bbox))
    (fill "red")))

(define bimage%
  (class image%
    (init (path "/home/hci/images/stanonik/crumb997.jpg"))
    (super-instantiate (dynapad path))))

(define (make-objs n o%)
  (do
      ((i 0 (+ i 1))
       (objs ()))
      ((> i (- n 1)) objs)
    (let*
        ((obj (make-object o%))
         (vx (- (random 15) 7))
         (vy (- (random 15) 7))
         (dx (- (random 15) 7))
         (dy (- (random 15) 7)))
      (send obj vx vx)
      (send obj vy vy)
      (send obj slide dx dy)
      (set! objs (cons obj objs)))))

(define make-balls
  (case-lambda
    (() (make-balls 10))
    ((n) (make-objs n ball%))))

(define make-images
  (case-lambda
    (() (make-images 10))
    ((n) (make-objs n bimage%))))

(define nb (make-object NBodyForce%))

(define tick 0.020)
(define drag 0.9)

(define nbforce #f)

(define (run l)
  (for-each (lambda(x) (send x physics? #t)) l)
  (thread (lambda()
            (let loop ()
              (let*
                  ((bbox (send dynapad bbox))
                   (x0 (list-ref bbox 0))
                   (y0 (list-ref bbox 1))
                   (x1 (list-ref bbox 2))
                   (y1 (list-ref bbox 3)))
                (for-each (lambda(x) (send x force (list 0 0))) l)
                (when nbforce
                  (send nb init l)
                  (for-each (lambda(x) (send nb force x)) l))
                (for-each
                 (lambda (o)
                   (let*
                       ((xy (send o xy))
                        (x (car xy))
                        (y (cadr xy))
                        (prevxy (send o prevxy))
                        (prevx (car prevxy))
                        (prevy (cadr prevxy))
                        (vx (send o vx))
                        (vy (send o vy))
                        (vmag (sqrt (+ (expt vx 2) (expt vy 2))))
                        (force (send o force))
                        (forcex (car force))
                        (forcey (cadr force))
                        (forcemag (sqrt (+ (expt forcex 2) (expt forcey 2)))))

                     (when (send o physics?)
                       (set! x (+ x vx))
                       (set! y (+ y vy))
                       (when (not (and (= vx 0) (= vy 0)))
                         (send o xy (list x y)))

                       (set! vx (* vx drag))
                       (if (< (abs vx) .1) (set! vx 0))
                       (set! vy (* vy drag))
                       (if (< (abs vy) .1) (set! vy 0))

                       ;(when (> forcemag 0.1)
                       ;  (set! vx (* vmag (/ forcex forcemag)))
                       ;  (set! vy (* vmag (/ forcey forcemag))))

                       ;(cond
                       ;  ((> (abs forcex) (abs vx))
                       ;    (if (< (* forcex vx) 0)
                       ;      (set! vx (- vx))))
                       ;    (else
                       ;      (set! vx (+ vx forcex))))
                       ;(cond
                       ;  ((> (abs forcey) (abs vy))
                       ;    (if (< (* forcey vy) 0)
                       ;      (set! vy (- vy))))
                       ;    (else
                       ;      (set! vy (+ vy forcey))))

                       ;(when (< (* vx forcex) 0)
                       ;  (set! vx (- vx)))
                       ;(when (< (* vy forcey) 0)
                       ;  (set! vy (- vy)))

                       (set! vx (+ vx forcex))
                       (set! vy (+ vy forcey))

                       (when (< x x0)
                         (if (= vx 0) (set! vx 10))
                         (set! vx (abs vx)))
                       (when (> x x1)
                         (if (= vx 0) (set! vx 10))
                         (set! vx (- (abs vx))))
                       (when (< y y0)
                         (if (= vy 0) (set! vy 10))
                         (set! vy (abs vy)))
                       (when (> y y1)
                         (if (= vy 0) (set! vy 10))
                         (set! vy (- (abs vy))))

                       (send o velocity (list vx vy)))

                     (when (not (send o physics?))
                       (send o velocity (list (- x prevx) (- y prevy))))

                     (send o prevxy (list x y))))
                 l))
              (sleep tick)
              (loop)))))
