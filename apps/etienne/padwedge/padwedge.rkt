(require (lib class.ss"))

;; Added by Etienne 5/23/02
(define padwedge%
  (class polygon%
    (init dynapad vx vy radius as ae)
    (inherit coords)
    (field (initcoords '()))

    (define (calc-x angle)
      (cos (* angle (/ pi 180))))

    (define (calc-y angle)
      (sin (* angle (/ pi 180))))

    (define (build-coords)
      (define l (list vx vy))
      (define n (floor (/ (- ae as) 10)))

      (set! l (append l (list (* radius (calc-x as)) (* radius (calc-y as)))))
      (do ((i as (+ i 10)))
          ((> i ae))
        (set! l (append l (list (* radius (calc-x i)) (* radius (calc-y i))))))
      (set! l (append l (list (* radius (calc-x ae)) (* radius (calc-y ae)))))
      l)

    (set! initcoords (build-coords))
    (super-instantiate (dynapad initcoords))))
