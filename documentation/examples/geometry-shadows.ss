(define (find-shadow room-obj bulb poly-obj)
  (if (list? bulb)
      (set! bulb (apply make-object geo-point% bulb)))
  (let* ((room       (geodize room-obj))
     (poly       (geodize poly-obj))
     (vtxs       (send poly vertices))
     (vtx-rays   (map (lambda (vtx) (send bulb vector-to vtx))
              vtxs))
     (ctr-pt     (send poly point-centroid))
     (ctr-line   (send bulb vector-to ctr-pt))
     (ang-sines  (map (lambda (vtx-ray)
                (send ctr-line unit-cross vtx-ray))
             vtx-rays))
     (max-lft    (apply max ang-sines))
     (max-rgt    (apply min ang-sines))
     (lft-index  (list-position ang-sines max-lft))
     (rgt-index  (list-position ang-sines max-rgt))
     (l-vtx      (list-ref vtxs lft-index))
     (r-vtx      (list-ref vtxs rgt-index))
     (max-range  (+ (send room width) (send room height)))
     (l-ray      (ic (make-object geo-segment% bulb l-vtx)
             (length max-range)))
     (r-ray      (ic (make-object geo-segment% bulb r-vtx)
             (length max-range)))
     (full-sector (make-object geo-polygon%
                   (list bulb
                     (send r-ray endpoint)
                     (send l-ray endpoint))))
     (sector-tip  (make-object geo-polygon% (list bulb r-vtx l-vtx)))
     (fat-sector-tip (send sector-tip margin
                   (/ (send sector-tip width) 10000)))
     (shadow      (car (AreaSubtractArea
                (car (AreaIntersectArea room full-sector))
                (car (AreaUnionArea fat-sector-tip poly))))))
    shadow))

(define shadow%
  (class named-actor%
    (field (_shade-obj (ic (make-object polygon% dynapad) ;no coords initially
               (pen "none") (fill "black") (findable #f))))
    (super-instantiate ())

    (define/public (update room bulb)
      (let ((poly (find-shadow room bulb (send this object))))
    (send _shade-obj coords (send poly coords))))

    (rename (super-die die))
    (define/override (die)
      (send _shade-obj delete)
      (super-die))
))

(define shadow-region%
  (class field-region%
     (init _obj)
     (send _obj fill "#333300")
     (field (_bulb-pt (make-object geo-point% (bbcenter (send _obj bbox)))))
     (field (_bulb-obj
         (let ((x (send _bulb-pt x))
               (y (send _bulb-pt y))
               (rad (/ (maxdim (send _obj bbox)) 100)))
           (ic (make-object oval% dynapad
               (list (- x rad) (- y rad) (+ x rad) (+ y rad)))
               (pen "yellow")
               (fill "yellow")
               (findable #f)))))
     (super-instantiate (_obj))

     (define/public (bulb) _bulb-pt)

     (send this enter-action
           (lambda (obj)
         (let ((shadow (make-object shadow%)))
           (send shadow attach-to obj 'shadow)
           (send shadow update (send this object) (send this bulb)))))

     (send this leave-action
           (lambda (obj)
         (send-actor-named obj 'shadow die)))

     (send this update-action
           (lambda (obj)
         (send-actor-named obj 'shadow update
                   (send this object) (send this bulb))))
     (send this refresh-contents)
))
