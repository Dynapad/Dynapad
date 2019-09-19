(require (lib "class.ss"))

; flow menu
; inner to outer transition enables selection
; inner to outer changes menus (flow menu)
; outer to inner transistion selects

(define flow%
  (class group%
    (init-field pad)
    (inherit layer members raise remove add position sticky dynaclass)
    ; flag indicates inner to outer transition and enables choice
    (field (flag #f) (choice #f) (_debug #f))

    (define (octant x y)
      (let*
          ((cen (send outer position))
           (x (- x (car cen)))
           (y (- y (cadr cen))))
        (cond
          ((and (>= x 0) (>= y 0))
           (cond
             ((>= x y) 0)
             (else 1)))
          ((and (< x 0) (> y 0))
           (cond
             ((<= (- x) y) 2)
             (else 3)))
          ((and (<= x 0) (<= y 0))
           (cond
             ((>= (- x) (- y)) 4)
             (else 5)))
          ((and (> x 0) (< y 0))
           (cond
             ((<= x (- y)) 6)
             (else 7))))))

    (define (octantbbox oct)
      (let*
          ((bbox (send outer bbox))
           (llx (car bbox))
           (lly (cadr bbox))
           (urx (caddr bbox))
           (ury (cadddr bbox))
           (zx (/ (+ llx urx) 2))
           (zy (/ (+ lly ury) 2))
           (a (/ (- urx llx) 4)))
        (case oct
          ((0) (list (- urx a) zy urx (- ury a)))
          ((1) (list zx (- ury a) (- urx a) ury))
          ((2) (list (+ llx a) (- ury a) zx ury))
          ((3) (list llx zy (+ llx a) (- ury a)))
          ((4) (list llx (+ lly a) (+ llx a) zy))
          ((5) (list (+ llx a) lly zx (+ lly a)))
          ((6) (list zx lly (- urx a) (+ lly a)))
          ((7) (list (- urx a) (+ lly a) urx zy))
          (else (error "octantbbox: oct not between 0 and 7, given " oct)))))

    (define indicator%
      (class rect%
        (init pad)
        (inherit lower)
        (rename-super (super-coords coords))
        (define/override (coords . args)
          (remove this)
          (super-coords . args)
          (add this)
          (lower))
        (define/public (off)
          (let*
              ((pos (send outer position))
               (x (car pos))
               (y (cadr pos)))
            (coords (list x y x y))))
        (define/override (write) #f)
        (super-instantiate (pad))))

    (field (indicator (make-object indicator% pad)))
    (send indicator fill "purple")
    (send indicator transparency 0.5)

    (field (outer (make-object oval% pad '(-100 -100 100 100))))
    (send outer bind "<Enter>"
          (lambda(o e)
            (if flag
                (let ((oct (octant (event-x e) (event-y e))))
                  (when flag
                    (send indicator coords (octantbbox oct))
                    (set! choice oct)
                    (let*
                        ((ent (vector-ref (car octstack) choice))
                         (outcb (and ent (caddr ent))))
                      (when outcb
                        (outcb this)))))
                (set! choice #f))))
    (send outer bind "<Leave>"
          (lambda(o e)
            (when choice
              (send indicator coords (octantbbox choice))
              (let*
                  ((ent (vector-ref (car octstack) choice))
                   (incb (and ent (cadr ent))))
                (when incb
                  (incb this)))
              (when _debug (printf "choice ~a~%" choice)))
            (set! flag #f)))
    (send outer bind "<Motion>"
          (lambda(o e)
            (let ((oct (octant (event-x e) (event-y e))))
              (when (and flag (not (eq? choice oct)))
                (when _debug (printf "~a -> ~a~%" choice oct))
                (send indicator coords (octantbbox oct))
                (set! choice oct)))))
    (send outer transparency 0.1)
    (send outer fill "black")
    (send outer anchor "center")

    (field (inner (make-object oval% pad '(-40 -40 40 40))))
    (send inner bind "<Leave>"
          (lambda(o e)
            (set! flag #t)))
    (send inner transparency 0.1)
    (send inner fill "black")
    (send inner anchor "center")

    (define (fitfac oct label)
      (let*
          ((octbbox (octantbbox oct))
           (octw (- (caddr octbbox) (car octbbox)))
           (octh (- (cadddr octbbox) (cadr octbbox)))
           (labbbox (send label bbox))
           (labw (- (caddr labbbox) (car labbbox)))
           (labh (- (cadddr labbbox) (cadr labbbox)))
           (facw (/ octw labw))
           (fach (/ octh labh))
           (fac (min facw fach)))
        fac))

    (define textfac 1)

    (define (bindoct oct label)
      (let*
          ((bbox (octantbbox oct))
           (llx (car bbox))
           (lly (cadr bbox))
           (urx (caddr bbox))
           (ury (cadddr bbox))
           (x (/ (+ llx urx) 2))
           (y (/ (+ lly ury) 2))
           (z (caddr (send label position))))

        (send label anchor "center")
        (send label position (list x y z))
        (if (is-a? label text%)
            (send label scale textfac)
            (send label scale (fitfac oct label)))
        (send label layer (send pad main-layer))
        (add label)
        (send label lower)))

    (define (bindvec v)
      (when (not (eq? (vector-length v) 8))
        (error "flow vector length must be 8, given " v))
      (let*
          ((ents (filter (lambda(ent) (and ent (is-a? (car ent) text%))) (vector->list v)))
           (facs (map (lambda(ent) (fitfac 0 (car ent))) ents)))
        (when (not (null? facs))
          (set! textfac (apply min facs))))
      (do
          ((i 0 (+ i 1)))
          ((= i 8))
        (let*
            ((ent (vector-ref v i))
             (label (and ent (car ent))))
          (when label
            (when _debug (printf "~a~%" ent))
            (bindoct i label)))))

    (field (octstack (list (make-vector 8 #f))))

    (define/public (hide-oct v)
      (do
          ((i 0 (+ i 1)))
          ((= i 8))
        (let*
            ((ent (vector-ref v i))
             (label (and ent (car ent))))
          (when label
            (let ((g (send label getgroup)))
              (when g (send g remove label)))
            (send* label
              (layer (send dynapad hidden))
              (writable? #f)
              (deletable? #f))))))

    (define/public (push v)
      (hide-oct (car octstack))
      (set! octstack (cons v octstack))
      (bindvec v))

    (define/public (pop)
      (hide-oct (car octstack))
      (set! octstack (cdr octstack))
      (when (not (null? octstack))
        (bindvec (car octstack))))

    (define/public debug
      (case-lambda
        (() (set! _debug (not _debug)))
        ((bool) (set! _debug bool))))

    (define/public (popdown)
      (layer (send dynapad hidden)))

    (define/public (popup)
      (layer (send dynapad main-layer)))

    (send dynapad bind "<Run-Double-ButtonPress-1>" #f)
    (send dynapad bind "<Double-ButtonPress-1>"
          (lambda(o e)
            (let
                ((x (event-x e))
                 (y (event-y e))
                 (z (caddr (position))))
              (position (list x y z))
              (popup))))

    (super-instantiate(pad))
    (dynaclass 'flow%)
    (members (list indicator inner outer))
    (for-each
     (lambda(x)
       (send* x
         (writable? #f)
         (deletable? #f)))
     (cons this (members)))
    (sticky "z")
    (raise)))
