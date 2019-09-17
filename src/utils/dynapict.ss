(require (lib "mrpict.ss" "texpict"))
(require (lib "etc.ss")) ;;for (rec)


;; Wrapper class stores dynapad object and an abstract pict struct
(define dynapict%
  (class object%
    (init-field _dynaobj)
    (field (_dynapict null)
           (_w (send _dynaobj width))
           (_h (send _dynaobj height))
           (_z (send _dynaobj z))
           (_a _h) ;; portion of height above top baseline
           (_d 0)) ;; portion of height below bottom baseline

    (super-instantiate())
    (when (is-a? _dynaobj text%)
      (set! _a (get-text-ascent _z))
      (set! _d (get-text-descent _z)))

    (set! _dynapict (make-pict 'draw _w _h _a _d '()))


    (define/public (pict) _dynapict)
    (define/public (dynaobj) _dynaobj)

    (define/public (bbe) (b0 (send _dynaobj bbox)))
    (define/public (bbs) (b1 (send _dynaobj bbox)))
    (define/public (bbw) (b2 (send _dynaobj bbox)))
    (define/public (bbn) (b3 (send _dynaobj bbox)))
    )
  )

(define (get-text-ascent zfac)
  (* (/ 7 9) (get-text-height zfac)))

(define (get-text-descent zfac)
  (* (/ 2 9) (get-text-height zfac)))

;; returns approximate height of obj at specified zfac
(define (get-text-height zfac)
  (define baseh 21.840003967285156) ;; height of text% obj at z=1 w/o newlines
  (* baseh zfac))


;; Calculations for appending/aligning
(define-values (vl-append-objs
                vc-append-objs
                vr-append-objs
                ht-append-objs
                hc-append-objs
                hb-append-objs
                htl-append-objs
                hbl-append-objs)
  (let ([append-objs
         (lambda (func-append)
           (rec *-append-objs
                (lambda (sep . args)
                  (unless (number? sep)
                    (set! args (cons sep args))
                    (set! sep 0))
                  (let* ([dynapicts (map (lambda (o) (make-object dynapict% o)) args)]
                         [picts (map (lambda (o) (send o pict)) dynapicts)]
                         [p (apply func-append sep picts)]
                         )
                    (slide-objects-append (pict-draw p) dynapicts)
                    ))))])
    (values
     (append-objs vl-append)
     (append-objs vc-append)
     (append-objs vr-append)
     (append-objs ht-append)
     (append-objs hc-append)
     (append-objs hb-append)
     (append-objs htl-append)
     (append-objs hbl-append))))


;; Calculations for superimposing
(define-values (lt-superimpose-objs
                lb-superimpose-objs
                lc-superimpose-objs
                ltl-superimpose-objs
                lbl-superimpose-objs
                rt-superimpose-objs
                rb-superimpose-objs
                rc-superimpose-objs
                rtl-superimpose-objs
                rbl-superimpose-objs
                ct-superimpose-objs
                cb-superimpose-objs
                cc-superimpose-objs
                ctl-superimpose-objs
                cbl-superimpose-objs)
  (let ([superimpose-objs
         (lambda (func-superimpose)
           (rec *-superimpose-objs
                (lambda args
                  (let* ([dynapicts (map (lambda (o) (make-object dynapict% o)) args)]
                         [picts (map (lambda (o) (send o pict)) dynapicts)]
                         [p (apply func-superimpose picts)]
                         )
                    (slide-objects-superimpose (cdddr (pict-draw p)) dynapicts)
                    ))))])
    (values
     (superimpose-objs lt-superimpose)
     (superimpose-objs lb-superimpose)
     (superimpose-objs lc-superimpose)
     (superimpose-objs ltl-superimpose)
     (superimpose-objs lbl-superimpose)
     (superimpose-objs rt-superimpose)
     (superimpose-objs rb-superimpose)
     (superimpose-objs rc-superimpose)
     (superimpose-objs rtl-superimpose)
     (superimpose-objs rbl-superimpose)
     (superimpose-objs ct-superimpose)
     (superimpose-objs cb-superimpose)
     (superimpose-objs cc-superimpose)
     (superimpose-objs ctl-superimpose)
     (superimpose-objs cbl-superimpose))))


(define (slide-objects-append pdraw args)
  (unless (null? (cdr args))
    (let* ([first (car args)]
           [second (cadr args)]
           [xyxy (parse-pict-draw-append pdraw)])

      (send (send second dynaobj) slide
            (+ (- (send first bbe) (send second bbe))
               (- (caddr xyxy) (car xyxy)))
            (+ (- (send first bbs) (send second bbs))
               (- (cadddr xyxy) (cadr xyxy))))

      (slide-objects-append (cadddr (cadr (cdddr pdraw))) (cdr args)))))


(define (slide-objects-superimpose pdraw args)
  (unless (null? (cdr args))
    (let* ([first (car args)]
           [second (cadr args)]
           [xyxy (parse-pict-draw-superimpose pdraw)])

      (send (send second dynaobj) slide
            (+ (- (send first bbe) (send second bbe))
               (- (caddr xyxy) (car xyxy)))
            (+ (- (send first bbs) (send second bbs))
               (- (cadddr xyxy) (cadr xyxy))))

      (slide-objects-superimpose (cdr pdraw) (cdr args)))))


(define (parse-pict-draw-append pdraw)
  (let* ([fourth (cadddr pdraw)]
         [fifth (cadr (cdddr pdraw))]
         [dx1 (cadr fourth)]
         [dy1 (caddr fourth)]
         [dx2 (cadr fifth)]
         [dy2 (caddr fifth)])
    (when (list? (cadddr fifth))
      (set! dx2 (+ dx2 (cadr (cadddr (cadddr fifth)))))
      (set! dy2 (+ dy2 (caddr (cadddr (cadddr fifth))))))
    (list dx1 dy1 dx2 dy2)
    ))


(define (parse-pict-draw-superimpose pdraw)
  (let* ([first (car pdraw)]
         [second (cadr pdraw)]
         [dx1 (cadr first)]
         [dy1 (caddr first)]
         [dx2 (cadr second)]
         [dy2 (caddr second)])
    (list dx1 dy1 dx2 dy2)
    ))

