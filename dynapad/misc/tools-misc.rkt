#lang racket/base

(provide round-to-int
         round-to-decimal
         sign
         increment-mean
         decrement-mean
         pct-string->number
         ; needed by dynapad/utils/geometry
         infinity
         -infinity
         hypotenuse
         2val-sign
         path-map
         path-template
         chain-map
         path-ref
         path-foreach
         chain-foreach
         path->list
         ring-map
         ring-template
         ring-ref
         ring-foreach
         mean
         weighted-mean
         ring-filter
         chain-filter
         ring->list
         other
         )

(require racket/class
         compatibility/defmacro
         scheme/math  ; alternately mzlib/math
         (for-syntax racket/base))

;---- STATS tools ----

(define (variance . lst)
  (let* ((mu (apply mean lst)))
    (/ (apply + (map (lambda (n) (sqr (- n mu))) lst)) (length lst))))

(define (mean . lst) (/ (apply + lst) (length lst)))

(define (weighted-mean vals wts)
  (/ (apply + (map * vals wts))
     (apply + wts)))

(define-macro (increment-mean mean n x)
  `(cset! ,mean
          (if (zero? ,n)
              (begin
                (++_ ,n)
                ,x)
              (/ (+ (* ,mean ,n) ,x) (++_ ,n)))))

(define-macro (decrement-mean mean n x)
  `(cset! ,mean (if (= 1 ,n)
                    (begin
                      (--_ ,n)
                      ,mean)
                    (/ (- (* ,mean ,n) ,x) (--_ ,n)))))

;---------------------

(define (other x eq-fn a b) ;returns whichever of a, b is not eqv? x
  (if (eq-fn x a)
      b
      a))

; x is a variable, toggles between vals a and b
(define-macro (toggle x eq-fn a b)
  `(set! ,x (other ,x ,eq-fn ,a ,b)))

(define-macro (toggleq x a b) `(toggle ,x eq? ,a ,b))
(define-macro (togglev x a b) '(toggle ,x eqv? ,a ,b))

;---- CHAIN tools ---
; A CHAIN is essentially a list, but with more direct control:
;  doesn't use cons cells; instead content elements (typically)
;  have next (and/or prev) ptr;
;  may be circularly linked

; +-----+ next-fn +-----+
; | chn |  ====>  | chn |  ====>...
; +-----+         +-----+
;    \               \
; stop-fn?      next-stop-fn? ...
;
(define (chain-map next-fn stop-fn next-stop-fn item-fn chn)
  (if (stop-fn chn)
      null
      (cons (item-fn chn)
            (chain-map next-fn next-stop-fn next-stop-fn item-fn (next-fn chn)))))

(define (chain-foreach next-fn stop-fn next-stop-fn item-fn chn)
  (when (not (stop-fn chn))
    (begin
      (item-fn chn)
      (chain-foreach next-fn next-stop-fn next-stop-fn item-fn (next-fn chn)))))

(define (chain-filter next-fn stop-fn next-stop-fn item-fn chn)
  (if (stop-fn chn)
      null
      (let* ((result (item-fn chn))
             (ahead (chain-filter next-fn next-stop-fn next-stop-fn item-fn (next-fn chn))))
        (if result
            (cons chn ahead)
            ahead))))

;returns first element to satisfy done-fn
(define (chain-first next-fn stop-fn next-stop-fn done-fn chn)
  (cond ((stop-fn chn) #f)
        ((done-fn chn) chn)
        (else (chain-first next-fn next-stop-fn next-stop-fn done-fn (next-fn chn)))))

(define (chain-ref next-fn stop-fn ref chn)
  (cond ((stop-fn chn) (error "chain-ref: index too large for chain"))
        ((zero? ref) chn)
        (else (chain-ref next-fn stop-fn (- ref 1) (next-fn chn)))))

;--- RINGS
; A RING is a circular-linked chain of objects with a "next" method
(define-macro (ring-template chain-fn item-fn rng)
  `(,chain-fn (lambda (it) (send it next))  ;next-fn
              (lambda (it) #f)              ;first stop-fn
              (lambda (it) (eq? it ,rng))   ;next-stop-fn
              ,item-fn ,rng))

(define-macro (ring-map fn rng)     `(ring-template chain-map ,fn ,rng))
(define-macro (ring-foreach fn rng) `(ring-template chain-foreach ,fn ,rng))
(define-macro (ring-filter fn rng)  `(ring-template chain-filter ,fn ,rng))
(define-macro (ring-first fn rng)   `(ring-template chain-first ,fn ,rng))

(define-macro (ring->list rng)  `(ring-map (lambda (it) it) ,rng))

(define (ring-ref rng n)
  (chain-ref (lambda (it) (send it next)) (lambda (it) #f) n rng))

;--- PATHS
; A PATH is a non-circular chain of objects with a "next" method,
;  whose last next-link is #f
(define-macro (path-template chain-fn item-fn rng)
  `(,chain-fn (lambda (it) (send it next)) ;next-fn
              not                          ;first-stop-fn
              not                          ;next-stop-fn
              ,item-fn ,rng))

(define-macro (path-map fn rng)     `(path-template chain-map ,fn ,rng))
(define-macro (path-foreach fn rng) `(path-template chain-foreach ,fn ,rng))
(define-macro (path-filter fn rng)  `(path-template chain-filter ,fn ,rng))
(define-macro (path-first fn rng)   `(path-template chain-first ,fn ,rng))

(define-macro (path->list rng)  `(path-map (lambda (it) it) ,rng))

(define (path-ref pth n)
  (chain-ref (lambda (it) (send it next)) (lambda (it) (not it)) n pth))

;---- STRING tools ----
(define percent-regexp (regexp "([0-9.]*)%"))
(define (pct-string->number strng) ; interprets strings like "xx.x%"
  (let* ((matches (regexp-match percent-regexp strng)))
    (if matches
        (/ (string->number (cadr matches)) 100)
        (string->number strng))))

;---- MATH tools ----
(define (round-to-int i)
  (inexact->exact (round i)))

(define (round-to-decimal n d)
  (let ((scale (expt 10 d)))
    (exact->inexact (/ (round (* n scale)) scale))))

;returns #t iff scalar b lies between a,c, inclusive
(define (between? a b c)
  (let* ((mx (max a c))
         (mn (min a c)))
    (and (<= b mx)
         (>= b mn))))
;returns #t iff scalar b lies between a,c, exclusive
(define (strictly-between? a b c)
  (let* ((mx (max a c))
         (mn (min a c)))
    (and (< b mx)
         (> b mn))))


;a,b,p are coord pairs '(xa ya)...
;returns #t iff p lies in box  with corners a,b
(define (pt-in-bbox? p a b)
  (and (between? (car a) (car p) (car b))
       (between? (cadr a) (cadr p) (cadr b))))

(define infinity 'infinity)
(define -infinity '-infinity)

(define-macro (infinity? x)
  `(eqv? ,x infinity))
(define-macro (-infinity? x)
  `(eqv? ,x -infinity))
(define-macro (infinite? x)
  `(or (infinity? ,x) (-infinity? ,x)))

;like max and min, but accomodate +/-infinity
(define (minfinity . lst)
  (let ((this (car lst)))
    (cond ((null? lst) #f)
          ((-infinity? this) this)
          (else (let ((min-ahead (apply minfinity (cdr lst))))
                  (cond ((or (-infinity? min-ahead)
                             (infinity? this))
                         min-ahead)
                        ((or (not min-ahead)
                             (infinity? min-ahead)
                             (< this min-ahead))
                         this)
                        (else min-ahead)))))))

(define (maxfinity . lst)
  (let ((this (car lst)))
    (cond ((null? lst) #f)
          ((infinity? this) this)
          (else (let ((max-ahead (apply maxfinity (cdr lst))))
                  (cond ((or (infinity? max-ahead)
                             (-infinity? this))
                         max-ahead)
                        ((or (not max-ahead)
                             (-infinity? max-ahead)
                             (> this max-ahead))
                         this)
                        (else max-ahead)))))))

(define (limit-between! x a b)
  (let ((lo (minfinity a b))
        (hi (maxfinity a b)))
    (set! x (maxfinity x lo)) ;raise if <min
    (set! x (minfinity x hi)) ;lower if >max
    x))

(define (sign n)
  (cond ((positive? n) 1)
        ((negative? n) -1)
        (else 0)))

(define (2val-sign n) ;unlike (sign x), returns 1 if x=0
  (if (negative? n) -1 1))

(define (hypotenuse dx dy)
  (sqrt (+ (sqr dx) (sqr dy))))

(define (distance p q)  ;p, q both lists of '(x y)
  (let ((dx2 (sqr (- (car p) (car q))))
        (dy2 (sqr (- (cadr p) (cadr q)))))
    (sqrt (+ dx2 dy2))))
