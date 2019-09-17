(dynaload "utils/lerp.ss")
(dynaload "tools-misc.ss") ;need: round-to-int

(define (tcl-color-from-mred-color mred-color)
  (cond ((eq? mred-color #f) #f)
        (else
          (let ((red (send mred-color red))
                (green (send mred-color green))
                (blue (send mred-color blue)))
        (tcl-color-from-RGB red green blue)))))

(define (tcl-color-from-RGB red green blue)
           (format "#~a~x~a~x~a~x"
             (if (< red 16) "0" "") red
             (if (< green 16) "0" "") green
             (if (< blue 16) "0" "") blue))

(define (mred-color-from-tcl-color tcl-color)
  (cond ((equal? tcl-color "none") #f)
        ((equal? tcl-color "white") (make-object color% 255 255 255))
        ((equal? tcl-color "red")   (make-object color% 255   0   0))
        ((equal? tcl-color "green") (make-object color%   0 255   0))
        ((equal? tcl-color "blue")  (make-object color%   0   0 255))
        ((equal? tcl-color "black") (make-object color%   0   0   0))
        ((equal? tcl-color "yellow")(make-object color% 248 252   0))
        ((equal? tcl-color "purple")(make-object color% 160  32 240))
        ((equal? tcl-color "orange")(make-object color% 248 164   0))
        ((equal? (substring tcl-color 0 1) "#")
          (make-object color%
            (string->number (substring tcl-color 1 3) 16)
            (string->number (substring tcl-color 3 5) 16)
            (string->number (substring tcl-color 5 7) 16)))
        (else (make-object color% 0 0 0)))) ; black as default on error

(define dynacolor%
  (class color%
    (init (r-arg 0)
          (g-arg 0)
          (b-arg 0))
    (if (string? r-arg)
        (let ((temp (mred-color-from-tcl-color r-arg)))
          (super-instantiate ((send temp red) (send temp green) (send temp blue))))
        (super-instantiate (r-arg g-arg b-arg)))

    (define/public (tcl-color)
      (tcl-color-from-mred-color this))

    ; independently settable RGB:
    (define/override red
      (case-lambda
       (() (super red))
       ((r) (send this set r (send this green) (send this blue)))))
    (define/override green
      (case-lambda
       (() (super green))
       ((g) (send this set (send this red) g (send this blue)))))
    (define/override blue
      (case-lambda
       (() (super blue))
       ((b) (send this set (send this red) (send this green) b))))

    (define/override set
      (case-lambda
       ((str) (cond ((string? str) (send this copy-from (mred-color-from-tcl-color str)))
                    ((list? str) (send/apply this set str))
                    (else (super set str str str))))
       ((r g b) (super set r g b))))

    (define/public rgb
      (case-lambda
       (() (list (send this red) (send this green) (send this blue)))
       ((val) (set val))
       ((r g b) (set r g b))))

    (define/public (eps-color) ;list of fractions 0-1
      (map (lambda (v) (round-to-decimal (/ v 255) 4)) (rgb)))

    (define/public blend
      ;returns new dynacolor whose rgb are interpolated by fraction t
      ; toward color
      (case-lambda
       ((t color)
          (when (string? color) (set! color (make-object dynacolor% color)))
          (send/apply this blend t (send color rgb)))
       ((t r g b)
         (when (> t 1) (set! t 1))   ;limit between -1 and 1
         (when (< t -1) (set! t -1))
         (apply make-object dynacolor%
               (map round-to-int
                    (map lerp (list t t t) (send this rgb) (list r g b)))))))

    (define/public (invert)
      (apply make-object dynacolor%
        (map (lambda (val) (- 255 val)) (send this rgb))))

    (define/public (lighten t)
      (blend t 255 255 255))

    (define/public (darken t)
      (blend t 0 0 0))
))

