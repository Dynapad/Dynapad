#lang racket/base

(require dynapad/misc/misc)

(provide lerp
         )
;--- lerp utilities ----------------------------------------------

;linear-interpolates between low and high by fraction t
(define (lerp t low high)
  [+ (* t high) (* (- 1 t) low) ])

(define (lerp-project x l1 h1 l2 h2)
  ; converts x, relative to range l1-h1, to corresponding range l2-h2
  (when (= l1 h1) (error "lerp source range is 0"))
  (let ((fract (/ (- x l1) (- h1 l1))))
    (lerp fract l2 h2)))

(define (view_lerp f old_vu new_vu)
  (def oldzoom (caddr old_vu))
  (def u1x (* (car old_vu) oldzoom))
  (def u1y (* (cadr old_vu) oldzoom))
  (def newzoom (caddr new_vu))
  (def u2x (* (car new_vu) newzoom))
  (def u2y (* (cadr new_vu) newzoom))
  (def v (exp (lerp f (log oldzoom) (log newzoom))))
  (def ux)
  (def uy)

  (if (let ((rat (/ oldzoom newzoom))) (and (> rat 0.95)(< rat 1.05)) )
      ; oldzoom and newzoom are nearly similar, simply lerp across slide
      (begin
        (set! ux (lerp f u1x u2x))
        (set! uy (lerp f u1y u2y)))
      ;else
      (begin
        (set! ux (+ u1x (* (- u2x u1x) (/ (- v oldzoom) (- newzoom oldzoom)))))
        (set! uy (+ u1y (* (- u2y u1y) (/ (- v oldzoom) (- newzoom oldzoom)))))))
  (list (/ ux v) (/ uy v) v)
  )

; slow-in, slow-out lerp
(define (siso_lerp t)
  (def t1 (* t t))
  (def t2 (- 1 (* (- 1 t) (- 1 t))))
  (def l (lerp t t1 t2))
  (lerp l t1 t2))

; slow-in, slow-out lerp, -1 to 1
(define (siso_lerp_mp t)
  (def t1 (* t t))
  (def tpositive (if (< t 0) (- t) t))
  (def t2 (- 1 (* (- 1 tpositive) (- 1 tpositive))))
  (def l (lerp tpositive t1 t2))
  (if (< t 0)
      (- (lerp l t1 t2))
      (lerp l t1 t2)))

