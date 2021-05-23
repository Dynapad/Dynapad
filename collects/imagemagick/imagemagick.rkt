#lang racket/base

(require (only-in racket/system process* system*))

(provide im/resize im/flip im/flop im/rotate im/identify
         im/convert im/mogrify im/composite im/montage)

(define identify (find-executable-path "identify" #f))
(when (not identify) (error "can't find identify"))
; Note - only provides width/height/type from identify

(define convert (find-executable-path "convert" #f))
(when (not convert) (error "can't find convert"))
(define (im/convert . args)
  (if (null? args) #f
      (apply system* convert args)))

(define mogrify (find-executable-path "mogrify" #f))
(when (not mogrify) (error "can't find mogrify"))
(define (im/mogrify . args)
  (if (null? args) #f
      (apply system* mogrify args)))

(define composite (find-executable-path "composite" #f))
(when (not composite) (error "can't find composite"))
(define (im/composite . args)
  (if (null? args) #f
      (apply system* composite args)))

(define montage (find-executable-path "montage" #f))
(when (not montage) (error "can't find montage"))
(define (im/montage . args)
  (if (null? args) #f
      (apply system* montage args)))

(define (im/resize src dst res)
  (apply system*
         (list convert "-geometry" (format "~ax~a" res res)
               "-filter" "Lanczos" src dst)))

(define (im/flip src dst)
  (apply system* (list convert "-flip" src dst)))

(define (im/flop src dst)
  (apply system* (list convert "-flop" src dst)))

(define (im/rotate src dst angle)
  (apply system* (list convert "-rotate" angle src dst)))

(define (im/identify src)
  (let*
      ((l (apply process* (list identify "-format" "%w %h %m" src)))
       (stdout (list-ref l 0))
       (stdin  (list-ref l 1))
       (pid    (list-ref l 2))
       (stderr (list-ref l 3))
       (proc (list-ref l 4))
       (result #f))
    (proc 'wait)
    (when (eq? (proc 'status) 'done-ok)
      (let
          ((width (read stdout))
           (height (read stdout))
           (type (read stdout)))
        (set! result (list width height type))))
    (close-input-port stdout)
    (close-output-port stdin)
    (close-input-port stderr)
    result))
