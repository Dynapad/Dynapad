#lang racket/base

;(require (lib "process.ss")) ; this was mzlib/process, maybe racket/system has what we need?
(require racket/class
         racket/system
         ;dynapad/pdf
         dynapad/image
         (only-in dynapad/pad-state
                  event-x
                  event-y)
         (only-in dynapad/misc/misc
                  sendf)
         dynapad/misc/progress
         dynapad/utils/handle
         (only-in dynapad/layout/bbox
                  b0
                  b1
                  b2
                  b3
                  )
         ;dynapad/image-utils/metadata
         collects/misc/pathhack
         ;collects/imagemagick/imagemagick
         collects/pdfrip/pdfrip
         ;collects/pdfrip/misc
         ;collects/thumbify/thumbify
         )

(provide limit-drag-to-bbox
         )

(announce-module-loading "PDF features...")



; never mind; use (make-object imagedata%..)  instead
#|
(define *image-size-regexp* (regexp "^[^ ]+ [A-Z]+ ([0-9]+)x([0-9]+) "))
(define (find-image-size imagepath)
  (let* ((strng (read-line (system (format "identify ~a" imagepath))))
         (match (regexp-match *image-size-regexp* strng)))
    (and match (list (string->number (cadr match))
                     (string->number (caddr match))))))
|#


(define (limit-drag-to-bbox obj bbox-fn union?)
  ;prevents obj from being dragged outside of bbox
  ;  bbox-fn is (lambda (obj) ...) which returns bbox enclosure
  ;  (can change during drag if needed)
  (let ((n-fn (if union? b1 b3))
        (s-fn (if union? b3 b1))
        (e-fn (if union? b0 b2))
        (w-fn (if union? b2 b0)))
    (send obj bind "<B1-Motion>"
          (lambda (eventPAD e)
            (let* ((last_x (sendf eventPAD evs lastx))
                   (last_y (sendf eventPAD evs lasty))
                   (x (event-x e))
                   (y (event-y e)))
              (if (and last_x last_y)
                  (let* ((bb-now (send obj bbox))
                         (bb-limit (bbox-fn obj))
                         (max-up (- (b3 bb-limit) (n-fn bb-now)))
                         (max-lf (- (b0 bb-limit) (w-fn bb-now)))
                         (max-rt (- (b2 bb-limit) (e-fn bb-now)))
                         (max-dn (- (b1 bb-limit) (s-fn bb-now)))
                         (dx (- x last_x))
                         (dy (- y last_y)))
                    (if (negative? dx)
                        (set! dx (max max-lf dx))
                        (set! dx (min max-rt dx)))
                    (if (negative? dy)
                        (set! dy (max max-dn dy))
                        (set! dy (min max-up dy)))
                    (send eventPAD do-duringdrag e)
                    (for-each
                     (lambda (o) (send o slide dx dy))
                     (send eventPAD getvar 'dragset))
                    (sendf eventPAD evs set-last-xy (+ last_x dx) (+ last_y dy)) )
                  (sendf eventPAD evs set-last-xy x y))
              #f ;override existing <Drag-B1-Motion> binding
              )))
    (send obj afterposition-callbacks 'add
          (lambda (obj xyz)
            (let* ((bb (send obj bbox))
                   (cage (bbox-fn obj))
                   (n (b3 cage))
                   (s (b1 cage))
                   (e (b2 cage))
                   (w (b0 cage)))
              (when (< n (n-fn bb)) (send obj bbox (list #f n #f #f))) ;snap to n
              (when (< e (e-fn bb)) (send obj bbox (list e #f #f #f))) ;snap to e
              (when (> s (s-fn bb)) (send obj bbox (list #f #f #f s))) ;snap to s
              (when (> w (w-fn bb)) (send obj bbox (list #f #f w #f))) ;snap to w
              ))
          )))

#|
(define (make-object-resizable obj)
  (let ((border (ic (make-object rect% dynapad (send obj bbox))
                    (fill "none")
                    (pen "white")
                    (transparency .5))))
    (send obj afterslide-callbacks 'add
          (lambda (o dx dy) (send border slide dx dy)))
    (resizer-bindings (send obj dynapad)
                      border
                      (lambda (frame bb) (send obj bbox bb)))))

(define draglimit-container%
  (class frame-container%
    (init dynaptr)
    (init (frame-arg #f))
    (inherit dynaclass)
    (super-instantiate (dynaptr frame-arg))
    (dynaclass 'draglimit-container%)

    (define (get-my-frame-bbox obj)  ;doesnt use obj arg
      (send (send this frame) bbox))

    (define/override (add obj)
      (limit-drag-to-bbox obj get-my-frame-bbox)
      (if (is-a? obj base-formation%)
          (foreach (send obj named-parts)
                   (lambda (prt)
                     (limit-drag-to-bbox prt get-my-frame-bbox))))
      (super add obj))

    (define/override (remove obj)
      (send obj bind "<Drag-B1-Motion>" #f)
      (super remove obj))

    (define/override contents
      (case-lambda
        (() (super contents))
        ((lst) (foreach lst (lambda (o) (limit-drag-to-bbox o get-my-frame-bbox))))))
    ))
|#

#|
(define thumbify-via-mogrify
  ;assumes dir/thumbs/size exists already
  (case-lambda
    ((filepath) (thumbify-via-mogrify filepath 125))
    ((filepath maxsize)
     (set! maxsize (round-to-int maxsize))
     (let-values (((dir name junk) (split-path->string filepath)))
       (let* ((newpath (format "~athumbs/~a/~a" dir maxsize
                               (insert-filenamebase-suffix
                                name (format "-~a" maxsize))))
              (copy-cmd (format "cp ~a ~a" filepath newpath))
              (mogrify-cmd (format "mogrify -format jpg -geometry ~ax~a ~a"
                                   maxsize maxsize newpath)))
         (system copy-cmd)
         (system mogrify-cmd))))))

|#
(update-progress 1)
