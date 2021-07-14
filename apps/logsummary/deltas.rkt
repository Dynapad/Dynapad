(define _memorized-set null)

(define (memorize-set-before-move set)
  (set! _memorized-set
        (map (lambda (o) (list o
                               (send o xy))) set)))
(define (memorize-more set)
  (set! _memorized-set
        (append _memorized-set
                (map (lambda (o) (list o (send o xy))) set))))

(define (compute-deltas)
  (filter
   (lambda (x) x)
   (map (lambda (tuple)
          (let* ((obj (car tuple))
                 (old (cadr tuple))
                 (new (if (send obj deleted?) #f (send obj xy))))
            (if (or (not new)
                    (equal? old new))
                #f
                (append tuple (list new)))))
        _memorized-set)))

(define (build-delta-arcs)
  (map (lambda (tuple)
         (let ((from (cadr tuple))
               (to (caddr tuple)))
           (ic (make-object line% dynapad (append from to))
               (pen "red"))))
       (compute-deltas)))

(define permalight%
  (class rect%
    (init object-arg)
    (super-instantiate ((send object-arg dynapad) (send object-arg bbox)))
    (send this dynaclass 'permalight%)
    (send this pen "green")
    ))

(define permaring%
  (class oval%
    (init object-arg)
    (let* ((obb (send object-arg bbox))
           (cx  (bxc obb))
           (cy  (byc obb))
           (r (* .5 (bb-diag obb))))
      (super-instantiate
       ((send object-arg dynapad)
        (list (- cx r) (- cy r) (+ cx r) (+ cy r)))))
    (send this dynaclass 'permaring%)
    (send this pen "green")
    ))

(define (make-permalights objs)
  (map (lambda (o) (make-object permaring% o)) objs))
