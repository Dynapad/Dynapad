#lang racket/base

(require (only-in racket/class
                  send
                  send/apply)
         dynapad/pad-state
         (only-in dynapad/undo-state
                  import-set)
         dynapad/misc/misc
         dynapad/layout/bbox
         )

(provide Copy-Selected
         clone-object
         *copy-buffer*
         Offset-Object
         Copy-Buffer-empty?
         )
;-----

(define *copy-buffer* '())
(define (Copy-Selected)
  (set! *copy-buffer* (write-set (send currentPAD selected)))
  )

;  _______.  Nudges object along NE vector by <offset> fraction of its bbox
;      _. |  (Neg. offset moves toward SW)
;   __./| |
;     |   |
(define Offset-Object
  (case-lambda
    ((obj) (Offset-Object obj .1))
    ((obj offset)
     (let* ((mybb  (send obj bbox))
            (newbb (bbstretch mybb offset))
            (dxy   (map - (cddr newbb) (cddr mybb))))
       (send/apply obj slide dxy)))))


(define (Copy-Buffer-empty?) (null?  *copy-buffer*))

(define (clone-object obj)
  (and obj
       (let ((build-exprs (send obj write-all)))
         ; FIXME eval is not going to work the way it used to
         (car (eval `(import-set ,@build-exprs))))))

(define (clone-objects set)
  (let ((build-exprs (write-set set)))
    ; FIXME eval is not going to work the way it used to
    (eval `(import-set ,@build-exprs))))
