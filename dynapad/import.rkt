#lang racket/base

(require compatibility/mlist
         dynapad/misc/misc
         dynapad/misc/alist
         )

(provide importing?
         push-deferred-expr
         set-load-context
         revert-load-context
         deferred-exprs
         order-by-phase
         (struct-out load-context))

;---- load-context stack
; The load-context stack stores frames for use by potentially-nested
; (import-set ...) and (restore-set ...) blocks.

(define-struct load-context
  (importing?    ; = 'import (default) or 'restore
   objs          ; set of all loaded objects
   deferred-exprs) ; set of exprs to eval after all objects loaded
  #:mutable)

(define *load-context-stack* null)  ;global stack: car is #t when importing file or clipboard

(define (importing?)
  (or (null? *load-context-stack*)
      (eq? 'import
           (load-context-importing? (mcar *load-context-stack*)))))

(define (set-load-context imp?)
  (mpush! (make-load-context imp? null null) *load-context-stack*))

(define (revert-load-context)
  (mpop! *load-context-stack*))

;(define (push-loaded-obj obj)
;  (let ((oldmlst (load-context-objs (mcar *load-context-stack*))))
;    (set-load-context-objs! (mcar *load-context-stack*)
;                (cons obj oldmlst))))

;(define (loaded-objs)
;  (load-context-objs (mcar *load-context-stack*)))

(define (push-deferred-expr phase expr)
  (let* ([rawml (load-context-deferred-exprs (mcar *load-context-stack*))]
         [oldmlst (if (mlist? rawml) ; FIXME we shouldn't need this?
                      rawml
                      (list->mlist rawml))])
    (pushq-onto-malist-val-always! phase expr oldmlst)
    (set-load-context-deferred-exprs! (mcar *load-context-stack*) oldmlst)))
;                     (cons expr oldmlst))))

(define (deferred-exprs)
  ;alist by phase ((0 expr...) (1 expr....) (n expr....))
  (let ([exprs (load-context-deferred-exprs (mcar *load-context-stack*))])
    (if (list? exprs)
        (list->mlist exprs)
        exprs)))

(define (order-by-phase mlst)
  ;mlst is ((0 x1 x2...) (1 x3 x4...) ...)
  ; returns appended list of all 0's, then 1's, etc
  ; requires a delicate dance between mlist and list
  (let ([phases (sort
                 (mlist->list mlst)
                 (lambda (phase-a phase-b)
                   (< (mcar phase-a) (mcar phase-b))))])
    (apply mappend (map mcdr phases))))

(module+ test
  (order-by-phase (mlist (mlist 2 'b)
                         (mlist 3 'c)
                         (mlist 1 'a))))
