#lang racket/base

(require dynapad/misc/misc)

(provide *undo-stack*
         *redo-stack*
         push-*undo-stack*!
         push-*redo-stack*!
         pop-*undo-stack*!
         pop-*redo-stack*!
         push-undo-op
         push-redo-op
         push-ops-no-exec
         clear-undo/redo
         abort-undo/redo
         )

;--- basic do/undo mechanism -------------------------------------
;  many of these funcs are overridden in logs.ss

(define *undo-stack* '())
(define *redo-stack* '())


(define (push-*undo-stack*! value) (push! value *undo-stack*))
(define (push-*redo-stack*! value) (push! value *redo-stack*))

(define (pop-*undo-stack*!) (pop! *undo-stack*))
(define (pop-*redo-stack*!) (pop! *redo-stack*))

(define (clear-undo/redo)
  (set! *undo-stack* '())
  (set! *redo-stack* '()))

(define *undo-ops* '())
(define *redo-ops* '())
;used to accumulate undo/redo exprs
; automatically wrapped in (begin... ) if needed
; reset when pushed (below)

;(define (push-ops-and-exec do_op undo_op) ;overridden in logs.ss
;    (push! (list do_op undo_op) *undo-stack*)
;    (set! *redo-stack* ())
;    (eval do_op)
;    )

(define (push-undo-op expr)
  (push! expr *undo-ops*))

(define (push-redo-op expr)
  (push! expr *redo-ops*))

(define (abort-undo/redo)
  (set! *undo-ops* '())
  (set! *redo-ops* '()))

(define (maybe-wrap ops)
  (cond ((null? ops) null)
        ((null? (cdr ops)) (car ops)) ;single op, no 'begin needed
        (else (cons 'begin ops)))) ; >1 ops, wrap in ('begin ...)

(define push-ops-no-exec ;overridden in logs.ss
  (case-lambda
    (()  ;if no ops specified, use *undo-ops* and *redo-ops*,
     ; which are assumed to be equal length >=0
     (unless (and (null? *undo-ops*) (null? *redo-ops*))
       (push! (list (maybe-wrap *redo-ops*)
                    (maybe-wrap *undo-ops*)) *undo-stack*)
       (set! *redo-stack* null)
       (set! *undo-ops* null)
       (set! *redo-ops* null)))
    ((do_op undo_op)
     (push! do_op *redo-ops*)
     (push! undo_op *undo-ops*)
     (push-ops-no-exec))
    ))
