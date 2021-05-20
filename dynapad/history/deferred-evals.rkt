#lang racket/base

(require
 #; ; XXX this is left as an example and a reminder about a error that reads
 ; cannot instantiate `racket/gui/base' a second time in the same process
 ; for some reason the way dynapad starts the gui process it does so manually
 ; and somehow manages to do it twice if certain modules are required at syntax time
 ; if you encounter issues after adding a for-syntax require, it is probably related
 (for-syntax dynapad/import)
 (only-in dynapad/import
          deferred-exprs
          order-by-phase)
 (only-in dynapad/history/ids import-expr-with-objs)
 (only-in compatibility/mlist mmap))

(provide do-deferred-evals)

#|
(define (deferred-exprs) '())
(define (order-by-phase l) l)
(define (import-expr-with-objs a b) (void))
(define (mmap f r) (void))
|#

(define *debug-deferred-exprs* null) ;delete this later
(define (do-deferred-evals idmap)
  (set! *debug-deferred-exprs* (deferred-exprs)) ;delete this later
  (mmap
   #;
   (lambda (e) (printf "~a~%" e) (eval e))
   eval
   (mmap (lambda (expr) (import-expr-with-objs expr idmap))
         (order-by-phase (deferred-exprs)))))

