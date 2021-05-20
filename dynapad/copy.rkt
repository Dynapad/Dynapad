#lang racket/base

(require
 (only-in racket/class
          send
          send/apply
          is-a?
          object%)
 dynapad/pad-state
 dynapad/misc/misc
 dynapad/layout/bbox
 (only-in dynapad/undo-state use-load-context)
 (only-in dynapad/spd show-possible-delay)
 dynapad/seval
 (only-in dynapad/history/deferred-evals do-deferred-evals))

(provide Copy-Selected
         clone-object
         *copy-buffer*
         Offset-Object
         Copy-Buffer-empty?
         import-set
         import-set-core
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
       (let* ((build-exprs (send obj write-all))
              (res (eval #`(import-set #,@build-exprs))))
         #;
         (println (cons 'clone-object| |build-exprs: build-exprs))
         (unless (void? res)
           (car res)))))

(define (clone-objects set)
  (let ((build-exprs (write-set set)))
    (eval #`(import-set #,@build-exprs))))

;these set load-context if restore/import-path are bypassed
(define-syntax import-set
  (syntax-rules ()
    ((_ expr ...)
     (use-load-context 'import
                       (import-set-core expr ...)))))

(define-syntax import-set-core
  (syntax-rules ()
    ((_ expr ...)
     (show-possible-delay
      currentPAD
      (let* ((objs (filter (lambda (o) (is-a? o object%))
                           (map safe-eval (list expr ...))))
             ;build temp id->obj map:
             (idmap  (filter ;omit objs with no id
                      (lambda (pair) pair)
                      (map
                       (lambda (o) (let ((i (send o wasid)))
                                     (and i (cons i o))));(list i o))))
                       objs))))
        (do-deferred-evals (make-immutable-hash idmap))
        objs)))))
