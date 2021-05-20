#lang racket/base

(require (only-in racket/class
                  send
                  send/apply)
         dynapad/pad-state
         dynapad/misc/misc
         dynapad/layout/bbox
         (only-in dynapad/undo-state use-load-context)
         (only-in dynapad/spd show-possible-delay)
         dynapad/seval
         #; ; FIXME induces a cycle
         (for-syntax (only-in dynapad/history/logs do-deferred-evals))
         )

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

(define-namespace-anchor anchor-copy)
(define namespace-copy (namespace-anchor->namespace anchor-copy))

(define (clone-object obj)
  (and obj
       (let* ((build-exprs (send obj write-all))
              (res (parameterize ([current-namespace anchor-copy])
                     (eval #`(import-set #,@build-exprs)))))
         ; FIXME eval is not going to work the way it used to
         (println build-exprs)
         (unless (void? res)
           (car res)))))

(define (clone-objects set)
  (let ((build-exprs (write-set set)))
    ; FIXME eval is not going to work the way it used to
    (parameterize ([current-namespace anchor-copy])
      (eval #`(import-set #,@build-exprs)))))

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
        ; FIXME do-deferred-evals is defined in dynapad/history/logs but we
        ; don't require it here due to circularity I think?
        ; similar issue with make-immutable-hash-table? no, miht is just an old name for make-immutable-hash
        (displayln "ERROR HAPPENS AFTER THIS LINE")
        (do-deferred-evals (make-immutable-hash idmap)) ; XXXXXXXXXXXXX
        (displayln "ERROR HAPPENS BEFORE THIS LINE")
        objs)))))

