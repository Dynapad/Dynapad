#lang racket/base

(require (only-in racket/class send)
         dynapad/import
         dynapad/pad-state
         dynapad/misc/misc
         dynapad/history/ids
         ;dynapad/history/logs ; danger i suspect ; yep
         )

(provide *undo-stack*
         *redo-stack*
         push-*undo-stack*!
         push-*redo-stack*!
         pop-*undo-stack*!
         pop-*redo-stack*!
         push-undo-op
         push-redo-op
         ;push-ops-no-exec ; use logs version
         ;push-ops-and-exec ; use logs version
         clear-undo/redo
         abort-undo/redo

         Set-Select--undoable

         saveable-objects
         delete-all
         restore-path
         import-path

         maybe-wrap

         *redo-ops*
         set-*redo-ops*!
         push-*redo-ops*!

         *undo-ops*
         set-*undo-ops*!
         push-*undo-ops*!
         undoify-fresh-obj
         undoify-fresh-objs
         push-ops-no-exec
         use-load-context
         Start-Changing-Select--undoable
         Done-Changing-Select--undoable
         store-drag-batch-for-undo
         store-drag-batch-for-redo
         undoable-delete-objs

         get-top-group
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

(define (set-*undo-ops*! value) (set! *undo-ops* value))
(define (push-*undo-ops*! value) (push! value *undo-ops*))

(define (set-*redo-ops*! value) (set! *redo-ops* value))
(define (push-*redo-ops*! value) (push! value *redo-ops*))

;used to accumulate undo/redo exprs
; automatically wrapped in (begin... ) if needed
; reset when pushed (below)

; temporarility here for debug
(define (push-ops-and-exec do_op undo_op) ;overridden in logs.ss
    (push! (list do_op undo_op) *undo-stack*)
    (set! *redo-stack* '())
    (eval do_op)
    )

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

;======= Dan's new generalized versions:
; Accumulate many incremental changes in same undo/redo-op
;  by calling multiple (store-attr...)
;  and finishing with a (push-ops-no-exec)

(define (expr-to-set-obj-attr obj attr)
  ; no idea what this was supposed to be
  (error 'not-implemented))

(define (store-attribute-of-specified-object-for-undo argPAD attr obj)
  (let ((expr (expr-to-set-obj-attr obj attr)))
    (push! expr *undo-ops*)))

(define (store-attribute-of-specified-object-for-redo argPAD attr obj)
  (let ((expr (expr-to-set-obj-attr obj attr)))
    (push! expr *redo-ops*)))

(define (store-attribute-of-selected-objects-for-undo argPAD attr)
  (let ((newops (map (lambda (o) (expr-to-set-obj-attr o attr))
                     (send argPAD selected))))
    (set! *undo-ops* (append newops *undo-ops*))))

(define (store-attribute-of-selected-objects-for-redo argPAD attr)
  (let ((newops (map (lambda (o) (expr-to-set-obj-attr o attr))
                     (send argPAD selected))))
    (set! *redo-ops* (append newops *redo-ops*))))

(define (store-selection-for-undo objs)
  (push! `(send dynapad selected
                ,(cons 'list (map obj->IDexpr objs)))
         *undo-ops*)
  ;(say "undo-ops:" *undo-ops*)
  )

(define (store-selection-for-redo objs)
  (push! `(send dynapad selected
                ,(cons 'list (map obj->IDexpr objs)))
         *redo-ops*)
  ;(say "redo-ops:" *redo-ops*)
  )

(define (fake-event x y)
  (event x y #f #f #f #f #f))

(define (loggable? obj)
  #t) ;for now, log all objects
;  (send obj loggable?))

(define (undrag-batch-expr eventPAD evnt objs)
  ; alternative for undoing moves
  (set! objs (filter loggable? objs))
  (if (null? objs)
      #f
      `(begin ,@(map (lambda (obj)
                       `(send ,(obj->IDexpr obj) position
                              ,(cons 'list (send obj position)))) objs))))

(define (drag-batch-expr eventPAD evnt objs)
  ;LATER: figure out how to externalize eventPAD instead of 'dynapad
  (set! objs (filter loggable? objs))
  (if (null? objs)
      #f
      `(drag-batch dynapad
                   (fake-event ,(event-x evnt) ,(event-y evnt))
                   ,(cons 'list (map obj->IDexpr objs))
                   ,(cons 'list (map (lambda (o) (cons 'list (send o position)))
                                     objs)))))

(define (store-drag-batch-for-undo eventPAD evnt objs)
  (let ((expr  (undrag-batch-expr eventPAD evnt objs)))
    (when expr (push! expr *undo-ops*))))
(define (store-drag-batch-for-redo eventPAD evnt objs)
  (let ((expr  (drag-batch-expr eventPAD evnt objs)))
    (when expr (push! expr *redo-ops*))))

(define (ensure-region-claims-expr reg objs)
  `(ic (get-rgn ,(obj->IDexpr (send reg object)))
       (receive (list ,@(map obj->IDexpr objs)))
       (finish)))

(define (ensure-region-claims-after-undo reg . objs)
  (push! (ensure-region-claims-expr reg objs) *undo-ops*))

(define (ensure-region-unclaims-expr reg objs)
  `(ic (get-rgn ,(obj->IDexpr (send reg object)))
       (remove (list ,@(map obj->IDexpr objs)))
       (finish)))

(define (ensure-region-unclaims-after-undo reg . objs)
  (push! (ensure-region-unclaims-expr reg objs) *undo-ops*))


;; bits needed for changmode

; For one-step selection changes:
(define (Set-Select--undoable argPAD set)
  (when (not (list? set)) (set! set (list set)))
  (Start-Changing-Select--undoable argPAD)
  (send argPAD selected set)
  (Done-Changing-Select--undoable argPAD set))

; For two-step (i.e. button down-->up) changes:
(define (Start-Changing-Select--undoable argPAD . oldset)
  (let ((old (if (null? oldset)
                 (send argPAD selected)
                 (car oldset))))
    (send argPAD setvar 'old-selection old)
    (store-selection-for-undo old)))

(define (Done-Changing-Select--undoable argPAD . newset)
  (let ((new (if (null? newset)
                 (send argPAD selected)
                 (car newset))))
    (store-selection-for-redo new)))

;; more

(define (delete-expr-for-obj obj . deep?)
  (if (null? deep?)
      `(send ,(obj->IDexpr obj) delete)
      `(send ,(obj->IDexpr obj) delete-all)))

(define (redo-undo-pair-for-obj obj . deep?)
  (if (null? deep?)
      (list (send obj write) (delete-expr-for-obj obj))
      (list (cons 'begin (send obj write-all))
            (delete-expr-for-obj obj #t))))

(define (undoify-fresh-obj obj)
  ; Expects newly-created obj;
  ; registers it and pushes rebuild/delete script onto redo/undo stack
  ; also appearing in undo-hooks.rkt
  (apply push-ops-no-exec (redo-undo-pair-for-obj obj))
  obj)

(define (undoify-fresh-objs objs)
  ;Expects a list of newly-created objs;
  ; registers them and pushes rebuild/delete scripts onto the redo/undo stack
  ; Returns original object list
  (let* ((do-undo-pairs
          (map redo-undo-pair-for-obj objs))
         (redos (cons 'begin (map car do-undo-pairs)))
         (undos (cons 'begin (map cadr do-undo-pairs))))
    (push-ops-no-exec redos undos)
    objs))

(define (undoable-delete-obj obj . deep?)
  (apply push-ops-and-exec (reverse (apply redo-undo-pair-for-obj obj deep?))))

(define (undoable-delete-objs objs . deep?)
  (let* ((do-undo-pairs
          (map (lambda (o) (apply redo-undo-pair-for-obj o deep?)) objs))
         (redos (cons 'begin (map cadr do-undo-pairs)))
         (undos (cons 'begin
                      (append
                       (map car do-undo-pairs)
                       (list `(send dynapad selected
                                    ,(cons 'list (map obj->IDexpr objs))))))))
    ;    (set! redos (append redos (list
    ;                   `(send dynapad selected
    ;                      ,(cons 'list (map obj->IDexpr objs))))))
    (push-ops-and-exec redos undos)))

;; delete all

(define *abstract-objects-callbacks* null)
(define abstract-objects-callbacks (callback-accessor-functions *abstract-objects-callbacks*))

(define saveable-objects
  (case-lambda
    ((argPAD)
     (apply append (send argPAD objects)
            (map (lambda (cb) ((car cb)))
                 *abstract-objects-callbacks*)))
    (() (saveable-objects currentPAD))))

(define delete-all
  (case-lambda
    ((argPAD) (delete-all argPAD (saveable-objects argPAD)))
    ((argPAD objs) (foreach objs (lambda (o) (send o delete-all))))))

(define (restore-path path)
  (use-load-context 'restore ;needed here to set (importing?) context for load-set
                    ; also nested in restore-set
                    ;    (show-possible-delay currentPAD
                    (delete-all currentPAD) ;DO NOT move these inside restore-set-core
                    (clear-undo/redo)
                    ;       )
                    (load path)) ; file should call load-set --> restore-set
  path)

(define-syntax use-load-context
  (syntax-rules ()
    ((_ type do-sth ...)
     (begin
       (set-load-context type)
       (let ((result (begin do-sth ...)))
         (revert-load-context)
         result)))))

(define (false-event-lambda o e) #f)

; gets topmost group which takes events
(define (get-top-group obj)
  (if (and (not (send obj takegroupevents))
           (send obj getgroup))
      (get-top-group (send obj getgroup))
      obj))

; gets topmost group regardless of event handling
(define (get-topmost-group obj)
  (if (send obj getgroup)
      (get-topmost-group (send obj getgroup))
      obj))

;these set load-context if restore/import-path are bypassed
(define-syntax import-set
  (syntax-rules ()
    ((_ expr ...)
     (use-load-context 'import
                       (import-set-core expr ...)))))

(define (import-path path)  ; FIXME seems like this should be defined elsewhere?
  (use-load-context 'import
                    (load path))  ; file at path should call (load-set ...)
  path)
