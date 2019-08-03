;(dynaload "ids.ss")

;--- basic do/undo mechanism -------------------------------------
;  many of these funcs are overridden in logs.ss

(define *undo-stack* '())
(define *redo-stack* '())

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

(define (undo) ;overridden in logs.ss
  (when (not (null? *undo-stack*))
    (let ((do_undo_pair (pop! *undo-stack*)))
      ; execute undo op
      (restore-set (cadr do_undo_pair))
      (push! do_undo_pair *redo-stack*)
    )
  )
)
(define (redo) ;overridden in logs.ss
  (when (not (null? *redo-stack*))
    (let ((do_undo_pair (pop! *redo-stack*)))
      ; execute redo op
      (restore-set (car do_undo_pair))
      (push! do_undo_pair *undo-stack*)
    )
  )
)

(define *beforedrag-prepare-undo-callbacks* null)
; This is NOT equivalent to a dynapad's _beforedrag-callbacks
;  Those are executed every time a drag is replayed off the redo stack;
;  THESE are executed only during the initial user-action,
; and prepare any conditions to restore pre-drag state
; onto the undo-stack

(define beforedrag-prepare-undo-callbacks
 (callback-accessor-functions *beforedrag-prepare-undo-callbacks*))

(define (do-beforedrag-prepare-undo-callbacks argPAD evnt objs)
  (foreach  *beforedrag-prepare-undo-callbacks*
	    (lambda (cb) ((car cb) argPAD evnt objs))))

(define *afterdrag-prepare-undo-callbacks* null)
(define afterdrag-prepare-undo-callbacks
 (callback-accessor-functions *afterdrag-prepare-undo-callbacks*))
(define (do-afterdrag-prepare-undo-callbacks argPAD evnt objs)
  (foreach  *afterdrag-prepare-undo-callbacks*
	    (lambda (cb) ((car cb) argPAD evnt objs))))
;==============================================
; OBJECT REGISTRATION:
; For redoable-creation,
; created obj must record its new id and reuse that id
;  every time it is re-created.


;Therefore, the do-op (expr on the redo stack)
; cannot contain a raw (make-object...);
; instead, it must contain a (rgstr-obj NEW-id '(build-str...))
;  Use macros below instead of (make-object...)
;  for pushing obj-creation do-ops onto stack.

;Naming conventions:
;pre-hoc:
; (undoable-... <expr>) has not yet eval'd expr (e.g. to create/delete objs)
;post-hoc:
; (undoify- ... <objs>) takes already-created objs, makes creation re/undoable

#|
(define-syntax undoable-make
  (syntax-rules ()
    ((_ cmd ...)
     (let ((newid (fresh-obj-id)))
       (push-ops-and-exec `(ic (,cmd ...) (id ,newid))
		   `(nuke-obj ,newid))))))

(define-syntax undoable-make-set
;args are series of object-making expressions
; e.g. (undoable-make-set '(make-object...) '(make-object...))
  (syntax-rules ()
    ((_ cmd ...)
     (let* ((do-undo-pairs
	     (map (lambda (expr)
		    (let ((newid (fresh-obj-id)))
		      (list `(ic ,expr (id ,newid))
			    `(nuke-obj  ,newid))))
		  `(,cmd ...)))
	    (dos (cons 'begin (map car do-undo-pairs)))
	    (undos (cons 'begin (map cadr do-undo-pairs))))
    (push-ops-and-exec dos undos)))))
|#
;=============

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
;			       `(send dynapad selected
;				      ,(cons 'list (map obj->IDexpr objs))))))
    (push-ops-and-exec redos undos)))

;-----------------------------------------------------------------
; Example 1 - slide command
;
; (define (gui-slide-objects object_list dx dy)
;   (let ((do-op   (lambda () (send-nd-objs object_list slide dx dy)))
;         (undo-op (lambda () (send-nd-objs object_list slide (- dx) (- dy))))
;         )
;     (push-ops-and-exec do-op undo-op) ))

;-----------------------------------------------------------------
; Example 2 - fill command
;
;(define (gui-fill-objects object_list newcolor)
;  (let* ((color_list (map (lambda (o) (send o fill)) object_list))
;         (undo-op (lambda () (fill-objects-aux object_list color_list)))
;         (do-op   (lambda () (fill-objects-aux object_list newcolor)))
;         )
;    (push-ops-and-exec do-op undo-op) ))
;
;(define (fill-objects-aux object_list color_or_colors)
;  (if (list? color_or_colors)
;    (for-each
;      (lambda (obj clr) (send-nd obj fill clr))
;      object_list
;      color_or_colors)
;
;    (for-each
;      (lambda (o) (send-nd o fill color_or_colors))
;      object_list)))
 

;-----------------------------------------------------------------
; Example 3 - delete command (move to "trash")
;
; (define (gui-delete-objects object_list)
;   (let ((do-op   (lambda () (delete-objects object_list)))
;         (undo-op (lambda () (undelete-objects object_list)))
;         )
;     (push-ops-and-exec do-op undo-op) ))
; 
; (define  main_layer (send dynapad main-layer))
; (define trash_layer (make-object layer% dynapad "trash"))
; (send trash_layer visible #f)
; 
; (define (delete-objects object_list)
;   (foreach object_list (lambda (o) 
;     (send-nd o layer trash_layer)
;     (send-nd o unselect))))
; (define (undelete-objects object_list)
;   (foreach object_list (lambda (o) 
;     (send-nd o layer main_layer)
;     (send-nd o select))))
; (define (empty-trash)
;   (foreach (send trash_layer members) (lambda (o) (send-nd o delete))))

;-----------------------------------------------------------------
; Example 4 - drag operation via mouse input
;

;(define *drag-do-op* #f)
;(define *drag-undo-op* #f)

; This function generates a closure which iterates
; over a list of individual closures.
; Is there a different, better way to do this?
;(define (gen-closure-to-reposition-objects object_list)
;  (define cmds_to_restore
;    (map
;      (lambda (o)
;        (let ((oldpos (send-nd o position)))
;          (lambda () (send-nd o position oldpos))))
;      object_list))
;
;  (new-lambda () (foreach cmds_to_restore (lambda (c) (c))))
;)
;
;(define (gen-closure-to-reposition-objects object_list)
;  (cons 'begin
;	(map (lambda (obj) 
;	       (let ((oldpos (send-nd obj position)))
;		 `(send ,obj position (list ,@oldpos))))
;	     object_list)))
;
;(define (store-positions-of-selected-objects-for-undo argPAD)
;  (set! *drag-undo-op* (gen-closure-to-reposition-objects (send argPAD selected))) )
;
;(define (store-positions-of-selected-objects-for-redo-and-push-on-stack argPAD)
;  (set! *drag-do-op* (gen-closure-to-reposition-objects (send argPAD selected)))
;  (push-ops-and-exec *drag-do-op* *drag-undo-op*) )


;(define (store-positions-of-specified-object-for-undo argPAD obj)
;  (set! *drag-undo-op* (gen-closure-to-reposition-objects (list obj))) )

;(define (store-positions-of-specified-object-for-redo-and-push-on-stack argPAD obj)
;  (set! *drag-do-op* (gen-closure-to-reposition-objects (list obj)))
;  (push-ops-and-exec *drag-do-op* *drag-undo-op*) )
 
;(define (store-positions-of-specified-object-for-redo-and-push-on-stack argPAD obj)
;  (set! *drag-do-op* (gen-closure-to-reposition-objects (list obj)))
;  (push-ops-and-exec *drag-do-op* *drag-undo-op*) )

; The above functions are incorporated into the binding definitions for
; "<Select-ButtonPress-1>" and "<Drag-ButtonRelease-1>"
; (see events.ss and event-binders.ss)

;======= Dan's new generalized versions:
; Accumulate many incremental changes in same undo/redo-op
;  by calling multiple (store-attr...)
;  and finishing with a (push-ops-no-exec)

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
  (make-event x y #f #f #f #f #f))

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

;====== Traps for non-logging =======
(define (load-log . args)
  (error "Must include logs.ss to read log files"))
