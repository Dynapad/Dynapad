
;---- load-context stack
; The load-context stack stores frames for use by potentially-nested
; (import-set ...) and (restore-set ...) blocks.

(define-struct load-context
  (importing?    ; = 'import (default) or 'restore
   objs          ; set of all loaded objects
   deferred-exprs)) ; set of exprs to eval after all objects loaded

(define *load-context-stack* null)  ;global stack: car is #t when importing file or clipboard
(define (importing?)
  (or (null? *load-context-stack*)
      (eq? 'import
	   (load-context-importing? (car *load-context-stack*)))))
(define (set-load-context imp?)
  (push! (make-load-context imp? null null) *load-context-stack*))
(define (revert-load-context)
  (pop! *load-context-stack*))
;(define (push-loaded-obj obj)
;  (let ((oldlst (load-context-objs (car *load-context-stack*))))
;    (set-load-context-objs! (car *load-context-stack*)
;			    (cons obj oldlst))))
;(define (loaded-objs)
;  (load-context-objs (car *load-context-stack*)))
(define (push-deferred-expr phase expr)
  (let ((oldlst (load-context-deferred-exprs (car *load-context-stack*))))
    (pushq-onto-alist-val-always! phase expr oldlst)
    (set-load-context-deferred-exprs! (car *load-context-stack*) oldlst)))
;				     (cons expr oldlst))))
(define (deferred-exprs)
  ;alist by phase ((0 expr...) (1 expr....) (n expr....))
  (load-context-deferred-exprs (car *load-context-stack*)))
(define (order-by-phase lst)
  ;lst is ((0 x1 x2...) (1 x3 x4...) ...)
  ; returns appended list of all 0's, then 1's, etc
  (let ((phases (sort
		 (lambda (phase-a phase-b)
		   (< (car phase-a) (car phase-b)))
		 lst)))
    (apply append (map cdr phases))
    ))

;---- IDs -----------
;modified to use hash-tbl instead of alist
(define *id->obj-index* (make-hash-table 'weak))
;master id index; alist of (id obj) pairs

(define *id-counter* 0)    ;highest id so far
(define (new-padid)        ;generate new id
  (++_ *id-counter*))      ;PREincrement
(define (set-max-padid new) ;use carefully...
  (unless (importing?)
	  (if (< new *id-counter*) ;decreasing;
	      ; if any entries remain in *id->obj-index,
	      ; make sure new is increased past them
	      (let* ((ids-still-in-use
		      (hash-table-map *id->obj-index* (lambda (k v) k)))
		     (max-id-in-use
		      (if (null? ids-still-in-use) 0
			  (apply max ids-still-in-use))))
		(set! new (max new max-id-in-use))))
	     ;   ;make sure index is cleared of all ids higher
	     ; (set! *id->obj-index*
		;    (filter (lambda (pair) (if (> (car pair) new) #f pair))
			;    *id->obj-index*)))
    (set! *id-counter* new)))
(define (max-padid new)
;May appear at the start of a load-set block;
; indicates the highest registered id in that block.
;ignore in import context, since all ids are remappped anyway
;  to a new id > *id-counter*
  (unless (importing?)
    (set! *id-counter* (max *id-counter* new))))
; Remember that not all objects are registered (have an id),
; and unregistered objs may be created as a side effect of rebuilding
; registered ones.
; So to ensure that unregisted objs don't collide,
; max-padid should be called at the *start* of a restore-set, not end.


(define (lookup-id id idmap)
  (hash-table-get idmap id false-fn))
;  (let ((tuple (assq id idmap))) ;assq assumes ids are exact integers
;;dont trap error here; return #f if not found
;    (and tuple (cadr tuple))))

(define id->obj
  (case-lambda
   ((id)       (or (lookup-id id *id->obj-index*)    ;use master index
		   (error "No object with id " id))) ;else fail
   ((id index) (or (lookup-id id index)  ;use provided index first,
		   (id->obj id)))  ; then try master index
))
(define ObjID id->obj) ;terse version for save files
(define objid id->obj) ;sadly, this variation propogated in log files
                       ;  when case-sensitivity was off in PLT

(define (obj->id obj)
  (send obj id))
(define ensure-id obj->id) ;probably obsolete

(define (obj->IDexpr obj)
;given an obj, returns expr which, when eval'd,
;  regenerates that obj from its id
  (let ((id (obj->id obj)))
    `(ObjID ,id)))

(define (obj->IDatom obj)
;given an obj, returns non-eval'able encoding of an id,
;  to be deferenced later
;Currently = "#id"
  (id->IDatom (obj->id obj)))

(define (id->IDatom idnum)
  (and idnum
       (format "#~a" idnum)))

(define IDatom-rexp (regexp "^#"))

(define (IDatom->id atom)
  (and (string? atom)
       (regexp-match IDatom-rexp atom)
       (string->number (substring atom 1))))

(define (register-obj-id obj id)
; error + return #f if already taken;
; else returns id
  (let ((found (hash-table-get *id->obj-index* id false-fn)))
	 ;(assq id *id->obj-index*)))
    (if found
	(handle-id-conflict id found obj)
	(begin
	  ;(push! (list id obj) *id->obj-index*)
	  (hash-table-put! *id->obj-index* id obj)
	  id))))

(define (unregister-id id)
  (hash-table-remove! *id->obj-index* id))
;  (let ((found (assq id *id->obj-index*)))
;    (if found
;	(set! *id->obj-index* (remq found *id->obj-index*)))))

(define (handle-id-conflict id oldobj newobj)
;  (send newobj delete)
  (error "id collision (id) (old) (new) = " id oldobj newobj)
  #f)

(define (revert-id obj oldid)
  (cond ((importing?)  ;importing causes remap; generate newid
	   (register-obj-id obj (new-padid)))
	((register-obj-id obj oldid) ;try to register with oldid
	     oldid) ;ok, use oldid
	(else #f))) ;(handle-id-conflict obj oldid))))

;--- Load contexts : restore, import ------------

; Importing: remap all ids, keep padid counter
; Restoring: advance padid counter>max, keep all ids

(define (current-error-ports) ;may be overridden to cc errors elsewhere
  (list (current-error-port)))

(define (safe-eval expr)
;serves as a reentry point in case of errors during eval
  (with-handlers
;   ([exn:user? (lambda (exn)
   ([exn:fail? (lambda (exn)
		 (foreach (current-error-ports)
		     (lambda (port)
		       (fprintf port  "Error (~a) in ~a~%" (exn-message exn) expr)))
		 #f)])
   (eval expr)))

(define-syntax use-load-context
  (syntax-rules ()
    ((_ type do-sth ...)
     (begin
       (set-load-context type)
       (let ((result (begin do-sth ...)))
	 (revert-load-context)
	 result)))))

(define (restore-path path)
  (use-load-context 'restore ;needed here to set (importing?) context for load-set
			     ; also nested in restore-set
;    (show-possible-delay currentPAD
	   (delete-all currentPAD) ;DO NOT move these inside restore-set-core
	   (clear-undo/redo)
;	   )
    (load path)) ; file should call load-set --> restore-set
  path)

(define-syntax restore-set-core
  (syntax-rules ()
    ((_ expr ...)
     (show-possible-delay currentPAD
       (let* ((objs (filter (lambda (o) (is-a? o object%))
			    (map safe-eval (list expr ...)))))
	 (do-deferred-evals *id->obj-index*)
	 objs)))))

(define-syntax import-set-core
  (syntax-rules ()
    ((_ expr ...)
     (show-possible-delay currentPAD
       (let* ((objs (filter (lambda (o) (is-a? o object%))
			    (map safe-eval (list expr ...))))
				;build temp id->obj map:
	      (idmap  (filter ;omit objs with no id
		       (lambda (pair) pair)
		       (map
			(lambda (o) (let ((i (send o wasid)))
				      (and i (cons i o))));(list i o))))
			objs))))
	 (do-deferred-evals (make-immutable-hash-table idmap))
	 objs)))))

(define (import-path path)
  (use-load-context 'import
    (load path))  ; file at path should call (load-set ...)
  path)

(define-syntax load-set
  (syntax-rules ()
    ((_ obj ...)
     (if (importing?)
	 (import-set-core obj ...)
	 (restore-set-core obj ...)))))

;these set load-context if restore/import-path are bypassed
(define-syntax import-set
  (syntax-rules ()
    ((_ expr ...)
     (use-load-context 'import
       (import-set-core expr ...)))))

(define-syntax restore-set
  (syntax-rules ()
    ((_ expr ...)
     (use-load-context 'restore
       (restore-set-core expr ...)))))


; ------------ Linkable object base class -------------------
(define linkable-obj%
  (class object%
     (super-instantiate ())
     (field (_id #f))
     (field (_wasid #f))
     (public delete id wasid refer-when-ready defer-send)

     (define (delete)
       (if _id (unregister-id _id)))

     (define id
       (case-lambda
	;auto-allocate ids on demand
	(() (if (not _id)
		(set! _id (register-obj-id this (new-padid))))
	 _id)
	((wasid) 
	 (set! _wasid wasid)
	 ;importing-->renumber; restoring->reuse:
	 (set! _id (revert-id this wasid)))
	))
     (define (wasid) _wasid)

     ;this replaces refer-when-ready
     ;overrides global defer-send (see below), assumes target is this
     (define defer-send
       (case-lambda
	((expr) (defer-send 0 expr)) ;default phase=0
	((phase expr) ;phase determines order of eval: all 0's, then all 1's, etc
       ;expr must be non-null list:
       ;("#id" msg . args) or (msg . args) where args may include nested "#id"s
       ;Maybe needs error trapping on expr?
       (let ((head (car expr))
	     (args (cdr expr)))
	 (if (IDatom->id head) ; ("#id" msg . args); send to obj "#id" instead
	   (push-deferred-expr phase ; as with global defer-send
	    `(send ,head ,@args))
	   (push-deferred-expr phase ; (msg . args); send to this
	    `(send ,this ,head ,@args)))))
	))

     ;----------- obsolete, keep for back-compat: --------------------------
     (define (refer-when-ready . args)
       ; args format is (msg . args) or (id msg . args) where args may include ids
       ; and all ids are raw nums;
       ;  must convert to new format "#N", or whatever id->IDatom yields
       ;  Also: nested lists of ids must have leading 'list renewed
       (let ((wrap-ids (map wrap-all-nums args)))
	 (send this defer-send wrap-ids)))

     ; temp fn to help refer-when-ready convert:
     (define (wrap-all-nums arg)
       (cond ((null? arg) null)
	     ((number? arg) (id->IDatom arg))
	     ((list? arg) ;(cons 'list (map wrap-all-nums arg)))
	      (if (number? (car arg))
		  (cons 'list (map wrap-all-nums arg))
		  (map wrap-all-nums arg)))
	     (else arg)))

))

(define defer-eval
  (case-lambda
   ((expr) (push-deferred-expr 0 expr))
   ((phase expr)
    (push-deferred-expr phase expr))
   ))

;(define (defer-send expr)
(define defer-send
  ;global version of linkable-obj% message defer-send
  (case-lambda
   ((expr) (defer-send 0 expr))
   ((phase expr)
  ; expr must be format ("#id" msg . args)
    (if (IDatom->id (car expr)) ;is first arg an obj ref?
	(push-deferred-expr phase `(send ,@expr))
	(error "No target for defer-send in " expr)))
   ))

;(define (replace-IDatoms->objs arg idmap)
(define import-expr-with-objs
  (case-lambda
   ((expr) (import-expr-with-obj expr *id->obj-index*))
   ((expr idmap)
;recursively descends arg, converting all IDatoms to corresponding obj, via idmap
    (let ((id (IDatom->id expr))) ;try to convert to an id
      (cond (id (id->obj id idmap)) ;if id, replace with obj
	    ((list? expr)         ;descend if list
	     (map (lambda (subexpr) (import-expr-with-objs subexpr idmap))
		  expr))
	    (else expr))))
))


;It seems tempting to write a full "export..." to replace all objs with ids,
; but it's not worth it; better to let the caller structure the syntax
; with the help of export-objs below


(define (export-objs objs)
; converts a single list of objs to '(list "#id" "#id"...)
; of a single obj to "#id"
  (if (list? objs)
      (cons 'list (map obj->IDatom objs))
      (obj->IDatom objs)))

(define *debug-deferred-exprs* null) ;delete this later

(define (do-deferred-evals idmap)
(set! *debug-deferred-exprs* (deferred-exprs)) ;delete this later
  (map
;(lambda (e) (printf "~a~%" e) (eval e))
   eval
       (map (lambda (expr) (import-expr-with-objs expr idmap))
	    (order-by-phase (deferred-exprs)))))

#|
;Example:

(define linkdemo%
  (class linkable-obj%
     (super-instantiate ())

     (public-field refs _refs null)

     (define/public (write)
       `(ic (make-object linkdemo%)
	    (id ,(send this id))
	    (defer-send `(refs (list ,@(map obj->id _refs))))))
))

(define a (make-object linkdemo%))
(define b (make-object linkdemo%))
(send a refs b)
(send b refs a)

; Copy a and b:

(define new (import-set (send a write) (send b write)))
(define aprime (car new))
(define bprime (cadr new))

;Proof that copies linked correctly:

(memq bprime (send aprime refs))  ;--> (<bprime>)
(memq aprime (send bprime refs))  ;--> (<aprime>)

|#
       
