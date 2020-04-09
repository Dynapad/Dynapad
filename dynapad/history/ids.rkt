#lang racket/base

(require racket/class
         dynapad/misc/misc
         dynapad/ffs
         dynapad/import
         ;dynapad/history/undo
         ;dynapad/misc/alist
         ;dynapad/menu/menu_functions
         )

(provide obj->IDexpr
         set-max-padid
         import-expr-with-objs
         )

;---- IDs -----------
(define (set-max-padid new) ;use carefully...
  (unless (importing?)
    (when (< new *id-counter*) ;decreasing;
      ; if any entries remain in *id->obj-index,
      ; make sure new is increased past them
      (let* ((ids-still-in-use
              (hash-map *id->obj-index* (lambda (k v) k)))
             (max-id-in-use
              (if (null? ids-still-in-use) 0
                  (apply max ids-still-in-use))))
        (set! new (max new max-id-in-use))))
    ;   ;make sure index is cleared of all ids higher
    ; (set! *id->obj-index*
    ;    (filter (lambda (pair) (if (> (car pair) new) #f pair))
    ;    *id->obj-index*)))
    (set-*id-counter*! new)))
(define (max-padid new)
  ;May appear at the start of a load-set block;
  ; indicates the highest registered id in that block.
  ;ignore in import context, since all ids are remappped anyway
  ;  to a new id > *id-counter*
  (unless (importing?)
    (set-*id-counter*! (max *id-counter* new))))
; Remember that not all objects are registered (have an id),
; and unregistered objs may be created as a side effect of rebuilding
; registered ones.
; So to ensure that unregisted objs don't collide,
; max-padid should be called at the *start* of a restore-set, not end.


(define (lookup-id id idmap)
  (hash-ref idmap id false-fn))
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

(define ensure-id obj->id) ;probably obsolete

(define (obj->IDexpr obj)
  ;given an obj, returns expr which, when eval'd,
  ;  regenerates that obj from its id
  (let ((id (obj->id obj)))
    `(ObjID ,id)))

;(define (replace-IDatoms->objs arg idmap)
(define import-expr-with-objs
  (case-lambda
    ((expr) (import-expr-with-objs expr *id->obj-index*))
    ((expr idmap)
     ;recursively descends arg, converting all IDatoms to corresponding obj, via idmap
     (let ((id (IDatom->id expr))) ;try to convert to an id
       (cond (id (id->obj id idmap)) ;if id, replace with obj
             ((list? expr)         ;descend if list
              (map (lambda (subexpr) (import-expr-with-objs subexpr idmap))
                   expr))
             (else expr))))
    ))

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

