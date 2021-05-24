#lang racket/base

(require racket/class
         (only-in mzscheme
                  make-hash-table)
         dynapad/misc/misc
         dynapad/import
         )

(provide export-objs
         obj->id
         id->IDatom
         IDatom->id
         linkable-obj%
         *id->obj-index* 
         new-padid
         *id-counter*
         set-*id-counter*!
         defer-send
         )

(define *id-counter* 0)    ;highest id so far
(define (new-padid)        ;generate new id
  (++_ *id-counter*))      ;PREincrement
(define (set-*id-counter*! value) (set! *id-counter* value))

;modified to use hash-tbl instead of alist
(define *id->obj-index* (make-hash-table 'weak))
;master id index; alist of (id obj) pairs

(define (obj->id obj)
  (send obj id))

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

;It seems tempting to write a full "export..." to replace all objs with ids,
; but it's not worth it; better to let the caller structure the syntax
; with the help of export-objs below


(define (export-objs objs)
  ; converts a single list of objs to '(list "#id" "#id"...)
  ; of a single obj to "#id"
  (if (list? objs)
      (cons 'list (map obj->IDatom objs))
      (obj->IDatom objs)))

(define (register-obj-id obj id)
  ; error + return #f if already taken;
  ; else returns id
  (let ((found (hash-ref *id->obj-index* id false-fn)))
    ;(assq id *id->obj-index*)))
    (if found
        (handle-id-conflict id found obj)
        (begin
          ;(push! (list id obj) *id->obj-index*)
          (hash-set! *id->obj-index* id obj)
          id))))

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

(define (unregister-id id)
  (hash-remove! *id->obj-index* id))
;  (let ((found (assq id *id->obj-index*)))
;    (if found
;    (set! *id->obj-index* (remq found *id->obj-index*)))))

; ------------ Linkable object base class -------------------
(define linkable-obj%
  (class object%
    (super-instantiate ())
    (field (_id #f))
    (field (_wasid #f))
    (public delete id wasid refer-when-ready defer-send)

    (define (delete)
      (when _id (unregister-id _id)))

    (define id
      (case-lambda
        ;auto-allocate ids on demand
        (() (when (not _id)
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

