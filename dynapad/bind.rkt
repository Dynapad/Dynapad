#lang racket/base

(require racket/class
         (only-in dynapad/libdynapad-wrapper sch_bind)
         dynapad/utils/lambdas)

(provide bind)

(define bind
  (case-lambda
    ((obj) (sch_bind (send obj get-cptr)))
    ((obj event) (sch_bind (send obj get-cptr) event))
    ((obj event callback) (bind obj event callback #f))
    ((obj event callback save?)
     (let ((bindings (bind obj event)))
       (cond
         ; don't forget to update *writelist* when removing bindings
         ((not callback) (set! bindings null)) ; grandfather #f to mean null
         ((list? callback) (set! bindings callback))
         (else
          (set! bindings (list callback))
          (when save? (make-object bind/write% obj event callback))))
       (sch_bind (send obj get-cptr) event bindings)))))

(define *writelist* null)

(define bind/write%
  (class object%
    (init-field obj event callback)

    (define/public (write)
      (cond
        ((annotated-proc? callback)
         (list 'make-object 'bind/write% (send obj padid) event
               (apply list 'lambda (procedure-args callback)
                      (procedure-body callback))))
        (else
         (list 'make-object 'bind/write% (send obj padid) event
               (object-name callback)))))

    (define/public (fix alist)
      (set! obj (cadr (assq obj alist)))
      (bind obj event callback))

    (define/public (delete)
      (set! *writelist* (remq *writelist* this)))

    (super-instantiate ())
    (set! *writelist* (cons this *writelist*))))
