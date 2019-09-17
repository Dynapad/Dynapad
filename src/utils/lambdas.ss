
; This example uses v202.2 procedure structures
(module new-lambda mzscheme

  (provide new-lambda ;(rename new-lambda lambda)
           procedure-body
           procedure-args
           write-lambda
           read-sterile-lambda
           read-fertile-lambda)

  (define-values (struct:ap make-annotated-proc annotated-proc? ap-ref ap-set!)
    (make-struct-type 'anotated-proc #f 3 0 #f null #f 0))

  (define procedure-args (make-struct-field-accessor ap-ref 1))
  (define procedure-body (make-struct-field-accessor ap-ref 2))

  (define-syntax new-lambda
    (syntax-rules ()
      [(_ args . body)
       (make-annotated-proc (lambda args . body) 'args 'body)]))

  ;DSB additions
  (define (clone-lambda new-lmb)
    (eval (write-lambda new-lmb)))

  (define (write-lambda new-lmb)
    (cons 'lambda (cons (procedure-args new-lmb) (procedure-body new-lmb))))

  (define (is-lambda? lst)
    (eqv? (car lst) 'lambda))
  (define (read-sterile-lambda lst)
    (if (is-lambda? lst)
        (eval lst)))
  (define (read-fertile-lambda lst)
    (if (is-lambda? lst)
        (eval (cons 'new-lambda (cdr lst)))))

  ) ;end module
