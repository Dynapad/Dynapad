#lang racket/base
(provide safe-eval)

(define (safe-eval expr)
  ;serves as a reentry point in case of errors during eval
  (with-handlers
    ;   ([exn:user? (lambda (exn)
    ([exn:fail? (lambda (exn)
                  (for-each (list (current-error-port))
                            (lambda (port)
                              (fprintf port  "Error (~a) in ~a~%" (exn-message exn) expr)))
                  #f)])
    (eval expr)))
