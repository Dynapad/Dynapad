#lang racket/base

(provide safe-eval
         mimic-old-load)

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

(define (mimic-old-load path)
  (with-input-from-file path
    (λ ()
      (let ([ns (current-namespace)])
        (let loop ([stx (read-syntax path)])
          (unless (eq? stx eof)
            ; XXX intentionally break hygene to match old behavior
            (let ([nstx (datum->syntax stx (syntax->datum stx) stx)])
              (eval nstx))
            (loop (read-syntax path))))))))