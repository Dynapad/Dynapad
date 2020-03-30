#lang racket/base

(require (only-in racket/class send)
         dynapad/misc/misc
         dynapad/events/mode)

(provide show-possible-delay)

; --- Show hourglass cursor for potentially long operations ---
;Note: mred includes (begin-busy-cursor) and (end-busy-cursor),
; but these change cursor only within a mred window, which dynapad is not.
(define-syntax show-possible-delay
  (syntax-rules ()
    ((_ the-delayed-pad do-sth ...)
     (with-handlers
       ([exn:fail?
         (lambda (exn)
           (foreach (current-error-ports)
                    (lambda (port)
                      (fprintf port "~a~%" (exn-message exn))))
           (pop-delay-cursor the-delayed-pad))])
       (begin
         (push-delay-cursor the-delayed-pad)
         (let ((result (begin do-sth ...)))
           (pop-delay-cursor the-delayed-pad)
           result))))))

(define (push-delay-cursor argPAD)
  (let ((delay? (send argPAD getvar 'delay-cursor?)))
    (when (not delay?) (set! delay? 0))
    (send argPAD setvar 'delay-cursor? (+ delay? 1))
    (send argPAD cursor 3)))
(define (pop-delay-cursor argPAD)
  (let ((delay? (send argPAD getvar 'delay-cursor?)))
    (set! delay? (if (and delay? (> delay? 1))
                     (- delay? 1)
                     #f))
    (send argPAD setvar 'delay-cursor? delay?)
    (gui-update-mode argPAD)))

