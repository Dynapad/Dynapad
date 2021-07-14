(require (lib "process.rkt"))

; originally from etienne

(define (system-command command . args)
  (def no-return-value? (not (null? args)))
  (def p (process command))
  (def in  (car    p))
  (def out (cadr   p))
  (def err (cadddr p))
  (def err-value (read-line err))
  (def return-value ())

  (when (not (eq? eof err-value))
    (display (format "error: ~a~%" command))
    (display (format "  --> ~a~%" err-value)))

  (if (not no-return-value?)
      (do ((line "" (read-line in)))
          ((eq? line eof))
        (push! line return-value)))

  (close-input-port  in)
  (close-output-port out)
  (close-input-port  err)

  return-value
  )


; execute command and do not wait for program to end
;  -- haven't found a consistent way to retrieve errors,
;     reading from error channel causes blocking
(define (system-application command)
  (thread (lambda ()
            (let ((p (process command)))
              (close-input-port  (car    p))
              (close-output-port (cadr   p))
              (close-input-port  (cadddr p))) )))

