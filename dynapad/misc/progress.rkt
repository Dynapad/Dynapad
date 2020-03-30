#lang racket/base

(require dynapad/misc/misc)
(provide announce-module-loading
         update-progress)

(define *progress-bar-total-len* 35)
(define *progress-bar-len* 0)
(define *progress-bar-gap* 0)

(define (announce-module-loading name)
  (let ((str (format "Loading ~a" name)))
    (display str)
    (flush-output)
    (set! *progress-bar-len* 0)
    (set! *progress-bar-gap* (- *progress-bar-total-len*
                                (string-length str)))
    ))

(define update-progress
  (case-lambda
    (() (display ".")
        (_++ *progress-bar-len*)
        (flush-output))
    ((pct)
     (when (> pct 1) (set! pct 1))
     (let* ((target (inexact->exact (round (* *progress-bar-gap* pct))))
            (fill (- target *progress-bar-len*)))
       (when (< fill 0) (set! fill 0))
       (display (make-string fill #\.))
       (+= *progress-bar-len* fill)
       (when (>= pct 1)
         (newline) ;might want to add end marker (e.g. "done!")
         (set! *progress-bar-len* 0)))
     (flush-output))
    ))