#lang racket/base

(provide tape-timeblock%
         reltime)

(require racket/class
         dynapad/pad-state
         dynapad/misc/misc
         dynapad/layout/bbox
         (only-in dynapad/events/text text%)
         )

; tape-timeblock format
;(block-name from-pair to-pair synch-pair synch-state)
(define (reltime m s)
  (make-object reltime% m s))

(define reltime%
  (class object%
    (init _min _sec)

    (super-instantiate ())
    ;normalize time
    (field (_total (+ (abs _sec) (* 60 (abs _min)))))
    (when  (or (negative? _min)
               (negative? _sec)) (set! _total (- _total)))

    (define/public total (get/set _total))
    (define/public (min)
      (inexact->exact (truncate (/ _total 60))))
    (define/public (sec)
      (inexact->exact (modulo (round (abs _total)) 60)))

    (define/public (write)
      (let ((sec (send this sec)))
        (format "~a:~a" (send this min)
                (if (< sec 10)
                    (format "0~a" sec)
                    sec))))
    (define/public (minus reltime)
      (make-object reltime% 0
                   (- (send this total)
                      (send reltime total))))
    (define/public (plus reltime)
      (make-object reltime% 0
                   (+ (send this total)
                      (send reltime total))))

    ))

(define tape-timeblock%
  (class object%
    (init-field _fromtime  ;reltime%
                _totime    ;reltime%
                _synchtime ;reltime%
                _synchstate) ;float
    (field (_prediff (send _synchtime minus _fromtime))
           (_postdiff (send _totime minus _synchtime))
           (_fromstate (send _prediff total))
           (_tostate (send _postdiff total)))
    (field (_counter #f))

    (super-instantiate ())

    (define/public (state->reltime state)
      (let ((diff (make-object reltime% 0 (- state _synchstate))))
        (send _synchtime plus diff)))

    (define/public (ratify-time time)
      (<= (send _fromtime total)
          (send time total)))

    (define/public (reltime->state time)
      (let ((diff (send time minus _synchtime)))
        (+ (send diff total) _synchstate)))

    (define (make-counter)
      (let* ((bb (send dynapad bbox))
             (x (b0 bb))
             (y (b3 bb))) ;upper left
        (set! _counter (ic (make-object text% dynapad "00:00" (list x y 100))
                           (anchor "nw")
                           (findable #f)
                           (sticky #t)))))

    (define/public (update-counter state)
      (unless _counter
        (make-counter))
      (send _counter sticky #f)
      (send _counter xy (let* ((bb (send dynapad bbox))
                               (x (b0 bb))
                               (y (b3 bb))) (list x y)))
      (send _counter z 100) ;shouldnt need this, but...
      (send _counter sticky #t)
      (send _counter text (send (send this state->reltime state) write)))
    ))


;probable synchtime and synchstate for tape1: 59:35 37641323
