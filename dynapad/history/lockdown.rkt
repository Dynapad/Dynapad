(load-relative "../../apps/logsummary/logmunge.rkt")


(define push-ops-no-exec
  (case-lambda
    (() #t)
    ((do-expr undo-expr) #t)))

(define *tape-block* #f)
(define (tape-synch state min sec)
  (set! *tape-block* (make-object tape-timeblock%
                                  (reltime 0 0)
                                  (reltime 0 0)
                                  (reltime min sec)
                                  state)))

(define (tape-time state)
  (let ((rt (send *tape-block* state->reltime state)))
    (send rt write)))

(define (set-current-state-id new) ;overrides version in logs.ss
  (set! *current-state-id* new)
  (when *tape-block*
    (send *tape-block* update-counter new)))

;even newer override includes state-counter:
(define (set-current-state-id new)
  (set! *current-state-id* new)
  (update-stateid-counter new))

(define *state-id-counter* #f)
(define (update-stateid-counter id)
  (let* ((bb (send dynapad bbox))
         (scale (send dynapad getzoom))
         (xy (pad-xy-from-corner -20 20 "se")))
    (when (not *state-id-counter*)
      ;         (x (- (b2 bb) (* scale 50)))
      ;         (y (+ (b1 bb) (* scale 50))))
      (set! *state-id-counter*
            (ic (make-object text% dynapad "0" (append xy (list (/ 1 scale))))
                (layer *logtree-layer*)
                (anchor "se")
                (findable #f)
                (sticky #t)
                (pen "white"))))
    (send* *state-id-counter*
      (text (if id (number->string id) "0"))
      (xy xy))))

;these override fns in logs.ss, adding a fourth term to each frame:
; (state redo-expr undo-expr view-bbox)
(define (make-undo/redo-frame state-id do-msg undo-msg) ;may be overridden
  (list state-id do-msg undo-msg *last-view-bbox*))

(define (null-undo-frame state-id) ;may be unneeded
  (update-last-view-bbox #f)
  (list state-id #f `(load-prev-log) #f))

(define *last-view-outline* #f)
(define (dynapad-zoom-to-bbox new-bb)
  (let* ((curr-bb (send dynapad bbox))
         (curr-w (bbwidth curr-bb))
         (new-w (bbwidth new-bb))
         (ratio (/ curr-w new-w)))
    (send dynapad moveto
          (append (bbcenter new-bb) (list (* (send dynapad getzoom) ratio)))
          500)
    ))

(send dynapad bind "<Control-KeyPress-h>"
      (lambda (p e) (send p moveto
                          (precompute-distant-view p (send p objects))
                          500)))
(send dynapad bind "<Control-KeyPress-g>"
      (lambda (p e)
        (when *last-view-outline*
          (dynapad-zoom-to-bbox (send *last-view-outline* bbox)))))

(define (update-last-view-bbox bb)
  (if bb
      (begin
        (unless *last-view-outline*
          (set! *last-view-outline*
                (ic (make-object rect% dynapad)
                    (penwidth 0)
                    (fill "none")
                    (layer (send dynapad getvar 'select-layer))
                    (findable #f)
                    ;(bind "<Double-ButtonPress-1>"
                    ;    (lambda (o e)
                    ;      (say "zooming...")
                    ;      (dynapad-zoom-to-bbox (send (event-obj e) bbox))
                    ;      #f))
                    (bind "Run-Shift-ButtonPress-1>" false-event-lambda)
                    (bind "Select-Shift-ButtonPress-1>" false-event-lambda)
                    (bind "Run-ButtonPress-1>" false-event-lambda)
                    (bind "Select-ButtonPress-1>" false-event-lambda)

                    )))
        (send* *last-view-outline* (coords bb) (pen "yellow"))
        ;(dynapad-zoom-to-bbox bb)
        )
      (when *last-view-outline*
        (send *last-view-outline* delete)
        (set! *last-view-outline* #f))
      ))

(define (restore-undo-frame frame)
  (update-last-view-bbox (cadddr frame))
  (restore-set (caddr frame)))
(define (restore-redo-frame frame)
  (update-last-view-bbox (cadddr frame))
  (restore-set (cadr frame)))
