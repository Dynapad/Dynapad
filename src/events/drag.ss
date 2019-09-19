(define (Start-Drag argPAD evnt obj-list)
  (push-event-mode argPAD "Drag")
  (for-each (lambda(x)(send x physics? #f)) obj-list)
  (send argPAD setvar 'dragset obj-list)
  (send argPAD setvar 'dragset-bbox (bbunion-objects obj-list))
  (send argPAD init-drag evnt obj-list)
  (send argPAD do-beforedrag-callbacks evnt obj-list)
  (let ((drag-lyr (send argPAD getvar 'drag-layer))
        (main-lyr (send argPAD main-layer)))
    (when drag-lyr
      (foreach obj-list
               (lambda (o) (when (and (not (send o getgroup))
                                      (eq? (send o layer) main-lyr))
                             (send o raise)
                             (send o layer drag-lyr))))))

  )

(define (Start-Undoable-Drag argPAD evnt obj-list)
  (do-beforedrag-prepare-undo-callbacks argPAD evnt obj-list)
  (Start-Drag argPAD evnt obj-list)
  (store-drag-batch-for-undo argPAD evnt obj-list)
  )

(define (Finish-Undoable-Drag argPAD evnt obj-list)
  (Finish-Drag argPAD evnt obj-list)
  (do-afterdrag-prepare-undo-callbacks argPAD evnt obj-list)
  )


(define (Finish-Drag argPAD evnt obj-list)
  (pop-event-mode argPAD "Drag")
  (for-each (lambda(x)(send x physics? #t)) obj-list)
  (send argPAD do-afterdrag-callbacks evnt obj-list)
  ; maybe drop objs from drag layer:
  (let ((drag-lyr (send argPAD getvar 'drag-layer))
        (main-lyr (send argPAD main-layer)))
    (when drag-lyr
      (foreach obj-list
               (lambda (o) (when (and (not (send o getgroup))
                                      (eq? (send o layer) drag-lyr))
                             (send o layer main-lyr)
                             (send o raise))))))
  )



; drag-batch is for replicating gui-style drag operations. (see undo.ss)
(define (drag-batch eventPAD evnt obj-list dest-list)
  (Start-Drag eventPAD evnt obj-list)
  ; NOTE: pick-up and drop events both currently use same evnt
  ; ideally should include both events as separate args
  ;  (for-each (lambda (o psn)
  ;             (send o position psn))
  ;            obj-list dest-list)
  ;  (Finish-Drag eventPAD evnt obj-list))
  (animate-batch-then-do obj-list dest-list 600
                         (lambda () (Finish-Drag eventPAD evnt obj-list)))
  )
;WARNING:
;  this version animates moves, but is non-blocking:
;  returns control to user before animation and Finish-Drag are complete;
;  therefore preemptive actions which don't wait for anim to finish
;  may corrupt workspace state.


;(define (undrag-batch eventPAD evnt

(define (bindDrag argPAD)
  (send argPAD bind "<Drag-B1-Motion>"
        (lambda (eventPAD e) (set! currentPAD eventPAD)
                (let* ((last_x (sendf eventPAD evs lastx))
                       (last_y (sendf eventPAD evs lasty))
                       (x (event-x e))
                       (y (event-y e))
                       )
                  (when (and last_x last_y)
                    (let ((dx (- x last_x))(dy (- y last_y)))
                      (send eventPAD do-duringdrag e)
                      (for-each
                       (lambda (o) (send o slide dx dy))
                       (send eventPAD getvar 'dragset) )))
                  (sendf eventPAD evs set-last-xy x y)
                  )))

  ;  (send argPAD bind "<Drag-ButtonRelease-1>"
  ;        End-Drag-Event)
  (send argPAD bind "<Drag-ButtonRelease-1>"
        (lambda (eventPAD e)
          (when (member "Drag" (send eventPAD eventModeStack))
            (End-Drag-Event eventPAD e))))

  )

#|
; example of drag callback
(send argPAD afterdrag-callbacks 'add
      (lambda (eventPAD event screendistance draglist)
        (if (> screendistance 10)
            (printf "~a objects moved.~%" (length draglist))
            ;else
            (printf "not a complete move.~%"))))
|#
