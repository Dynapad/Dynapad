(define Start-Pan-Event
  (lambda (eventPAD e)
    (set! currentPAD eventPAD)
    (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
    ))

(define Do-Pan-Event
  (lambda (eventPAD e)
    (set! currentPAD eventPAD)
    (let*
        ((l (send eventPAD view))
         (view_x (car l))
         (view_y (cadr l))
         (zoom (caddr l))
         (last_x (sendf eventPAD evs lastx))
         (last_y (sendf eventPAD evs lasty))
         (x (event-x e))
         (y (event-y e)))

      (when (and last_x last_y)
        (send eventPAD moveto (list (- view_x (- x last_x))
                                    (- view_y (- y last_y))
                                    zoom)) ))))

(define (pan-view-or-slide-selected dx dy)
  (lambda (eventPAD e) (set! currentPAD eventPAD)
          (let* ((vu (send eventPAD view))
                 (view_x (car vu))
                 (view_y (cadr vu))
                 (view_z (caddr vu)))
            (if (any-selected?)  ;currentPAD
                ;PROBLEM: this slide is not undoable, not logged;
                ; consider using Start-Undoable-Drag --> slide --> Finish-Undoable-Drag
                (send-selected slide (/ (- dx) view_z) (/ (- dy) view_z))

                (send eventPAD moveto
                      (list (+ view_x (/ dx view_z)) (+ view_y (/ dy view_z)) view_z))))))

(define (bindArrows argPAD)
  (send argPAD bind "<KeyPress-Left>"  (pan-view-or-slide-selected  15 0))
  (send argPAD bind "<KeyPress-Right>" (pan-view-or-slide-selected -15 0))
  (send argPAD bind "<KeyPress-Up>"    (pan-view-or-slide-selected 0 -15))
  (send argPAD bind "<KeyPress-Down>"  (pan-view-or-slide-selected 0  15))
  (send argPAD bind "<Shift-KeyPress-Left>"  (pan-view-or-slide-selected  1 0))
  (send argPAD bind "<Shift-KeyPress-Right>" (pan-view-or-slide-selected -1 0))
  (send argPAD bind "<Shift-KeyPress-Up>"    (pan-view-or-slide-selected 0 -1))
  (send argPAD bind "<Shift-KeyPress-Down>"  (pan-view-or-slide-selected 0  1))
  )

(define (bindClassicPan argPAD)
  (send argPAD bind "<Run-ButtonPress-1>" Start-Pan-Event)
  (send argPAD bind "<Run-B1-Motion>" Do-Pan-Event)
  ; control while in select mode is same as run mode
  (send argPAD bind "<Control-Select-ButtonPress-1>" Start-Pan-Event)
  (send argPAD bind "<Control-Select-B1-Motion>" Do-Pan-Event)
  )
