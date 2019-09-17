(define Start-Shift-Select-Event
  (lambda (eventPAD e)
    (set! currentPAD eventPAD)
    (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
    (sendf eventPAD evs set-sx0sy0 (event-sx e) (event-sy e))
    ;    (choose-appropriate-drag-find-function (event-obj e))
    (sendf eventPAD evs obj0 (event-obj e))
    (push-event-mode eventPAD "BBox")
    #f
    ))


(define Start-Drag/Select-Event
  (lambda (eventPAD e)
    (set! currentPAD eventPAD)
    (let ((obj (event-obj e)))
      (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
      ;      (sendf eventPAD evs x0 (event-x e))
      ;      (sendf eventPAD evs y0 (event-y e))
      (sendf eventPAD evs set-sx0sy0 (event-sx e) (event-sy e))

      (if (buttonpress-on-background? obj)
          (begin ; begin dragging select rect...
            (Start-Changing-Select--undoable eventPAD)
            (send eventPAD selected null)
            ;        (choose-appropriate-drag-find-function obj)
            (sendf eventPAD evs obj0 obj)
            (push-event-mode eventPAD "BBox")
            )
          (begin ;else begin dragging object(s)
            ;get parent group if it exists. (and then the group above that, etc.)
            (set! obj (get-top-group obj))
            (if (send obj selected?)
                ; dragging selected object(s)
                (Start-Undoable-Drag eventPAD e (send eventPAD selected))
                ; else dragging fresh obj --> unselect
                (begin
                  (Start-Changing-Select--undoable eventPAD)
                  (send eventPAD selected null)
                  (Start-Undoable-Drag eventPAD e (list obj)))))))
    #t;    #f
    ))

(define End-Select-Event
  (lambda (eventPAD e)
    (set! currentPAD eventPAD)
    (let (;(x (event-x e))
          ;(y (event-y e))
          ;(obj (event-obj e))
          ;(last_x (sendf eventPAD evs lastx))
          ;(last_y (sendf eventPAD evs lasty))
          (selector (sendf eventPAD evs update-selector e)))
      (when selector
        ; end select rect
        (send eventPAD selected
              (sendf eventPAD evs selector-contains))
        ;          (find-enclosed-objects eventPAD e (list last_x last_y x y)))
        (sendf eventPAD evs selector-delete))
      (Done-Changing-Select--undoable eventPAD)
      (if (selection-changed? eventPAD)
          (push-ops-no-exec)
          (abort-undo/redo)) ;ignore if no change
      )
    ;      (begin ; else end bkgnd click
    ;        (Done-Changing-Select--undoable eventPAD)
    ;(if (not (buttonpress-on-background? obj))
    ;    (say "Special case: obj unclick"))
    ;        (if (and (buttonpress-on-background? obj) ;redundant?
    ;             (not (selection-changed? eventPAD)))
    ;        (abort-undo/redo) ;ignore if bg click and no change
    ;        (push-ops-no-exec)))))
    (pop-event-mode eventPAD "BBox")
    #t;    #f
    ))

(define End-Drag-Event
  (lambda (eventPAD evnt)
    (let ((draglist (send eventPAD getvar 'dragset)))
      (store-drag-batch-for-redo eventPAD evnt draglist)
      (Finish-Undoable-Drag eventPAD evnt draglist)
      (Done-Changing-Select--undoable eventPAD)
      (push-ops-no-exec)
      )))

(define End-Shift-Select-Event
  (lambda (eventPAD e)
    (set! currentPAD eventPAD)
    (let (;(x (event-x e))
          ;(y (event-y e))
          ;(last_x (sendf eventPAD evs lastx))
          ;(last_y (sendf eventPAD evs lasty))
          (obj (event-obj e))
          (selector (sendf eventPAD evs update-selector e)))
      (if (and selector
               (sendf eventPAD evs moved-far-enough? e))
          ; dragged selection rect...
          (begin
            (Start-Changing-Select--undoable eventPAD)
            (map (lambda (o) (if (send o selected?)
                                 (send o unselect)
                                 (send o select)))
                 (sendf eventPAD evs selector-contains))
            ;(send selector contained-objects obj))
            ;         (find-enclosed-objects eventPAD e (list last_x last_y x y)))
            (Done-Changing-Select--undoable eventPAD)
            (if (selection-changed? eventPAD)
                (push-ops-no-exec)
                (abort-undo/redo))
            (sendf eventPAD evs selector-delete))
          ; else single object (or background):
          (begin
            (sendf eventPAD evs selector-delete)
            (let ((obj (event-obj e)))
              (when (and (not (is-a? obj dynapad%)) (send obj findable))
                (set! obj (get-top-group obj))
                (Start-Changing-Select--undoable eventPAD)
                (if (send obj selected?)
                    (send obj unselect)
                    (send obj select))
                (Done-Changing-Select--undoable eventPAD)
                (push-ops-no-exec)
                ))))
      (pop-event-mode eventPAD "BBox")
      #t;    #f
      )))
