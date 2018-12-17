(load "/home/etienne/dynapad/formation.ss")
(load-relative "control-panel.ss")

(define task-region%
  (class container-form%
     (init dynaptr)
     (inherit dynaclass)
     (super-instantiate (dynaptr))
     (dynaclass 'task-region%)

     (name-part _region region)
     (name-part _area   area)
     (name-part _panel  panel)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Instantiation stuff
     (panel  (make-object control-panel% dynapad))
     (area   (make-object rect% dynaptr '(0 0 275 -200)))
     (region (regionize _area tray%))

     ;; Set control panel properties
     (send (panel) setanchor (car (send (area) bbox)) (cadddr (send (area) bbox)))
     (send (panel) vertical)

     ;; Set the rectangle properties
     (send (area) fill "blue")
     (send (area) transparency 0.75)

     ;; Set the region properties
     (send (region) title "Workspace")
     )
  )

