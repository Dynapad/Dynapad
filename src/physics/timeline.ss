(announce-module-loading "region tools...")

(dynaload "bbox.ss")
(dynaload "actor.ss")
(dynaload "tools-lists.ss")
(dynaload "tools-misc.ss")
(dynaload "tools-cmp.ss")
(dynaload "geometry.ss")

(update-progress .5)
(dynaload "regions.ss")
(update-progress .7)
(dynaload "actortimer.ss")
(dynaload "layout.ss")

(update-progress .85)
;(dynaload "image-dates.ss")
(dynaload "get-user-bbox.ss")

(dynaload "metadata.ss")

(dynaload "colors.ss")


;(dynaload "piles.ss")

;(display "    libraries loaded...")(newline)

;(define (image? obj)
;  (is-a? obj image%))

;(set! *selection-layer* *mid-layer*) ;(make-object layer% dynapad "selection"))
;(set! *selection-layer-filter-fn* image?)
;(send dynapad setvar 'drag-layer *selection-layer*)


;==========================================================
;(define make-timeline-box
;  (case-lambda
;   ((bbox)          (make-timeline-box bbox timeline-container% rect%))
;   ((bbox rgn-type) (make-timeline-box bbox rgn-type rect%))
;   ((bbox rgn-type obj-type)
;    (define timeline-box (make-object obj-type dynapad bbox))
;    (define timeline-region (regionize timeline-box rgn-type))
;    (send timeline-box transparency .7)
;    timeline-box)))

; (define (Arrange-Images-Into-Timeline)
;   (let* ((temp-callback 0)
;          (region-box (car (send dynapad selected)))
;          (region-actor (get-region-actor region-box)))

;     (set! temp-callback (lambda (dir imagelist)
;        (for-each
;          (lambda (img) (send region-actor include-obj img))
;          imagelist)

;        (send dynapad center region-box 2000 #t)
;        (send region-actor trigger-post-action)
;        (remove-image-dir-callback temp-callback)
;     ))
;     (append-image-dir-callback temp-callback)
;     (Arrange-Images)
;   )
; )

(define (make-popup-menu-for-1D-projector object)
  (let ((popmenu (new-popup "Region Tool...")))
    (make-submenu-Layout  popmenu object)
    (make-submenu-Spacing popmenu object)
    (make-submenu-SortBy  popmenu object)
    (add-menu-separator popmenu) ;------------------------------
    (make-submenu-Edit    popmenu object)
    (include-custom-popup-items popmenu object)
    popmenu ))

(define (DumpSelectedTrays)
  (let* ((rgns (apply append (map 
                  (lambda (o) (get-actors-named o 'region))
                  (send dynapad selected))))
     (trays (filter (lambda (r) (is-a? r tray%)) rgns)))
    (for-each (lambda (t) (send t remove-objects (send t contents))) trays)))

(define (make-popup-menu-for-tray object)
     (let ((popmenu (new-popup "Tray Options...")))
       (add-menu-item popmenu "Dump contents here" DumpSelectedTrays)
       (add-menu-separator popmenu) ;------------------------------
       (make-submenu-Edit    popmenu object)
       ;(make-submenu-Arrange popmenu object)
       ;(make-submenu-Object  popmenu object)
       popmenu))

(define (make-submenu-Layout mb object)
  (let* ((sb (add-submenu mb "Layout Format..."))
     (grid-proj? (get-actor-named object 'raster-projector))
     (line-proj? (get-actor-named object 'linear-projector)))
    (add-checkable-menu-item sb "Grid"
           (lambda (i) (SetProjectorForSelected raster-projector%))
       grid-proj?)
    (add-checkable-menu-item sb "Linear"
       (lambda (i) (SetProjectorForSelected labeled-linear-projector%))
       line-proj?)
    ))

(define (make-submenu-SortBy mb object)
  (let* ((proj (get-actor-named object projector-actor-name))
     (sb (add-submenu mb "Sort by..." proj))
     (param (and proj (send proj param))))
    (add-checkable-menu-item sb "Date Created"
       (lambda (i) (SetParamForSelectedProjectors date-created-parameter))
       (eq? param date-created-parameter) proj)
    (add-checkable-menu-item sb "Date Acquired"
       (lambda (i) (SetParamForSelectedProjectors date-acquired-parameter))
       (eq? param date-acquired-parameter) proj)
    (add-checkable-menu-item sb "Focus Distance (Images)"
       (lambda (i) (SetParamForSelectedProjectors focus-dist-parameter))
       (eq? param focus-dist-parameter) proj)
    (add-checkable-menu-item sb "Camera Aperture (Images)"
       (lambda (i) (SetParamForSelectedProjectors fstop-parameter))
       (eq? param fstop-parameter) proj)
    ))

(define (make-submenu-Spacing mb object)
  (let* ((proj (get-actor-named object 'linear-projector))
     (sb (add-submenu mb "Linear Spacing..." proj))
     (iq (and proj (send proj iq-weight))))
    (add-checkable-menu-item sb "Evenly Spread"
       (lambda (i) (SetSelectedLinearProjectorWeight 0))
       (and iq (= iq 0)) proj)
    (add-checkable-menu-item sb "Loosely Clumped"
       (lambda (i) (SetSelectedLinearProjectorWeight .3))
       (and iq (= iq .3)) proj)
    (add-checkable-menu-item sb "Tightly Clumped"
       (lambda (i) (SetSelectedLinearProjectorWeight .7))
       (and iq (= iq .7)) proj)
    (add-checkable-menu-item sb "Precisely Scaled"
       (lambda (i) (SetSelectedLinearProjectorWeight 1))
       (and iq (= iq 1)) proj)
    ))

; dont call this raw
(define (make-region-tool-from-bb argPAD bbox obj-type form-type rgn-type proj-type . params)
  (let* ((obj (make-object obj-type argPAD bbox))
     (form (regionize obj rgn-type form-type)))
    (apply attach-projector-to-obj form proj-type params)
    (send (get-rgn form) refresh-contents)
    form))

(define (make-region-tool-with-objs argPAD objs obj-type form-type rgn-type proj-type . params)
  (let* ((bbox (bbunion-objects objs))
     (obj  (make-object obj-type argPAD bbox))
     (form (regionize obj rgn-type form-type)))
    (apply attach-projector-to-obj form proj-type params)
    (send (get-rgn form) final-contents objs)
    form))
    ;(send (get-rgn form) refresh-contents)


(define NewRegionTool
  (case-lambda
   ((rgn-type)
      (NewRegionTool rgn-type *default-region-form-type*))
   ((rgn-type form-type)
      (NewRegionTool rgn-type form-type labeled-linear-projector%)) ;raster-projector%))
   ((rgn-type form-type proj-type)
      (NewRegionTool rgn-type form-type proj-type date-acquired-parameter)) ;date-created-parameter))
   ((rgn-type form-type proj-type param . more)
    (if (any-selected?)
    (let ((form #f))
      (Start-Changing-Select--undoable dynapad)
      (set! form
        (apply make-region-tool-with-objs dynapad (fs) rect%
               form-type rgn-type proj-type param more))
      (send dynapad selected (list form))
      (Done-Changing-Select--undoable dynapad)
      (undoify-fresh-obj form))
    (ask-user-for-bbox dynapad
       (lambda (bb)
         (undoify-fresh-obj
          (apply make-region-tool-from-bb dynapad bb rect%
             form-type rgn-type proj-type param more))))))
))

(when *popup-menus-enabled?*
    (append-mainmenu-constructor
     (lambda (mb obj)
       (add-menu-separator mb)
       (add-menu-item mb (if (any-selected?)
                 "Make Lens w. Selected"
                 "Make Lens...")
              (lambda () (NewRegionTool lens%
                        resizable-frame-container%
                        ;optional form type
                        )))
       (add-menu-item mb (if (any-selected?)
                 "Make Magnet w. Selected"
                 "Make Magnet...")
              (lambda () (NewRegionTool mutator%
                        resizable-frame-container%
                        ;optional form type
                        )))
       (add-menu-item mb (if (any-selected?)
                 "Make Tray w. Selected"
                 "Make Tray...")
              (lambda () (NewRegionTool fusing-container%
                        resizable-dissolving-fusing-frame-container%
                        )))
       )))


(update-progress 1)
