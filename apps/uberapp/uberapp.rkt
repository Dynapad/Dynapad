(dynaload "event-binders-newdraw.rkt")
(make-object newpaddraw-event-binder% dynapad newpad-event-state%)
(dynaload "progress.rkt")

;(dynaload "events/new-events.rkt")

;(define (main-menu-title obj) ;overrides generic version in dynapad.ss
;  (if obj "Object Menu" "Dynapad Menu"))

(dynaload "menu_popup.rkt")

(dynaload "newbrush.rkt")

(dynaload "logs.rkt")

(dynaload "piles.rkt")
(dynaload "timeline.rkt")

(dynaload "composite.rkt")

; automatic brushing, popup-menus for images
(dynaload "image-menu.rkt")

(dynaload "edges.rkt")

; enable automatic hires when hit zoom limit
(dynaload "auto-hires.rkt")

(dynaload "log-views.rkt")


(dynaload "lockdown.rkt") ;puts dp into "analyst mode"; protects histories
(dynaload "write-eps.rkt")

(dynaload "back-compat.rkt")