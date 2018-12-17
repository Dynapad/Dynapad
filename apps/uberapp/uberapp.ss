(dynaload "event-binders-newdraw.ss")
(make-object newpaddraw-event-binder% dynapad newpad-event-state%)
(dynaload "progress.ss")

;(dynaload "events/new-events.ss")

;(define (main-menu-title obj) ;overrides generic version in dynapad.ss
;  (if obj "Object Menu" "Dynapad Menu"))

(dynaload "menu_popup.ss")

(dynaload "newbrush.ss")

(dynaload "logs.ss")

(dynaload "piles.ss")
(dynaload "timeline.ss")

(dynaload "composite.ss")

; automatic brushing, popup-menus for images
(dynaload "image-menu.ss")

(dynaload "edges.ss")

; enable automatic hires when hit zoom limit
(dynaload "auto-hires.ss")

(dynaload "log-views.ss")


(dynaload "lockdown.ss") ;puts dp into "analyst mode"; protects histories
(dynaload "write-eps.ss")

(dynaload "back-compat.ss")