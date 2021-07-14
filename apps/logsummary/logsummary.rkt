(dynaload "event-binders-newdraw.rkt")
(make-object newpaddraw-event-binder% dynapad newpad-event-state%)
(dynaload "progress.rkt")

(dynaload "menu_popup.rkt")
(dynaload "dir-brush.rkt")
(dynaload "piles.rkt")
(dynaload "timeline.rkt")
(dynaload "composite.rkt")

;DO NOT LOAD "logs.rkt" in the same session as this:
(load-relative "summarize-log.rkt")
