(dynaload "event-binders-newdraw.ss")
(make-object newpaddraw-event-binder% dynapad newpad-event-state%)
(dynaload "progress.ss")

(dynaload "menu_popup.ss")
(dynaload "dir-brush.ss")
(dynaload "piles.ss")
(dynaload "timeline.ss")
(dynaload "composite.ss")

;DO NOT LOAD "logs.ss" in the same session as this:
(load-relative "summarize-log.ss")
