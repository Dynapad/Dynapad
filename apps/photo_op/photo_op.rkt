;;
;; photo op startup file v.1
;;
(display "Including photo_op features...")(newline)

(dynaload "events/new-events.ss")
(dynaload "menu/new-menu_popup.ss")


;--- init ---

(set-load-z 1.0)
(enable-popups dynapad)

;-- Optional features: --
;(dynaload "piles.ss")
;(dynaload "timeline.ss")
;(dynaload "undo.ss")
;(dynaload "composite.ss")
;(dynaload "brush.ss")
