;;
;; photo op startup file v.1
;;
(display "Including photo_op features...")(newline)

(dynaload "events/new-events.rkt")
(dynaload "menu/new-menu_popup.rkt")


;--- init ---

(set-load-z 1.0)
(enable-popups dynapad)

;-- Optional features: --
;(dynaload "piles.rkt")
;(dynaload "timeline.rkt")
;(dynaload "undo.rkt")
;(dynaload "composite.rkt")
;(dynaload "brush.rkt")
