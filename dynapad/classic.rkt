#lang racket/base

(require
 dynapad/base
 dynapad/menu/menu-draw ; XXX it seems this has to be imported before event binders?
 dynapad/events/event-state
 dynapad/events/event-binders-classic)

(provide
 ;paddraw-event-binder% event-state% ; XXX FIXME have to provide these
 ; if we want to be able to compose the event system with the custom
 ; menu items otherwise none of the menu buttons work until we construct
 ; the event binder class below AFTER requiring the menu code, there
 ; must be some stateful stuff happening that needs to be fixed
 (all-from-out
  racket/base
  dynapad/base
  ))

;#; ; FIXME this has to be run after all the menu stuff is set up?
; ideally we would do it at the end of the module, likely need to
; follow the way i did it for protc/ur
(void (make-object paddraw-event-binder% dynapad event-state%))

(module reader syntax/module-reader dynapad/classic)
