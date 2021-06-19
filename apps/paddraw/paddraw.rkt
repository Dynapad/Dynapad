#lang racket/base
; FIXME we are probably going to want either #lang/racket or #lang/dynapad
; that will provide all the forms that we need, I think that is what dynapad.rkt
; is more or less doing
(require racket/class
         dynapad/events/event-state
         (only-in dynapad/pad-state dynapad)
         ; a newline is printing here somewhere
         dynapad/events/event-binders-classic ; this is where our culpret is? or in menu-draw
         dynapad/menu/menu-draw
         "../../dynapad.rkt"
         )

(provide (all-from-out "../../dynapad.rkt"))

; FIXME now eternally stuck in a wx queue due to changes related to uberapp

(void (make-object paddraw-event-binder% dynapad event-state%))
