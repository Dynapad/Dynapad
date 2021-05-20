#lang racket/base
; FIXME we are probably going to want either #lang/racket or #lang/dynapad
; that will provide all the forms that we need, I think that is what dynapad.rkt
; is more or less doing
(require racket/class
         dynapad/events/event-state
         (only-in dynapad/pad-state
                  dynapad)
         dynapad/events/event-binders-classic
         dynapad/menu/menu-draw
         "../../dynapad.rkt"
         )

(provide (all-from-out "../../dynapad.rkt"))

(make-object paddraw-event-binder% dynapad event-state%)
