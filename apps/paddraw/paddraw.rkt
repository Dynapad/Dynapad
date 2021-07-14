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

; now eternally stuck in a wx queue due to changes related to uberapp?
; nope, this is just what happens if you fail to run racket{,cgc} with
; the options --repl and --require

(void (make-object paddraw-event-binder% dynapad event-state%))

; LOOOL the behavior of loading paddraw and uberapp together at the
; same time is amusing, the zooms interact with eachother and the
; scene vanishes into nothingness
