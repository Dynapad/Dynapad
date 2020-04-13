#lang racket/base

(require racket/class
         dynapad/events/draw
         dynapad/events/hyperlink
         dynapad/events/event-binders-new)

(provide (all-defined-out))

; TODO (provide (all-from-out ...))

(define newpaddraw-event-binder%
  (class newpad-event-binder%

    (init argPAD evs-class)
    (super-instantiate (argPAD evs-class))
    (bindHyperlink argPAD) ;hyperlink.ss
    (bindDrawMode argPAD) ;draw.ss
    (bindTextMode argPAD) ;text.ss
    ))
