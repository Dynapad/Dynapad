#lang racket/base

(require racket/class
         dynapad/events/zoom-classic
         dynapad/events/zoom-obj
         (only-in dynapad/events/text
                  bindDrawMode
                  bindTextMode
                  )
         dynapad/events/pan
         dynapad/events/hyperlink
         (only-in dynapad/events/mode
                  changemode)
         (only-in dynapad/history/undo
                  bindSelect
                  basepad-event-binder%
                  )
         )

(provide (all-defined-out))

(define classicpad-event-binder%
  (class basepad-event-binder%
    (init argPAD evs-class)
    (super-instantiate (argPAD evs-class))

    (bindClassicPan argPAD) ;pan.ss
    (bindClassicZoom argPAD) ;zoom-classic.ss
    (bindZoomSelection argPAD) ;zoom-objs.ss
    (bindSelect argPAD "Select") ;select.ss

    (send argPAD setvar 'default-mode "Select")
    (changemode argPAD "Run")
    ))

(define paddraw-event-binder%
  (class classicpad-event-binder%
    (init argPAD evs-class)
    (super-instantiate (argPAD evs-class))

    (bindHyperlink argPAD) ;hyperlink.ss
    (bindDrawMode argPAD) ;draw.ss
    (bindTextMode argPAD) ;text.ss
    ))
