#lang racket/base

(provide (all-from-out "../../dynapad.rkt"))

(require racket/class
         ; I think we need to be reproviding from this list so that
         ; the tooling can be reused because not all functionality is
         ; accessible via the ui right now
         dynapad/events/event-state
         dynapad/events/event-binders-newdraw
         dynapad/events/zoom-pushpull
         dynapad/menu/image-menu
         dynapad/menu/menu-draw
         dynapad/physics/regions
         dynapad/history/lockdown
         dynapad/utils/newbrush
         dynapad/utils/write-eps
         dynapad/utils/edges
         dynapad/physics/piles
         dynapad/physics/timeline
         dynapad/layout/composite
         dynapad/image-utils/auto-hires
         dynapad/history/logs
         dynapad/history/log-views
         dynapad/history/back-compat
         ; not happy with these here but not sure about the side
         ; effect of enabling it for all pad apps
         (only-in dynapad/pad-state dynapad)
         (only-in dynapad/menu/menu_popup enable-popups)
         (only-in dynapad/physics/regions create-tool-layers)
         (only-in dynapad/history/lockdown lockdown-bind-keys)
         "../../dynapad.rkt"
         )

; XXX ideally these would be activated in dynapad.rkt
(enable-popups dynapad)
(create-tool-layers dynapad)
(lockdown-bind-keys dynapad)

(make-object newpaddraw-event-binder% dynapad newpad-event-state%)
