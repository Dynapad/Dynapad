#lang racket/base
(require racket/class
         dynapad/events/event-state
         (only-in dynapad/pad-state
                  dynapad)
         dynapad/events/event-binders-newdraw
         dynapad/menu/image-menu
         dynapad/menu/menu-draw
         dynapad/menu/menu_popup
         dynapad/utils/newbrush
         dynapad/utils/write-eps
         dynapad/utils/edges
         dynapad/physics/piles
         dynapad/physics/timeline
         dynapad/layout/composite
         dynapad/image-utils/auto-hires
         dynapad/history/logs
         dynapad/history/log-views
         dynapad/history/lockdown
         dynapad/history/back-compat
         "../../dynapad.rkt"
         )

(provide (all-from-out "../../dynapad.rkt"))

(make-object newpaddraw-event-binder% dynapad newpad-event-state%)
