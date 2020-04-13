#lang racket/base
(require racket/class
         dynapad/events/event-state
         (only-in dynapad/pad-state
                  dynapad)
         dynapad/events/event-binders-classic
         dynapad/menu/menu-draw
         )
(make-object paddraw-event-binder% dynapad event-state%)
