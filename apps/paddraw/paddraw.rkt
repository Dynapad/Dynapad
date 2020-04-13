#lang racket/base
(require racket/class
         dynapad/pad-state
         dynapad/events/event-binders-classic
         dynapad/events/event-state
         )
(make-object paddraw-event-binder% dynapad event-state%)
