#lang racket/base

(require (only-in racket/class send is-a?)
         dynapad/objects
         dynapad/copy
         dynapad/undo-state
         dynapad/pad-state
         dynapad/events/mode
         ;dynapad/events/text ; lol cycle
         )

(provide set-Draw-object!
         drag-shape-vertex-event
         update-shape-preview-event
         fix-shape-vertex-event
         set-Draw-multiple!
         Draw-mode
         Draw-class
         Draw-object
         Draw-multiple
         initDraw
         initDrawPreview
         finishDraw
         cancelDrawObject 
         resetDrawObject 
         add-shape-vertex-event
         )
;(dynaload "menu-draw.rkt")
;======= Draw Rect, Oval, Line, and Polygon from Menubar ========

(define Draw-mode #f)
(define Draw-class #f)
(define Draw-object #f)
(define Draw-preview #f)
(define Draw-multiple #f)

(define (set-Draw-object! obj)
  (set! Draw-object obj))

(define (set-Draw-multiple! obj)
  (set! Draw-multiple obj))

(define (resetDrawPreview)
  (when Draw-preview
    (send Draw-preview delete)
    (set! Draw-preview #f)))

(define (cancelDrawObject argPAD)
  (when Draw-object
    (send Draw-object delete))
  (set! Draw-object #f)
  (resetDrawPreview)
  )

(define (resetDrawObject argPAD)
  (when Draw-object
    (undoify-fresh-obj Draw-object)) ;save draw object for undo
  (set! Draw-object #f)
  (resetDrawPreview)
  )

(define (initDraw argPAD drawclass drawmode)
  (resetDrawObject argPAD)
  (set! Draw-class drawclass)
  (set! Draw-mode drawmode)
  (changemode argPAD drawmode)
  )

(define (initDrawPreview argPAD)
  (when (not Draw-preview)
    (set! Draw-preview (clone-object Draw-object))
    ; FIXME TODO Draw-preview void due to changes in import-set eval
    (send Draw-preview transparency .3))
  (let ((c (send Draw-object coords)))
    (send Draw-preview coords c)
    (send Draw-preview save-coords c))
  )

(define (finishDraw argPAD)
  (resetDrawObject argPAD)
  (changemode--no-gui argPAD (default-mode argPAD))
  ;(Set-Select--undoable argPAD Draw-object)
  (gui-update-mode argPAD))

;=== Bound events ===

(define (drag-shape-vertex-event eventPAD e)  ;move unfixed vertex
  (set-currentPAD! eventPAD)
  (when Draw-object ;should always be #t
    (let*
        ((c (or (send Draw-object recall-coords) null))
         (x (event-x e))
         (y (event-y e))
         (newc (append c (list x y))))
      (send Draw-object coords newc)
      (when Draw-preview
        (send Draw-preview coords newc)
        (send Draw-preview save-coords newc))
      (when (is-a? Draw-object freehand%)
        (send Draw-object save-coords newc)))))

(define (update-shape-preview-event eventPAD e) ;rubber-band to cursor
  (set-currentPAD! eventPAD)
  (let ((c (or (send Draw-preview recall-coords) null))
        (x (event-x e))
        (y (event-y e)))
    (send Draw-preview coords (append c (list x y)))))

(define (fix-shape-vertex-event eventPAD e) ;fix vertex
  (set-currentPAD! eventPAD)
  (send Draw-object save-coords
        (send Draw-object coords)))

(define (add-shape-vertex-event eventPAD e) ;add unfixed vertex
  (set-currentPAD! eventPAD)
  (let ((c (send Draw-preview coords)))
    (send Draw-preview save-coords c)
    (send Draw-object coords c)
    ))
