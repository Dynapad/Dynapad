#lang racket/base
(require racket/class
         dynapad/events/event-state
         (only-in dynapad/pad-state dynapad)
         ; a newline is printing here somewhere
         dynapad/events/event-binders-classic ; this is where our culpret is? or in menu-draw
         dynapad/menu/menu-draw
         dynapad/base)

(provide (all-from-out
          dynapad/base))

(require errortrace)
(require
 dynapad/history/logs
 dynapad/undo-state
 (only-in dynapad/events/draw set-Draw-object! finishDraw)
 )

(void (make-object paddraw-event-binder% dynapad event-state%))

(define (make-test-objects)
  (set-Draw-object! (ic
    (make-object text% dynapad)
    (id 1)
    (anchor "nw")
    (position -72.0f0 125.5f0 1.0f0)
    (minsize
     (quote
      (0.0f0 #f)))
    (maxsize
     (quote
      (-1.0f0 #f)))
    (text "hello")
    (font "Times")
    (pen "#ffffff")))
  (finishDraw dynapad)

  (set-Draw-object! (ic
    (make-object text% dynapad)
    (id 2)
    (anchor "nw")
    (position -181.0f0 -26.5f0 1.0f0)
    (minsize
     (quote
      (0.0f0 #f)))
    (maxsize
     (quote
      (-1.0f0 #f)))
    (text "world")
    (font "Times")
    (pen "#ffffff")))
  (finishDraw dynapad)

  (set-Draw-object! (ic
    (make-object polygon% dynapad)
    (id 3)
    (position 102.0f0 -273.5f0 1.0f0)
    (minsize
     (quote
      (0.0f0 #f)))
    (maxsize
     (quote
      (-1.0f0 #f)))
    (coords 294.0f0 -344.5f0 237.0f0 -543.5f0 -39.0f0 -446.5f0 -126.0f0 -142.5f0 149.0f0 -3.5f0 330.0f0 -76.5f0)
    (pen "#ffffff")
    (fill "none")))
  (finishDraw dynapad)
  )

(module+ test
  (require
   (only-in dynapad/menu/menu_functions
            Save-All-As)

   #;
   dynapad/save)
  
  (make-test-objects)
  (println "setup complete")
  (println "starting undos")

  (undo)
  (undo)
  (undo)

  (println "starting redos")
  (redo)
  (redo)
  (redo)

  (println "saving")
  (Save-All-As "/tmp/dynapad-test-1-save.rktd") ; breaks undo/redo

  (println "starting undos")
  (undo)
  (undo)
  (undo)

  (println "starting redos")
  (redo)
  (redo)
  (redo)

  (println "starting done")
  )
