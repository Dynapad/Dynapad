;Contains dummy functions (needed in events.ss, etc)
;  which are be overridden by undo.ss if included

(define (store-attribute-of-specified-object-for-undo argPAD attr obj) #f)
(define (store-attribute-of-specified-object-for-redo argPAD attr obj) #f)

(define (store-attribute-of-selected-objects-for-undo argPAD attr) #f)
(define (store-attribute-of-selected-objects-for-redo argPAD attr) #f)

(define (store-selection-for-undo objs) #f)
(define (store-selection-for-redo objs) #f)

(define (store-drag-batch-for-undo eventPAD evnt objs) #f)
(define (store-drag-batch-for-redo eventPAD evnt objs) #f)

;(define (obj->IDexpr obj) (send obj write))
(define (obj-buildstr obj) (send obj write))

(define (undo) #f)
(define (redo) #f)
(define (push-undo-op op) #f)
(define (push-redo-op op) #f)
(define (push-ops-no-exec) #f)
(define (push-ops-and-exec) #f)
(define (undoify-fresh-obj obj) obj)
(define (undoify-fresh-objs objs) objs)
(define (undoable-delete-obj obj . more)
  (send obj delete))
(define (undoable-delete-objs objs . more)
  (map (lambda (o) (send o delete)) objs))

(define (clear-undo/redo) #f)
(define (abort-undo/redo) #f)

;(define *id->obj-index* null)

; new/tentative:
(define (beforedrag-prepare-undo-callbacks . args) #f)
(define (do-beforedrag-prepare-undo-callbacks . args) #f)
(define (afterdrag-prepare-undo-callbacks . args) #f)
(define (do-afterdrag-prepare-undo-callbacks . args) #f)

;--------- These are overridden by logs.ss ----------

(define (start-state . args)
  ;if not overridden generates a warning when trying to load log files
  (printf "Cannot load log files unless \"logs.ss\" has been included~%"))

(define (change-state . args) #f)
(define (change-view . args) #f)
(define (log-continues) #f)
