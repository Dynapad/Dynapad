#lang racket/base

(require (only-in racket/date date->string)
         dynapad/import
         dynapad/misc/misc
         )
(provide reset-stacks
         make-timestamp-ID
         )

;---------- Main Load/Save, State-Stack ---

(define *undo-stack* null); (list (list 0 #f #f)))
; stack of tuples: (state-id do-expr undo-expr)
; top of stack (head of list) is most recent frame/state
; *undo_stack* should never be null: bottom frame always represents
; intial state (possibly 0 state shown above)
(define *redo-stack* null)
(define *current-state-id* 0) ;=(caar *undo-stack*)
; state-id of current view;
; Can be inferred from (car (list-ref *state-stack* *frame-offset*))
(define *heed-start-state?* #t)
;when #f, skips start-state when log is read
;(e.g. when undoing from future log)
(define *future-log-path* null)
;stack of logbranches (excluding current-logbranch)
;which specify path to future "target" state
(define *log-continues?* #f)

;these do nothing when log is loaded:
(define (created-by state-id winid username hostname) #t)
(define (visit-state when state-id) #t)
(define (visit-start when state-id winid username hostname) #t)
(define (change-view when state-id . args) #t)

(define (make-undo/redo-frame state-id do-msg undo-msg) ;may be overridden
  (list state-id do-msg undo-msg))

(define (change-state state-id do-msg undo-msg . misc)
  ;doesn't actually change the state, just pushes do/undo ops onto stack:
  ; past or future, depending on *current-state-id*
  ;
  ; <<undo-stack<<   *current-state-id*  >>redo-stack>>
  ;               ^                                    ^
  ;               | add past                           | add future
  (unless (importing?) ;ignore all changes to history if loading in import context
    (let ((newframe (make-undo/redo-frame state-id do-msg undo-msg)))
      (if (> state-id *current-state-id*)
          (set-append! *redo-stack* (list newframe)) ;append onto future
          (push! newframe *undo-stack*))) ;push onto past
    ))

(define (log-continues)
  (unless (importing?)
    (set! *log-continues?* #t)))

(define (null-undo-frame state-id)
  (list state-id #f `(load-prev-log)))

; potentially competing with (clear-undo/redo) in undo.ss?:
(define (reset-stacks state-id)
  (set! *undo-stack* null)
  (push! (null-undo-frame state-id) *undo-stack*)
  (set! *redo-stack* null)
  (set! *log-continues?* #f) ;assume end unless continued later
  )

;---------- State IDs / Timestamps -----------
(define *timestamp-offset* 1041408000) ;from (find-seconds 0 0 0 1 1 2003)):
;         DONT CHANGE THIS ^^^^^^^^^^ or all logged timestamps will be wrong
(define *last-timestamp-ID* 0)
(define (make-timestamp-ID)
  (let ((newID (- (current-seconds) *timestamp-offset*)))
    (when (<= newID *last-timestamp-ID*)
      (begin
        (+= newID (* (modulo (current-milliseconds) 1000) .001))
        (when (<= newID *last-timestamp-ID*)
          (set! newID (/ (truncate (+ (* 1000 *last-timestamp-ID*) 1)) 1000)))))
    (set! *last-timestamp-ID* newID)
    newID))

(define (timestamp->date id)
  (let ((secs (+ id *timestamp-offset*)))
    (seconds->date (inexact->exact (floor secs)))))
(define (timestamp->datestring id)
  (date->string (timestamp->date id) #t))
;-------------------------------------------
