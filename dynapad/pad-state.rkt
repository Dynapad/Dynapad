#lang racket/base

(require (only-in racket/class send make-object)
         (only-in racket/gui/base
                  frame%)
         syntax/location
         (only-in dynapad/misc/user-preferences
                  *draw-menu-x*
                  *draw-menu-y*
                  )
         )

(provide currentPAD set-currentPAD!
         dynapad set-dynapad!
         dp1 set-dp1!
         *menubar*
         (struct-out event)
         (struct-out tablet-event)
         *dynapad-directory*
         *list-of-all-dynapads* 
         set-*list-of-all-dynapads*!
         set-initial-pad-state!
         )

(define *list-of-all-dynapads* '())
(define (set-*list-of-all-dynapads*! value)
   (set! *list-of-all-dynapads* value))

(define (set-initial-pad-state! pad)
  (set-dp1! pad)
  (set-dynapad! pad)
  (set-currentPAD! pad))

; should only be set once at the begining in dynapad.rkt
(define dp1 null)
(define (set-dp1! pad) (set! dp1 pad))

; should only be set once at the begining in dynapad.rkt
(define dynapad null)
(define (set-dynapad! pad) (set! dynapad pad))

(define currentPAD null)
(define (set-currentPAD! pad) (set! currentPAD pad))

;; not needed by misc
(define *menubar* (make-object frame% "Draw Tools" #f #f #f *draw-menu-x* *draw-menu-y*))


(define-struct event (x y type key obj sx sy))
(define-struct (tablet-event event) (xid p tiltx tilty state button))

;; default-directory or something like that
; FIXME this is a hack that is not remotely robust
(define *dynapad-directory*
  ; clrd -> #f since we no longer use load ...
  (build-path (let ([clrd (current-load-relative-directory)])
                (if clrd clrd
                    ;(quote-source-file) ; doesn't do what we want ...
                    "~/git/dynapad/dynapad" ; FIXME :/
                    )
                ) 'up))
