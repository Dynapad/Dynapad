#lang racket/base

(require (only-in racket/class send)
         syntax/location
         )

(provide currentPAD set-currentPAD!
         dynapad set-dynapad!
         dp1 set-dp1!
         *menubar*
         (struct-out event)
         (struct-out tablet-event)
         write-set
         *dynapad-directory*
         )

;; needed by misc

; should only be set once at the begining in dynapad.rkt
(define dp1 null)
(define (set-dp1! pad) (set! dp1 pad))

; should only be set once at the begining in dynapad.rkt
(define dynapad null)
(define (set-dynapad! pad) (set! dynapad pad))

(define currentPAD null)
(define (set-currentPAD! pad) (set! currentPAD pad))

;; not needed by misc
(define *menubar* #f)

(define-struct event (x y type key obj sx sy))
(define-struct (tablet-event event) (xid p tiltx tilty state button))

;; dynapad-c-api and menu_functions candidate to move to `dynapad/misc/misc'
(define (write-set objs)
  (if (null? objs)
      null
      (append
       (let ((obj (car objs)))
         (and obj
              (send obj write-all)))
       (write-set (cdr objs)))))

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
(println *dynapad-directory*)
