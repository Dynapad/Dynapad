#lang racket/base

(require racket/class
         (only-in dynapad/utils/actor
                  actor%)
         )

(provide add-object-menu
         add-custom-popup-items
         )

;--- an example of object specific menus -------------------------
;
; The make-popup-menu function (above) tests whether an object
; wants to provide it's own popup menu, before displaying the
; default object menu.  The test is made by sending an actor-message
; called "provides-popup?".  If the object has an attached actor
; which responds to this query, then a second actor-message
; "make-popup-menu" is used to generate the menu.
;
; In the following example code we see a generic actor that handles
; these messages, as well as a utility for adding the actor and
; a small example popup-menu.
;

(define popup-provider%
  (class actor%
    (field (_make-popup-menu #f))
    (inherit-field _object)

    (define/public (provides-popup?) _make-popup-menu)
    (define/public (make-popup-menu) (_make-popup-menu _object))
    (define/public (use-this-popup-fnc newfnc) (set! _make-popup-menu newfnc))

    (super-instantiate ())))

(define (add-object-menu menu-maker dynaobject)
  (define ppr (make-object popup-provider%))
  (send ppr use-this-popup-fnc menu-maker)
  (send ppr attach-to dynaobject))

; Addendum:
; In addition to entire menus, objects can include individual menu
; items as follows: any menu-maker can call
; (include-popup-items obj)

; (define (add-object-menu object)
;   (define ppr (make-object popup-provider%))
;   (when ppr
;     (send ppr use-this-popup-fnc  make-popup-menu-for-region)
;     (send ppr attach-to object)))
;
; (define (make-popup-menu-for-region regn)
;   (let ((popmenu (new-popup "Region Menu")))
;
;     (make-submenu-File    popmenu regn)
;     (make-submenu-Edit    popmenu regn)
;
;     (add-menu-item popmenu
;        "Arrange in Grid"  (lambda () (Arrange-in-Grid regn)))
;
;     (add-menu-item popmenu
;        "Arrange in Spiral"  (lambda () (Arrange-in-Spiral regn)))
;
;     popmenu
;   )
; )
;

(define popup-item-provider%
  (class actor%
    (field (_make-popup-menu-item #f))
    (inherit-field _object)

    (define/public (provides-popup-item?) _make-popup-menu-item)
    (define/public (make-popup-items menu) (_make-popup-menu-item _object menu))
    (define/public (use-this-popup-fnc newfnc) (set! _make-popup-menu-item newfnc))
    (super-instantiate ())))

(define (add-custom-popup-items item-maker obj)
  (define ppir (make-object popup-item-provider%))
  (send ppir use-this-popup-fnc item-maker)
  (send ppir attach-to obj))
