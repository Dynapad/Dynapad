(dynaload "wxmenu.ss")
(dynaload "menu_shared.ss")
(dynaload "actor.ss")

(define (handle-popup-event e)
  (define pmenu #f)
  (if (is-a? (event-obj e) dynapad%)
    (set! pmenu (make-popup-menu #f))
    (let ((obj (get-top-group (event-obj e))))
      (when #t ;(send obj findable)
        (unless (send obj selected?)
            (let ((already-selected (send (send obj dynapad) selected)))
              (foreach already-selected (lambda (o) (send o unselect)))
              (send obj select)))
        (set! pmenu (make-popup-menu obj)))))
  (when pmenu (show-popup-function pmenu (event-sx e) (event-sy e)))
)

(define show-popup-function (show-popup-lambda dynapad (make-popup-server)))

(define (enable-popups-old-events argPAD)
  (send dynapad bind "<Control-Run-ButtonPress-3>"
    (lambda (w e) (handle-popup-event e) #f))
  (send dynapad bind "<Control-Select-ButtonPress-3>"
    (lambda (w e) (handle-popup-event e) #f))
)

(define (enable-popups argPAD)
  (set! *popup-menus-enabled?* #t)
;  (re-map-old-zoom-functions-to-Alt-modifier-versions argPAD)
; ^NEEDS ATTENTION...

  (send dynapad bind "<Run-ButtonRelease-3>"
    (lambda (w e) (handle-popup-event e) #f))
  (send dynapad bind "<Select-ButtonRelease-3>"
    (lambda (w e) (handle-popup-event e) #f))
)

(define (re-map-old-zoom-functions-to-Alt-modifier-versions argPAD)
  (send argPAD bind "<Run-Control-ButtonPress-2>"   Zoom-In-lambda)
  (send argPAD bind "<Run-Control-ButtonRelease-2>" Zoom-In-Stop-lambda)
  (send argPAD bind "<Run-Control-ButtonPress-3>"   Zoom-Out-lambda)
  (send argPAD bind "<Run-Control-ButtonRelease-3>" Zoom-Out-Stop-lambda)

  (send argPAD bind "<Select-Control-ButtonPress-2>"   Select-Zoom-In-lambda)
  (send argPAD bind "<Select-Control-ButtonRelease-2>" Select-Zoom-In-Stop-lambda)
  (send argPAD bind "<Select-Control-ButtonPress-3>"   Select-Zoom-Out-lambda)
  (send argPAD bind "<Select-Control-ButtonRelease-3>" Select-Zoom-Out-Stop-lambda)
)

;-----------------------------------------------------------------
;
; Application Specific submenus
;
; Applications should call (append-mainmenu .... (lambda (menu obj) ... ))
;  where (lambda...) builds app-specific menus using (add-menu-item...) etc.

(define *application-mainmenu-constructors* null)
;list of fns to generate app-specific submenus

(define (append-mainmenu-constructor fn)
  (set! *application-mainmenu-constructors*
    (append *application-mainmenu-constructors* (list fn))))

(define (include-application-mainmenu-constructors popmenu object)
  (for-each (lambda (fn) (fn popmenu object)) *application-mainmenu-constructors*))

; object is dynaobject% or #f (for dynapad%)
(define (make-popup-menu object)
  (if (and object (send-actor object 'provides-popup?))
    ;object-specific menu
    (let ((popmenu (send-actor object 'make-popup-menu)))
      (unless (send object findable)  ;kludgy, but this allows unfindable objs
                                  ; to have menus w/o being selected
          (send object unselect))
      (when popmenu (set! popmenu (car popmenu))) ;in case multiple actors respond
      popmenu
    )
    ;generic/background menu
    (let ((popmenu (new-popup (main-menu-title object))))
      (make-submenu-File    popmenu object)
      (make-submenu-Selector popmenu object) ;in event-state.ss
      (make-submenu-Edit    popmenu object)
      (make-submenu-Draw    popmenu object)
      (make-submenu-Arrange popmenu object)
      (make-submenu-Object  popmenu object)
      (make-submenu-Font    popmenu object)
      (make-submenu-Tools   popmenu object)

      (include-custom-popup-items popmenu object)

      (include-application-mainmenu-constructors popmenu object)

      popmenu
    )
  )
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
(define popup-item-provider%
  (class actor%
    (field (_make-popup-menu-item #f))
    (inherit-field _object)

    (define/public (provides-popup-item?) _make-popup-menu-item)
    (define/public (make-popup-items menu) (_make-popup-menu-item _object menu))
    (define/public (use-this-popup-fnc newfnc) (set! _make-popup-menu-item newfnc))
    (super-instantiate ())))

(define (include-custom-popup-items menu obj)
  (when (and obj (send-actor obj 'provides-popup-item?))
    (send-actor obj 'make-popup-items menu)))

(define (add-custom-popup-items item-maker obj)
  (define ppir (make-object popup-item-provider%))
  (send ppir use-this-popup-fnc item-maker)
  (send ppir attach-to obj))


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

(define (special-menu-item-maker obj menu)
  (add-menu-item menu "I'm special!" void #f)
  )

; Make it so:
(enable-popups dynapad)
