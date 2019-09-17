(dynaload "menu_popup.ss")


; redefine top level menu for this application

; object is dynaobject% or #f (for dynapad%)
(define (make-popup-menu object)
  (if (and object (send-actor object 'provides-popup?))
      (let ((popmenu (send-actor object 'make-popup-menu)))
        (if popmenu (set! popmenu (car popmenu)))
        popmenu
        )
      ;else
      (let ((popmenu 0))

        (if object
            (set! popmenu (new-popup "Object Menu"))
            ;else
            (set! popmenu (new-popup "Photo Op Menu")))

        ;----------

        (add-menu-item popmenu "Import Images From Directory..." Arrange-Images)
        (add-menu-item popmenu "Make Bigger" make-selected-bigger (any-selected?))
        (add-menu-item popmenu "Make Smaller" make-selected-smaller (any-selected?))
        (add-menu-separator popmenu)

        ;----------

        (make-submenu-File    popmenu object)
        (make-submenu-Edit    popmenu object)
        (make-submenu-Arrange popmenu object)
        (make-submenu-Object  popmenu object)
        (make-submenu-Font    popmenu object)
        (make-submenu-Tools   popmenu object)

        (include-submenus-for-applications popmenu object)
        ;(make-submenus-for-applications popmenu object)

        popmenu
        )
      )
  )
