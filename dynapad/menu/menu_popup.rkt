#lang racket/base

(require racket/class
         (only-in dynapad/pad-state
                  dynapad
                  ; accessors from struct
                  event-obj
                  event-sx
                  event-sy
                  )
         (only-in dynapad/base
                  dynapad%)
         (only-in dynapad/undo-state
                  get-top-group
                  )
         dynapad/misc/misc
         (only-in dynapad/utils/actor
                  actors%)
         dynapad/menu/menu-state
         (only-in dynapad/utils/import-dirs
                  make-submenu-Import
                  )
         dynapad/menu/wxmenu
         (only-in dynapad/menu/menu_functions
                  make-submenu-Edit
                  )
         (only-in dynapad/events/zoom-classic
                  Zoom-In-lambda
                  Zoom-In-Stop-lambda
                  Zoom-Out-lambda
                  Zoom-Out-Stop-lambda
                  )
         (only-in dynapad/events/zoom-obj
                  Select-Zoom-In-lambda
                  Select-Zoom-In-Stop-lambda
                  Select-Zoom-Out-lambda
                  Select-Zoom-Out-Stop-lambda
                  )
         )

(provide add-custom-popup-items
         *popup-menus-enabled?*
         )

(define *popup-menus-enabled?* #f)

(define (main-menu-title obj) ;probably overridden by application module
  (if obj "Object Menu" "Dynapad Menu"))

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

(define (include-custom-popup-items menu obj)
  (when (and obj (send-actor obj 'provides-popup-item?))
    (send-actor obj 'make-popup-items menu)))


(define (special-menu-item-maker obj menu)
  (add-menu-item menu "I'm special!" void #f)
  )

;
; File submenu
;
(define (make-submenu-File mb object)
  (define sb (add-submenu mb "File"))

  (add-menu-item sb "New Pad" (nyi "New-Pad") #f)

  ;Loading/Importing:
  (add-menu-item sb "Revert Workspace"         Restore-Current)
  (add-menu-item sb "Restore Workspace..." Select-and-Restore-File)
  ;(lambda () (LoadPad-Dialog #t)))

  ;  (add-menu-item sb "Import Pad File..."  Select-and-Import-File)
  (make-submenu-Import sb object)

  (add-menu-separator sb) ;------------------------------
  ;Saving/Exporting:
  (add-menu-item sb "Save" Save-Current)
  (add-menu-item sb "Save As..." Select-and-Save-All)
  (make-submenu-Export sb object)

  ;  (add-menu-item sb "Import Image..." Load-Image)
  ;  (add-menu-item sb "Import Image Dir..." (lambda () (Arrange-Images)))
  ;  ;(add-menu-item sb "Import..." (nyi "Import") #f)

  ;  (add-menu-separator sb) ;------------------------------

  ;  (add-menu-item sb "Import PDF..." (nyi "Import PDF")  #f)
  ;  (add-menu-item sb "Import PDF Dir..." (lambda () (Arrange-Pdfs)))

  (add-menu-separator sb) ;------------------------------
  ;
  ; Start submenu
  ;
  (let* ((sb_up sb)
         (sb (add-submenu sb_up "Starting view")))
    (add-menu-item sb "Set Start View"   (nyi "pad_set_start_view") #f)
    (add-menu-item sb "Goto Start View"  (nyi "pad_goto_start_view") #f)
    (add-menu-item sb "Clear Start View" (nyi "pad_clear_start_view") #f)
    )

  (add-menu-separator sb) ;------------------------------

  (add-menu-item sb "Close" (nyi "padClose") #f)

  (add-menu-separator sb) ;------------------------------

  (add-menu-item sb "Exit" exit)
  )

; Make it so:
(enable-popups dynapad)
