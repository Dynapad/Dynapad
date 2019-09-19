;(dynaload "menu_functions.ss")
(dynaload "import-dirs.ss")

(define (nyi msg) (lambda () (printf "~a: not yet implemented~%" msg)))

; global vars
(define *wintop_menu* #f)

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

;
; Export submenu
;
(define (make-submenu-Export mb object)
  (define sb (add-submenu mb "Export"))
  (add-menu-item sb "Export Selected..."
                 (lambda () (Export-To-Directories (send currentPAD selected)))
                 (any-selected?))
  (add-menu-item sb "Export All..." Export-To-Directories))




;
; Edit submenu
;
(define (make-submenu-Edit mb object)
  (define sb (add-submenu mb "Edit"))

  (add-menu-item sb "Undo" undo)

  (add-menu-separator sb) ;------------------------------

  (add-menu-item sb "Cut" Cut-Selected       (or (eq? object #t) (any-selected?)))
  (add-menu-item sb "Copy" Copy-Selected     (or (eq? object #t) (any-selected?)))
  (add-menu-item sb "Paste" Do-Paste         (or (eq? object #t) (not (Copy-Buffer-empty?))))
  (add-menu-item sb (if (many-selected?) "Delete Selected" "Delete")
                 Delete-Selected (or (eq? object #t) (any-selected?)))
  (add-menu-item sb (if (many-selected?) "Delete Selected Groups" "Delete Group")
                 Deep-Delete-Selected (or (eq? object #t) (any-selected?)))

  (add-menu-separator sb) ;------------------------------

  (add-menu-item sb "Select All" Select-All
                 (or (eq? object #t) (any-objects?)))
  (add-menu-item sb "Delete All" Confirm-and-Delete-All
                 (or (eq? object #t) (any-objects?)))
  )


;
; Arrange submenu
;
(define (make-submenu-Arrange mb object)
  (define sb (add-submenu mb "Arrange"))

  (add-menu-item sb "Bring to Front" (lambda () (Raise-Selected dynapad)))
  (add-menu-item sb "Send to Back" (lambda () (Lower-Selected dynapad)))
  (add-menu-item sb "Bring Closer" (lambda () (RaiseByOne-Selected dynapad)))
  (add-menu-item sb "Send Further" (lambda () (LowerByOne-Selected dynapad)))

  (add-menu-separator sb) ;------------------------------

  (add-menu-item sb "Group" Group-Selected
                 (or (eq? object #t) (more-than-one-selected?)))
  (add-menu-item sb "Ungroup" UnGroup-Selected
                 (or (eq? object #t) (any-accept-method? 'ungroup)))

  (add-menu-separator sb) ;------------------------------

  (add-menu-item sb "Lock" (nyi "Lock-Selected") #f)
  (add-menu-item sb "UnLock" (nyi "UnLock-Selected") #f)
  (add-menu-item sb "Hide" (nyi "Hide-Selected") #f)
  (add-menu-item sb "Show All" (nyi "ShowAll") #f)
  )

;
; Object submenu
;
(define (make-submenu-Object mb object)
  (define sb (add-submenu mb "Object"))

  (add-menu-item sb "Make Bigger" make-selected-bigger (any-selected?))
  (add-menu-item sb "Make Smaller" make-selected-smaller (any-selected?))
  (add-menu-separator sb)
  (add-menu-item sb "Resize" Resize-Selected (any-selected?))
  (add-menu-item sb "Reshape" Reshape-Selected (any-selected?))
  (add-menu-item sb "Rotate" (nyi "Rotate") #f)
  (add-menu-item sb "Flip Vertical" (nyi "Flip Vertical") #f)
  (add-menu-item sb "Flip Horizontal" (nyi "Flip Horizontal") #f)
  ;   (add-menu-item sb "Smooth" (nyi "Smooth") #f)
  ;   (add-menu-item sb "Unsmooth" (nyi "Unsmooth") #f)

  (when (or (eq? object #t) (any-accept-method? 'fill))
    (let ((mi (add-menu-item sb "Fill Color..." Fillcolor-Dialog)))
      (when (eq? object #t) (set! *wintop_obj_fill_mi* mi))))

  (when (or (eq? object #t) (any-accept-method? 'pen))
    (let ((mi (add-menu-item sb "Pen Color..." Pencolor-Dialog)))
      (when (eq? object #t) (set! *wintop_obj_pen_mi* mi))))

  ;
  ; Fade submenu
  ;
  (let* ((sb_up sb)
         (sb (add-submenu sb_up "Fade")))
    (add-menu-item sb "None" (nyi "None") #f)
    (add-menu-item sb "Default" (nyi "Default") #f)
    (add-menu-item sb "When smaller" (nyi "When smaller") #f)
    (add-menu-item sb "When larger" (nyi "When larger") #f)
    (add-menu-item sb "Cross" (nyi "Cross") #f)
    )

  (add-menu-separator sb) ;------------------------------

  ;
  ; Sticky submenu
  ;
  (let* ((sb_up sb)
         (sb (add-submenu sb_up "Sticky")))
    (add-menu-item sb "UnSticky" (nyi "UnSticky") #f)
    (add-menu-item sb "Sticky" (nyi "Sticky") #f)
    (add-menu-item sb "Sticky X" (nyi "Sticky X") #f)
    (add-menu-item sb "Sticky Y" (nyi "Sticky Y") #f)
    (add-menu-item sb "Sticky Z" (nyi "Sticky Z") #f)
    (add-menu-item sb "Sticky View" (nyi "Sticky View") #f)
    )

  (add-menu-item sb "Center" (nyi "Center") #f)
  (add-menu-item sb "Make same size" (nyi "Make same size") #f)

  (add-menu-separator sb) ;------------------------------

  (add-menu-item sb "Properties..." (nyi "Properties") #f)
  (add-menu-item sb "Line Props..." (nyi "Line Props") #f)
  )

;
; Font submenu
;
(define (make-submenu-Font mb object)
  (define sb (add-submenu mb "Font"))
  (add-menu-item sb "Size" (nyi "Size") #f)
  (add-menu-item sb "Name" Get-Font-Name-For-Selected)
  (add-menu-item sb "Style" (nyi "Style") #f)
  )

;
; Tools submenu
;
(define (make-submenu-Draw mb object)
  (let ((pad (cond ((is-a? object dynapad%) object)
                   ((not object) currentPAD)
                   (else (send object dynapad)))))
    (define sb (add-submenu mb "Draw"))
    (add-menu-item sb "Text" (lambda () (initDraw dynapad text% "DrawText")))
    (add-menu-item sb "Rect" (lambda () (initDraw dynapad rect% "Draw")))
    (add-menu-item sb "Oval" (lambda () (initDraw dynapad oval% "Draw")))
    (add-menu-item sb "Polygon" (lambda () (initDraw dynapad polygon% "Draw")))
    (add-menu-item sb "Line" (lambda () (initDraw dynapad line% "Draw")))
    (add-menu-item sb "PolyLine" (lambda () (initDraw dynapad polyline% "Draw")))
    (add-menu-separator sb) ;------------------------------
    (add-checkable-menu-item sb "Fill shapes"
                             (lambda (i)
                               (send pad fill? (send i is-checked?)))
                             (send pad fill?))
    (add-checkable-menu-item sb "Draw multiple"
                             (lambda (i)
                               (set! Draw-multiple (send i is-checked?)))
                             Draw-multiple)
    ))

(define (make-submenu-Tools mb object)
  (define sb (add-submenu mb "Tools"))
  ;
  ; Create Object submenu
  ;
  (add-menu-item sb "Drawing Tool..." (nyi "Drawing Tool") #f)
  (add-menu-item sb "Colors Tool..." (nyi "Colors Tool") #f)

  (add-menu-item sb "Fill Color..." Fillcolor-Dialog)
  (add-menu-item sb "Pen Color..." Pencolor-Dialog)

  (add-menu-item sb "Layers..." (nyi "Layers Tool") #f)
  (add-menu-item sb "Selection..." (nyi "Selection Tool") #f)

  ;
  ; Layout submenu
  ;
  (let* ((sb_up sb)
         (sb (add-submenu sb_up "Layout...")))
    (add-menu-item sb "Align..." (nyi "Align Dialog") #f)
    (add-menu-item sb "Distribute..." (nyi "Distribute Dialog") #f)
    )
  )

(define (make-submenu-Select-Highlighted mb obj) #f) ;hook to be overridden
