#lang racket/base

(provide make-submenu-Edit
         make-submenu-Select-Highlighted
         make-submenu-Arrange
         make-submenu-Object
         make-submenu-Font
         make-submenu-Tools
         ; needed by menubar
         Load-Image
         Get-Font-Name-For-Selected
         fillgetcolor
         pengetcolor
         Resize-Selected
         Reshape-Selected
         Raise-Selected
         Lower-Selected
         Group-Selected
         UnGroup-Selected
         Save-All-As ; useful for direct saves from code but dangerous
         Save-Current
         Select-and-Save-All
         ask-user-for-color
         *default_directory*
         Select-and-Import-File)

(require (only-in racket/class
                  send
                  make-object
                  is-a?
                  )
         (only-in racket/gui/base
                  get-file
                  dialog%
                  list-box%
                  horizontal-pane%
                  button%
                  get-color-from-user
                  message-box
                  )
         dynapad/pad-state
         dynapad/base
         dynapad/copy
         dynapad/save  ; Select-File-Dialog
         dynapad/misc/misc
         dynapad/misc/progress
         (only-in dynapad/utils/colors
                  mred-color-from-tcl-color
                  tcl-color-from-mred-color
                  )
         (only-in collects/misc/pathhack
                  split-path->string)
         (only-in dynapad/history/logs
                  ensure-keyframe
                  )
         (only-in dynapad/history/logbranch
                  current-logtree
                  current-logbranch
                  )
         (only-in dynapad/undo-state
                  undoify-fresh-obj
                  delete-all
                  Set-Select--undoable
                  Start-Changing-Select--undoable
                  Done-Changing-Select--undoable
                  restore-path
                  import-path
                  )
         (only-in dynapad/history/undo
                  undo
                  undoable-delete-all
                  Paste-From-Copy-Buffer
                  Copy-Paste-ReSelect
                  Delete-Selected
                  Deep-Delete-Selected
                  Confirm-and-Delete-All
                  )
         (only-in dynapad/menu/menu_shared
                  Select-and-Load-File
                  nyi
                  )
         (only-in dynapad/image
                  image%
                  pdf%
                  )
         (only-in dynapad/events/reshape
                  reshape-polygon)
         (only-in dynapad/utils/resize
                  resize-object)
         (only-in dynapad/events/draw
                  initDraw
                  )
         (only-in dynapad/utils/resize
                  make-selected-bigger
                  make-selected-smaller)
         (only-in dynapad/events/text
                  text%
                  )
         (only-in dynapad/events/hyperlink
                  initCreateLink)
         (only-in dynapad/layout/arrange
                  arrange-in-spiral-in-current-view
                  arrange-in-grid-in-current-view
                  )
         (only-in dynapad/misc/command-shortcuts
                  get-hostname  ; FIXME cmd shorts should not be used in core here
                  )
         (only-in dynapad/menu/menu-state
                  append-mainmenu-constructor)

         (only-in dynapad/menu/wxmenu
                  add-menu-separator
                  add-menu-item
                  add-submenu
                  )
         )

#; ; no longer needed
(announce-module-loading "Menu functions...")

;======= Menu Functions =================
(define (any-accept-method? fnc)
  (define select-list
    (filter (lambda (obj) (has-method? obj fnc)) (send currentPAD selected)))
  (not (null? select-list)) )

(define-syntax send-selected-that-accept-method
  (syntax-rules ()
    ((_ msg args ...)
     (for-each (lambda (obj) (when (has-method? obj 'msg) (send obj msg args ...)))
               (send currentPAD selected)))))


(define modebutton #f)  ; placeholder variable

(define *default_directory* #f) ; FIXME parameter probably

(define (Load-Image)
  (let* ((path (send currentPAD get-path))
         (dir *default_directory*)
         (filename #f)
         (dir? #f))

    (when path
      (let-values ([(a b c) (split-path->string path)])
        (set!-values (dir filename dir?)
                     (values (if (eq? 'relative a) dir a) b c))))

    ; racket cannot easily control the location at which the dialog
    ; is created if this uses the native gui (which we want)
    (set! path (get-file "Load" *menubar* dir #f #f '()))

    (set! *default_directory* dir)

    (when path
      (load-image-current-view currentPAD path))))

(define (load-image-current-view PAD fullpath)
  (undoify-fresh-obj
   (make-object image% PAD fullpath (center-of-view PAD))))

(define (make-image-fullsize img)
  (send img widthheight (send (send img imagedata) dimensions)))

(define (load-pdf-current-view PAD fullpath)
  (make-object pdf% PAD fullpath (center-of-view PAD)))

#; ; defined in dynapad/image-utils/arrangeimages
(define (Arrange-Images) (not-implemented "Load Image Dir")) ; placeholder function

(define (fontdialog apply-new-font-callback cancel-callback)
  (let
      ((fontnames (send dynapad fontnames))
       (dialog #f)
       (listbox #f)
       (buttonpane #f)
       ;(objfonts (map (lambda (o) (list o (send o font))) l))
       )

    (define (cb lbo e)
      (let*
          ((n (send lbo get-selections))
           (font (if (null? n) #f (list-ref fontnames (car n)))))

        (cond
          ((eq? (send e get-event-type) 'list-box)
           (when font
             (apply-new-font-callback font)))
          ((eq? (send e get-event-type) 'list-box-dclick)
           (send (send lbo get-parent) show #f)))))

    (set! dialog (make-object dialog% "Font" *menubar* 220 700 #f #f null))
    (set! listbox (make-object list-box% #f fontnames dialog cb '(single)))
    (set! buttonpane (make-object horizontal-pane% dialog))
    (send buttonpane stretchable-height #f)
    (send buttonpane set-alignment 'right 'center)
    (make-object button% "Cancel" buttonpane
                 (lambda (button event)
                   ;(for-each (lambda (objfont) (send (car objfont) font (cadr objfont))) objfonts)
                   (cancel-callback)
                   (send dialog show #f)))
    (make-object button% "Ok" buttonpane
                 (lambda (button event)  (send dialog show #f)))
    (send dialog show #t)))

(define (Get-Font-Name-For-Selected apply-callback cancel-callback)
  (fontdialog apply-callback cancel-callback))

;       ((focus (send argPAD getfocus))
;        (objs (if focus (list focus)
;          (filter
;            (lambda (x)
;              (cond
;                ((method-in-interface? 'font (object-interface x)) x)
;                (else #f)))
;            (send argPAD selected)))))


(define (Reshape-Selected argPAD)
  (foreach (send argPAD selected)
           (lambda (obj)
             (send obj unselect) (send obj select) ; force removal of existing handles
             (cond
               ((is-a? obj polygon%) (reshape-polygon argPAD obj))
               ((is-a? obj line%) (reshape-polygon argPAD obj))
               ((or (is-a? obj rect%)
                    (is-a? obj oval%)
                    (is-a? obj image%))
                (resize-object argPAD obj))))))

(define (Resize-Selected argPAD)
  (foreach (send argPAD selected)
           (lambda (obj)
             (send obj unselect) (send obj select) ; force removal of existing handles
             (resize-object argPAD obj))))

(define (makegroup argPAD l)
  (let ((g (make-object group% argPAD (reverse l))))
    (Set-Select--undoable argPAD g)))
; UNDO HERE
;    (map (lambda (o) (send o unselect)) l)
;    (send g select)))

(define (unmakegroup argPAD item-list)
  (Start-Changing-Select--undoable argPAD)
  (foreach item-list
           (lambda (g)
             (when (is-a? g group%)
               (let ((mems (send g members)))
                 (when (send g ungroup)
                   (foreach mems (lambda (m) (send m select))))
                 ))))
  ;;UNDO HERE
  (Done-Changing-Select--undoable argPAD)
  )

(define (Group-Selected)
  (let ((l (send currentPAD selected)))
    (when (not (and (= (length l) 1) (is-a? (car l) group%)))
      (makegroup currentPAD l))))

(define (UnGroup-Selected)
  (unmakegroup currentPAD (send currentPAD selected)))

(define (makealist objs)
  (let ((alist (map (lambda (o) (list (send o oldid) o)) objs)))
    alist))

(define (fix-object-ids objs alist)
  (for-each
   (lambda (o)
     (send o fix alist))
   objs))

(define fix fix-object-ids) ;for backward compatibility

; === Saving and Loading ===

;(define (Select-File-Dialog mode) ;mode is 'load or 'save
;  (let ((path (send currentPAD get-path))
;        (dir *current_directory*)
;        (filename #f)
;        (dir? #f))
;    (if path
;        (set!-values (dir filename dir?) (split-path->string path)))
;    (set! path
;          (cond ((eq? mode 'save)
;                 (put-file "Save" *menubar* dir filename #f null))
;                (else
;                 (get-file "Load" *menubar* dir #f #f null))))
;    path))

(define (Save-Current)
  (let ((path (send currentPAD get-path)))
    (if path
        (Save-All-As path)
        (Select-and-Save-All))))

(define (Select-and-Save-All)
  (let ((path (Select-File-Dialog 'save)))
    (when path (Save-All-As path))
    path))

; Old:
(define (Write-All-Objects-To-Port port)
  (let ((ok-to-write?
         (lambda (o)
           (andmap (lambda (fn) (not (fn o)))
                   (send currentPAD reject-save-filters) ))))
    (for-each
     (lambda (o)
       (let ((write-str
              (and (ok-to-write? o)
                   ;(not (child-of-formation? o))
                   (send o writable?)
                   (send o write))))
         (when write-str
           (fprintf port "~s~%" write-str))))
     (send currentPAD objects))))

; Old Save Format:

(define (Select-and-Import-File)
  (Select-and-Load-File import-path))

(define (Fillcolor-Dialog)
  (let* ((tclcolor (fillgetcolor)))
    (when tclcolor (send-selected-that-accept-method fill tclcolor))))

(define (Pencolor-Dialog)
  (let* ((tclcolor (pengetcolor)))
    (when tclcolor (send-selected-that-accept-method pen tclcolor))))

(define (Penwidth-Dialog) (not-implemented "Penwidth"))

(define (DrawRect-Mode filled?) (send currentPAD fill? filled?) (initDraw currentPAD rect% "Draw"))
(define (DrawOval-Mode filled?) (send currentPAD fill? filled?) (initDraw currentPAD oval% "Draw"))
(define (DrawPoly-Mode filled?) (send currentPAD fill? filled?) (initDraw currentPAD polygon% "Draw"))
(define (DrawLine-Mode poly?)   (send currentPAD fill? poly?)   (initDraw currentPAD line% "Draw"))
(define (DrawFree-Mode filled?) (send currentPAD fill? filled?) (initDraw currentPAD freehand% "Draw"))
(define (DrawText-Mode)         (initDraw currentPAD text% "DrawText"))
(define (ImageBrowse-Dialog)    (Load-Image))
(define (DrawLink-Mode)         (initCreateLink currentPAD))

;OBSOLETE?
;(define (LoadPad-Dialog . do_clear?)
;  (if (null? do_clear?) (set! do_clear? #t) (set! do_clear? (car do_clear?)))
;  (if do_clear?
;    ;Delete-All
;    (let ((l (filter (lambda (x) (send x deletable?)) (send currentPAD objects))))
;      (foreach l (lambda (x) (send x delete)))
;    )
;  )
;  (Load-File)
;)

;--- not implemented --------------------------------------------

(define (warning msg) (printf "menu_functions warning: ~a~%" msg))
(define (not-implemented op_name)
  (warning (string-append op_name ": not implemented yet")) )

;----------------------------------------------------------------

(define (Cut-Selected) (not-implemented "Cut"))
;(define (Copy-Selected) (not-implemented "Copy"))
(define (Do-Paste) (Paste-From-Copy-Buffer))
(define (Duplicate-Selected) (not-implemented "Dup"))

(define (Call-Inspect) (not-implemented "Inspect"))

(define (DocOpen-Dialog) (not-implemented "Docopen"))
(define (DocInfo-Dialog) (not-implemented "Docinfo"))
(define (PermissionBits-Dialog) (not-implemented "Pbits"))

(define (Transparency-Dialog) (not-implemented "Transparency Dialog"))

(define (SaveRegion-Dialog) (not-implemented "Regions"))
(define (SaveRegionAs-Dialog) (not-implemented "Regions"))

(define (BackgroundColor-Dialog) (not-implemented "Background Color"))
(define (FullscreenToggle) (not-implemented "FullScreen"))
(define (ShowConsole) (not-implemented "Console"))
(define (Menustyle-Dialog) (not-implemented "Menu Style"))

(define (MagnifyTool-Mode) (not-implemented "Magnify Tool"))

(define (Do-MakeBookmark) (not-implemented "Bookmark"))
(define (Bookmarks-Dialog) (not-implemented "Bookmark"))

(define (RenameBookmark) (not-implemented "Rename Bookmark"))
(define (TakeSnapshot) (not-implemented "Snapshot"))

(define (MakePadButton) (not-implemented "Button Widget"))
(define (MakePadScrollbar) (not-implemented "Scrollbar Widget"))
(define (MakePadTextEntry) (not-implemented "TextEntry Widget"))
(define (MakePadCheckBox) (not-implemented "CheckBox Widget"))

(define (Color-Dialog) (not-implemented "Color Editor"))
(define (Font-Dialog) (not-implemented "Font Dialog"))

(define (Selection-Dialog) (not-implemented "Select Modes"))

(define (Select-All)
  (Set-Select--undoable currentPAD (send currentPAD objects)))

(define (Arrange-Selected patternname)
  (case patternname
    ('Horz (not-implemented "Horizontal Arrange"))
    ('Vert (not-implemented "Vertical Arrange"))
    ('Spiral (arrange-in-spiral-in-current-view (send currentPAD selected)))
    ('Grid (arrange-in-grid-in-current-view (send currentPAD selected)))
    ('Circle (not-implemented "Circle Arrange"))
    ))


;--- Pen color and Fill color functions --------------------------
;functions regarding tcl-color moved to colors.ss

(define (ask-user-for-color currentcolor)
  (set! currentcolor (mred-color-from-tcl-color currentcolor))
  (tcl-color-from-mred-color (get-color-from-user "color" *menubar* currentcolor null))
  )

(define (fillgetcolor)
  (let ((result (ask-user-for-color (send currentPAD defaultfill))))
    (if result
        (begin
          (send currentPAD defaultfill result)
          result)
        (send currentPAD defaultfill))))

(define (pengetcolor)
  (let ((result (ask-user-for-color (send currentPAD defaultpen))))
    (if result
        (begin
          (send currentPAD defaultpen result)
          result)
        (send currentPAD defaultpen))))

;; from logs.rkt

(define (Save-All-To-Port port) ;overrides Save-All-To-Port in menu_functions.ss
  ; save port is merely a wrapper file
  ; which redirects to the current log branch
  (ensure-keyframe)
  (fprintf port "(load-log ~s ~s ~s)~%"
           (get-hostname)
           (send (current-logtree) treename)
           (send (current-logbranch) logid))
  (send (current-logtree) cache-maxid) ;this could happen more frequently also
  )

; ================= HACK! THHPTH! ==========
;add to popup menu
(append-mainmenu-constructor
 (lambda (menu obj)
   (add-menu-separator menu)
   (add-menu-item menu "Save state..."
                  (lambda () (Save-All-As (Select-File-Dialog 'save) Save-All-To-Port))
                  )))

;; from menu_functions Old Save Format:
; Used to re-save current pad.  (Does no error checking)
(define Save-All-As
  (case-lambda
    ((fullfilename) (Save-All-As fullfilename Save-All-To-Port))
    ((fullfilename save-fn)
     (with-handlers
       ([exn?
         (lambda (exn)
           (message-box  "Save" (exn-message exn) *menubar* '(ok)))])
       (unless fullfilename
         (error "no filename specified"))
       (send currentPAD set!-path fullfilename)
       (call-with-output-file fullfilename #:exists 'truncate
         save-fn)))))

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

(define (Raise-Selected argPAD)
  (foreach (send argPAD selected)
           (lambda (obj)
             (send obj raise))))

(define (Lower-Selected argPAD)
  (foreach (send argPAD selected)
           (lambda (obj)
             (send obj lower))))

(define (RaiseByOne-Selected) (send-selected raise 'one))
(define (LowerByOne-Selected) (send-selected lower 'one))

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
(define *wintop_obj_fill_mi* #f)
(define *wintop_obj_pen_mi* #f)
(define (make-submenu-Object mb object)
  (define sb (add-submenu mb "Object"))

  (add-menu-item sb "Make Bigger" make-selected-bigger (any-selected?))
  (add-menu-item sb "Make Smaller" make-selected-smaller (any-selected?))
  (add-menu-separator sb)
  ; FIXME maybe for these the right solution is to wrap in lambda and pass dynapad?
  (add-menu-item sb "Resize" Resize-Selected (any-selected?)) ; FIXME this seems very wrong
  (add-menu-item sb "Reshape" Reshape-Selected (any-selected?)) ; FIXME this seems very wrong
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
