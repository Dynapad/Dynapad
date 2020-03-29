#lang racket/base

(require (only-in racket/class
                  send)
         (only-in racket/gui/base
                  get-file)
         dynapad/pad-state
         dynapad/copy
         dynapad/misc/misc
         dynapad/misc/progress
         dynapad/utils/colors
         (only-in collects/misc/pathhack
                  split-path->string)
         (only-in dynapad/history/undo
                  undoify-fresh-obj
                  delete-all
                  undoable-delete-all
                  Paste-From-Copy-Buffer
                  Copy-Paste-ReSelect))

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

(define *default_directory* #f)

(define Export-To-Directories
  (case-lambda
    (() (Export-To-Directories (send dynapad objects)))
    ((objs)
     (let ((newdir (Select-File-Dialog 'save)))
       (if (file-exists? newdir)
           (error "File or directory already exists:" newdir)
           (begin
             (make-directory newdir)
             (foreach objs (lambda (o) (send o export newdir)))
             newdir))))))

(define (Load-Image)
  (let* ((path (send currentPAD get-path))
         (dir *default_directory*)
         (filename #f)
         (dir? #f))

    (when path
      (set!-values (dir filename dir?) (split-path->string path)))

    (set! path (get-file "Load" *menubar* dir #f #f null))
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
  (Start-Changing-Select argPAD)
  (foreach item-list
           (lambda (g)
             (when (is-a? g group%)
               (let ((mems (send g members)))
                 (when (send g ungroup)
                   (foreach mems (lambda (m) (send m select))))
                 ))))
  ;;UNDO HERE
  (Done-Changing-Select argPAD)
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
(define *current_directory* (current-directory))
(define Select-File-Dialog
  (case-lambda
    ;mode is 'load or 'save
    ((mode) (Select-File-Dialog mode (send currentPAD get-path)))
    ((mode path)
     (let ((dir *current_directory*)
           (filename #f)
           (dir? #f))
       (when path
         (set!-values (dir filename dir?) (split-path->string path)))
       (when dir?
         (set! dir (build-path dir filename))
         (set! filename #f))
       (set! path
             (cond ((eq? mode 'save)
                    (put-file "Save" *menubar* dir filename #f null))
                   (else
                    (get-file "Load" *menubar* dir #f #f null))))
       path))))


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

;(define (Write-All-Objects-To-Port port)
;  (let ((allwrites (write-set (send currentPAD objects))))
;    (foreach allwrites (lambda (w) (fprintf port "~s~%" w)))))
;
;(define (Write-All-Objects-To-String)
;  (let ((allwrites (write-set (send currentPAD objects))))
;    (apply string-append (map (lambda (w) (format "~a~%" w)) allwrites))))
;
(define (Write-All-Objects-To-Expr)
  (let ((allwrites (write-set (saveable-objects))))
    allwrites))

; Old Save Format:
#; ; overwritten below so commenting out until it can be removed
(define (Save-All-To-Port port)
  (fprintf port "(let* ((objs (list~%")
  (Write-All-Objects-To-Port port)
  (fprintf port "))~%")
  (fprintf port "(id-alist (makealist objs))~%")
  (fprintf port "(writes (list~%")
  (for-each (lambda (x) (fprintf port "~s~%" (send x write))) *writelist*)
  (fprintf port ")))~%")
  (fprintf port "(fix-object-ids objs id-alist)~%")
  (fprintf port "(fix-object-ids writes id-alist)~%")
  (fprintf port ")~%")
  )

; New Save Format:
(define (Save-All-To-Expr)
  ;first build list for all objs so that *id-counter* can be incremented
  ; as far as necessary
  (let ((obj-expr-list (Write-All-Objects-To-Expr)))
    (cons 'load-set
          (cons `(max-padid ,*id-counter*)
                obj-expr-list))))

(define (Format-SaveAll-Expr-Into-String expr)
  (string-append "("
                 (apply string-append (map (lambda (e) (format "~s~%" e)) expr))
                 ")\n"))

(define (Save-All-To-String)
  (Format-SaveAll-Expr-Into-String (Save-All-To-Expr)))

;(define (Save-All-To-Port port)
;  (fprintf port "(load-set~%")
;  (fprintf port "(max-padid ~s)~%" *id-counter*)
;  (Write-All-Objects-To-Port port)
;  (fprintf port ")~%")
;)

(define (Save-All-To-Port port)
  (fprintf port (Format-SaveAll-Expr-Into-String (Save-All-To-Expr))))

;(define (Save-All-To-String)
;  (string-append
;   "(load-set\n"
;   (format "(max-padid ~a)~%" *id-counter*)
;   (Write-All-Objects-To-String)
;   ")\n"))


; Used to re-save current pad.  (Does no error checking)
(define Save-All-As
  (case-lambda
    ((fullfilename) (Save-All-As fullfilename Save-All-To-Port))
    ((fullfilename save-fn)
     (with-handlers
       ((exn?
         (lambda (exn)
           (message-box  "Save" (exn-message exn) *menubar* '(ok)))))
       (let ((port #f))
         (unless fullfilename
           (error "no filename specified"))
         (send currentPAD set!-path fullfilename)
         (set! port (open-output-file fullfilename #:exists 'truncate))
         (save-fn port)
         (close-output-port port))))))

(define Select-and-Load-File
  (case-lambda
    (() (Select-and-Load-File import-path)) ;import by default
    ((load-context-fn)
     (let ((path (Select-File-Dialog 'load)))
       (when path
         (load-context-fn path))
       path))))

(define (Select-and-Import-File)
  (Select-and-Load-File import-path))

(define (Select-and-Restore-File)
  (let ((path (Select-and-Load-File restore-path)))
    (when path
      (send currentPAD set!-path path))))

(define (Restore-Current)
  (let ((path (send currentPAD get-path)))
    (if path
        (restore-path path)
        (Select-and-Restore-File))))

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

(define (warning msg) (printf "~a~%" msg))
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
