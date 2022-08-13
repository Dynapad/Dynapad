#lang racket/base
; obsolete

(require
 racket/class
 (only-in racket/gui/base
          button%
          check-box%
          horizontal-pane%
          message%
          dialog%
          frame%)
 (only-in dynapad/pad-state
          *list-of-all-dynapads*
          *dynapad-directory*
          dynapad
          currentPAD)
 (only-in dynapad/base
          rect%
          oval%
          line%
          polyline%
          polygon%
          freehand%)
 (only-in dynapad/events/hyperlink
          initCreateLink)
 (only-in dynapad/events/mode
          changemode
          changemode--no-gui
          gui-mode-cursor)
 (only-in collects/misc/pathhack
          build-path->string)
 (only-in dynapad/menu/wx_emulator
          dywx-bitmap%
          dywx-frame%
          dywx-horizontal-pane%
          dywx-button%
          color-ellipse%)
 (only-in dynapad/misc/misc
          mlet
          push!
          foreach
          has-method?)
 (only-in dynapad/misc/user-preferences
          *draw-menu-x*
          *draw-menu-y*)
 (only-in dynapad/events/draw
          set-Draw-multiple!
          initDraw)
 (only-in dynapad/events/text
          text%)
 (only-in dynapad/menu/menu_functions
          Load-Image
          Get-Font-Name-For-Selected
          pengetcolor
          fillgetcolor
          Resize-Selected
          Reshape-Selected
          Raise-Selected
          Lower-Selected
          Group-Selected
          UnGroup-Selected
          Select-and-Import-File
          Save-Current
          Select-and-Save-All)
 (only-in dynapad/image-utils/arrangeimages
          Arrange-Images
          Arrange-Pdfs)
 (only-in dynapad/menu/menu_shared
          Restore-Current
          Select-and-Restore-File)
 (only-in dynapad/copy
          Copy-Selected)
 (only-in dynapad/history/undo
          Confirm-and-Delete-All
          Delete-Selected
          Paste-From-Copy-Buffer)
 (only-in dynapad/layout/bbox
          bbnw))

;Most of this is obsolete or redundant;
; see menubar.ss as example of revision.
;======= Simple Menubar ========


(define (for-all-pads body) (for-each body *list-of-all-dynapads*))

(define (gui-update-mode argPAD)
  (let* ((mode (send argPAD modifier 'get))
         (cursor (gui-mode-cursor mode)))

    ; change all others pads to match mode (?)
    (for-all-pads (lambda (PAD)
                    (when (not (eq? PAD argPAD))
                      (changemode--no-gui PAD mode))))

    (cond ((equal? mode "Run")
           (for-all-pads (lambda (PAD)
                           (send PAD cursor 12)))
           (clear-all-menu-buttons)
           (send btn_run set-label on_run))
          ((equal? mode "Select")
           (for-all-pads (lambda (PAD)
                           (send PAD cursor 13)))
           (clear-all-menu-buttons)
           (send btn_select set-label on_select))
          ((equal? mode "CreateLink") #t)
          ((equal? mode "Draw")    (for-all-pads (lambda (PAD) (send argPAD cursor 1))))
          ((equal? mode "Drag")    (for-all-pads (lambda (PAD) (send argPAD cursor 12))))
          ((equal? mode "DrawText")    (for-all-pads (lambda (PAD) (send argPAD cursor 2))))
          ((equal? mode "EditText")    (for-all-pads (lambda (PAD) (send argPAD cursor 2))))
          ((equal? mode "GetBBox") (for-all-pads (lambda (PAD) (send argPAD cursor 1))))
          (cursor (for-all-pads (lambda (PAD) (send argPAD cursor cursor))))
          (else
           ; unrecognized mode
           (for-all-pads (lambda (PAD) (send argPAD cursor 1))))
          )))

(define *time_last_button_press* #f)
(define (button-double-clicked?)
  (define dt 0)
  (if *time_last_button_press*
      (begin
        (set! dt (- (current-milliseconds) *time_last_button_press*))
        (set! *time_last_button_press* (current-milliseconds))
        (if (< dt 400) #t #f))
      ;else
      (begin
        (set! *time_last_button_press* (current-milliseconds))
        #f)))

(define (use-bitmap-or-string bitmapfile string)
  (let ((filename (build-path->string *dynapad-directory* bitmapfile)))
    (if (file-exists? filename)
        (let*((len (string-length bitmapfile))
              (filetype (string->symbol (substring bitmapfile (- len 3) len)))
              )
          (make-object dywx-bitmap% filename filetype)
          )
        string)))

(define *dynamenubar* (make-object dywx-frame% "Draw Tools" #f #f #f *draw-menu-x* *draw-menu-y*))
(define *frame* (make-object frame% "Draw Tools" #f #f #f 0 0))

(define *dynamenubar_offlist* '())

(define runpane      (make-object dywx-horizontal-pane% *dynamenubar*))
(define rectpane     (make-object dywx-horizontal-pane% *dynamenubar*))
(define ovalpane     (make-object dywx-horizontal-pane% *dynamenubar*))
(define linepane     (make-object dywx-horizontal-pane% *dynamenubar*))
(define polypane     (make-object dywx-horizontal-pane% *dynamenubar*))
(define freetextpane (make-object dywx-horizontal-pane% *dynamenubar*))
(define arcpane      (make-object dywx-horizontal-pane% *dynamenubar*)) ;may be used by edges.ss
(define hyperpane    (make-object dywx-horizontal-pane% *dynamenubar*))
(define selectpane   (make-object dywx-horizontal-pane% *dynamenubar*))


(define off_run        (use-bitmap-or-string "pad/bitmaps/hand.jpg"          "Run"         ))
(define off_select     (use-bitmap-or-string "pad/bitmaps/pointer.jpg"       "Select"      ))
(define off_rect       (use-bitmap-or-string "pad/bitmaps/rect.jpg"          "Rect"        ))
(define off_oval       (use-bitmap-or-string "pad/bitmaps/oval.jpg"          "Oval"        ))
(define off_poly       (use-bitmap-or-string "pad/bitmaps/polygon.jpg"       "Polygon"     ))
(define off_filledRect (use-bitmap-or-string "pad/bitmaps/filledRect.jpg"    "Filled Rect" ))
(define off_filledOval (use-bitmap-or-string "pad/bitmaps/filledOval.jpg"    "Filled Oval" ))
(define off_filledPoly (use-bitmap-or-string "pad/bitmaps/filledPolygon.jpg" "Filled Poly" ))
(define off_line       (use-bitmap-or-string "pad/bitmaps/line.jpg"          "Line"        ))
(define off_polyline   (use-bitmap-or-string "pad/bitmaps/lines.jpg"         "PolyLine"    ))
(define off_freehand   (use-bitmap-or-string "pad/bitmaps/drawing.jpg"       "Free"        ))
(define off_text       (use-bitmap-or-string "pad/bitmaps/letter.jpg"        "Text"        ))
(define off_portal     (use-bitmap-or-string "pad/bitmaps/portal.jpg"        "Portal"      ))
(define off_hyperlink  (use-bitmap-or-string "pad/bitmaps/hyperlink.jpg"     "HyperLink"   ))
(define off_marquee    (use-bitmap-or-string "pad/bitmaps/marquee.jpg"       "Marquee"     ))
(define off_lasso      (use-bitmap-or-string "pad/bitmaps/lasso.jpg"         "Lasso"       ))

(define on_run        (use-bitmap-or-string "pad/bitmaps/On_hand.jpg"          "Run"         ))
(define on_select     (use-bitmap-or-string "pad/bitmaps/On_pointer.jpg"       "Select"      ))
(define on_rect       (use-bitmap-or-string "pad/bitmaps/On_rect.jpg"          "Rect"        ))
(define on_oval       (use-bitmap-or-string "pad/bitmaps/On_oval.jpg"          "Oval"        ))
(define on_poly       (use-bitmap-or-string "pad/bitmaps/On_polygon.jpg"       "Polygon"     ))
(define on_filledRect (use-bitmap-or-string "pad/bitmaps/On_filledRect.jpg"    "Filled Rect" ))
(define on_filledOval (use-bitmap-or-string "pad/bitmaps/On_filledOval.jpg"    "Filled Oval" ))
(define on_filledPoly (use-bitmap-or-string "pad/bitmaps/On_filledPolygon.jpg" "Filled Poly" ))
(define on_line       (use-bitmap-or-string "pad/bitmaps/On_line.jpg"          "Line"        ))
(define on_polyline   (use-bitmap-or-string "pad/bitmaps/On_lines.jpg"         "PolyLine"    ))
(define on_freehand   (use-bitmap-or-string "pad/bitmaps/On_drawing.jpg"       "Free"        ))
(define on_text       (use-bitmap-or-string "pad/bitmaps/On_letter.jpg"        "Text"        ))
(define on_portal     (use-bitmap-or-string "pad/bitmaps/On_portal.jpg"        "Portal"      ))
(define on_hyperlink  (use-bitmap-or-string "pad/bitmaps/On_hyperlink.jpg"     "HyperLink"   ))
(define on_marquee    (use-bitmap-or-string "pad/bitmaps/On_marquee.jpg"       "Marquee"     ))
(define on_lasso      (use-bitmap-or-string "pad/bitmaps/On_lasso.jpg"         "Lasso"       ))

(define *guest-button-off-labels* null)

(define (clear-all-menu-buttons)
  (send btn_run         set-label off_run        )
  (send btn_select      set-label off_select     )
  (send btn_rect        set-label off_rect       )
  (send btn_filledRect  set-label off_filledRect )
  (send btn_oval        set-label off_oval       )
  (send btn_filledOval  set-label off_filledOval )
  (send btn_line        set-label off_line       )
  (send btn_polyline    set-label off_polyline   )
  (send btn_poly        set-label off_poly       )
  (send btn_filledPoly  set-label off_filledPoly )
  (send btn_freehand    set-label off_freehand   )
  (send btn_text        set-label off_text       )
  (send btn_portal      set-label off_portal     )
  (send btn_hyperlink   set-label off_hyperlink  )
  (when btn_marquee (send btn_marquee set-label off_marquee))
  (when btn_lasso (send btn_lasso set-label off_lasso))
  (foreach *guest-button-off-labels*
           (lambda (pair) (mlet (((btn lbl) pair)) (send btn set-label lbl))))
  )

(define btn_run
  (make-object dywx-button%
               off_run
               runpane
               (lambda (button event)
                 (clear-all-menu-buttons)
                 (send button set-label on_run)
                 (for-all-pads (lambda (d)
                                 (changemode d "Run")))
                 ))
  )
(define btn_select
  (make-object dywx-button%
               off_select
               runpane
               (lambda (button event)
                 (clear-all-menu-buttons)
                 (send button set-label on_select)
                 (for-all-pads (lambda (argPAD)
                                 (changemode argPAD "Select")))
                 ))
  )

(define btn_rect
  (make-object dywx-button%
               off_rect
               rectpane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #f)))
                 (clear-all-menu-buttons)
                 (send button set-label on_rect)
                 (set-Draw-multiple! (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD rect% "Draw")))))
  )

(define btn_filledRect
  (make-object dywx-button%
               off_filledRect
               rectpane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #t)))
                 (clear-all-menu-buttons)
                 (send button set-label on_filledRect)
                 (set-Draw-multiple! (button-double-clicked?))
                 (update-panel-colors)
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD rect% "Draw")))))
  )

(define btn_oval
  (make-object dywx-button%
               off_oval
               ovalpane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #f)))
                 (clear-all-menu-buttons)
                 (send button set-label on_oval)
                 (set-Draw-multiple! (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD oval% "Draw")))))
  )
(define btn_filledOval
  (make-object dywx-button%
               off_filledOval
               ovalpane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #t)))
                 (clear-all-menu-buttons)
                 (send button set-label on_filledOval)
                 (set-Draw-multiple! (button-double-clicked?))
                 (update-panel-colors)
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD oval% "Draw")))))
  )


(define btn_line
  (make-object dywx-button%
               off_line
               linepane
               (lambda (button event)
                 (clear-all-menu-buttons)
                 (send button set-label on_line)
                 (set-Draw-multiple! (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD line% "Draw")))))
  )

(define btn_polyline
  (make-object dywx-button%
               off_polyline
               linepane
               (lambda (button event)
                 (clear-all-menu-buttons)
                 (send button set-label on_polyline)
                 (set-Draw-multiple! (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD polyline% "Draw")))))
  )

(define btn_portal
  (make-object dywx-button%
               off_portal
               hyperpane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #f)))
                 (clear-all-menu-buttons)
                 (send button set-label on_portal)
                 (set-Draw-multiple! (button-double-clicked?))
                 (printf "portals not migrated yet~%")
                 ))
  )

(define btn_hyperlink
  (make-object dywx-button%
               off_hyperlink
               hyperpane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #f)))
                 (clear-all-menu-buttons)
                 (send button set-label on_hyperlink)
                 (set-Draw-multiple! (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initCreateLink argPAD)))
                 ))
  )

(define btn_marquee #f)
(define btn_lasso #f)

; not shown in the default.  see event-binders.ss
(define (enable-marquee-and-lasso)
  (set! btn_marquee
        (make-object dywx-button%
                     off_marquee
                     selectpane
                     (lambda (button event)
                       (for-all-pads (lambda (argPAD)
                                       (send argPAD fill? #f)))
                       (clear-all-menu-buttons)
                       (send button set-label on_marquee)
                       (printf "marquee select not available yet~%") )))

  (set! btn_lasso
        (make-object dywx-button%
                     off_lasso
                     selectpane
                     (lambda (button event)
                       (for-all-pads (lambda (argPAD)
                                       (send argPAD fill? #f)))
                       (clear-all-menu-buttons)
                       (send button set-label on_lasso)
                       (printf "lasso select not available yet~%") )))
  )

(define btn_poly
  (make-object dywx-button%
               off_poly
               polypane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #f)))
                 (clear-all-menu-buttons)
                 (send button set-label on_poly)
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD polygon% "Draw")))))
  )

(define btn_filledPoly
  (make-object dywx-button%
               off_filledPoly
               polypane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #t)))
                 (clear-all-menu-buttons)
                 (send button set-label on_filledPoly)
                 (set-Draw-multiple! (button-double-clicked?))
                 (update-panel-colors)
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD polygon% "Draw")))))
  )

(define btn_freehand
  (make-object dywx-button%
               off_freehand
               freetextpane
               (lambda (button event)
                 (clear-all-menu-buttons)
                 (send button set-label on_freehand)
                 (set-Draw-multiple! (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD freehand% "Draw")))))
  )

(define btn_text
  (make-object dywx-button%
               off_text
               freetextpane
               (lambda (button event)
                 (clear-all-menu-buttons)
                 (send button set-label on_text)
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD text% "DrawText")))))
  )

(define imagefontpane (make-object dywx-horizontal-pane% *dynamenubar*))

(make-object dywx-button% "Image" imagefontpane
             (lambda (button event) (Load-Image)))

(make-object dywx-button% "Font" imagefontpane
             (lambda (b e)
               (let ((restore-callback (make-restore-font-callback-all-pads)))
                 (Get-Font-Name-For-Selected apply-new-font-to-all-pads restore-callback))))

(define (apply-new-font-to-all-pads newfont)
  (for-all-pads (lambda (PAD)
                  (send PAD defaultfont newfont)
                  (foreach (send PAD selected) (lambda (obj)
                                                 (when (has-method? obj 'font)
                                                   (send obj font newfont)))))))

(define (make-restore-font-callback-all-pads)
  (let ((op-list '()))
    (for-all-pads (lambda (PAD)
                    (let ((oldfont (send PAD defaultfont)))
                      (push! (lambda() (send PAD defaultfont oldfont)) op-list))
                    (foreach (send PAD selected) (lambda (obj)
                                                   (when (has-method? obj 'font)
                                                     (let ((oldfont (send obj font)))
                                                       (push! (lambda () (send obj font oldfont)) op-list)))))))
    (lambda () (foreach op-list (lambda (op) (op))))))

;--- Pen color and Fill color buttons and indicators -------------

(define (pen-command . args)
  (lambda (button event)
    (let ((tclcolor 0))
      (update-panel-colors)
      (cond ((null? args) (set! tclcolor (pengetcolor)))
            (else
             (set! tclcolor "none")
             (for-all-pads (lambda (argPAD) (send argPAD defaultpen "none")))))
      (update-panel-colors)
      (for-all-pads (lambda (argPAD)
                      (foreach (send argPAD selected)
                               (lambda (o)
                                 (when (has-method? o 'pen) (send o pen tclcolor)))))) )))

(define (fill-command . args)
  (lambda (button event)
    (let ((tclcolor 0))
      (update-panel-colors)
      (cond ((null? args) (set! tclcolor (fillgetcolor)))
            (else
             (set! tclcolor "none")
             (for-all-pads (lambda (argPAD) (send argPAD defaultfill "none")))))
      (update-panel-colors)
      (for-all-pads (lambda (argPAD)
                      (foreach (send argPAD selected)
                               (lambda (o)
                                 (when (has-method? o 'fill) (send o fill tclcolor)))))) )))


(define fillpane (make-object dywx-horizontal-pane% *dynamenubar*))
(define midpane (make-object dywx-horizontal-pane% *dynamenubar*))
(define penpane (make-object dywx-horizontal-pane% *dynamenubar*))


(make-object dywx-button% "Fill" fillpane (fill-command))
(make-object dywx-button% "<none>" fillpane (fill-command #f))
(define color-ellipse (make-object color-ellipse% midpane))
(make-object dywx-button% "Pen" penpane (pen-command))
(make-object dywx-button% "<none>" penpane (pen-command #f))


;;;(require (prefix wx: (lib "kernel.rkt" "mred" "private")))

(define (update-panel-colors)
  (let ((penclr (send dynapad defaultpen))
        (fillclr (send dynapad defaultfill)))
    (send color-ellipse pencolor penclr)
    (send color-ellipse fillcolor fillclr)
    ))

;--- (end) Pen color and Fill color buttons and indicators -------

(define (createlinkdialog l)
  (let
      ((dialog #f)
       (looppane #f)
       (loop? #f)
       (cancelokpane #f))

    (define (loopcb cbo e)
      (set! loop? (send cbo get-value)))

    (set! dialog (make-object dialog% "Link" #f #f #f #f #f null))
    (make-object message% "Link selected objects?\n" dialog null)
    (set! looppane (make-object horizontal-pane% dialog))
    (send looppane stretchable-height #f)
    (send looppane set-alignment 'right 'center)
    (make-object check-box% "loop" looppane loopcb null)
    (set! cancelokpane (make-object horizontal-pane% dialog))
    (send cancelokpane stretchable-height #f)

    (make-object button% "Cancel" cancelokpane
                 (lambda (button event)
                   (send dialog show #f)))
    (make-object button% "Ok" cancelokpane
                 (lambda (button event)
                   (let loop
                       ((first (car l)) (l l))
                     (cond
                       ((null? (cdr l)) (when loop? (send (car l) link first)))
                       (else
                        (send (car l) link (cadr l))
                        (loop first (cdr l)))))
                   (send dialog show #f)))
    (send dialog show #t)))

(define (deletelinkdialog o)
  (let
      ((dialog #f)
       (followpane #f)
       (follow? #f)
       (cancelokpane #f))

    (define (followcb cbo e)
      (set! follow? (send cbo get-value)))

    (set! dialog (make-object dialog% "Link" #f #f #f #f #f null))
    (make-object message% "Delete link for selected object?\n" dialog null)
    (set! followpane (make-object horizontal-pane% dialog))
    (send followpane stretchable-height #f)
    (send followpane set-alignment 'right 'center)
    (make-object check-box% "Follow and delete links" followpane followcb null)
    (set! cancelokpane (make-object horizontal-pane% dialog))
    (send cancelokpane stretchable-height #f)

    (make-object button% "Cancel" cancelokpane
                 (lambda (button event)
                   (send dialog show #f)))
    (make-object button% "Ok" cancelokpane
                 (lambda (button event)
                   (let loop
                       ((o o))
                     (let ((next (send o link)))
                       (send o link #f)
                       (when (and follow? next) (loop next))))
                   (send dialog show #f)))
    (send dialog show #t)))

(define (Resize-Selected-all-pads)
  (for-all-pads (lambda (PAD)
                  (Resize-Selected PAD))))

(define (Reshape-Selected-all-pads)
  (for-all-pads (lambda (PAD)
                  (Reshape-Selected PAD))))

(define (Raise-Selected-all-pads)
  (for-all-pads (lambda (PAD)
                  (Raise-Selected PAD))))

(define (Lower-Selected-all-pads)
  (for-all-pads (lambda (PAD)
                  (Lower-Selected PAD))))

(define resizereshapepane (make-object dywx-horizontal-pane% *dynamenubar*))
(make-object dywx-button% "Resize" resizereshapepane (lambda (b e) (Resize-Selected-all-pads)))
(make-object dywx-button% "Reshape" resizereshapepane (lambda (b e) (Reshape-Selected-all-pads)))

(define raiselowerpane (make-object dywx-horizontal-pane% *dynamenubar*))
(make-object dywx-button% "Raise" raiselowerpane (lambda (b e) (Raise-Selected-all-pads)))
(make-object dywx-button% "Lower" raiselowerpane (lambda (b e) (Lower-Selected-all-pads)))

(define grouppane (make-object dywx-horizontal-pane% *dynamenubar*))
(make-object dywx-button% "Group" grouppane (lambda (b e) (Group-Selected)))
(make-object dywx-button% "Ungroup" grouppane (lambda (b e) (UnGroup-Selected)))

(define copypastepane (make-object dywx-horizontal-pane% *dynamenubar*))
(make-object dywx-button% "Copy" copypastepane (lambda (b e) (Copy-Selected)))
(make-object dywx-button% "Paste" copypastepane (lambda (b e) (Paste-From-Copy-Buffer)))

(define importpane (make-object dywx-horizontal-pane% *dynamenubar*))
(make-object dywx-button% "Import..." importpane (lambda (b e) (Select-and-Import-File)))

(define loadpane (make-object dywx-horizontal-pane% *dynamenubar*))
(make-object dywx-button% "Revert" loadpane (lambda (b e) (Restore-Current)))
(make-object dywx-button% "Restore..." loadpane (lambda (b e) (Select-and-Restore-File)))

(define savepane (make-object dywx-horizontal-pane% *dynamenubar*))
(make-object dywx-button% "Save" savepane (lambda (b e) (Save-Current)))
(make-object dywx-button% "Save As..." savepane (lambda (b e) (Select-and-Save-All)))

(define deletepane (make-object dywx-horizontal-pane% *dynamenubar*))
(make-object dywx-button% "Delete" deletepane (lambda (b e) (Delete-Selected)))

(define deleteallpane (make-object dywx-horizontal-pane% *dynamenubar*))
(make-object dywx-button% "Delete All" deleteallpane (lambda (b e) (Confirm-and-Delete-All)))

(define loadimagespane (make-object dywx-horizontal-pane% *dynamenubar*))
(make-object dywx-button% "Load Images" loadimagespane (lambda (button event) (Arrange-Images)))

(define loadpdfspane (make-object dywx-horizontal-pane% *dynamenubar*))
(make-object dywx-button% "Load PDFs" loadpdfspane (lambda (button event) (Arrange-Pdfs)))

; reserve a pane for new buttons
(define app_pane (make-object dywx-horizontal-pane% *dynamenubar*))

(define (repos-menubar)
  (send *dynamenubar* update-all)
  (let ((xy (bbnw (send currentPAD bbox))))
    (send *dynamenubar* xy (car xy) (cadr xy)))
  )


(for-all-pads (lambda (argPAD)
                (changemode argPAD "Run")))
(send *dynamenubar* show #t)
(update-panel-colors)

#; ; old load stuff ?
(after 2000 repos-menubar)

