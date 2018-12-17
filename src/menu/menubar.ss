;======= Simple Menubar ========

(define (for-all-pads body) (for-each body *list-of-all-dynapads*))

(dynaload "mode.ss")
(push-onto-alist-val! assoc "Run"
		      (lambda (argPAD)
			(clear-all-menu-buttons)
			(send btn_run set-label on_run))
		      gui-mode-alist)
(push-onto-alist-val! assoc "Select"
		      (lambda (argPAD)
			(clear-all-menu-buttons)
			(send btn_select set-label on_select))
		      gui-mode-alist)


#| ;obsolete:
(define (gui-update-mode argPAD)
  (let* ((mode (send argPAD modifier 'get))
         (cursor (gui-mode-cursor mode)))

    ; change all others pads to match mode (?)
    (for-all-pads (lambda (PAD)
      (if (not (eq? PAD argPAD))
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
          ((equal? mode "Zoom")    (for-all-pads (lambda (PAD) (send argPAD cursor 8))))
          ((equal? mode "Pan")     (for-all-pads (lambda (PAD) (send argPAD cursor 14))))
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
|#

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
      (make-object bitmap% filename filetype)
      )
      string)))

(define *menubar* (make-object frame% "Draw Tools" #f #f #f *draw-menu-x* *draw-menu-y*))

(define *menubar_offlist* ())

(define runpane      (make-object horizontal-pane% *menubar*))
(define drawpane     (make-object vertical-pane%   *menubar*))
(define arcpane      (make-object horizontal-pane% *menubar*)) ;may be used by edges.ss
(define hyperpane    (make-object horizontal-pane% *menubar*))
(define guestpane    (make-object horizontal-pane% *menubar*))
(define selectpane   (make-object horizontal-pane% *menubar*))


(define off_run        (use-bitmap-or-string "pad/bitmaps/hand.xbm"          "Run"         ))
(define off_select     (use-bitmap-or-string "pad/bitmaps/pointer.xbm"       "Select"      ))
(define off_portal     (use-bitmap-or-string "pad/bitmaps/portal.xbm"        "Portal"      ))
(define off_hyperlink  (use-bitmap-or-string "pad/bitmaps/hyperlink.xbm"     "HyperLink"   ))
(define off_marquee    (use-bitmap-or-string "pad/bitmaps/marquee.xpm"       "Marquee"     ))
(define off_lasso      (use-bitmap-or-string "pad/bitmaps/lasso.xpm"         "Lasso"       ))

(define on_run        (use-bitmap-or-string "pad/bitmaps/On_hand.xpm"          "Run"         ))
(define on_select     (use-bitmap-or-string "pad/bitmaps/On_pointer.xpm"       "Select"      ))
(define on_portal     (use-bitmap-or-string "pad/bitmaps/On_portal.xpm"        "Portal"      ))
(define on_hyperlink  (use-bitmap-or-string "pad/bitmaps/On_hyperlink.xpm"     "HyperLink"   ))
(define on_marquee    (use-bitmap-or-string "pad/bitmaps/On_marquee.xpm"       "Marquee"     ))
(define on_lasso      (use-bitmap-or-string "pad/bitmaps/On_lasso.xpm"         "Lasso"       ))

(define *guest-button-off-labels* null)
(define *draw-button-off-labels* null)

(define (clear-all-menu-buttons)
  (send btn_run         set-label off_run        )
  (send btn_select      set-label off_select     )
  (send btn_portal      set-label off_portal     )
  (send btn_hyperlink   set-label off_hyperlink  )
  (if btn_marquee (send btn_marquee set-label off_marquee  ))
  (if btn_lasso (send btn_lasso set-label off_lasso  ))
  (foreach *draw-button-off-labels*
	   (lambda (pair) (mlet (((btn lbl) pair)) (send btn set-label lbl))))
  (foreach *guest-button-off-labels*
	   (lambda (pair) (mlet (((btn lbl) pair)) (send btn set-label lbl))))
)

(define btn_run
  (make-object button%
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
  (make-object button%
    off_select
    runpane
    (lambda (button event)
      (clear-all-menu-buttons)
      (send button set-label on_select)
      (for-all-pads (lambda (argPAD)
        (changemode argPAD "Select")))
      ))
)

(define btn_portal
  (make-object button%
    off_portal
    hyperpane
    (lambda (button event)
      (for-all-pads (lambda (argPAD)
        (send argPAD fill? #f)))
      (clear-all-menu-buttons)
      (send button set-label on_portal)
      (set! Draw-multiple (button-double-clicked?))
      (printf "portals not migrated yet~%")
      ))
)

(define btn_hyperlink
  (make-object button%
    off_hyperlink
    hyperpane
    (lambda (button event)
      (for-all-pads (lambda (argPAD)
        (send argPAD fill? #f)))
      (clear-all-menu-buttons)
      (send button set-label on_hyperlink)
      (set! Draw-multiple (button-double-clicked?))
      (for-all-pads (lambda (argPAD)
        (initCreateLink argPAD)))
      ))
)

(define btn_marquee #f)
(define btn_lasso #f)

; not shown in the default.  see event-binders.ss
(define (enable-marquee-and-lasso)
  (set! btn_marquee
    (make-object button%
      off_marquee
      selectpane
      (lambda (button event)
        (for-all-pads (lambda (argPAD)
          (send argPAD fill? #f)))
        (clear-all-menu-buttons)
        (send button set-label on_marquee)
        (printf "marquee select not available yet~%") )))
  
  (set! btn_lasso
    (make-object button%
      off_lasso
      selectpane
      (lambda (button event)
        (for-all-pads (lambda (argPAD)
          (send argPAD fill? #f)))
        (clear-all-menu-buttons)
        (send button set-label on_lasso)
        (printf "lasso select not available yet~%") )))
)



(define imagefontpane (make-object horizontal-pane% *menubar*))

(make-object button% "Image" imagefontpane
  (lambda (button event) (Load-Image)))

(make-object button% "Font" imagefontpane
  (lambda (b e)
    (let ((restore-callback (make-restore-font-callback-all-pads)))
    (Get-Font-Name-For-Selected apply-new-font-to-all-pads restore-callback))))

(define (apply-new-font-to-all-pads newfont)
  (for-all-pads (lambda (PAD)
    (send PAD defaultfont newfont)
    (foreach (send PAD selected) (lambda (obj)
      (if (has-method? obj 'font)
        (send obj font newfont)))))))

(define (make-restore-font-callback-all-pads)
  (let ((op-list ()))
    (for-all-pads (lambda (PAD)
      (let ((oldfont (send PAD defaultfont)))
        (push! (lambda() (send PAD defaultfont oldfont)) op-list))
      (foreach (send PAD selected) (lambda (obj)
        (if (has-method? obj 'font)
          (let ((oldfont (send obj font)))
            (push!  (lambda () (send obj font oldfont)) op-list)))))))
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
            (if (has-method? o 'pen) (send o pen tclcolor)))))) )))

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
            (if (has-method? o 'fill) (send o fill tclcolor)))))) )))


(make-object canvas% *menubar*)
(define fillpane (make-object horizontal-pane% *menubar*))
(define midpane (make-object horizontal-pane% *menubar*))
(define penpane (make-object horizontal-pane% *menubar*))
(make-object canvas% *menubar*)

(make-object button% "Fill" fillpane (fill-command))
(make-object button% "<none>" fillpane (fill-command #f))
(define fakecanvas (make-object canvas% midpane))
(define fillcanvas (make-object canvas% midpane))
  (send fillcanvas min-height 30)
(make-object button% "Pen" penpane (pen-command))
(make-object button% "<none>" penpane (pen-command #f))


(require (prefix wx: (lib "kernel.ss" "mred" "private")))
(define no-color (make-object color% 208 212 208))

(define bg-color (wx:get-panel-background))


(define menu-fill-pen 0)
(define menu-pen-pen 0)

(let ((penclr (if (equal? (send dynapad defaultpen) "none")
                no-color
                (mred-color-from-tcl-color (send dynapad defaultpen))))
      (fillclr (if (equal? (send dynapad defaultfill) "none")
                no-color
                (mred-color-from-tcl-color (send dynapad defaultfill))))
     )
  (set! menu-fill-pen
    (send (wx:get-the-pen-list) find-or-create-pen fillclr 15 'solid))
  (set! menu-pen-pen
    (send (wx:get-the-pen-list) find-or-create-pen penclr 8 'solid))
)

(define (update-panel-colors)
  (let ((penclr (if (equal? (send dynapad defaultpen) "none")
                  no-color
                  (mred-color-from-tcl-color (send dynapad defaultpen))))
        (fillclr (if (equal? (send dynapad defaultfill) "none")
                  no-color
                  (mred-color-from-tcl-color (send dynapad defaultfill))))
        (dc (send fillcanvas get-dc))
       )
  (wx:fill-private-color (send fakecanvas get-dc) no-color)

  (wx:fill-private-color dc no-color)

  (set! menu-fill-pen (send (wx:get-the-pen-list) find-or-create-pen fillclr 15 'solid))
  (send dc set-pen menu-fill-pen)
  (send dc draw-line 10 15 28 15)

  (set! menu-pen-pen (send (wx:get-the-pen-list) find-or-create-pen penclr 8 'solid))
  (send dc set-pen menu-pen-pen)
  (send dc draw-ellipse 5 5 30 19)
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
            ((null? (cdr l)) (if loop? (send (car l) link first)))
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
	    (if (and follow? next) (loop next))))
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

(define resizereshapepane (make-object horizontal-pane% *menubar*))
(make-object button% "Resize" resizereshapepane (lambda (b e) (Resize-Selected-all-pads)))
(make-object button% "Reshape" resizereshapepane (lambda (b e) (Reshape-Selected-all-pads)))

(define raiselowerpane (make-object horizontal-pane% *menubar*))
(make-object button% "Raise" raiselowerpane (lambda (b e) (Raise-Selected-all-pads)))
(make-object button% "Lower" raiselowerpane (lambda (b e) (Lower-Selected-all-pads)))

(define grouppane (make-object horizontal-pane% *menubar*))
(make-object button% "Group" grouppane (lambda (b e) (Group-Selected)))
(make-object button% "Ungroup" grouppane (lambda (b e) (UnGroup-Selected)))

(define copypastepane (make-object horizontal-pane% *menubar*))
(make-object button% "Copy" copypastepane (lambda (b e) (Copy-Selected)))
(make-object button% "Paste" copypastepane (lambda (b e) (Paste-From-Copy-Buffer)))

;(define importpane (make-object horizontal-pane% *menubar*))
;(make-object button% "Import..." importpane (lambda (b e) (Select-and-Import-File)))

(define loadpane (make-object horizontal-pane% *menubar*))
(make-object button% "Revert" loadpane (lambda (b e) (Restore-Current)))
(make-object button% "Restore..." loadpane (lambda (b e) (Select-and-Restore-File)))

(define savepane (make-object horizontal-pane% *menubar*))
(make-object button% "Save" savepane (lambda (b e) (Save-Current)))
(make-object button% "Save As..." savepane (lambda (b e) (Select-and-Save-All)))

(define exportpane (make-object horizontal-pane% *menubar*))
(make-object button% "Export Dirs..." exportpane (lambda (b e) (Export-To-Directories)))

(define deletepane (make-object horizontal-pane% *menubar*))
(make-object button% "Delete" deletepane (lambda (b e) (Delete-Selected)))

(define deleteallpane (make-object horizontal-pane% *menubar*))
(make-object button% "Delete All" deleteallpane (lambda (b e) (Confirm-and-Delete-All)))

;(define loadimagespane (make-object horizontal-pane% *menubar*))
;(make-object button% "Load Images" loadimagespane (lambda (button event) (Arrange-Images)))

;(define loadpdfspane (make-object horizontal-pane% *menubar*))
;(make-object button% "Load PDFs" loadpdfspane (lambda (button event) (Arrange-Pdfs)))

; reserve a pane for new buttons
(define app_pane (make-object horizontal-pane% *menubar*))


(for-all-pads (lambda (argPAD)
  (changemode argPAD "Run")))
(send *menubar* show #t)
(update-panel-colors)

