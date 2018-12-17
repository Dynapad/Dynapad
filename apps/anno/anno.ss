; anno.ss   03/10/03  mbh

(display "Including annotation features...")(newline)

;(define rootdir (getenv "DYNAPADHOME"))
;(load (string-append rootdir "/dynapad.ss"))
;(load (string-append rootdir "/actor.ss"))
;(load (string-append rootdir "/menu_popup.ss"))
(dynaload "actor.ss")
(dynaload "menu_popup.ss")
;(define iconpath (string-append rootdir "/apps/anno/icons/"))
(define iconpath (string-append *dynapad-directory* "/apps/anno/icons/"))

;
;  Engage the popup menu activations for annotating & toggling anno icons
;
;(enable-popups2)
(define (Annotate) (send anno_mgr start-record))
(define (Toggle) (send anno_mgr toggle-icon-display))

;(set! make-submenus-for-applications
(push-submenus-for-applications
  (lambda (mb obj)
    (add-menu-separator mb)
    (add-menu-item mb "Annotate" Annotate)
    (add-menu-item mb "Toggle Anno Icons" Toggle)
  )
)



;
;  ANNOTATIONS MANAGER (anno_mgr%) CLASS
;
;  the anno_list it maintains is composed of entries in one of the following formats 
;  (one such entry per target object that has annotations associated with it):
;      (target, anno1, anno2, anno3, ...)  if the anno icons aren't displayed for that target
;      ((target,group), anno1, anno2, anno3, ...) if they are (and are therefore grouped)

(define anno_mgr%     
  (class object%
    (public toggle-icon-display display-all-icons hide-all-icons
            start-record ok-record-text cancel-record-text
            show-anno hide-anno
            add-anno delete-anno
            get-annos 
            load-annos save-annos )
    (field (anno_list ()) (display_icons_flag #t) (rec_target #f)
           (msg_box (make-object msgbox%)) (text_box (make-object msgbox%))) 
    (super-instantiate ())

    (define (toggle-icon-display)
      (if display_icons_flag (hide-all-icons) (display-all-icons) ) )    
    
    (define (display-all-icons)
      (set! display_icons_flag #t)
      (set! anno_list (map display-icons anno_list))  
    )
    (define (hide-all-icons)
      (set! display_icons_flag #f)
      (set! anno_list (map hide-icons anno_list))
    )
    
    (define (display-icons entry)
      (if [icons-displayed? entry] 
        entry         
        (let ((target (extract-target entry)) (annos (extract-annos entry)) (new_group #f))
          (for-each (lambda (anno coord) (send anno draw-icon coord target)) 
                      annos (get-coord-list target (length annos)))
          (set! new_group (make-object group% dynapad))
          (send new_group divisible #t)
          (send new_group add target)
          (for-each (lambda (anno) (send new_group add (send anno get-icon))) annos )
          (cons (list target new_group) annos)
    ) ) )    

    (define (hide-icons entry)
      (if [not (icons-displayed? entry)]
        entry
        (let ((target (extract-target entry)) (group (extract-group entry)) 
              (annos (extract-annos entry)))
          (send group ungroup)
          (for-each (lambda (anno) (send anno hide-icon)) annos)
          (cons target annos)
    ) ) ) 
        
    (define (start-record)
      (let ((sel (fs)))
        (cond 
          ([null? sel] (send msg_box go-error "nothing selected."))
          ([> (length sel) 1] (send msg_box go-error "select only one item."))
          (else
            (set! rec_target (car sel))
            (send rec_target unselect)
            (send msg_box go-caption "enter text annotation:" rec_target)
            (send text_box go-stext-entry rec_target)
    ) ) ) )
    
    (define (ok-record-text)
      (let ((text ""))
        (changemode dynapad "Select")
        (set! text (send text_box get-text))
        (send text_box delete)
        (send msg_box delete)
        (add-anno rec_target text)
    ) )
    
    (define (cancel-record-text)
       (changemode dynapad "Select")
       (send text_box delete)
       (send msg_box delete)
    )
    
    (define (show-anno anno)
      (if [send text_box is-displayed?] (send text_box delete))
      (send text_box go-stext-display (send anno get-content) (extract-target (find-entry anno)))
    )
    
    (define (hide-anno) (send text_box delete))

    (define (add-anno target text)
      (let ((entry (find-entry target)))
        (cond  
          ([null? entry]    ; first anno for this object
            (set! anno_list (cons (list target (make-object anno% text)) anno_list ))
            (if display_icons_flag (display-all-icons)) )
          ([not (icons-displayed? entry)]  
            (delete-entry entry)
            (set! anno_list (cons (append entry (list (make-object anno% text))) anno_list)))
          (else
            (hide-icons entry)
            (delete-entry entry)
            (set! anno_list (cons (cons (extract-target entry) (append (extract-annos entry) (list (make-object anno% text)))) anno_list ))
            (display-all-icons))        
    ) ) )      
          
    (define (save-annos target) 
       (cons target (map (lambda (anno) (send anno get-content)) (get-annos target))))
    (define (load-annos l) (for-each (lambda (content) (add-anno (car l) content)) (cdr l)))
    
      ; anno_list utilities
    (define (extract-target entry) (if [list? (car entry)] (caar entry) (car entry)))
    (define (extract-group entry)  (if [list? (car entry)] (cadar entry) (car entry)))
    (define (extract-annos entry) (cdr entry))
    (define (icons-displayed? entry) (list? (car entry)))
    (define (find-entry obj) (recurs-find obj anno_list))
    (define (recurs-find obj list) 
      (if [null? list] '()
        (if [or (memv obj (car list)) (and (list? (caar list)) (memv obj (caar list)))]
          (car list)
          (recurs-find obj (cdr list))
    ) ) )
    (define (delete-entry entry) (set! anno_list (recurs-delete entry anno_list)))
    (define (recurs-delete entry list)
      (if [null? list] '()
        (if [eqv? entry (car list)] (cdr list) (cons (car list) (recurs-delete entry (cdr list))))))
    (define (get-annos target) 
      (let ((entry (find-entry target))) (if [null? entry] '() (cdr entry) ) ) )
        (define (delete-anno anno) (set! anno_list (recurs-delete-anno anno annolist)))
    (define (recurs-delete-anno anno list)
      (if [null? list] 
        '()
        (if [memv anno (car list)] 
          (if [= 2 (length (car list))]
            (cdr list)  ; it was the only anno for that entry, so delete the whole entry
            (cons (recurs-delete anno (car list)) (cdr list)))   ; else just delete the anno
          (cons (car list) (recurs-delete-anno anno (cdr list)))
    ) ) )

   (define/public (find-target obj) (find-entry obj))
   (define/public (get-target obj)
     (let ((t (find-entry obj))) (if (not (null? t)) (extract-target t) #f)))
    
    ;
    ;  GEOMETRY MANAGEMENT STUFF
    ;
   (define (get-coord-list target num) 
       (recurs-build-coord-list target num (* 0.15 (send target width)) 
                   (car (send target bbox)) (cadr (send target bbox))) )
    (define (recurs-build-coord-list target num w tx ty)
      (if (= num 0) ()
          (append (recurs-build-coord-list target (- num 1) w tx ty) 
                  (list (list (+ tx (* w (- num 1))) ty w)) ) ) )    
    
  )
)

(define (is-annotated? obj) (not (null? (send anno_mgr find-target obj))))
(define (get-anno-target obj) (or (send anno_mgr get-target obj) obj))
(define (get-annos obj) (send anno_mgr save-annos (send anno_mgr get-target obj)))
(define (put-annos obj l) (send anno_mgr load-annos (cons obj l)))

  
; 
;   ANNOTATION (anno%) CLASS
;

(define anno%
  (class object%
    (public draw-icon hide-icon get-icon get-content)
    (init-field (content ""))
    (field (icon 0) (icon_file (string-append iconpath "stext.gif")))
    (super-instantiate ())
    
    (define (draw-icon coords target)  ; the coords are actually x y & width
      (set! icon (make-object image% dynapad icon_file))
      (send icon anchor "sw")
      (send icon position (list (car coords) (cadr coords) 1)) 
      (send icon scale (/ (caddr coords) (send icon width)))
      (send icon raise target)
      (send icon bind "<Enter>" (lambda (obj e) (send anno_mgr show-anno this)))
      (send icon bind "<Leave>" (lambda (obj e) (send anno_mgr hide-anno)))
    )
    
    (define (hide-icon) 
      (send icon delete)
      (set! icon 0)
    )
    
    (define (get-icon) icon)
    (define (get-content) content)
  )  
)


;
;  MESSAGE BOX (msgbox%) CLASS
;

(define msgbox%     
  (class object%
    (public go-error go-caption go-stext-display go-stext-entry get-text is-displayed? delete)
    (field (box #f) (text #f) (ok_btn #f) (cancel_btn #f))
    (super-instantiate ())
    
    (define (go-error etext)
      (if box (delete))
      (set! text (make-object text% dynapad etext))
      (send text pen "gold")
      (send text anchor "center")
      (send text position (send dynapad view))
      (make-box (expand (send text bbox) 10))
      (send box lower text)
      (make-ok)
      (send ok_btn bind "<ButtonPress>" (lambda (obj e) (delete)))
    )   
    
    (define (go-caption stext target) 
      (let ((bb (send target bbox)))
        (if box (delete))
        (set! text (make-object text% dynapad stext))
        (send text anchor "center")
        (send text position (caption-pos target))
        (send text pen "gold")
        (make-box (expand (send text bbox) 4))
        (send box lower text)
      )
    )
    
    (define (go-stext-display stext target) 
      (if box (delete))
      (make-box (stext-boxpos target))
      (set! text (make-object text% dynapad stext))
      (send text pen "gold")
      (send text position (list (+ 10 (car (send box bbox))) (- (cadddr (send box bbox)) 5) (caddr (send text position))))
    )
 
    (define (go-stext-entry target) 
      (if box (delete))
      (make-box (stext-boxpos target))
      (make-ok)
      (make-cancel)
      (send ok_btn bind "<ButtonPress>" (lambda (obj e) (send anno_mgr ok-record-text)))
      (send cancel_btn bind "<ButtonPress>" (lambda (obj e) (send anno_mgr cancel-record-text)))
      (set! text (make-object text% dynapad ""))
      (send text pen "gold")
      (send text position (list (+ 10 (car (send box bbox))) (- (cadddr (send box bbox)) 5) (caddr (send text position))))
      (changemode dynapad "Text")
      (send text focus)
    )

    (define (get-text) (send text text))
           
    (define (is-displayed?) (if box #t #f) )
    
    (define (delete)
      (if box (send box delete))
      (if text (send text delete))
      (if ok_btn (send ok_btn delete))
      (if cancel_btn (send cancel_btn delete))
      (set! box #f) (set! text #f) (set! ok_btn #f) (set! cancel_btn #f)
    )

    (define (make-box coords)
      (set! box (make-object rect% dynapad coords))
      (send box pen "gold")
      (send box fill (send dynapad background))  
    )
    (define (make-ok) 
      (let ((bb (send box bbox)) (offset 0))
        (set! ok_btn (make-object image% dynapad (string-append iconpath "ok.gif")))
        (send ok_btn anchor "center")
        (send ok_btn position (list (car bb) (cadr bb) (* 0.4 (caddr (send dynapad view)))))
        (set! offset (* 0.5 (send ok_btn width))) 
        (send ok_btn slide offset (* -1 offset))   
    ) )
    (define (make-edit) 
      (let ((bb (send box bbox)) (offset 0))
        (set! edit_btn (make-object image% dynapad (string-append iconpath "edit.gif")))
        (send edit_btn anchor "center")
        (send edit_btn position (list (/ (+ (car bb) (caddr bb)) 2) (cadr (send ok_btn position)) (* 0.31 (caddr (send dynapad view)))))
    ) )    
    (define (make-cancel)
      (let ((bb (send box bbox)) (offset 0))
        (set! cancel_btn (make-object image% dynapad (string-append iconpath "cancel.gif")))
        (send cancel_btn anchor "center")
        (send cancel_btn position (list (caddr bb) (cadr bb) (* 0.4 (caddr (send dynapad view)))))
        (set! offset (* 0.5 (send cancel_btn width))) 
        (send cancel_btn slide (* -1 offset) (* -1 offset)) 
    ) )      
        
    (define (caption-pos target)
      (let* ((bb (send target bbox)) (x1 (car bb)) (x2 (caddr bb)))
        (list (/ (+ x1 x2) 2) (+ 15 (cadr bb)))
    ) )
    (define (stext-boxpos target)
      (let ((bb (send target bbox)))
        (list (car bb) (- (cadr bb) 155) (caddr bb) (- (cadr bb) 5))
    ) ) 
    (define (expand l d)
      (list (- (car l) d) (- (cadr l) d) (+ (caddr l) d) (+ (cadddr l) d))
    )
  )
)



; create the one global annotation manager
(define anno_mgr (make-object anno_mgr%))
