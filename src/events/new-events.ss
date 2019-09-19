(dynaload "event-binders.ss")
(apply-newpad-bindings dynapad)

;(if *menubar* (send *menubar* show #f))

;(send dynapad select-render-script
;  (lambda (cptr obj)
;    (sch_pen cptr "red")
;   ;(sch_transparency cptr 0.5)
;    (sch_penwidth cptr 4)))

;(set-selectrect-color dynapad "red")
(sendf dynapad evs select-rect-color "red")

;-----------------------------------------------------------------

; redefine the standard binding for switch to hires:
; (this must be redefined _before_ images are loaded/created)


; Need to rewrite these to accomodate compound images
;  (e.g. formation w. image) i.e. outer obj event toggles inner image
(define (init-image-bindings img)
  (send img bind "<Double-ButtonPress-1>" image-hires-and-center)
  (send img bind "<Shift-Double-ButtonPress-1>" image-also-hires-and-center)
  )


(define *list_of_hirez_images* ())
(define *last_view_before_image_centering* '(0 0 1))

(define (image-hires-and-center eventPAD e)
  (def imagelist  (list (event-obj e)))
  (show-possible-delay eventPAD
                       (if (send (car imagelist) selected?)
                           (set! imagelist (send eventPAD selected)))

                       (if (andmap (lambda (img) (member img *list_of_hirez_images*)) imagelist)
                           (remove-images-from-hires-list *list_of_hirez_images*)
                           ;else
                           (set-image-hires-list imagelist)
                           )
                       (center-images-or-return-to-previous-view)
                       #f
                       ))

(define (image-also-hires-and-center eventPAD e)
  (def imagelist  (list (event-obj e)))
  (show-possible-delay eventPAD
                       (if (send (car imagelist) selected?)
                           (set! imagelist (send eventPAD selected)))

                       (if (andmap (lambda (img) (member img *list_of_hirez_images*)) imagelist)
                           (remove-images-from-hires-list imagelist)
                           ;else
                           (add-images-to-hires-list imagelist)
                           )
                       (center-images-or-return-to-previous-view)
                       #f
                       ))

(define (center-images-or-return-to-previous-view)
  (if (pair? *list_of_hirez_images*)
      (send dynapad center *list_of_hirez_images* 1000 #t)
      (send dynapad moveto *last_view_before_image_centering* 1000 #t)))

(define (remove-images-from-hires-list imagelist)
  (foreach imagelist (lambda (img)
                       (if (member img *list_of_hirez_images*)
                           (set! *list_of_hirez_images* (remq img *list_of_hirez_images*)))
                       (when (send img hires?)
                         (send img thumb) ))))

(define (add-images-to-hires-list imagelist)
  (if (not (pair? *list_of_hirez_images*))
      (set! *last_view_before_image_centering* (send dynapad view)))
  (foreach imagelist (lambda (img)
                       (if (not (member img *list_of_hirez_images*))
                           (push! img *list_of_hirez_images*))
                       (if (not (send img hires?))
                           (send img hires) ))))

(define (set-image-hires-list imagelist)
  (remove-images-from-hires-list *list_of_hirez_images*)
  (add-images-to-hires-list imagelist) )


;-----------------------------------------------------------------

(define (make-selected-bigger)
  (let* ((selected (send dynapad selected))
         (grp (make-object group% dynapad (send dynapad selected))) )
    (send grp scale 1.414)
    (send grp ungroup)
    (foreach selected (lambda (i) (send i unselect)(send i select)))
    ))

(define (make-selected-smaller)
  (let* ((selected (send dynapad selected))
         (grp (make-object group% dynapad (send dynapad selected))) )
    (send grp scale 0.707)
    (send grp ungroup)
    (foreach selected (lambda (i) (send i unselect)(send i select)))
    ))

(send dynapad bind "<Run-KeyPress-b>" (lambda (d e) (make-selected-bigger)))
(send dynapad bind "<Run-KeyPress-s>" (lambda (d e) (make-selected-smaller)))

;(send dynapad focus)
