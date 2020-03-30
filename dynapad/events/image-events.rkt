#lang racket/base

;======= Image Bindings and Utilities ============================

(require (only-in racket/class send)
         dynapad/pad-state
         dynapad/misc/misc
         dynapad/history/undo ; show-possible-delay
         )

;------ New Auto-hires....
; Need to rewrite these to accomodate compound images
;  (e.g. formation w. image) i.e. outer obj event toggles inner image
(define (init-image-bindings img)
  (send img bind "<Double-ButtonPress-1>" image-hires-and-center)
  (send img bind "<Shift-Double-ButtonPress-1>" image-also-hires-and-center)
  )

(define *list_of_hirez_images* '())
;(define *last_view_before_image_centering* '(0 0 1))

(define image-hires-and-center
  (case-lambda
    ((eventPAD e) (image-hires-and-center (list (event-obj e))))
    ((imagelist)
     (unless (null? imagelist)
       (let ((eventPAD (send (car imagelist) dynapad)))
         (show-possible-delay eventPAD
                              (when (send (car imagelist) selected?)
                                (set! imagelist (send eventPAD selected)))

                              (if (andmap (lambda (img) (member img *list_of_hirez_images*)) imagelist)
                                  (remove-images-from-hires-list *list_of_hirez_images*)
                                  ;else
                                  (set-image-hires-list imagelist)
                                  )
                              (center-images-or-return-to-previous-view)
                              )))
     #f
     )))

(define image-also-hires-and-center
  (case-lambda
    ((eventPAD e) (image-also-hires-and-center (list (event-obj e))))
    ((imagelist)
     (unless (null? imagelist)
       (let ((eventPAD (send (car imagelist) dynapad)))
         (show-possible-delay eventPAD
                              (when (send (car imagelist) selected?)
                                (set! imagelist (send eventPAD selected)))

                              (if (andmap (lambda (img) (member img *list_of_hirez_images*)) imagelist)
                                  (remove-images-from-hires-list imagelist)
                                  ;else
                                  (add-images-to-hires-list imagelist)
                                  )
                              (center-images-or-return-to-previous-view)
                              )))
     #f
     )))

;The old system is wrong; don't return to the last dbl-click view,
; but the last wide view
(define (center-images-or-return-to-previous-view)
  (if (pair? *list_of_hirez_images*)
      (send dynapad center *list_of_hirez_images* 1000 #t)
      ;(send dynapad moveto *last_view_before_image_centering* 1000 #t)
      (let ((returnview (sendf dynapad evs vwide)))
        (when returnview
          (send dynapad moveto returnview 1000 #t)))))

(define (remove-images-from-hires-list imagelist)
  (foreach imagelist (lambda (img)
                       (when (member img *list_of_hirez_images*)
                         (set! *list_of_hirez_images* (remq img *list_of_hirez_images*)))
                       (when (send img hires?)
                         (send img thumb)))))

(define (add-images-to-hires-list imagelist)
  (when (not (pair? *list_of_hirez_images*))
    ;       (set! *last_view_before_image_centering* (send dynapad view)))
    (sendf dynapad evs vwide (send dynapad view)))
  (foreach imagelist (lambda (img)
                       (when (not (member img *list_of_hirez_images*))
                         (push! img *list_of_hirez_images*))
                       (when (not (send img hires?))
                         (send img hires)))))

(define (set-image-hires-list imagelist)
  (remove-images-from-hires-list *list_of_hirez_images*)
  (add-images-to-hires-list imagelist) )

