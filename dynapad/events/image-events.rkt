#lang racket/base

;======= Image Bindings and Utilities ============================

(require (only-in racket/class send)
         dynapad/pad-state
         dynapad/misc/misc
         dynapad/history/undo ; show-possible-delay
         collects/thumbify/thumbify)

;(require (lib "thumbify.ss" "thumbify"))

(define gifjpg_rexp (regexp "(.*[/\\]|)(.*)(\\.[^\\.]*)$"))
(define thumb_rexp (regexp "(.*[/\\]|)(.+)(\\.|-)(125|150)(\\.([gG][iI][fF]|[jJ][pP][gG]|[pP][pP][mM]|[pP][nN][gG]|[tT][iI][fF]|[bB][mM][pP]))$"))

(define (image-toggle-hires-thumb img)
  (lambda (eventPAD e) (set-currentPAD! eventPAD)
          (if (send img hires?)
              (send img thumb)
              (send img hires))))

#; ; this is overwritten by a later definition
; This function is called when an image% object is created.
; Some applications may redefine this function to change
; the default binding of new images.
(define (init-image-bindings img)
  (send img bind "<Run-Shift-ButtonPress-1>"
        (image-toggle-hires-thumb img) ))


; List of places to look for corresponding thumbnail.
; (findthumb) no longer supports thumbnails in same directory
; with images.  This can be adjusted by changing the following
; list to include ("" "-125"), for example.
(define *thumbnail-variants*
  '( ("thumbs/125/" "-125") ; <-- try default first
     ("thumbs/"     "-125")
     ("thumbs/"     "-150")
     ("thumbs/150/" "-150")
     ("thumbs/"     ".125")
     ("thumbs/"     ".150")
     ("thumbs/125/" ".125")
     ("thumbs/150/" ".150")))

(define (findthumb hires)
  (call/cc (lambda (return)
             (let* ((s (if (path? hires) (path->string hires) hires))
                    (l (regexp-match gifjpg_rexp s))
                    (path   (list-ref l 1))
                    (base   (list-ref l 2))
                    (suffix (list-ref l 3)))
               (foreach *thumbnail-variants* (lambda (dir_sz)
                                               ; try thumbnail with same suffix as hires image
                                               (let ((thumbfile (string-append path (car dir_sz) base (cadr dir_sz) suffix)))
                                                 (when (file-exists? thumbfile) (return thumbfile)))
                                               ; try thumbnail with .jpg suffix
                                               (let ((thumbfile (string-append path (car dir_sz) base (cadr dir_sz) ".jpg")))
                                                 (when (file-exists? thumbfile) (return thumbfile))) ))
               (return #f)))))

(define (ensure-thumb hirespath)
  (let ((thumbpath (findthumb hirespath)))
    (unless (and thumbpath (file-exists? thumbpath))
      (thumbify hirespath))
    thumbpath))

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

