;;
;; Add invisible hyperlinks
;; Left top goes back, Right top goes forward
;; Bottom half centers this image
;;

(define (load-and-link-images dir)
  (link-images-as-slides (arrangedir dir)))

(define (link-images-as-slides images)
  (let* ((first (car images))
     (last-two (link-to-next first images)))
   (fixup-last first last-two)))

(define (fixup-last first last-two)
  (let ((areas (add-click-areas (cadr last-two))))
    (send (car areas) link (car last-two))
    (send (cadr areas) link first)
    (send (cadr last-two) link (cadr last-two))))


(define (link-to-next last l)
  (cond ((null? (cdr l))
     (list last (car l)))
    (else
     (let ((areas (add-click-areas (car l))))
       (send (car areas) link last)
       (send (cadr areas) link (cadr l))
       (send (car l) link (car l)))
      (link-to-next (car l) (cdr l)))))

;;
;; Click-Areas for Slides
(define (add-click-areas obj)
  (let* ((bb (send obj bbox))
     (right (make-object rect% dynapad bb))
     (left (make-object rect% dynapad bb)))
    (send* right (fill "black") (scale .5) (transparency .001) (anchor "sw"))
    (send* left  (fill "black") (scale .5) (transparency .001) (anchor "se"))
    (make-object group% dynapad (list right left obj))
    (list left right)))



;;Misc Routines

;;
;; imac-like center
;;
(define (center-imac obj)
  (send obj center 1000 #f .5 .5 1.5)
  (send obj center 1500 #f .5 .5 .95))


(define (link-hook o)
  (if (send o link)
    (send o bind "<Run-Double-ButtonPress-1>"
      (lambda(pad e)
        (let
      ((link (send (event-obj e) link)))
          (cond
       ((is-a? link dynaobject%) (send link center 1000 #f .5 .5 .95))
        ((list? link) (send pad moveto link 1000))))))
      (send o bind "<Run-Double-ButtonPress-1>" #f)))

;;
;; View an object, list, or everything
;;
(define view-it
  (case-lambda
   ((it)
    (send it center 2000)
    (sleep 3))
   ((it view-time)
    (send it center 2000)
    (sleep view-time))
   ((it slide-time view-time)
    (send it center slide-time)
    (sleep view-time))))

(define view-all
  (case-lambda
   (()
    (for-each
     (lambda (obj)
       (view-it obj))
     (send dynapad objects)))
   ((lst view-time)
    (for-each
     (lambda (obj)
       (view-it obj view-time))
     lst))
   ((lst view-time slide-time)
    (for-each
     (lambda (obj)
       (view-it obj slide-time view-time))
     lst))))

;;
;; View a list in random order
;;
(define (view-random lst)
  (view-it (list-ref lst (random (length lst))))
  (view-random lst))
