#lang racket/base

(provide lasso-contained-objects
         ask-user-for-lasso
         )

(require racket/class
         (only-in dynapad/objects freehand% polygon%)
         dynapad/history/undo
         dynapad/events/mode
         dynapad/pad-state ; event
         dynapad/misc/misc
         dynapad/layout/bbox
         (only-in dynapad/libdynapad-wrapper sch_xy_in_poly)
         )

;These are similar to get-user-bbox.ss

(define ask-user-for-lasso--event-binder%
  (class object%
    (init-field _dynapad)
    (init-field _release_callback)
    (super-instantiate ())

    (field (_freehand #f))
    (field (_lastxy #f))
    (field (_lastsxy #f))

    (push-event-mode _dynapad "GetLasso")
    ;(send dynapad cursorname "press.xbm" "white")

    (define (Lasso_ButtonPress e)
      (let ((x (event-x e))
            (y (event-y e)))
        (set! _lastxy (list x y))
        (set! _lastsxy (list (event-sx e) (event-sy e)))
        (set! _freehand (ic (make-object freehand% _dynapad)
                            (pen "red")
                            (coords _lastxy)))
        ))

    (define minjump 7) ;min spacing between points (reduces vtx density)

    (define (Lasso_ButtonMotion e)
      (let ((c (send _freehand coords))
            (x (event-x e))
            (y (event-y e))
            (sx (event-sx e))
            (sy (event-sy e)))
        (when (or (> (abs (- sx (car _lastsxy))) minjump)
                  (> (abs (- sy (cadr _lastsxy))) minjump))
          (begin
            (set! _lastxy (list x y))
            (set! _lastsxy (list sx sy))
            (send _freehand coords (append c _lastxy))
            ))))

    (define (Lasso_ButtonRelease e)
      (let ((crds (send _freehand coords)))
        (send _freehand delete) (set! _freehand #f)
        ;        (send _dynapad bind "<GetLasso-Motion>"          #f)
        (send _dynapad bind "<GetLasso-ButtonPress-1>"   #f)
        (send _dynapad bind "<GetLasso-B1-Motion>"       #f)
        (send _dynapad bind "<GetLasso-ButtonRelease-1>" #f)
        (pop-event-mode _dynapad)
        (when _release_callback
          (_release_callback crds))))

    ;   (send _dynapad bind "<GetLasso-Motion>"          (lambda (d e) (Lasso_Motion e)))
    (send _dynapad bind "<GetLasso-ButtonPress-1>"   (lambda (d e) (Lasso_ButtonPress e)))
    (send _dynapad bind "<GetLasso-B1-Motion>"       (lambda (d e) (Lasso_ButtonMotion e) #f))
    (send _dynapad bind "<GetLasso-ButtonRelease-1>" (lambda (d e) (Lasso_ButtonRelease e)))

    (send _dynapad bind "<GetLasso-KeyPress-Escape>"
          (lambda (eventPAD e)
            (pop-event-mode _dynapad)
            (when _freehand (send _freehand delete)
                  (set! _freehand #f))
            (when _release_callback
              (_release_callback #f))))
    ))


(define (ask-user-for-lasso argPAD callback)
  (make-object ask-user-for-lasso--event-binder% argPAD callback))

;======== Lasso selection =========

(define (contained-in-lasso? obj poly)
  (def bbc (bbcenter (send obj bbox)))
  (sch_xy_in_poly (car bbc) (cadr bbc) (send poly get-cptr)))

(define (lasso-contained-objects poly)
  (def bbox (send poly bbox))
  (def intruders (send dynapad find 'groupmembers 'overlapping bbox))
  (filter (lambda (it) (and (not (eq? it poly))
                            (contained-in-lasso? it poly)))
          intruders))

(define (make-selection-from-coords crds)
  (if crds
      (let ((p (ic (make-object polygon% dynapad crds) (fill "none") (pen "red"))))
        (let ((slctn (lasso-contained-objects p)))
          (send p delete)
          (send dynapad selected slctn)
          slctn))
      ;else
      null))

(define (make-lasso-selection)
  (ask-user-for-lasso dynapad make-selection-from-coords))


