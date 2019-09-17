;load this file after starting dynapad
(load-relative "flow.ss")
(load-relative "zoom.ss")

; example
; Draw is a flow menu (inner-to-out transition pushes submenu)
; folks is pie menu

(define menu (make-object flow% dynapad))

(define main
  (let ((v (make-vector 8 #f)))
    (vector-set! v 0
                 (list (make-object text% dynapad "Dismiss")
                       (lambda(menu)(send menu popdown))
                       #f))
    (vector-set! v 1
                 (list (make-object text% dynapad "Run")
                       #f
                       (lambda(menu)(changemode dynapad "Run"))))
    (vector-set! v 2
                 (list (make-object text% dynapad "Select")
                       #f
                       (lambda(menu)(changemode dynapad "Select"))))
    (vector-set! v 3
                 (list (make-object text% dynapad "Draw")
                       #f
                       (lambda(menu)(send menu push drawvec))))
    (vector-set! v 4
                 (list (make-object text% dynapad "Zoom")
                       #f
                       (lambda(menu)
                         (set! zoomtype 0)
                         (changemode dynapad "Zoom"))))
    (vector-set! v 5
                 (list (make-object text% dynapad "Zoom1")
                       #f
                       (lambda(menu)
                         (set! zoomtype 1)
                         (changemode dynapad "Zoom"))))
    (vector-set! v 6
                 (list (make-object text% dynapad "folks")
                       #f
                       (lambda(menu)(send menu push folks))))
    (send menu hide-oct v)
    v))

(send menu push main)

(define folks
  (let
      ((v (make-vector 8 #f))
       (dir (build-path->string *dynapad-directory* "images/people")))
    (vector-set! v 0
                 (list (make-object image% dynapad (build-path->string dir "dbauer.gif"))
                       (lambda(menu)(printf "what's shakin'?~%"))
                       #f))
    (vector-set! v 1
                 (list (make-object image% dynapad (build-path->string dir "mhayward.gif"))
                       (lambda(menu)(printf "mike~%"))
                       #f))
    (vector-set! v 2
                 (list (make-object image% dynapad (build-path->string dir "apoon.jpg"))
                       (lambda(menu)(printf "alvin~%"))
                       #f))
    (vector-set! v 3
                 (list (make-object image% dynapad (build-path->string dir "jhollan.gif"))
                       (lambda(menu)(printf "jim~%"))
                       #f))
    (vector-set! v 4
                 (list (make-object image% dynapad (build-path->string dir "ehutchins.gif"))
                       (lambda(menu)(printf "ed~%"))
                       #f))
    (vector-set! v 5
                 (list (make-object image% dynapad (build-path->string dir "rstanonik.gif"))
                       (lambda(menu)(printf "ron~%"))
                       #f))
    (vector-set! v 6
                 (list (make-object text% dynapad "Dismiss")
                       (lambda(menu)(send menu pop))
                       (lambda(menu)(send menu pop))))
    (vector-set! v 7
                 (list (make-object image% dynapad (build-path->string dir "rstanonik.gif"))
                       (lambda(menu)(printf "yow!~%"))
                       #f))
    (send menu hide-oct v)
    v))

(define drawvec
  (let ((v (make-vector 8 #f)))
    (vector-set! v 0
                 (list (make-object text% dynapad "rect")
                       (lambda(menu)(initDraw dynapad rect%))
                       #f))
    (vector-set! v 1
                 (list (make-object text% dynapad "oval")
                       (lambda(menu)(initDraw dynapad oval%))
                       #f))
    (vector-set! v 3
                 (list (make-object text% dynapad "Dismiss")
                       (lambda(menu)(send menu pop))
                       (lambda(menu)(send menu pop))))
    (vector-set! v 4
                 (list (make-object text% dynapad "line")
                       (lambda(menu)(initDraw dynapad line%))
                       #f))
    (vector-set! v 5
                 (list (make-object text% dynapad "free")
                       (lambda(menu)(initDraw dynapad freehand%))
                       #f))
    (vector-set! v 6
                 (list (make-object text% dynapad "poly")
                       (lambda(menu)(initDraw dynapad polygon%))
                       #f))
    (send menu hide-oct v)
    v))
