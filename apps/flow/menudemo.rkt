; load this file after starting dynapad
(load-relative "menu.ss")
(load-relative "zoom.ss")
(dynaload "arrangeimages.ss")

(define *menu* (make-object menu% dynapad))

(define (mk-image imgname)
  (make-object image% dynapad (build-path->string
                               "/home/hci/dynapad/images/people" imgname)))

;-----------------------------------------------------------------

(define image_menu (make-menu *menu*
                              (list 0 "Image"   (lambda (menu) (Load-Image)))
                              (list 1 "Arrange" (lambda (menu) (Arrange-Images)))
                              (list 6 "Dismiss" (lambda (menu) (send menu pop)))
                              ))

(define folks_menu (make-menu *menu*
                              (list 0 (mk-image "dbauer.gif") (lambda (menu) (printf "what's shakin'?~%")) )
                              (list 1 (mk-image "mhayward.gif")   (lambda (menu) (printf "mike~%")) )
                              (list 2 (mk-image "apoon.jpg")      (lambda (menu) (printf "alvin~%")) )
                              (list 3 (mk-image "jhollan.gif")    (lambda (menu) (printf "jim~%")) )
                              (list 4 (mk-image "ehutchins.gif")  (lambda (menu) (printf "ed~%")) )
                              (list 5 (mk-image "rstanonik.gif") (lambda (menu) (printf "ron~%")) )
                              (list 6 (mk-image "rstanonik.gif")  (lambda (menu) (printf "yow!~%")) )
                              (list 7 "Dismiss"                   (lambda (menu) (send menu pop)) )
                              ))

(define draw_menu (make-menu *menu*
                             (list 0 "rect"    (lambda (menu) (initDraw dynapad rect%)) )
                             (list 1 "oval"    (lambda (menu) (initDraw dynapad oval%)) )
                             (list 3 "Dismiss" (lambda (menu) (send menu pop)) )
                             (list 4 "line"    (lambda (menu) (initDraw dynapad line%)) )
                             (list 5 "free"    (lambda (menu) (initDraw dynapad freehand%)) )
                             (list 6 "poly"    (lambda (menu) (initDraw dynapad polygon%)) )
                             ))


(define run_menu (make-menu *menu*
                            (list 0 "Pan"     (lambda (menu) (changemode dynapad "Run")) )
                            (list 1 "Dismiss" (lambda (menu) (send menu pop)) )
                            (list 4 "Zoom"    (lambda (menu) (set! zoomtype 0)
                                                      (changemode dynapad "Zoom")) )
                            (list 5 "Zoom1"   (lambda (menu) (set! zoomtype 1)
                                                      (changemode dynapad "Zoom")) )
                            ))

(define select_menu (make-menu *menu*
                               (list 0 "Delete All" (lambda (menu) (Delete-All)))
                               (list 2 "Dismiss"    (lambda (menu) (send menu pop)))
                               (list 3 "Raise"      (lambda (menu) (Raise-Selected)))
                               (list 4 "Lower"      (lambda (menu) (Lower-Selected)))
                               (list 5 "Group"      (lambda (menu) (Group-Selected)))
                               (list 6 "Ungroup"    (lambda (menu) (UnGroup-Selected)))
                               (list 7 "Delete"     (lambda (menu) (Delete-Selected)))
                               ))

(define main_menu (make-menu *menu*
                             (list 0 "Dismiss" (lambda (menu) (send menu popdown)))

                             (list 1 "Run"     (lambda (menu)
                                                 (send menu push run_menu)
                                                 (changemode dynapad "Run")))

                             (list 2 "Select"  (lambda (menu)
                                                 (send menu push select_menu)
                                                 (changemode dynapad "Select")))

                             (list 3 "Draw"    (lambda (menu) (send menu push draw_menu)))
                             (list 4 "Save"    (lambda (menu) (Save-All)))
                             (list 5 "Load"    (lambda (menu) (Load-File)))
                             (list 6 "Image"   (lambda (menu) (send menu push image_menu)))
                             (list 7 "folks"   (lambda (menu) (send menu push folks_menu)))
                             ))

(send *menu* push main_menu)
