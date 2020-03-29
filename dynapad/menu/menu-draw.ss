(dynaload "menubar.ss")

(define rectpane     (make-object horizontal-pane% drawpane))
(define ovalpane     (make-object horizontal-pane% drawpane))
(define linepane     (make-object horizontal-pane% drawpane))
(define polypane     (make-object horizontal-pane% drawpane))
(define freetextpane (make-object horizontal-pane% drawpane))

(define off_rect       (use-bitmap-or-string "pad/bitmaps/rect.xbm"          "Rect"        ))
(define off_oval       (use-bitmap-or-string "pad/bitmaps/oval.xbm"          "Oval"        ))
(define off_poly       (use-bitmap-or-string "pad/bitmaps/polygon.xbm"       "Polygon"     ))
(define off_filledRect (use-bitmap-or-string "pad/bitmaps/filledRect.xbm"    "Filled Rect" ))
(define off_filledOval (use-bitmap-or-string "pad/bitmaps/filledOval.xbm"    "Filled Oval" ))
(define off_filledPoly (use-bitmap-or-string "pad/bitmaps/filledPolygon.xbm" "Filled Poly" ))
(define off_line       (use-bitmap-or-string "pad/bitmaps/line.xbm"          "Line"        ))
(define off_polyline   (use-bitmap-or-string "pad/bitmaps/lines.xbm"         "PolyLine"    ))
(define off_text       (use-bitmap-or-string "pad/bitmaps/letter.xbm"        "Text"        ))
(define off_freehand   (use-bitmap-or-string "pad/bitmaps/drawing.xbm"       "Free"        ))
;note on off_freehand: drawing.xpm is antialiased (like On_drawing.xpm) but
;  not monochrome like all others, and looks different on cygwin
;  Therefore replaced with uglier but monochrome drawing.xbm

(define on_rect       (use-bitmap-or-string "pad/bitmaps/On_rect.xpm"          "Rect"        ))
(define on_oval       (use-bitmap-or-string "pad/bitmaps/On_oval.xpm"          "Oval"        ))
(define on_poly       (use-bitmap-or-string "pad/bitmaps/On_polygon.xpm"       "Polygon"     ))
(define on_filledRect (use-bitmap-or-string "pad/bitmaps/On_filledRect.xpm"    "Filled Rect" ))
(define on_filledOval (use-bitmap-or-string "pad/bitmaps/On_filledOval.xpm"    "Filled Oval" ))
(define on_filledPoly (use-bitmap-or-string "pad/bitmaps/On_filledPolygon.xpm" "Filled Poly" ))
(define on_line       (use-bitmap-or-string "pad/bitmaps/On_line.xpm"          "Line"        ))
(define on_polyline   (use-bitmap-or-string "pad/bitmaps/On_lines.xpm"         "PolyLine"    ))
(define on_text       (use-bitmap-or-string "pad/bitmaps/On_letter.xpm"        "Text"        ))
(define on_freehand   (use-bitmap-or-string "pad/bitmaps/On_drawing.xpm"       "Free"        ))

(define btn_rect
  (make-object button%
               off_rect
               rectpane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #f)))
                 (clear-all-menu-buttons)
                 (send button set-label on_rect)
                 (set! Draw-multiple (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD rect% "Draw")))))
  )

(define btn_filledRect
  (make-object button%
               off_filledRect
               rectpane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #t)))
                 (clear-all-menu-buttons)
                 (send button set-label on_filledRect)
                 (set! Draw-multiple (button-double-clicked?))
                 (update-panel-colors)
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD rect% "Draw")))))
  )

(define btn_oval
  (make-object button%
               off_oval
               ovalpane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #f)))
                 (clear-all-menu-buttons)
                 (send button set-label on_oval)
                 (set! Draw-multiple (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD oval% "Draw")))))
  )
(define btn_filledOval
  (make-object button%
               off_filledOval
               ovalpane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #t)))
                 (clear-all-menu-buttons)
                 (send button set-label on_filledOval)
                 (set! Draw-multiple (button-double-clicked?))
                 (update-panel-colors)
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD oval% "Draw")))))
  )


(define btn_line
  (make-object button%
               off_line
               linepane
               (lambda (button event)
                 (clear-all-menu-buttons)
                 (send button set-label on_line)
                 (set! Draw-multiple (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD line% "Draw")))))
  )

(define btn_polyline
  (make-object button%
               off_polyline
               linepane
               (lambda (button event)
                 (clear-all-menu-buttons)
                 (send button set-label on_polyline)
                 (set! Draw-multiple (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD polyline% "Draw")))))
  )
(define btn_poly
  (make-object button%
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
  (make-object button%
               off_filledPoly
               polypane
               (lambda (button event)
                 (for-all-pads (lambda (argPAD)
                                 (send argPAD fill? #t)))
                 (clear-all-menu-buttons)
                 (send button set-label on_filledPoly)
                 (set! Draw-multiple (button-double-clicked?))
                 (update-panel-colors)
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD polygon% "Draw")))))
  )

(define btn_text
  (make-object button%
               off_text
               freetextpane
               (lambda (button event)
                 (clear-all-menu-buttons)
                 (send button set-label on_text)
                 (set! Draw-multiple (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD text% "DrawText")))))
  )

(define btn_freehand
  (make-object button%
               off_freehand
               freetextpane
               (lambda (button event)
                 (clear-all-menu-buttons)
                 (send button set-label on_freehand)
                 (set! Draw-multiple (button-double-clicked?))
                 (for-all-pads (lambda (argPAD)
                                 (initDraw argPAD freehand% "Draw")))))
  )

(set! *draw-button-off-labels*
      (list
       (list btn_rect        off_rect)
       (list btn_filledRect  off_filledRect )
       (list btn_oval        off_oval       )
       (list btn_filledOval  off_filledOval )
       (list btn_line        off_line       )
       (list btn_polyline    off_polyline   )
       (list btn_poly        off_poly       )
       (list btn_filledPoly  off_filledPoly )
       (list btn_text        off_text       )
       (list btn_freehand    off_freehand   )))
