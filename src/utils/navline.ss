;(define navline-pane (make-object horizontal-pane% *menubar*))

(define off_navline (use-bitmap-or-string "pad/bitmaps/navline.xbm"    "Nav Line"))
(define on_navline  (use-bitmap-or-string "pad/bitmaps/On_navline.xpm" "Nav Line"))

(define btn_navline
  (make-object button%
               off_navline
               guestpane
               (lambda (button event)
                 (clear-all-menu-buttons)
                 (send button set-label on_navline)
                 (set! Draw-multiple (button-double-clicked?))
                 (for-all-pads
                  (lambda (argPAD)
                    (initDraw argPAD navline% "Draw")))
                 )))

(define *navline-message* (make-object message% "(bearing)" guestpane))

(define (coords->bearing crds)
  (mlet (((x2 y2 x1 y1) crds))
        (let* ((pi/2 (/ pi 2))
               (dx (- x2 x1))
               (dy (- y2 y1))
               (rads (cond ((not (zero? dx)) (/ dy dx))
                           ((< dy 0) (- (tan pi/2)))
                           (else (tan pi/2))))
               (bearing (* (/ 180 pi) (atan rads))))
          (set! bearing (if (>= dx 0)
                            (- 90 bearing)
                            (- 270 bearing)))
          (round-to-int bearing))))

(define navline%
  (class line%
    (init _dynapad (initcoords #f))
    (inherit dynaclass coords)
    (super-instantiate (_dynapad))
    (dynaclass 'navline%)

    (send this coords-callbacks 'add
          (lambda (obj crds)
            (send *navline-message*
                  set-label
                  (format "~a" (coords->bearing crds)))))

    (if initcoords (coords initcoords))

    ))

(push! (list btn_navline off_navline)
       *guest-button-off-labels*)


