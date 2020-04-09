#lang racket/base

(require (only-in racket/class send)
         compatibility/mlist
         dynapad/misc/misc
         dynapad/misc/alist
         dynapad/undo-state
         )

(provide gui-mode-malist
         gui-update-mode
         changemode
         changemode--no-gui
         default-mode
         )

(define _gui-mode-cursor '())

(define gui-mode-cursor
  (case-lambda
    ((mode)
     (let ((cursor (massoc mode _gui-mode-cursor)))
       (if cursor (cadr cursor) #f)))
    ((mode cursor)
     (set! _gui-mode-cursor
           (append _gui-mode-cursor (list (list mode cursor)))))))

(define (gui-add-mode-cursor mode cursor)
  (set! gui-mode-cursor (append gui-mode-cursor (list (list mode cursor)))))

; Cursors:
; 0,13 = arrow
;    1 = crosshair
;    2 = text I-bar
;    3 = hourglass
;  4,7 = NE/SW dual arrow
;  5,6 = NW/SE dual arrow
;  8,9 = N/S dual arrow
;10,11 = E/W dual arrow
;   12 = hand
;  14+ = mystery spiral

(define (createModes arg_PAD)
  (mfor-each (lambda (mode) (send arg_PAD modifier 'create mode))
             (mmap mcar gui-mode-malist))
  )


(define gui-mode-malist
  ; malist of modes and cursors for each
  ; other modules (e.g. menubar.ss) may also push callbacks into each entry
  ; (anywhere after mode)
  (mlist (mlist "Run" 0)
         (mlist "Select" 0) ; mostly obsolete
         (mlist "Zoom" 8)
         (mlist "Pan" 14)
         (mlist "BBox" 1)   ; includes lasso
         (mlist "GetBBox" 1)  ; redundant?
         (mlist "GetLasso" 1) ; redundant?
         (mlist "CreateLink" '()) ; don't care, keep current cursor
         (mlist "Draw" 1)
         (mlist "DrawAdd" 1)
         (mlist "DrawText" 2)
         (mlist "EditText" 0)
         (mlist "Drag" 12)
         (mlist "DragOne" 12))
  )

; initial version of function
; (may be redefined if menubar.ss is loaded)
(define gui-update-mode
  (case-lambda
    ((argPAD) (gui-update-mode argPAD #f))
    ((argPAD cursor)
     (if cursor
         (send argPAD cursor cursor)
         (let* ((mode (send argPAD modifier 'get))
                ;(cursor (gui-mode-cursor mode))
                (tuple (massoc mode gui-mode-malist)))
           (cond
             (tuple
              (mforeach (mcdr tuple)
                        (lambda (val)
                          (cond ((number? val) (send argPAD cursor val))
                                ((procedure? val) (val argPAD))
                                (else val)))))
             (cursor (send argPAD cursor cursor))
             (else (send argPAD cursor 1)) ; unrecognized mode
             ))))))

(define changemode
  (case-lambda
    ((argPAD mode) (changemode argPAD mode #f))
    ((argPAD mode cursor)
     (changemode--no-gui argPAD mode)
     (gui-update-mode argPAD cursor))))

(define (changemode--no-gui argPAD mode)
  (let ((curmode (send argPAD modifier 'get)))
    (send argPAD focus)

    ;    (cond
    ;      ((equal? curmode "DrawText")
    ;;          (equal? curmode "EditText")
    ;        (let ((obj (send argPAD getfocus)))
    ;          (if obj (send obj unfocus)))))

    (when (and (string=? curmode "Select")
               (or (string=? mode "Run")
                   (string=? mode "Pan")
                   (string=? mode "Zoom")
                   (string=? mode "BBox")
                   (string=? mode "Draw")
                   (string=? mode "DrawAdd")
                   (string=? mode "DrawText")
                   (string=? mode "EditText")
                   (string=? mode "")))
      (Set-Select--undoable argPAD null))

    ;    (if (and (string=? mode "Draw") (pair? (send argPAD selected)))
    ;        (Set-Select--undoable argPAD null))

    (send argPAD modifier 'set mode))
  )

(define (default-mode argPAD)
  (send argPAD getvar 'default-mode))

(define (push-event-mode argPAD mode)
  (unless (member mode (send argPAD eventModeStack)) ;dont push same mode twice
    (remote-push! mode argPAD eventModeStack))
  (changemode argPAD mode))

(define (pop-event-mode argPAD . mode)
  ;if mode included, will remove mode even if not on top
  (let* ((oldstack (send argPAD eventModeStack))
         (newstack (if (null? mode)
                       (cdr oldstack)
                       (remove (car mode) oldstack)))
         (newmode (if (null? newstack)
                      (default-mode argPAD)
                      (car newstack))))
    (send argPAD eventModeStack newstack)
    (changemode argPAD newmode)))

(define (clear-delay-cursor argPAD)
  (send argPAD setvar 'delay-cursor? #f)
  (gui-update-mode argPAD))
