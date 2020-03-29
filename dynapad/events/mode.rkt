#lang racket/base

(require (only-in racket/class send)
         compatibility/mlist
         dynapad/misc/misc
         )

(provide gui-mode-malist
         gui-update-mode)

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
