;======= Change Modifier ========
#| ;obsolete?
(define _gui-mode-cursor ())

(define gui-mode-cursor
  (case-lambda
    ((mode)
       (let ((cursor (assoc mode _gui-mode-cursor)))
         (if cursor (cadr cursor) #f)))
    ((mode cursor)
       (set! _gui-mode-cursor
             (append _gui-mode-cursor (list (list mode cursor)))))))

(define (gui-add-mode-cursor mode cursor)
  (set! gui-mode-cursor (append gui-mode-cursor (list (list mode cursor)))))
|#

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

(define gui-mode-alist
; alist of modes and cursors for each
;  other modules (e.g. menubar.ss) may also push callbacks into each entry
;  (anywhere after mode)
  '(("Run" 0)
    ("Select" 0) ; mostly obsolete
    ("Zoom" 8)
    ("Pan" 14)
    ("BBox" 1)   ; includes lasso
    ("GetBBox" 1)  ; redundant?
    ("GetLasso" 1) ; redundant?
    ("CreateLink") ; don't care, keep current cursor
    ("Draw" 1)
    ("DrawAdd" 1)
    ("DrawText" 2)
    ("EditText" 0)
    ("Drag" 12)
    ("DragOne" 12))
)

(define (createModes arg_PAD)
  (foreach (map car gui-mode-alist)
           (lambda (mode) (send arg_PAD modifier 'create mode)))
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
               (tuple (assoc mode gui-mode-alist)))
          (cond 
           (tuple
            (foreach (cdr tuple)
                     (lambda (val)
                       (cond ((number? val) (send argPAD cursor val))
                             ((procedure? val) (val argPAD))
                             (else val)))))
           (cursor (send argPad cursor cursor))
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
   

; --- Show hourglass cursor for potentially long operations ---
;Note: mred includes (begin-busy-cursor) and (end-busy-cursor),
; but these change cursor only within a mred window, which dynapad is not.
(define-syntax show-possible-delay
  (syntax-rules ()
    ((_ the-delayed-pad do-sth ...)
     (with-handlers
      ([exn:fail?
        (lambda (exn) 
          (foreach (current-error-ports)
                   (lambda (port)
                     (fprintf port "~a~%" (exn-message exn))))
          (pop-delay-cursor the-delayed-pad))])
      (begin
        (push-delay-cursor the-delayed-pad)
        (let ((result (begin do-sth ...)))
          (pop-delay-cursor the-delayed-pad)
          result))))))

(define (push-delay-cursor argPAD)
  (let ((delay? (send argPAD getvar 'delay-cursor?)))
    (when (not delay?) (set! delay? 0))
    (send argPAD setvar 'delay-cursor? (+ delay? 1))
    (send argPAD cursor 3)))
(define (pop-delay-cursor argPAD)
  (let ((delay? (send argPAD getvar 'delay-cursor?)))
    (set! delay? (if (and delay? (> delay? 1))
                     (- delay? 1)
                     #f))
    (send argPAD setvar 'delay-cursor? delay?)
    (gui-update-mode argPAD)))
(define (clear-delay-cursor argPAD)
  (send argPAD setvar 'delay-cursor? #f)
  (gui-update-mode argPAD))

