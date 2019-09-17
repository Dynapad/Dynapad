(define undo-lambda (lambda (eventPAD e) (set! currentPAD eventPAD) (undo)))
(define redo-lambda (lambda (eventPAD e) (set! currentPAD eventPAD) (redo)))

(define (bindControlKeys argPAD)
  (send argPAD bind "<Control-KeyPress-z>"  undo-lambda)

  (send argPAD bind "<Control-KeyPress-c>"
    (lambda (eventPAD e) (set! currentPAD eventPAD) (Copy-Selected)))

  (send argPAD bind "<Control-KeyPress-v>"
    (lambda (eventPAD e) (set! currentPAD eventPAD) (Paste-From-Copy-Buffer)))

  (send argPAD bind "<Control-KeyPress-x>"
    (lambda (eventPAD e) (set! currentPAD eventPAD) (Copy-Selected) (send-selected delete)))

  (send argPAD bind "<Control-KeyPress-d>"
    (lambda (eventPAD e) (set! currentPAD eventPAD) (Copy-Selected)(Paste-From-Copy-Buffer)))

  ; two key bindings for redo function
  (send argPAD bind "<Control-KeyPress-y>"       redo-lambda)
  (send argPAD bind "<Control-Shift-KeyPress-Z>" redo-lambda)

  (send argPAD bind "<Select-KeyPress-Delete>"
      (lambda (eventPAD e) (Delete-Selected)))
    (send argPAD bind "<Select-KeyPress-BackSpace>"
      (lambda (eventPAD e) (Delete-Selected)))
    (send argPAD bind "<Run-KeyPress-Delete>"
      (lambda (eventPAD e) (Delete-Selected)))
    (send argPAD bind "<Run-KeyPress-BackSpace>"
      (lambda (eventPAD e) (Delete-Selected)))

    (send argPAD bind "<Select-Shift-KeyPress-Delete>"
      (lambda (eventPAD e) (Deep-Delete-Selected)))
    (send argPAD bind "<Select-Shift-KeyPress-BackSpace>"
      (lambda (eventPAD e) (Deep-Delete-Selected)))
    (send argPAD bind "<Run-Shift-KeyPress-Delete>"
      (lambda (eventPAD e) (Deep-Delete-Selected)))
    (send argPAD bind "<Run-Shift-KeyPress-BackSpace>"
      (lambda (eventPAD e) (Deep-Delete-Selected)))
)
