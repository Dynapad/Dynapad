#lang racket/base

(require (only-in racket/class
                  is-a?
                  send)
         dynapad/base
         (only-in dynapad/pad-state
                  dynapad
                  currentPAD
                  )
         (only-in dynapad/undo-state
                  import-path
                  restore-path
                  )
         (only-in dynapad/save
                  Select-File-Dialog
                  )
         (only-in dynapad/misc/misc
                  foreach
                  any-selected?
                  send-selected
                  )
         (only-in dynapad/events/draw
                  initDraw
                  Draw-multiple
                  set-Draw-multiple!
                  )
         (only-in dynapad/events/text
                  text%)
         dynapad/menu/wxmenu
         )

(provide Select-and-Load-File
         Restore-Current
         nyi
         Select-and-Restore-File
         Export-To-Directories
         )

(define (nyi msg) (lambda () (printf "~a: not yet implemented~%" msg)))

; global vars
(define *wintop_menu* #f)

;; from menu_functions

(define Select-and-Load-File
  (case-lambda
    (() (Select-and-Load-File import-path)) ;import by default
    ((load-context-fn)
     (let ((path (Select-File-Dialog 'load)))
       (when path
         (load-context-fn path))
       path))))

(define (Select-and-Restore-File)
  (let ((path (Select-and-Load-File restore-path)))
    (when path
      (send currentPAD set!-path path))))

(define (Restore-Current)
  (let ((path (send currentPAD get-path)))
    (if path
        (restore-path path)
        (Select-and-Restore-File))))

;
; Export submenu
;

(define Export-To-Directories
  (case-lambda
    (() (Export-To-Directories (send dynapad objects)))
    ((objs)
     (let ((newdir (Select-File-Dialog 'save)))
       (when newdir
         (if (file-exists? newdir)
             (error "File or directory already exists:" newdir)
             (begin
               (make-directory newdir)
               (foreach objs (lambda (o) (send o export newdir)))
               newdir)))))))

(define (make-submenu-Export mb object)
  (define sb (add-submenu mb "Export"))
  (add-menu-item sb "Export Selected..."
                 (lambda () (Export-To-Directories (send currentPAD selected)))
                 (any-selected?))
  (add-menu-item sb "Export All..." Export-To-Directories))


;
; Tools submenu
;
(define (make-submenu-Draw mb object)
  (let ((pad (cond ((is-a? object dynapad%) object)
                   ((not object) currentPAD)
                   (else (send object dynapad)))))
    (define sb (add-submenu mb "Draw"))
    (add-menu-item sb "Text" (lambda () (initDraw dynapad text% "DrawText")))
    (add-menu-item sb "Rect" (lambda () (initDraw dynapad rect% "Draw")))
    (add-menu-item sb "Oval" (lambda () (initDraw dynapad oval% "Draw")))
    (add-menu-item sb "Polygon" (lambda () (initDraw dynapad polygon% "Draw")))
    (add-menu-item sb "Line" (lambda () (initDraw dynapad line% "Draw")))
    (add-menu-item sb "PolyLine" (lambda () (initDraw dynapad polyline% "Draw")))
    (add-menu-separator sb) ;------------------------------
    (add-checkable-menu-item sb "Fill shapes"
                             (lambda (i)
                               (send pad fill? (send i is-checked?)))
                             (send pad fill?))
    (add-checkable-menu-item sb "Draw multiple"
                             (lambda (i)
                               (set-Draw-multiple! (send i is-checked?)))
                             Draw-multiple)
    ))
