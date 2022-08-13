#lang racket/base

(require (only-in racket/class send)
         (only-in racket/gui/base
                  message-box
                  put-file
                  get-file
                  )
         (only-in dynapad/pad-state
                  *menubar*
                  currentPAD)
         (only-in dynapad/menu/menu-state
                  append-mainmenu-constructor)
         (only-in dynapad/undo-state saveable-objects)
         (only-in dynapad/ffs *id-counter*)
         (only-in dynapad/misc/misc
                  write-set)
         collects/misc/pathhack
         )

(provide Save-All-To-Expr
         Save-All-To-String
         Format-SaveAll-Expr-Into-String
         Select-File-Dialog
         )
;;; from menu_functions

;(define (Write-All-Objects-To-Port port)
;  (let ((allwrites (write-set (send currentPAD objects))))
;    (foreach allwrites (lambda (w) (fprintf port "~s~%" w)))))
;
;(define (Write-All-Objects-To-String)
;  (let ((allwrites (write-set (send currentPAD objects))))
;    (apply string-append (map (lambda (w) (format "~a~%" w)) allwrites))))
;
(define (Write-All-Objects-To-Expr)
  (let ((allwrites (write-set (saveable-objects))))
    allwrites))

; New Save Format:
(define (Save-All-To-Expr)
  ;first build list for all objs so that *id-counter* can be incremented
  ; as far as necessary
  (let ((obj-expr-list (Write-All-Objects-To-Expr)))
    (cons 'load-set
          (cons `(max-padid ,*id-counter*)
                obj-expr-list))))

(define (Format-SaveAll-Expr-Into-String expr)
  (string-append "("
                 (apply string-append (map (lambda (e) (format "~s~%" e)) expr))
                 ")\n"))

(define (Save-All-To-String)
  (Format-SaveAll-Expr-Into-String (Save-All-To-Expr)))

;(define (Save-All-To-Port port)
;  (fprintf port "(load-set~%")
;  (fprintf port "(max-padid ~s)~%" *id-counter*)
;  (Write-All-Objects-To-Port port)
;  (fprintf port ")~%")
;)

#; ; apparently overwritten by the bit below from logs
(define (Save-All-To-Port port)
  (fprintf port (Format-SaveAll-Expr-Into-String (Save-All-To-Expr))))

;(define (Save-All-To-String)
;  (string-append
;   "(load-set\n"
;   (format "(max-padid ~a)~%" *id-counter*)
;   (Write-All-Objects-To-String)
;   ")\n"))

;; from menu_functions :/
(define *current_directory* (current-directory))
(define Select-File-Dialog
  (case-lambda
    ;mode is 'load or 'save
    ((mode) (Select-File-Dialog mode (send currentPAD get-path)))
    ((mode path)
     (let ((dir *current_directory*)
           (filename #f)
           (dir? #f))
       (when path
         (let-values ([(a b c) (split-path->string path)])
           (set!-values (dir filename dir?)
                        (values (if (eq? 'relative a) dir a) b c))))
       (when dir?
         (set! dir (build-path dir filename))
         (set! filename #f))
       (set! path
             (cond ((eq? mode 'save)
                    (put-file "Save" *menubar* dir filename #f null))
                   (else
                    (get-file "Load" *menubar* dir #f #f null))))
       path))))
