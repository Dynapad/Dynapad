#lang racket/base

(require (only-in dynapad/pad-state write-set)
         (only-in dynapad/undo-state saveable-objects)
         (only-in dynapad/ffs *id-counter*)
         )

(provide Save-All-To-Expr
         Save-All-To-String
         Format-SaveAll-Expr-Into-String
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

(define (Save-All-To-Port port)
  (fprintf port (Format-SaveAll-Expr-Into-String (Save-All-To-Expr))))

;(define (Save-All-To-String)
;  (string-append
;   "(load-set\n"
;   (format "(max-padid ~a)~%" *id-counter*)
;   (Write-All-Objects-To-String)
;   ")\n"))


