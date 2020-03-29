(require (lib "defmacro.ss"))
(require compatibility/mlist)

;remote-get, -set, and -push work on either lvals (settable variables)
;  or object fields accessed via message:
; (send obj msg) for getting; (send obj msg val) for setting
(define-macro remote-get
  (case-lambda
    ((var) var)
    ((obj msg) `(send ,obj ,msg))))

(define-macro remote-set!
  (case-lambda
    ((var val)
     `(set! ,var ,val))
    ((obj msg val)
     `(send ,obj ,msg ,val))))

(define-macro (remote-push! atom . lst)
  `(remote-set! ,@lst (cons ,atom (remote-get ,@lst))))

;---- Macros for operating on m(utable)alists  ----

;(...) --> ((new-key-val)..)
; new-key-val is list (key v0 v1..)
; If key already in alist, returns corresp val
;  if not, adds and returns new-key-val
; (to see if added, check (eq? result new-key-val))
(define-macro (get-else-push-onto-malist! massoc-fn new-key-val . alist)
  `(let* ((key (mcar ,new-key-val))
          (key-vals (,massoc-fn key (remote-get ,@alist))))
     (if key-vals
         key-vals
         (begin
           (remote-push! ,new-key-val ,@alist)
           ,new-key-val))))

;(..(key v0 v1..)..)  --> (...)
(define-macro (get-and-rem-from-malist! massoc-fn rem-fn key . alist)
  `(let ((key-vals (,massoc-fn ,key (remote-get ,@alist))))
     (if key-vals
         (begin
           (remote-set! ,@alist (,rem-fn key-vals (remote-get ,@alist))) ;use set-cdr?
           key-vals)
         #f)))

;(..(key v0 v1..)..)  -->  (..(key vals)..)  or
;        (...)        -->  ((key vals)..)
(define-macro (replace-else-push-onto-malist! massoc-fn key vals . alist)
  `(let ((key-vals (,massoc-fn ,key (remote-get ,@alist))))
     (if key-vals ;already there; set new val and return old
         (let ((old-vals (mcdr key-vals)))
           (set-mcdr! key-vals ,vals)
           (mcons ,key old-vals))
         (begin ;push new
           (remote-push! (mcons ,key ,vals) ,@alist)
           #f))))

;(..(key v0 v1..)..)  --> (..(key vals)..)
(define-macro (replace-malist-val! massoc-fn key vals . alist)
  `(let* ((key-vals (,massoc-fn ,key (remote-get ,@alist))))
     (if key-vals
         (begin
           (set-mcdr! key-vals ,vals)
           key-vals)
         #f)))

;(..(key v0 v1..)..)  --> (..(key fn[v0 v1..])..)
(define-macro (modify-malist-val! massoc-fn change-fn key . alist)
  `(let* ((key-vals (,massoc-fn ,key (remote-get ,@alist))))
     (if key-vals
         (begin
           (set-mcdr! key-vals (,change-fn (mcdr key-vals)))
           key-vals)
         #f)))

;(..(key v0 v1..)..) --> (..(key val v0 v1..)..)
(define-macro (push-onto-malist-val! massoc-fn key val . alist)
  `(let* ((key-vals (,massoc-fn ,key (remote-get ,@alist))))
     (if key-vals
         (begin
           (set-mcdr! key-vals (mcons ,val (mcdr key-vals)))
           key-vals)
         #f)))
(define-macro (pushv-onto-malist-val! key val . alist)
  `(push-onto-malist-val! massv ,key ,val ,@alist))
(define-macro (pushq-onto-malist-val! key val . alist)
  `(push-onto-malist-val! massq ,key ,val ,@alist))

;(..(key v0..)..) --> (..(key val v0..)..)  or
;      (..)       --> ((key val)..)
(define-macro (push-onto-malist-val-always! massoc-fn key val . alist)
  `(let* ((key-vals (,massoc-fn ,key (remote-get ,@alist))))
     (if key-vals
         (set-mcdr! key-vals (mcons ,val (mcdr key-vals)))
         (begin
           (set! key-vals (mlist ,key ,val))
           (remote-push! key-vals ,@alist))) ;no such key, so make one
     key-vals)) ;return entry in either case
(define-macro (pushv-onto-malist-val-always! key val . alist)
  `(push-onto-malist-val-always! massv ,key ,val ,@alist))
(define-macro (pushq-onto-malist-val-always! key val . alist)
  `(push-onto-malist-val-always! massq ,key ,val ,@alist))

;(..(key ..val..)..) --> (..(key...)..)
(define-macro (delete-from-malist-val! massoc-fn del-fn key val . alist)
  `(let* ((key-vals (,massoc-fn ,key (remote-get ,@alist))))
     (if key-vals
         (begin
           (set-mcdr! key-vals (,del-fn ,val (mcdr key-vals)))
           key-vals)
         #f)))
(define-macro (remv-from-malist-val! key val . alist)
  `(delete-from-malist-val! massv remv ,key ,val ,@alist))
(define-macro (remq-from-malist-val! key val . alist)
  `(delete-from-malist-val! massq remq ,key ,val ,@alist))

;Like delete-from-malist-val!, but also removes key when del'ing last val
;(..(key ..val..)..) --> (..(key...)..) or (...) if last val of key
(define-macro (delete-clean-from-malist-val! massoc-fn del-fn key val . malist)
  `(let* ((key-vals (,massoc-fn ,key (remote-get ,@malist))))
     (if key-vals
         (begin
           (set-mcdr! key-vals (,del-fn ,val (mcdr key-vals)))
           (when (null? (mcdr key-vals))
             (remote-set! ,@malist (remv key-vals (remote-get ,@malist))))
           key-vals)
         #f)))
(define-macro (remv-clean-from-malist-val! key val . malist)
  `(delete-clean-from-malist-val! massv remv ,key ,val ,@malist))
(define-macro (remq-clean-from-malist-val! key val . malist)
  `(delete-clean-from-malist-val! massq remq ,key ,val ,@malist))

;=================================================================
; alternative alist functions

;--- alist functions on objects ----------------------------------
; (Dan says on 10/05):
; I think these are redundant.  The above macros were designed to work
;  through messages, as in:
;  (replace-malist-val! assq key val OBJ 'alist)
; which equals
;  (set-object-keyval OBJ key val)


(define (set-object-keyval obj key val)
  (send obj alist (set-alist-keyval (send obj alist) key val)))

(define (get-object-keyval obj key . notfoundval)
  (if (pair? notfoundval)
      (get-alist-keyval (send obj alist) key (car notfoundval))
      (get-alist-keyval (send obj alist) key)))

(define (get-object-keyvals obj key . notfoundval)
  (if (pair? notfoundval)
      (get-alist-keyvals (send obj alist) key (car notfoundval))
      (get-alist-keyvals (send obj alist) key)))

(define (rem-object-keyval obj key)
  (send obj alist (rem-alist-key (send obj alist) key)))

; Because these alist functions are intended for xsml,
; these alist functions always use eq? and assq

; returns a new alist with keyval pair modified or added
(define (set-alist-keyval alist key val)
  (if (or (not alist) (null? alist))
      (set! alist (list (list key val)))
      ;else
      (let ((elt (assq key alist)))
        (if elt
            (set-mcdr! elt (list val))
            ;else
            (set! alist (cons (list key val) alist))
            )
        )
      )
  alist
  )

(define (get-alist-keyval alist key . notfoundval)
  (if (null? notfoundval)
      (set! notfoundval #f)
      (set! notfoundval (car notfoundval)))
  (let ((val (assq key alist)))
    (if val (cadr val) notfoundval)))

(define (get-alist-keyvals alist key . notfoundval)
  (if (null? notfoundval)
      (set! notfoundval #f)
      (set! notfoundval (car notfoundval)))
  (let ((val (assq key alist)))
    (if val (cdr val) notfoundval)))

; returns a new alist with keyval pair removed
(define (rem-alist-key alist key)
  (if (and (not (null? alist)) (assq key alist))
      (cond
        ((null? alist) alist)
        ((equal? (caar alist) key) (cdr alist))
        (else (cons (car alist) (rem-alist-key (cdr alist) key))))
      ;else
      alist
      )
  )

