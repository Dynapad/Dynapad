(require (lib "defmacro.ss"))

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

;---- Macros for operating on alists  ----

;(...) --> ((new-key-val)..)
; new-key-val is list (key v0 v1..)
; If key already in alist, returns corresp val
;  if not, adds and returns new-key-val
; (to see if added, check (eq? result new-key-val))
(define-macro (get-else-push-onto-alist! assoc-fn new-key-val . alist)
`(let* ((key (car ,new-key-val))
	(key-vals (,assoc-fn key (remote-get ,@alist))))
   (if key-vals
       key-vals
       (begin
	 (remote-push! ,new-key-val ,@alist)
	 ,new-key-val))))

;(..(key v0 v1..)..)  --> (...)
(define-macro (get-and-rem-from-alist! assoc-fn rem-fn key . alist)
  `(let ((key-vals (,assoc-fn ,key (remote-get ,@alist))))
     (if key-vals
	 (begin
	   (remote-set! ,@alist (,rem-fn key-vals (remote-get ,@alist))) ;use set-cdr?
	   key-vals)
	 #f)))

;(..(key v0 v1..)..)  -->  (..(key vals)..)  or
;        (...)        -->  ((key vals)..)
(define-macro (replace-else-push-onto-alist! assoc-fn key vals . alist)
  `(let ((key-vals (,assoc-fn ,key (remote-get ,@alist))))
     (if key-vals ;already there; set new val and return old
	 (let ((old-vals (cdr key-vals)))
	   (set-cdr! key-vals ,vals)
	   (cons ,key old-vals))
	 (begin ;push new
	   (remote-push! (cons ,key ,vals) ,@alist)
	   #f))))

;(..(key v0 v1..)..)  --> (..(key vals)..)
(define-macro (replace-alist-val! assoc-fn key vals . alist)
  `(let* ((key-vals (,assoc-fn ,key (remote-get ,@alist))))
     (if key-vals
	 (begin
	   (set-cdr! key-vals ,vals)
	   key-vals)
	 #f)))

;(..(key v0 v1..)..)  --> (..(key fn[v0 v1..])..)
(define-macro (modify-alist-val! assoc-fn change-fn key . alist)
  `(let* ((key-vals (,assoc-fn ,key (remote-get ,@alist))))
     (if key-vals
	 (begin
	   (set-cdr! key-vals (,change-fn (cdr key-vals)))
	   key-vals)
	 #f)))

;(..(key v0 v1..)..) --> (..(key val v0 v1..)..)
(define-macro (push-onto-alist-val! assoc-fn key val . alist)
  `(let* ((key-vals (,assoc-fn ,key (remote-get ,@alist))))
     (if key-vals
	 (begin
	   (set-cdr! key-vals (cons ,val (cdr key-vals)))
	   key-vals)
	 #f)))
(define-macro (pushv-onto-alist-val! key val . alist)
  `(push-onto-alist-val! assv ,key ,val ,@alist))
(define-macro (pushq-onto-alist-val! key val . alist)
  `(push-onto-alist-val! assq ,key ,val ,@alist))

;(..(key v0..)..) --> (..(key val v0..)..)  or
;      (..)       --> ((key val)..)
(define-macro (push-onto-alist-val-always! assoc-fn key val . alist)
  `(let* ((key-vals (,assoc-fn ,key (remote-get ,@alist))))
     (if key-vals
	 (set-cdr! key-vals (cons ,val (cdr key-vals)))
	 (begin
	   (set! key-vals (list ,key ,val))
	   (remote-push! key-vals ,@alist))) ;no such key, so make one
     key-vals)) ;return entry in either case
(define-macro (pushv-onto-alist-val-always! key val . alist)
  `(push-onto-alist-val-always! assv ,key ,val ,@alist))
(define-macro (pushq-onto-alist-val-always! key val . alist)
  `(push-onto-alist-val-always! assq ,key ,val ,@alist))

;(..(key ..val..)..) --> (..(key...)..) 
(define-macro (delete-from-alist-val! assoc-fn del-fn key val . alist)
  `(let* ((key-vals (,assoc-fn ,key (remote-get ,@alist))))
     (if key-vals
	 (begin
	   (set-cdr! key-vals (,del-fn ,val (cdr key-vals)))
	   key-vals)
	 #f)))
(define-macro (remv-from-alist-val! key val . alist)
  `(delete-from-alist-val! assv remv ,key ,val ,@alist))
(define-macro (remq-from-alist-val! key val . alist)
  `(delete-from-alist-val! assq remq ,key ,val ,@alist))

;Like delete-from-alist-val!, but also removes key when del'ing last val
;(..(key ..val..)..) --> (..(key...)..) or (...) if last val of key
(define-macro (delete-clean-from-alist-val! assoc-fn del-fn key val . alist)
  `(let* ((key-vals (,assoc-fn ,key (remote-get ,@alist))))
     (if key-vals
	 (begin
	   (set-cdr! key-vals (,del-fn ,val (cdr key-vals)))
	   (if (null? (cdr key-vals))
	       (remote-set! ,@alist (remv key-vals (remote-get ,@alist))))
	   key-vals)
	 #f)))
(define-macro (remv-clean-from-alist-val! key val . alist)
  `(delete-clean-from-alist-val! assv remv ,key ,val ,@alist))
(define-macro (remq-clean-from-alist-val! key val . alist)
  `(delete-clean-from-alist-val! assq remq ,key ,val ,@alist))

;=================================================================
; alternative alist functions

;--- alist functions on objects ----------------------------------
; (Dan says on 10/05):
; I think these are redundant.  The above macros were designed to work
;  through messages, as in:
;  (replace-alist-val! assq key val OBJ 'alist)
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
	(set-cdr! elt (list val))
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

