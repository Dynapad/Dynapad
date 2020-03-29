(require (prefix-in wx: (only-in mred/private/kernel
                                 fill-private-color
                                 get-panel-background
                                 the-pen-list))
         racket/class)
#;
(require (lib "class100.ss"))

; Someday we might decide to create an entire library of widgets for pad.
; Until then we need to leverage of effort by using existing widgets.
; Drscheme uses a chopped subset of wxWindows (apparently), which is fine
; except that dynapad isn't implemented within that widget world.  My
; hacked solution is to create a dummy toplevel window from which popup
; menus called be created.  I call this the popup-server.

; see method: /home/hci/plt-200/collects/mred/mred.ss line 3554

(define popup-server%
  (class canvas%  (parent)

    (inherit popup-menu)
    (define/override
      (on-focus on?) (say on?))
    (define/public
      (makepup pum x y) (popup-menu pum
                                    (inexact->exact x)
                                    (inexact->exact y)))
    (sequence (apply super-init (list parent '())))))

(define topframe dynapad)
(define (make-popup-server) #f)
;(define (make-popup-server)
;  (when (not topframe)
;    (set! topframe (make-object frame% "ps"  #f  1 1  0 0 '(float) )))
;  (make-object popup-server% topframe))

(define (Xscreen-xy argPAD sx sy)
  (let* ((bb (send argPAD bbox))
         (winfo (send argPAD winfo))
         (Xsx (+ (car winfo) (/ (caddr winfo) 2) sx))
         (Xsy (- (+ (cadr winfo) (/ (cadddr winfo) 2)) sy)))
    (values (inexact->exact (floor Xsx)) (inexact->exact (floor Xsy))) ))

(define *last-Xscreen-xy* '(100 100))

; given dynapad and popserver, returns lambda for popping-up menu at x y.
; (x y) in X windows coordinates of full screen
(define (show-popup-lambda argPAD popserver)
  (lambda (pmenu sx sy)
    (let-values (((sx sy) (Xscreen-xy argPAD sx sy)))
      (set! *last-Xscreen-xy* (list sx sy))
      ;      (send popserver makepup pmenu sx sy))))
      (send argPAD makepup pmenu sx sy))))

;---

(define (new-popup title)
  (make-object popup-menu% title
               (lambda (m e)
                 (unless (is-a? m popup-menu%)
                   (error "bad menu object"))
                 (unless
                     (and (is-a? e control-event%)
                          (memq (send e get-event-type) '(menu-popdown menu-popdown-none)))
                   (error "bad event object")))))

(define (add-submenu _menu _label . args)
  (define sb (make-object menu% _label _menu))
  (when (not (null? args)) (send sb enable (car args)))
  sb
  )


(define (add-menu-item menu label func . args)
  (define mi (make-object menu-item% label menu (lambda (w e) (func))))
  (when (not (null? args)) (send mi enable (car args)))
  mi
  )

(define (add-checkable-menu-item menu label func checked? . enabled?)
  (let ((cmi (make-object checkable-menu-item% label menu
                          (lambda (i e) (func i)) #f #f void checked?)))
    (when (not (null? enabled?)) (send cmi enable (car enabled?)))
    cmi))

#|
;these are only needed if menu objs persist between popups
(define exclusive-checkable-item-set%
  (class object%
    (init-field _menu)
    (field (_items null))
    (super-instantiate ())
    (define/public (menu) _menu)
    (define/public (items) _items)
    (define/public (new-item item) (push! item _items))
    (define/public (uncheck-others exception)
      (foreach _items (lambda (i) (unless (eq? i exception)
                                    (if (send i is-checked?)
                                        (send i check #f))))))
    ))

(define (add-exclusive-checkable-item-set menu)
  (make-object exclusive-checkable-item-set% menu))

(define (add-exclusive-checkable-menu-item checkable-set label funct)
  (let* ((menu (send checkable-set menu))
         (full-funct (lambda (item evnt)
                       (unless (send item is-checked?) ;already checked?
                         (send checkable-set uncheck-others item)
                         (send item check #t)
                         (funct))))
         (newitem (make-object checkable-menu-item% label menu full-funct)))
    (send checkable-set new-item newitem)
    newitem))
|#

(define (add-menu-separator menu)
  (make-object separator-menu-item% menu))

;(define (add-checkable-menu-item menu label func . args)
;  (define mi (make-object checkable-menu-item% label menu (lambda (w e) (func w))))
;  (if (not (null? args)) (send mi check (car args)))
;  mi
;  )

;example
; assume S1 is an object with method (state . newval)
;  (add-checkable-menu-item popmenu
;    "Current Object State"
;    (lambda (mi) (send S1 state (send mi is-checked?)) )
;    (send S1 state))


;---

(define (new-wintopmenu argPAD title)
  (let ((winfo (send argPAD winfo))
        (topmenu_frame 0)
        (menubar 0)
        )
    (set! topmenu_frame (make-object frame% title #f
                                     (caddr winfo) 0 (car winfo) (cadr winfo)
                                     '()))
    (send topmenu_frame show #t)
    (set! menubar (make-object menu-bar% topmenu_frame))
    menubar
    )
  )

(define (hide-wintopmenu menu) (send (send menu get-frame) show #f))
(define (show-wintopmenu argPAD menu)
  (let ((winfo (send argPAD winfo))
        (height (send (send menu get-frame) get-height))
        )
    (send (send menu get-frame) show #t)
    (send (send menu get-frame) move (car winfo) (cadr winfo))
    (send (send menu get-frame) resize (caddr winfo) height)))


;---


