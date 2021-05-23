(dynaload "import-dirs.rkt")
(dynaload "brush.rkt")
(dynaload "relation.rkt")

;(alist-filters 'add
;   (make-alist-modifier-function 'brush-set
;      (lambda (vals) (filter
;              identity
;              (map (lambda (val)
;                 (if (is-a? val directory-brush-set%)
;                 `(send this refer-when-ready ,(send val id) 'members 'add this)
;;                 `(refer-when-ready 'in-relation ,(send val id))
;                 #f))
;               vals)))))

(define *samedir-relations* null)
(abstract-objects-callbacks 'add
                            (lambda () *samedir-relations*)) ;includes samedir-relations when saving workspace

(define *highlight-samedir-relations?* #f)
(define highlight-samedir-relations
  (case-lambda
    (() *highlight-samedir-relations?*)
    ((bool)
     (set! *highlight-samedir-relations?* bool)
     (foreach *samedir-relations* (lambda (rel) (send rel enable bool)))
     )))

(define samedir-hilight%
  (class hilight%
    (init dynapad-arg object-arg)
    (inherit-field _dynapad _object _cptr)

    (super-instantiate (dynapad-arg object-arg 'brush))
    (sch_pen _cptr "green")
    ))
(define (hilight-samedir-fn obj)
  (default-highlight-fn obj samedir-hilight%))
(define (unhilight-samedir-fn obj)
  (default-unhighlight-fn obj))

(define samedir-relation%
  (class exhaustive-brushed-relation%
    (init-field (_dir #f))
    (init-field (_parent #f))
    (super-instantiate (_dir))
    (send this myclass 'samedir-relation%)
    (push! this *samedir-relations*)

    (let ((bs (send this brush-set)))
      (send bs enable *highlight-samedir-relations?*)
      (send bs targets 'add (list bs hilight-samedir-fn unhilight-samedir-fn)))

    (define/public dir (get/set _dir))

    (define/override (activate-member memb)
      (super activate-member memb)
      (set-object-keyval memb 'samedir-relation this))

    (define/override (delete)
      (set! *samedir-relations*
            (remq this *samedir-relations*))
      (super delete))
    ))

; (define directory-brush-set%
;   (class exhaustive-brush-set%
;     (init-field _dir)
;     (init-field (_parent #f))
;     (super-instantiate ())

;     (push! this *samedir-relations*)
;     (send this targets 'add (list this samedir-hilight%))

;     (define/public dir (get/set _dir))
;     (define/public (add obj)
;       (send this members 'add obj))

; ;    (define/override (add-member obj)
; ;      (super add-member obj)
; ;      (send obj post-build-ops 'add
; ;        (lambda (o)
; ;          `(send obj refer-when-ready ,(send this id) members 'add obj))
; ;        this))

; ;    (define/override (remove-member obj)
; ;      (super remove-member obj)
; ;      (send obj post-build-ops 'remove #f this))
;     ))

;these override defaults in import-dirs.ss
(define (wrap-directory dir . parent+args)
  (apply make-object samedir-relation% dir parent+args))

(define (add-to-wrapper parent obj)
  (send parent members 'add obj))

(define (parent-wrapper-dir parent)
  (send parent dir))

;========== GUI =============

(define (make-submenu-Highlighting mb obj)
  (let* ((sb (add-submenu mb "Highlighting")))
    (add-checkable-menu-item sb "Highlight duplicates"
                             (lambda (i) (highlight-duplicates (send i is-checked?)))
                             *highlight-duplicates?*)
    (add-checkable-menu-item sb "Highlight directory members"
                             (lambda (i) (highlight-samedir-relations (send i is-checked?)))
                             *highlight-samedir-relations?*)))

(define (select-highlighted obj brush-set)
  (let* ((target-fn (and brush-set (send brush-set target-objs-fn)))
         (others (if target-fn (target-fn obj) null))
         (all (filter
               (lambda (o) (and
                            (not (send o deleted?))
                            (send o findable)))
               (cons obj others))))
    (Set-Select--undoable (send obj dynapad) all)
    ))

(define (make-submenu-Select-Highlighted mb obj)
  (let* ((sb (add-submenu mb "Select")))
    (add-menu-item sb "Select duplicates"
                   (lambda ()
                     (select-highlighted
                      obj
                      (cond ((is-a? obj image%)
                             (let ((found (assq 'images *brush-sets*)))
                               (and found (cadr found))))
                            ((is-a? obj pdf-portrait%)
                             (let ((found (assq 'pdfs *brush-sets*)))
                               (and found (cadr found))))
                            (else #f)))))
    (add-menu-item sb "Select directory members"
                   (lambda ()
                     (select-highlighted
                      obj
                      (let ((found (get-object-keyval obj 'samedir-relation)))
                        (and found (send found brush-set))))))
    ))

(if *popup-menus-enabled?*
    (append-mainmenu-constructor
     (lambda (mb obj)
       (make-submenu-Highlighting mb obj)
       )))
