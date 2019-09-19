(dynaload "newrelation.ss")
(dynaload "hilights.ss") ;probably redundant

(define relation-brush%
  ; Works with a relation to brush targets when source is "touched".
  (class object%
    (super-instantiate ())
    (init-field _relation) ;relation may actually be a relset,
    ;  if this is a private brush
    (init-field (_reverse? #f)) ;if #t, brushes opposite relation
    (init       (initially-active? #f))
    (field      (_active? #f))

    ;(define/public (activate relation)
    ;( ;go to each relation-subset of relation,
    ;get members, activate e. member

    ;both of these fns are (lambda (source-obj target-obj)...)
    ; they may choose to ignore brushing (e.g. if source=target)
    ; otherwise (usually) do something to the target obj
    (public-field brush-obj-fn   _brush-obj-fn)
    (public-field unbrush-obj-fn _unbrush-obj-fn)
    (public-field class _class)

    (send this class 'relation-brush%)
    ;(send this brush-obj-fn default-brush-obj-fn)
    ;(send this unbrush-obj-fn default-unbrush-obj-fn)

    (define/public (relation) _relation)
    (define/public (active?)  _active?)

    (define/public (brush-object on? source-obj target-obj)
      ;(subclass may override)
      ; do something to target-obj via stored brush-unbrush fns
      (if on?
          ((brush-obj-fn)   source-obj target-obj)
          ((unbrush-obj-fn) source-obj target-obj)))

    (define/public (brush-targets on? source-obj . targets)
      ;receives a set of targets (out-relsets) and (un)brushes them
      ;  using source source-obj
      (define brush-obj-lambda
        (lambda (o) (send this brush-object on? source-obj o)))
      (foreach targets
               (lambda (target)
                 (foreach (send target members) brush-obj-lambda)))
      )

    ;(define (activate-member rel obj)
    ;activate member:
    ; bind Enter/Leave (tagged w. self) to:
    ; return my target objs,
    ; tell apt. re-subset to highlight targets

    ;deact member:


    ; This activates (or deactivates) brushing if not already on/off
    (define/public (activate-relset-brushing-for-obj turnon? obj relset)
      ;now turn on or off the particular relset
      ;(when (active?)
      (if turnon?
          (begin
            ;make sure obj has brushing-trigger bound to <Enter>,<Leave>
            (ensure-brush-trigger-bindings-for-obj obj)
            ;add to alist
            (pushq-onto-malist-val-always! 'brushed-relsets relset obj alist))
          ;else remove
          (remq-from-malist-val! 'brushed-relsets relset obj alist)
          )
      ;)
      )

    (define/public activate
      (case-lambda
        (() (activate #t))
        ((on?)
         (unless (eq? on? (active?))
           (set! _active? on?)
           (let ((domain-sets
                  (if _reverse? ;this is an antibrush?
                      (send (relation) range-sets)
                      (send (relation) domain-sets))))
             (foreach domain-sets
                      ;somewhere need to eliminate duplicates...
                      (lambda (relset)
                        (let ((members (send relset members)))
                          (foreach members
                                   (lambda (obj)
                                     (activate-relset-brushing-for-obj on? obj relset))))))
             ))
         )))

    (define/public (deactivate) (activate #f))

    (define/public (delete)
      (when (active?) (activate #f)))

    (define/public (writeargs) (list _active?))
    ;return list of customization args this should receive


    (when initially-active? (send this activate))
    ))

; ----- Helper Functions ----------

(define (install-private-hilight owner target hilight-obj)
  (remove-private-hilight owner target) ;clear any existing
  (remote-push! (list owner hilight-obj) target alist)
  hilight-obj)
;  (make-object hilight-class (send target dynapad) target owner))

(define (remove-private-hilight owner target)
  ;use owner as alist key
  (let ((found (get-and-rem-from-malist!
                assq remq owner target alist)))
    (when found
      (foreach (cdr found) (lambda (hl) (send hl delete))))))
;  (let ((hl (send target hilights 'find #f owner)))
;    (if hl (send hl delete))))

;------- Usable Brush Types --------

(define halo-brush%
  (class relation-brush%
    (init _relation)
    (init (_reverse? #f))
    (init (_active? #f))
    (init-field (_color "yellow"))

    (define/public color (get/set _color))

    (define/override (writeargs)
      (append (super writeargs)
              (list _color)))

    (super-instantiate (_relation _reverse? _active?))
    (send this class 'halo-brush%)

    (send this brush-obj-fn
          (lambda (s t)
            (let ((halo (make-object halo-hilight% (send t dynapad) t)))
              (install-private-hilight this t halo)
              (sch_pen (send halo get-cptr) _color)
              (send halo update))))
    (send this unbrush-obj-fn
          (lambda (s t) (remove-private-hilight this t)))

    ))

(define box-brush%
  (class relation-brush%
    (init _relation)
    (init (_reverse? #f))
    (init (_active? #f))
    (init-field (_color "yellow"))

    (define/public color (get/set _color))

    (define/override (writeargs)
      (append (super writeargs)
              (list _color)))

    (super-instantiate (_relation _reverse? _active?))
    (send this class 'box-brush%)

    (send this brush-obj-fn
          (lambda (s t)
            (let ((box (make-object concentric-hilight% (send t dynapad) t)))
              (install-private-hilight this t box)
              (sch_pen (send box get-cptr) _color)
              (send box update))))
    (send this unbrush-obj-fn
          (lambda (s t) (remove-private-hilight this t)))

    ))

(define edge-brush%
  (class relation-brush%
    (init _relation)
    (init (_reverse? #f))
    (init (_active? #f))
    (super-instantiate (_relation _reverse? _active?))
    (send this class 'edge-brush%)

    (send this brush-obj-fn
          (lambda (s t)
            (unless (eq? s t)
              (let ((edge (Make-Tie-Between graph-edge% s t)))
                (send edge pen "#444444")
                (install-private-hilight this t edge)))))
    (send this unbrush-obj-fn
          (lambda (s t)
            (unless (eq? s t)
              (remove-private-hilight this t))))
    ))

(define arc-brush%
  (class relation-brush%
    (init _relation)
    (init (_reverse? #f))
    (init (_active? #f))
    (super-instantiate (_relation _reverse? _active?))
    (send this class 'arc-brush%)

    (send this brush-obj-fn
          (lambda (s t)
            (unless (eq? s t)
              (let ((arc (make-object graph-arc% (send t dynapad))))
                (send arc pen "#444444")
                ;setting color before endpoints sets arrowhead color too
                (send arc fromto s t)
                (install-private-hilight this t arc)))))
    (send this unbrush-obj-fn
          (lambda (s t)
            (unless (eq? s t)
              (remove-private-hilight this t))))
    ))


(define antiarc-brush%
  (class relation-brush%
    (init _relation)
    (init (_reverse? #f))
    (init (_active? #f))
    (super-instantiate (_relation _reverse? _active?))
    (send this class 'antiarc-brush%)

    (send this brush-obj-fn
          (lambda (s t)
            (unless (eq? s t)
              (let ((arc (make-object graph-arc% (send t dynapad))))
                (send arc pen "#552222")
                (send arc fromto t s)
                (install-private-hilight this t arc)))))
    (send this unbrush-obj-fn
          (lambda (s t)
            (unless (eq? s t)
              (remove-private-hilight this t))))
    ))

(define magnifying-brush%
  (class relation-brush%
    (init _relation)
    (init (_reverse? #f))
    (init (_active? #f))
    (super-instantiate (_relation _reverse? _active?))
    (send this class 'magnifying-brush%)

    (send this brush-obj-fn
          (lambda (s t)
            (unless (eq? s t) (send t z 1.5))))
    (send this unbrush-obj-fn
          (lambda (s t)
            (unless (eq? s t) (send t z 1))))
    ))

; ========== GUI/Menus ===========
(define *menued-brush-styles* null)
(define (menuify-brush-style style name)
  (push! (list style name) *menued-brush-styles*))


;these appear in menu in reverse order:
(menuify-brush-style 'magnifying-brush% "Magnify")
(menuify-brush-style 'antiarc-brush%   "Anti-Arrow")
(menuify-brush-style 'arc-brush% "Arrow")
(menuify-brush-style 'edge-brush% "Edge")
(menuify-brush-style 'box-brush%  "Box")
(menuify-brush-style 'halo-brush% "Halo")

;----------- Menu-controlled brushing ---------
(define (make-halfmenu-BrushStyles sb relation curr-bstyle brush anti?)
  (let ((found-style-yet? #f)
        (active? (and curr-bstyle (send brush active?))))
    (add-checkable-menu-item sb (format "~a ~a" (if anti? "Antibrush" "Brush")
                                        (if active? "active" "inactive"))
                             (lambda (i)
                               (send brush activate (send i is-checked?)))
                             active?
                             curr-bstyle)
    (add-menu-separator sb)
    (add-checkable-menu-item sb "  (none)"
                             (lambda (i)
                               (if anti?
                                   (send relation antibrush-style #f)
                                   (send relation brush-style #f)))
                             (not curr-bstyle))
    (foreach *menued-brush-styles*
             (lambda (bs+name)
               (add-checkable-menu-item sb (format "  ~a" (cadr bs+name))
                                        (lambda (i)
                                          (send (if anti?
                                                    (send relation antibrush-style (eval (car bs+name))) ;eval 'class% --> class%
                                                    (send relation brush-style     (eval (car bs+name))))
                                                activate #t)) ;active?))
                                        (cset! found-style-yet? (equal? (car bs+name) curr-bstyle)))
               (when (and found-style-yet? (has-method? brush 'color))
                 (add-menu-item sb (format "    ~a color..." (cadr bs+name))
                                (lambda ()
                                  (let* ((curr-color (send brush color))
                                         (new-color  (ask-user-for-color curr-color)))
                                    (when new-color
                                      (send brush color new-color))))))))
    ))

(define (make-submenu-BrushStyles mb obj rel-arg name)
  (let* ((relation (if (procedure? rel-arg) (rel-arg) rel-arg))
         (sb (add-submenu mb name))
         (curr-style (send relation brush-style))
         (curr-class (eval (send relation class)))) ;eval turns 'class% --> class%
    (when (subclass? curr-class clique-relation%)
      (make-submenu-Cliques sb obj relation))
    (make-halfmenu-BrushStyles sb relation (send relation brush-style)
                               (send relation brush) #f)
    ;possible anti-brushes:
    (when (subclass? curr-class directed-relation%)
      (add-menu-separator sb)
      (make-halfmenu-BrushStyles sb relation (send relation antibrush-style)
                                 (send relation antibrush) #t))

    ))

(define (make-submenu-CliqueBrushStyles mb obj relation key)
  (let* ((cq (send relation find-clique key))
         (sb (add-submenu mb (ensure-string key)))
         (curr-style (send cq brush-style)))
    (make-halfmenu-BrushStyles sb cq (send cq brush-style)
                               (send cq brush) #f)))
(define (ensure-string<? a b)
  (string<? (ensure-string a) (ensure-string b)))

(define (make-submenu-Cliques mb obj relation)
  (let* ((sb (add-submenu mb "Cliques...")))
    (foreach (sort (send relation brushed-keys) ensure-string<?)
             (lambda (key) (make-submenu-CliqueBrushStyles sb obj relation key)))))

(define (make-submenu-Relations mb obj)
  (let* ((sb (add-submenu mb "Highlight Relations...")))
    (foreach *menued-relations*
             (lambda (r) (make-submenu-BrushStyles sb obj (car r) (cadr r))))))


(when *popup-menus-enabled?*
  (append-mainmenu-constructor
   (lambda (mb obj)
     (make-submenu-Relations mb obj)
     ;       (make-submenu-Highlighting mb obj)
     )))

; ====== Preset Special-relation brushes ======
;  these may be overridden by customized brushes saved with workspace
;(send *samedir-relation* brush-style halo-brush% #f "green")  ;initially inactive
;(send *sameobj-relation* brush-style halo-brush% #t "yellow") ;initially active
;Never mind; *same...* are both #f at this point

; ============== Changes to Member OBJECTS ================
; obj keeps alist entry for:
;  all relsets where it's a member:         ('relsets <rs> <rs> ...); see newrelation.ss
;  all relsets which are actively brushed:  ('brushed-relsets <rs> <rs> ...)
; The latter list changes when brushes are activated/deactivated
;
; Recycles one binding for each of <Enter>, <Leave>;
;  follow each brushed relset, get its targets, then send targets to its
;  relation's brush, which does the brushing

(define (brushing-trigger o e)   (brush-outflow-lambda o e #t))
(define (unbrushing-trigger o e) (brush-outflow-lambda o e #f))

(define (brush-outflow-lambda o e on?) ; o is dynapad;
  ; source object is (event-obj e)
  (let* ((obj (event-obj e))
         (found (assq 'brushed-relsets (send obj alist)))
         (relsets (and found (cdr found))))
    (when relsets
      (foreach relsets
               (lambda (relset)
                 (send relset brush-targets on? obj))))
    #t))

;(bind "<Leave>" (lambda (o e) (brush-outflow-lambda o e #f)))
;(bind "<Enter>" (lambda (o e) (brush-outflow-lambda o e #t)))
(define (ensure-brush-trigger-bindings-for-obj obj)
  (unless (memq brushing-trigger (bind obj "<Enter>"))
    (bind obj "<Enter>" brushing-trigger))
  (unless (memq unbrushing-trigger (bind obj "<Leave>"))
    (bind obj "<Leave>" unbrushing-trigger))
  )



;============= OBSOLETE? ================
(define (default-brush-obj-fn source target)
  (default-highlight-fn target))
(define (default-unbrush-obj-fn source target)
  (default-unhighlight-fn target))

; ---- these 3 recycled from brush.ss
(define (default-highlight-fn obj . highlight-class)
  ;return existing or new highlight
  (set! highlight-class (if (null? highlight-class)
                            default-brush-hilight%
                            (car highlight-class)))
  (let ((found (get-object-keyval obj 'highlight)))
    (if found
        found
        (let ((hl (make-object highlight-class (send obj dynapad) obj))
              (slct (send obj selected?)))
          ;(if slct (send hl lower slct))
          (set-object-keyval obj 'highlight hl)
          hl))))

(define (default-unhighlight-fn obj)
  (let ((hl (get-object-keyval obj 'highlight)))
    (when hl
      (send hl delete)
      (rem-object-keyval obj 'highlight))))

#|
(define recolorable-hilight%
  (class hilight%
    (init dynapad-arg object-arg)
    (init-field (_color "white"))
    (inherit-field _dynapad _object _cptr)
    (super-instantiate (dynapad-arg object-arg 'brush))
    (sch_pen _cptr _color)

    (define/public color
      (case-lambda
        (() _color)
        ((c) (set! _color c)
             (sch_pen _cptr c))))
    ))

;(define (install-private-hilight owner target hilight-class)
;  (let ((existing-hl (get-object-keyval target owner))
;    (new-hl (make-object hilight-class (send target dynapad) target)))
;    (if existing-hl (send existing-hl delete))
;    (set-object-keyval target owner new-hl)
;    new-hl))

;(define (remove-private-hilight owner target)
;  (let ((existing-hl (get-and-rem-from-malist! assq remq owner target alist)))
;    (if existing-hl (foreach (cdr existing-hl)
;                 (lambda (hl) (send hl delete))))))


(define recolorable-brush%
  (class relation-brush%
    (init _relation)
    (init (_reverse? #f))
    (init (_active? #f))

    (init-field (_color "yellow"))

    (super-instantiate (_relation _reverse? _active?))
    (send this class 'recolorable-brush%)

    (send this brush-obj-fn
          (lambda (s t)
            (send (default-highlight-fn t recolorable-hilight%)
                  color _color)))
    (send this unbrush-obj-fn
          (lambda (s t) (default-unhighlight-fn t)))

    (define/public color (get/set _color))

    (define/override (writeargs)
      (append (super writeargs)
              (list _color)))
    ))

(define (add-nultihighlight obj key make-fn)
  ; looks for a key within the 'multihighlight alist tuple of obj
  (let ((multihilights (assq 'multihighlights (send obj alist)))
        (found (and multihighlights
                    (assq key (cdr multihilights))))
        (new   (and (not found)
                    (make-fn obj))))
    (push-onto-malist-val! assq multihighlights
                           (list key new) obj alist)
    new))

;(define (remove-multihighlight obj key unmake-fn)
;  (let ((multihighlights (assq
|#
