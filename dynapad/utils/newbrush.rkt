#lang racket

(provide halo-brush%
         samedir-relation
         track-duplicates-of-objs
         )

(require racket/class
         ;dynapad/ffs
         (only-in dynapad/base alist-filters)
         dynapad/misc/misc
         dynapad/misc/alist
         (except-in dynapad/misc/tools-lists mmap) ; conflict with mlist mmap
         dynapad/menu/menu-state
         dynapad/pad-state
         dynapad/bind
         )

(require racket/class
         compatibility/mlist
         compatibility/defmacro
         dynapad/ffs
         dynapad/undo-state
         dynapad/misc/misc
         dynapad/misc/alist
         dynapad/utils/hilights
         dynapad/utils/graph
         dynapad/utils/edges
         (except-in dynapad/misc/tools-lists mmap)
         (only-in dynapad/libdynapad-wrapper
                  sch_pen)

         (only-in dynapad/menu/menu_popup
                  push-popup-menus-requested!)

         (only-in dynapad/menu/wxmenu
                  add-checkable-menu-item
                  add-menu-separator
                  add-menu-item
                  add-submenu
                  )
         (only-in dynapad/menu/menu_functions
                  ask-user-for-color)
         (for-syntax racket/base)
         )

;; originally from newrelation
(define-macro (brush-style-template brush-field direction)
  `(case-lambda
     (()  (and ,brush-field (send ,brush-field class)))
     ((type . args)
      ;if changing brushes, make sure last is deleted
      ; so that it unbrushes targets first
      (unless (eq? type (and ,brush-field (send ,brush-field class)))
        (when ,brush-field (send ,brush-field delete))
        (set! ,brush-field
              (and type (apply make-object type this ,direction args))))
      ,brush-field) ;returns brush; caller must activate it
     ))

(define relation%
  (class linkable-obj%
    (super-instantiate ())
    (field (_brush #f))   ;may have a brush
    (field (_sets null))  ;list of relation subsets
    (public-field class _class)
    (send this class 'relation%)

    (say "new relation..." this)
    (abstract-objects-callbacks 'add
                                (lambda () (list this)) this) ;includes this in things to be saved

    (define/override (delete)
      (abstract-objects-callbacks 'remove #f this) ;don't save this anymore
      (send this brush-style #f)                   ;delete brush
      (foreach (sets) (lambda (set) (send set delete))) ;delete all subsets
      (super delete)
      )

    (define/public (delete-all) (delete)) ;mildest interpretation;
    ;could also mean delete all objects in domain/range?

    (define/public sets (get/set _sets)) ;NOT FOR PUBLIC USE

    (define/public (members) ;union of domain, range,
      ;ie all objects involved in relation
      (apply append
             (map (lambda (set) (send set members)) _sets)))

    (define/public (domain)
      (apply append
             (map (lambda (set) (send set members))
                  (domain-sets))))
    (define/public (domain-sets)
      (filter (lambda (set) (send set domain?)) _sets))
    ;(define/public (out-sets) (domain-sets))

    (define/public (range)
      (apply append
             (map (lambda (set) (send set members))
                  (range-sets))))
    (define/public (range-sets)
      (filter (lambda (set) (send set range?)) _sets))
    ;(define/public (in-sets) (range-sets))

    (define/public (relate from-objs to-objs)
      ; for now, take stupid approach: do no checking,recycling
      (unless (list? from-objs) (set! from-objs (list from-objs)))
      (unless (list? to-objs) (set! to-objs (list to-objs)))
      (let* ((type relation-subset%); (send this set-type))
             (from-set (make-object type this))
             (to-set (make-object type this)))
        (push! from-set _sets)
        (push! to-set _sets)
        (send from-set members from-objs)
        (send to-set members to-objs)
        (send from-set add-target to-set)
        (list from-set to-set) ;return sets
        ))

    (define (sets-with-obj obj)
      (let* ((found (assq 'relsets (send obj alist)))
             (relsets (and found (cdr found)))
             (my-relsets
              (filter (lambda (set) (eq? (send set relation) this))
                      relsets)))
        my-relsets))

    (define/public (outflow obj)
      (apply append (map (lambda (relset) (send relset out-neighbors))
                         (sets-with-obj obj))))
    (define/public (inflow obj)
      (apply append (map (lambda (relset) (send relset in-neighbors))
                         (sets-with-obj obj))))

    (define/public (brush) _brush)
    (define/public brush-style (brush-style-template _brush #f))

    (define/public (write)
      `(ic (make-object ,(send this class))
           ,@(filter (lambda (opt) opt) (writeoptions))))

    (define/public (write-all)
      (cons (write)
            (map (lambda (s) (send s write)) (sets))))

    (define/public (writeoptions)
      (let ((bstyle (send this brush-style)))
        `((id ,(send this id))
          (defer-send '(sets ,(export-objs (send this sets))))
          ,(if bstyle
               `(brush-style ,bstyle ,@(send (brush) writeargs))
               #f)
          )))

    ))

(define directed-relation%
  (class relation%
    (super-instantiate ())
    (field (_antibrush #f)) ;may have an additional brush for reverse dir
    (send this class 'directed-relation%)

    ;    (define (relate-out from to...) )
    ;    (define (relate-in to from...) )
    ;    (define (unrelate-out to from...) )
    ;    (define (unrelate-in from to...) )

    (define/public (antibrush) _antibrush)
    (define/public antibrush-style (brush-style-template _antibrush 'reverse))

    (define/override (writeoptions)
      `(,@(super writeoptions)
        ,(let ((abstyle (antibrush-style)))
           (if abstyle
               `(antibrush-style ,abstyle ,@(send (antibrush) writeargs))
               #f))
        ))
    ))
(define asymmetric-relation% directed-relation%)

(define undirected-relation%
  (class relation%
    (super-instantiate ())
    (send this class 'undirected-relation%)

    (rename-super [super-relate relate])
    (define/override (relate from-objs to-objs)
      (let ((newsets (super-relate from-objs to-objs)))
        (send (car newsets) add-source (cadr newsets)) ;make relation symmetric
        newsets))

    (define/public (antibrush) #f) ;never has antibrush, would be redundant
    ;(define (unrelate) )
    (define/public (neighbors obj) ;subsumes outflow, inflow
      (send this outflow obj))
    ))
(define symmetric-relation% undirected-relation%)

(define clique-relation%
  ; non-exclusive "tags"/categories
  ; EXAMPLE: same-X relation: each x is a clique,
  ;   items with more than one x are part of multiple cliques
  ;
  ; an obj in a clique may be in other cliques,
  ; but for all related pairs, both must be in the same cliques
  (class undirected-relation%
    (super-instantiate ())
    (send this class 'clique-relation%)

    (define/override (relate . args) #f)
    (define/public (new-clique key)
      (let ((new (make-object clique% this key)))
        (send this sets (cons new (send this sets)))
        new))

    (define/public find-clique
      (case-lambda
        ((key) (find-clique key equal?))
        ((key eq-fn)
         (let ((found (first-valid (send this sets)
                                   (lambda (set) (eq-fn (send set key) key)))))
           (and found (caar found))))))

    (define/public (add-to-clique objs clique)
      (unless (list? objs)
        (set! objs (list objs)))
      (foreach objs (lambda (o) (send clique add-member o))))

    (define/public (remove-from-clique objs clique)
      (unless (list? objs)
        (set! objs (list objs)))
      (foreach objs (lambda (o) (send clique remove-member o))))

    (define/public add-objs-with-key
      ; objs args may be single obj or list
      (case-lambda
        ((objs key) (add-objs-with-key objs key equal?))
        ((objs key eq-fn)
         (let ((use-clique (or (find-clique key eq-fn)
                               (new-clique key))))
           (add-to-clique objs use-clique)
           use-clique))))

    (define/public (cliques) ;perhaps redundant, but seems nicer
      (send this sets))

    (define/public (keys)
      (map (lambda (clq) (send clq key))
           (send this sets)))

    (define/public (brushed-keys) ;may be overridden
      (send this keys))
    ))

(define plural-clique-relation%
  (class clique-relation%
    (super-instantiate ())
    (send this class 'plural-clique-relation%)

    (define/override (new-clique key)
      (let ((new (make-object plural-clique% this key)))
        (send this sets (cons new (send this sets)))
        new))

    (define/override (brushed-keys) ;returns only keys of sets w. >1 member
      (map (lambda (clq) (send clq key))
           (filter (lambda (set) (let ((m (send set members)))
                                   (and (not (null? m)) (not (null? (cdr m))))))
                   (send this sets))))
    ))

;(define exclusive-clique-relation%
; any member belongs to exactly one clique and has no other relates to nothing
;  outside that clique
; Therefore each relset (clique) has only one target, itself

;=======================
(define relation-subset% ;relation-clique% ;relation-subdomain%
  ; Each relation-subset is dedicated to a relation R.
  ; if a subset targets itself wrt. relation R, it is a clique in R:
  ;   internally exhaustive/interconnected;
  ; all items are equivalent wrt. facet R
  ; Adding an item to clique means tagging/categorizing it as that R val
  (class linkable-obj%
    (super-instantiate ())

    (init-field (_relation #f)) ;must be supplied later if #f

    ;store: out/target-sets, in/source-sets
    (public-field targets _targets) ;out-sets (range) of this set as domain
    (public-field sources _sources) ;in-sets (domain) of this set as range
    (field (_members null)) ;objs in this set
    (field (_private-brush #f))
    (field (_private-antibrush #f)) ;currently no way to make one
    (public-field class _class)
    (send this class 'relation-subset%)
    (send this targets null)
    (send this sources null)

    (define/override (delete)
      (send this brush-style #f)
      (send this members null)
      (foreach _targets (lambda (t) (remove-target t)))
      (foreach _sources (lambda (s) (remove-source s)))
      (super delete))

    (define/public relation (get/set _relation))

    ;new member must have relations homomorphic to existing members;
    ;initially all members put this into their 'relations alist
    ;add/remove member must notify brusher of proper relation

    (define/public (add-member obj)
      (push! obj _members)
      (ensure-relset-delete-callbacks-for-objs obj)
      ;add this to obj's alist 'relsets entry
      (pushq-onto-malist-val-always! 'relsets this obj alist)
      ;if this has an active brush, activate it for obj
      (let ((mybrush (brush)))
        (when (and mybrush (send mybrush active?))
          (send mybrush activate-relset-brushing-for-obj #t obj this))))

    (define/public (remove-member obj)
      (set! _members (remq obj _members))
      ;remove this from obj's alist 'relsets entry
      (remq-from-malist-val! 'relsets this obj alist)
      ;if this has an active brush, dactivate it for obj
      (let ((mybrush (brush)))
        (when (and mybrush (send mybrush active?))
          (send mybrush activate-relset-brushing-for-obj #f obj this))))

    ; WARNING: setting, adding, or removing members may have
    ; unexpected consequences for the relation; it's up to the caller
    ; to verify whether the add or remove is enough, or the subset must
    ; be subdivided
    (define/public members
      (case-lambda
        (() _members)
        ((lst . more)
         (unless (list? lst)
           (set! lst (cons lst more)))
         (let* ((diff-int-diff (list-overlapq lst _members))
                (adds (car diff-int-diff))
                (rems (caddr diff-int-diff)))
           (foreach adds (lambda (m) (send this add-member m)))
           (foreach rems (lambda (m) (send this remove-member m)))
           ))))

    (define/public (domain-sets) (list this))

    (define/public (range-sets) _targets)  ;i.e. targets

    (define/public (antirange-sets)  _sources)  ;sets that target this

    (define/public (domain?)
      (not (null? _targets)))
    (define/public (range?)
      (not (null? _sources)))

    (define/public (add-target target)
      (unless (memq target _targets) ;halts mutual recursion
        (push! target _targets)
        (send target add-source this)))
    (define/public (remove-target target)
      (when (memq target _targets) ;halts mutual recursion
        (set! _targets (remq target _targets))
        (send target remove-source this)))


    (define/public (add-source source)
      (unless (memq source _sources) ;halts mutual recursion
        (push! source _sources)
        (send source add-target this)))
    (define/public (remove-source source)
      (when (memq source _sources) ;halts mutual recursion
        (set! _sources (remq source _sources))
        (send source remove-target this)))


    (define/public (out-neighbors)
      (apply append (map (lambda (tgt) (send tgt members)) _targets)))
    (define/public (in-neighbors)
      (apply append (map (lambda (src) (send src members)) _sources)))
    ;  for each target/source, collects members

    (define/public (brush-targets on? source-obj)
      ;brushes all members of all targets, via relation's brush
      ;source is sometimes needed by the brush
      (let ((br (brush))
            (abr (antibrush)))
        (when br ;notify potential brushes
          (send/apply br brush-targets on? source-obj (range-sets)))
        (when abr ;notify potential antibrushes
          (send/apply abr brush-targets on? source-obj (antirange-sets)))
        ))

    (define/public (brush)
      (or _private-brush (send (relation) brush)))
    (define/public (antibrush)
      (or _private-antibrush (send (relation) antibrush)))
    (define/public brush-style (brush-style-template _private-brush #f))

    (define/public (write)
      `(ic (make-object ,(send this class))
           ,@(filter (lambda (opt) opt) (writeoptions))))

    (define/public (write-all) (list (write)))

    (define/public (writeoptions)
      (let ((bstyle (send this brush-style))
            (mems   (send this members))
            (srcs   (send this sources))
            (tgts   (send this targets)))
        `((id ,(send this id))
          (defer-send '(relation ,(export-objs (relation))))
          ,(if bstyle
               `(brush-style ,bstyle ,@(send (brush) writeargs))
               #f)
          ,(if (null? mems) #f
               `(defer-send 2 '(members ,(export-objs mems))))
          ,(if (null? tgts) #f
               `(defer-send '(targets ,(export-objs tgts))))
          ,(if (null? srcs) #f
               `(defer-send '(sources ,(export-objs srcs))))
          )))
    ))

(define clique%
  (class relation-subset%
    ; a subset which always targets itself, representing an
    ; equivalence class whose members share a value
    (init (_relation #f)) ;must be filled in later if #f
    (init-field (_key #f))
    (super-instantiate (_relation))
    (send this class 'clique%)

    (send this add-target this) ;link to itself

    (define/public key (get/set _key))

    (define/override (writeoptions)
      `(,@(super writeoptions)
        (key ,(key))))
    ))

(define plural-clique%
  ;only brushes when it has more than one member
  (class clique%
    (init (_relation #f))
    (init (_key #f))
    (super-instantiate (_relation _key))
    (send this class 'plural-clique%)

    (define/override (brush-targets on? source)
      (let ((m (send this members)))
        (unless (or (null? m) (null? (cdr m)))
          (super brush-targets on? source))))
    ))

;=== API for save/log files ====
;(define (enclique clique-id member-id)
;  (defer-send `(,clique-id add-member ,member-id))
;)

; example: (in-rset "#123" "#234")
;  puts obj w. id 123 in set (e.g. clique) w. id 234
(define (in-rset member-id rset-id)
  (defer-send `(,rset-id add-member ,member-id))
  )

; Tweak the object itself:

(define (ensure-relset-delete-callbacks-for-objs obj)
  (send obj delete-callbacks 'add
        (lambda (o)
          (let ((found (assq 'relsets (send o alist))))
            (when found
              (foreach (cdr found) (lambda (r) (send r remove-member o))))))))

; tell alist-filters how to deal:
; ('relsets rs1 rs2...) --> (#f (refer-when-ready 1 'add-member this)...)
; which, when loaded, will rebuild ('relsets....) and itself vanish
;(alist-filters 'add
;   (lambda (obj tuple)
;     (if (eq? (car tuple) 'relsets)
;     (begin
;       (foreach (cdr tuple) ;for each relset this is a member of
;            (lambda (relset)
;              (send obj onetime-writeoptions 'add
;                `(defer-send 1 '(,(export-objs relset) add-member ,(export-objs obj))))))
;       null)
;     (list tuple))))

(alist-filters 'add
               (lambda (obj tuple)
                 (if (eq? (car tuple) 'relsets)
                     (list
                      (cons #f
                            (map
                             (lambda (relset)
                               ; use in-rset wrapper to 1) simplify save files; 2) add hook for possible changes
                               `(in-rset ,(export-objs obj) ,(export-objs relset))
                               ;`(defer-send 1 '(,(export-objs relset) add-member ,(export-objs obj)))
                               )
                             (cdr tuple))))
                     (list tuple))))

; ========= Special Relations =========
; ------ Special clique-relation subclass for directory-memory ------
;  There can be only one...

(define *samedir-relation* #f) ;will be reset below
;serves as a handle to changing relation object

;don't use samedir-relation% ; it's old code but still needed for
;  back-compat of certain logs
(define samedir-clique-relation%
  ; There should be only one of these;
  ;  if a saved workspace is loaded with another,
  ;  it redirects *samedir-relation* handle to itself
  (class clique-relation%
    (super-instantiate ())
    (send this class 'samedir-clique-relation%)
    (when *samedir-relation*
      (send *samedir-relation* delete)) ;delete existing
    ; this is temporary; should eventually merge new with old
    (set! *samedir-relation* this)  ;redirect handle

    (define/override (delete)
      (super delete)
      (set! *samedir-relation* #f))

    (send this brush-style halo-brush% #f "green")
    ))

; Create only on demand:
(define (samedir-relation)
  (or *samedir-relation* (make-object samedir-clique-relation%)))

; ------- Object Duplicate Tracking ----------
; (these are essentially handles; eventually move to handles.ss?

(define *sameobj-relation* #f)
; as above, this is a handle to changing relation object

(define sameobj-relation%
  (class plural-clique-relation%
    (super-instantiate ())
    (send this class 'sameobj-relation%)
    (when *sameobj-relation*
      (send *sameobj-relation* delete))
    (set! *sameobj-relation* this)

    (define/override (delete)
      (super delete)
      (set! *sameobj-relation* #f))

    ;set initial default brush?
    (send this brush-style halo-brush% #t "yellow")
    ))

; Create only on demand:
(define (sameobj-relation)
  (or *sameobj-relation* (make-object sameobj-relation%)))

;call this to register objs for duplicate tracking
(define (track-duplicates-of-objs objs)
  (unless (list? objs)
    (set! objs (list objs)))
  (foreach objs
           (lambda (obj)
             (send (sameobj-relation) add-objs-with-key obj (send obj id))))
  ;This should be equivalent:
  ;         (send (sameobj-relation) add-to-clique ;guarantees unique clique
  ;           obj
  ;           (send (sameobj-relation) new-clique (send obj id)))))
  ;use id of original as key; someday use shared URL instead?
  )


; This puts imported objs into both relations above.
;  Replaces add-to-wrapper hook in import-dirs.ss
(define (add-to-wrapper parent obj)
  (send (samedir-relation) add-objs-with-key (list obj) parent)

  (track-duplicates-of-objs obj)
  ;this may not be the only place where this should be called
  )

; ========= GUI/Menu Stuff ============
(define *menued-relations* null)
(define (menuify-relation rel name)
  (push! (list rel name) *menued-relations*))

(menuify-relation samedir-relation "Shared directory...")
;NEEDS TO BE LAMBDA, since it changes
(menuify-relation sameobj-relation "Duplicates...")
;NEEDS TO BE LAMBDA, since it changes

;; originally from newbrush
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


(define (request-popup-menu-for-newbrush)
  (append-mainmenu-constructor
   (lambda (mb obj)
     (make-submenu-Relations mb obj)
     ;       (make-submenu-Highlighting mb obj)
     )))

(push-popup-menus-requested! request-popup-menu-for-newbrush)

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

; ---- these 4 recycled from brush.ss

(define default-brush-hilight%
  (class hilight%
    (init dynapad-arg object-arg)
    (inherit-field _dynapad _object _cptr)

    (super-instantiate (dynapad-arg object-arg 'brush)) ;'brush label is optional
    (sch_pen _cptr "yellow")
    ))

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
