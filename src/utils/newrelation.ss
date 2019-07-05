(dynaload "tools-lists.ss")

(define-macro (brush-style-template brush-field direction)
  `(case-lambda
    (()  (and ,brush-field (send ,brush-field class)))
    ((type . args)
       ;if changing brushes, make sure last is deleted
       ; so that it unbrushes targets first
     (unless (eq? type (and ,brush-field (send ,brush-field class)))
	     (if ,brush-field (send ,brush-field delete))
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
	(if (and mybrush (send mybrush active?))
	    (send mybrush activate-relset-brushing-for-obj #t obj this))))

    (define/public (remove-member obj)
      (set! _members (remq obj _members))
      ;remove this from obj's alist 'relsets entry
      (remq-from-malist-val! 'relsets this obj alist)
      ;if this has an active brush, dactivate it for obj
      (let ((mybrush (brush)))
	(if (and mybrush (send mybrush active?))
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
	(if br ;notify potential brushes
	    (send/apply br brush-targets on? source-obj (range-sets)))
	(if abr ;notify potential antibrushes
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
	    (if found
		(foreach (cdr found) (lambda (r) (send r remove-member o))))))))

; tell alist-filters how to deal:
; ('relsets rs1 rs2...) --> (#f (refer-when-ready 1 'add-member this)...)
; which, when loaded, will rebuild ('relsets....) and itself vanish
;(alist-filters 'add
;   (lambda (obj tuple)
;     (if (eq? (car tuple) 'relsets)
;	 (begin
;	   (foreach (cdr tuple) ;for each relset this is a member of
;		    (lambda (relset)
;		      (send obj onetime-writeoptions 'add
;			    `(defer-send 1 '(,(export-objs relset) add-member ,(export-objs obj))))))
;	   null)
;	 (list tuple))))

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
;	     (send (sameobj-relation) add-to-clique ;guarantees unique clique
;		   obj
;		   (send (sameobj-relation) new-clique (send obj id)))))
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
