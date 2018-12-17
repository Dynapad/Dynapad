; $Id: brush.ss,v 1.1 2006/04/02 05:51:38 dsbauer Exp $
; THIS SHOULD BE ALL OBSOLETE, replaced by newbrush.ss
; keep for back-compat
; 
; Brush sets contain objects (not brushes). Selecting an object in a brush set
; may cause other objects with the same handle to be highlighted, but only if
; the proper relation exists between the brush sets to which the objects
; respectively belong.
; 
; For example, suppose obj1 and obj2 share a handle. Then we
; 
;   (define bs1 (make-object brush-set%))
;   (define bs2 (make-object brush-set%))
;   (send bs1 members 'add obj1)
;   (send bs2 members 'add obj2)
; 
; which puts obj1 in bs1 and obj2 in bs2. Now we
; 
;   (send bs1 targets 'add bs2)
; 
; which tells bs1 to brush bs2, so selecting obj1 highlights obj2. ; The
; reverse is not true, however, unless we
; 
;   (send bs2 targets 'add bs1)
; 
; which makes bs2 brush bs1 as well. Another way to give obj1 and obj2 a
; mutual brushing relationship is to
; 
;   (define bs (make-object brush-set%))
;   (send bs1 members 'add obj1 obj2)
;   (send bs1 targets 'add bs1)
; 
; which puts both objects in a set that brushes itself. Note, however, that
; while obj1 and obj2 will highlight each other, selecting an object will not
; highlight itself.

(announce-module-loading "brushing feature...")
;(dynaload "hndl.ss") ; is it good style to do this here?
(dynaload "handle.ss")
(define (target-via-handle obj)
  (let ((hndl (get-hndl obj)))
    (if hndl (send hndl instances-except obj)
	null)))

(define default-brush-hilight%
  (class hilight%
    (init dynapad-arg object-arg)
    (inherit-field _dynapad _object _cptr)

    (super-instantiate (dynapad-arg object-arg 'brush)) ;'brush label is optional
    (sch_pen _cptr "yellow")
))

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


(define brush-set%
  (class object%
    (super-instantiate ())
    (init-field (_target-objs-fn target-via-handle))
    ; target-objs-fn is (lambda (obj) ...) which specifies what obj brushes
    (field (target-list ()) ; alist of (target highlight-fn unhilight-fn) tuples
           (member-list ())
	   (_enabled? #t))
    (public targets brushes?
            members selected-members)

    (define/public target-objs-fn (get/set _target-objs-fn))

    (define targets
      (case-lambda
        (() target-list)
        ((new-targets)
         (for-each remove-target member-list)
         (for-each add-target new-targets))
        ((cmd . args)
         (case cmd
           ('add (for-each add-target args))
           ('remove (for-each remove-target args))))))

    (define (add-target targ . fns)
      (when (list? targ)
	    (set! fns (cdr targ))
	    (set! targ (car targ)))
      (if (null? fns)
	  (set! fns (list default-highlight-fn default-unhighlight-fn)))
      (replace-else-push-onto-alist! assq targ fns target-list))
;      (when (not (assq obj target-list))
;	    (push! 
;	     (cons obj hilight-fn) target-list)))

    (define (remove-target obj)
      (get-and-rem-from-alist! assq remq obj target-list))

    (define (brushes? some-brush-set)
; returns #f or list of (hilight-fn unhilight-fn) which specify how to (un)hilight obj
; list may have length<2-- then use default fns
      (let ((tuple (assq some-brush-set target-list)))
	(and tuple (cdr tuple))))

    (define members
      (case-lambda
        (() member-list)
        ((new-members)
         (for-each remove-member member-list)
         (for-each add-member new-members))
        ((cmd . args)
         (case cmd
           ('add (for-each add-member args))
           ('remove (for-each remove-member args))))))

    (define (selected-members)
      (filter (lambda (x) (send x selected?)) member-list))

    (define (add-member obj)
;(say "brush " this "adding obj " obj)
      (when (not (member obj member-list))
	    (if _enabled?
		(activate-member obj))
	(send obj delete-callbacks 'add
	      (lambda (o)
		(send this members 'remove o)) this)
;	(send obj post-build-ops 'add
;	      (lambda (o)
;		`(send obj refer-when-ready ,(send this id) 'members ''add obj)))
        (set! member-list (cons obj member-list))))

    (define (remove-member obj)
;(say "brush " this "removing obj " obj)
      (if _enabled? (deactivate-member obj))
;      (send obj delete-callbacks 'remove #f this)
;      (send obj post-build-ops 'remove #f this)
      (set! member-list (remq obj member-list)))

    (define (notify-brushes-of-obj-state brushes obj on?)
;      (say brushes)
      (foreach brushes (lambda (b) (send b brush-obj-targets obj on?))))
    
    (define (bind-notify-brushes obj brushes)
      (if (null? brushes)
	  (begin
	    (let* ((tuple (get-and-rem-from-alist! assq remq 'brush-lambdas obj alist))
		   (brush-lmb (cadr tuple))
		   (unbrush-lmb (caddr tuple)))
	      (bind obj "<Enter>" (remq brush-lmb (bind obj "<Enter>")))
	      (bind obj "<Leave>" (remq unbrush-lmb (bind obj "<Leave>")))
	    ; DiamondTouch-specific:
	      (bind obj "dt-down" (remq brush-lmb (bind obj "dt-down")))
	      (bind obj "dt-up"   (remq unbrush-lmb (bind obj "dt-up")))
	    ))
	  (begin
	    (let ((brush-lambda (lambda (o e) (notify-brushes-of-obj-state brushes obj #t)))
		  (unbrush-lambda (lambda (o e) (notify-brushes-of-obj-state brushes obj #f))))
	      (get-else-push-onto-alist!
	         assq (list 'brush-lambdas brush-lambda unbrush-lambda) obj alist)
	      (bind obj "<Enter>" (cons brush-lambda (bind obj "<Enter>")))
	      (bind obj "<Leave>" (cons unbrush-lambda (bind obj "<Leave>")))
	      (bind obj "dt-down" (cons brush-lambda (bind obj "dt-down")))
	      (bind obj "dt-up"   (cons unbrush-lambda (bind obj "dt-up")))
	      ))))

    (define (activate-member obj)
     ; (when (get-hndl obj) ; objects only have handles after one tries to get them
;      (send obj select-callbacks 'add brush-when-selected)
	    (if (has-method? obj 'divisible)
		(send obj divisible #f)) ;may be trouble...
	    (let* ((entry (pushq-onto-alist-val-always! 'brush-set this obj alist))
		   (brushes (cdr entry)))
	      (bind-notify-brushes obj brushes))
;(say (send obj alist))
;	    (let ((old-brush-set (get-object-keyval obj 'brush-set)))
;	      (when old-brush-set (send old-brush-set members 'remove obj)))
;	    (set-object-keyval obj 'brush-set this))
	    )

    (define (deactivate-member obj)
;      (send obj select-callbacks 'remove brush-when-selected)
      (let ((remaining-entry (remq-clean-from-alist-val! 'brush-set this obj alist)))
	(bind-notify-brushes obj (cdr remaining-entry))))


    (define/public enable
      (case-lambda
       (() (enable #t))
       ((bool)
	(if bool
	    (unless _enabled? ;enable if not already
		    (set! _enabled? #t)
		    (foreach (members) (lambda (m) (activate-member m))))
	    (when _enabled?   ;disable if not already
		  (set! _enabled? #f)
		  (foreach (members) (lambda (m) (deactivate-member m))))))))
    (define/public (disable) (enable #f))

    (define (should-highlight? agent patient)
      ;returns #f or list of (hilight-fn unhilight-fn)
      (and ;already excluded: (not (eq? agent patient)) ; objects don't highlight themselves
           ;assumed: (not (send patient deleted?)) ; don't highlight deleted objects
           (let (;(agent-brush-set (get-object-keyval agent 'brush-set))
                 (patient-brush-sets (get-object-keyvals patient 'brush-set)))
             (and ;agent-brush-set
                  patient-brush-sets
		  (ormap (lambda (bs) (send this brushes? bs))
			 patient-brush-sets)))))
    
;    (define/public brush-when-selected
;      (lambda (obj on)
;        (for-each (lambda (x)
;		    (let ((highlight? (should-highlight? obj x)))
;		      (when highlight?
;			    (if on
;				(apply highlight x highlight?)
;				(apply unhighlight x highlight?)))))
;		  (_target-objs-fn obj))))

    (define/public (brush-obj-targets obj on?)
      (let ((targets (_target-objs-fn obj)))
        (for-each (lambda (x)
		    (let ((highlight-fns (should-highlight? obj x)))
		      (when highlight-fns
			    (if on?
				;at this point, assumes (len highlight-fns)>1
				((car highlight-fns) x)
			        ((cadr highlight-fns) x)))))
		  targets)))
  )
)


;(define (objects-brushed-by-obj-via-set obj brush-set)
;#f) ;FINISH...  

(define exhaustive-brush-set%
  ; all members brush all members of targets
  (class brush-set%
    (super-instantiate (void))
    (send this target-objs-fn
	  (lambda (obj) ;for any obj, return union of all target-set members
	                ; duplicates are possible if objs belong to >1 target-set
	    (apply append (map (lambda (trgt) (send trgt members))
			       (map car (send this targets))))))

    (define/override (brush-obj-targets obj on?)
      (foreach (send this targets)
	       (lambda (tuple)
		 (let* ((brushset (car tuple))
			(hilight-fns (cdr tuple))
			(do-fn (if on? (car hilight-fns) (cadr hilight-fns))))
		   (foreach (send brushset members) do-fn)))))
))

#|
(define (make-highlight-frame bbox)
  (let ((r1 (make-object rect% dynapad bbox))
        (r2 (make-object rect% dynapad bbox))
        (r3 (make-object rect% dynapad bbox)))
    (send* r1 (penwidth 20) (pen "yellow") (fill "none")
           (minsize '(20 #f)) (maxsize '(140 #f)))
    (send* r2 (penwidth 10) (pen "yellow") (fill "none")
           (minsize '(100 #f)))
    (send* r3 (penwidth 0) (pen "none") (fill "yellow")
           (maxsize '(25 #f)))
    (make-object group% dynapad (list r1 r2 r3))))

(define (highlight obj)
  (when (not (get-object-keyval obj 'highlight))
    (let* ((highlight-frame (make-highlight-frame (send obj bbox))))
      (send highlight-frame add obj)
      (send obj lower) ; otherwise it's in front of the rectangles
      (set-object-keyval obj 'highlight highlight-frame))))

(define (unhighlight obj)
  (let ((g (get-object-keyval obj 'highlight)))
    (when g
      (for-each (lambda (x) (when (not (eq? obj x)) (send x delete)))
               (send g members))
      (send g ungroup)
      (rem-object-keyval obj 'highlight))))
|#

;potentially overrides hook defn:
(define *brush-sets* null) ;alist by name (symbol)
(define (register-object-with-brush-set obj setname)
  (let ((found (assq setname *brush-sets*)))
    (if (not found)
	(begin
	  (set! found (list setname
			    (with newset (make-object brush-set%)
				(send newset targets (list newset)))))
	  (push! found *brush-sets*)))
    (send (cadr found) members 'add obj)))

(define *highlight-duplicates?* #t)
(define highlight-duplicates
  (case-lambda
   (() *highlight-duplicates?*)
   ((bool) 
      (set! *highlight-duplicates?* bool)
      (foreach (map cadr *brush-sets*)
	       (lambda (bs) (send bs enable bool))))
))
  

#|
(define (register-object-with-exhaustive-brush-set obj setname)
  (let ((found (assoc setname *brush-sets*)))
    (if (not found)
	(begin
	  (set! found (list setname
			    (with newset (make-object exhaustive-brush-set%)
				  (send newset targets (list newset)))))
	  (push! found *brush-sets*)))
    (send (cadr found) members 'add obj)))
|#

(update-progress 1)
; 0xDEADC0DE
;
;(define get-handles-after-loading-image-directory
;  (lambda (dir imagelist)
;    (for-each (lambda (obj) (get-hndl obj)) imagelist)))
;
;(push-image-dir-callback get-handles-after-loading-image-directory)
;
;(define (add-highlight-callback obj)
;  (send obj select-callbacks 'add brush-when-selected))
;
;(define add-highlight-callbacks-after-loading-image-directory
;  (lambda (dir imagelist)
;    (for-each (lambda (obj) (add-highlight-callback obj)) imagelist)))
;
;(push-image-dir-callback add-highlight-callbacks-after-loading-image-directory)
