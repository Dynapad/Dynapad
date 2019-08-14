(require (lib "class.ss"))
(require (lib "defmacro.ss"))
(require (lib "math.ss"))

(define *all-regions* null)
(define *greedy-regions* null)

(define *default-region-form-type* resizable-frame-container%)

;Create tool layers:
(send dynapad setvar 'drag-layer (make-object layer% dynapad "drag"))
(send dynapad setvar 'lens-layer (ic
				  (make-object layer% dynapad "lens")
				  (raise)))
(let ((select-layer (send dynapad getvar 'select-layer)))
  (when select-layer
      (send select-layer raise)))
(define *brush-layer* (send dynapad getvar 'lens-layer)) ;temp

(define region-actor-name 'rgnactor)
;(define (is-a-region? obj) (assq region-actor-name (send obj alist)))
;(define (get-region-actor obj) (cond ((is-a-region? obj) => cadr) (else #f)))

(define-syntax send-rgn (syntax-rules ()
  ((_ obj msg ...) (send-actor-named obj region-actor-name msg ...))))
(define (get-rgn obj)
  (get-actor-named obj region-actor-name))


(define effect-set%
; a non-spatial region, which maintains an effect on a set of contents
  (class named-actor%
    (super-instantiate ())
    (field (_contents null))
    (field (_dynaclass 'effect-set%))

    (field (_leave-actions '())   ;action when obj leaves for any reason
	   (_abandon-actions '()) ;action when obj left behind (regn moves away)
	   (_remove-actions '())  ;action when obj removed (obj moves away)
	   (_enter-actions '())   ;action when obj enters for any reason
	   (_absorb-actions '())  ;action when rgn moves to obj
	   (_receive-actions '()) ;action when obj moves to rgn
	   (_update-actions '())  ;action when obj moved (relatively) within regn
	   (_pickup-actions '())  ;action when obj picked up at start of drag
	   (_finish-actions '()))
    (field (_filter-fns '()))

    (field (_loyal #f)) ;if #t does not abandon contents
    (define/public loyal (get/set _loyal))

    (define/public dynaclass (get/set _dynaclass))

    (define/public (apply-filters o)
      (and (send o findable) ;could be too bold but: ACCEPT ONLY FINDABLES
	   (andmap (lambda (fn-pair) ((car fn-pair) o))
;      (andmap (lambda (fn-pair) (let ((result ((car fn-pair) o)))
;				  (if (not result) (say "filter failed: " o fn-pair))
;				  result))
		   _filter-fns)))

    (define/public contents
      (case-lambda
       (() _contents)
       ((objs)
	(let* ((old-contents _contents)
	       (new-contents (filter (lambda (o) (apply-filters o)) objs))
	       (ins (list-diff new-contents old-contents memq))
	       (stays+outs (list-intersect+diff old-contents new-contents memq))
	       (stays (car stays+outs))
	       (outs (cadr stays+outs)))
	  (set! _contents new-contents)
	  (if _loyal
	      (set! _contents (append outs _contents))
	      (for-each (lambda (obj) (abandon-obj obj)) outs))
	  (for-each (lambda (obj) (absorb-obj obj)) ins)
	  (for-each (lambda (obj) (update-obj obj))  stays)
	  ;(send this finish)
	  ))))

    (define/public (final-contents objs)
      (contents objs)
      (finish))
    
    (define/public (visible-contents) _contents)  ;overridden by lenses

    ; Undoable effects: ------------------------------
    (field (_restore-always-alist null)  ;restorations when any obj exits
	   (_restore-abandoned-alist null)) ;restorations when unretained objs exit

	;upon entry of obj, call sequence is:
	;  include-obj -> _enter-action -> undo-on-exit -> do-fn
    (define/public (undo-on-exit obj do-fn undo-fn)
      (pushq-onto-malist-val-always! obj undo-fn _restore-always-alist)
      (do-fn obj))
    (define/public (undo-on-abandon obj do-fn undo-fn)
      (pushq-onto-malist-val-always! obj undo-fn _restore-abandoned-alist)
      (do-fn obj))
    
    (define (restore-any-obj obj)
      (let ((undo-fns (get-and-rem-from-malist! assq remv obj _restore-always-alist)))
	(when undo-fns
	    (for-each (lambda (fn) (fn obj)) (cdr undo-fns)))))
    (define (restore-abandoned-obj obj)
      (let ((undo-fns (get-and-rem-from-malist! assq remv obj _restore-abandoned-alist)))
	(when undo-fns
	    (for-each (lambda (fn) (fn obj)) (cdr undo-fns)))))

    ; Adding/removing: ----------------------------
    (define/public (claim obj)
      ; obj is not already in rgn; add it and mark it
      (pushq-onto-malist-val-always! 'in-region this obj alist)
      (pushq-onto-malist-val-always! this obj *claims-this-round*))

    (define (include objs absorb?)
      (when (not (list? objs))
	  (set! objs (list objs)))
      (set! objs (filter (lambda (o) (apply-filters o)) objs))
      (let* ((int+diff (list-intersect+diff objs _contents memq))
	     (old-objs (car int+diff))
	     (new-objs (cadr int+diff)))
	(set! _contents (append new-objs _contents))
	(if absorb?
	    (for-each (lambda (o) (absorb-obj o)) new-objs)
	    (for-each (lambda (o) (receive-obj o)) new-objs))
	(for-each (lambda (o) (update-obj o)) old-objs)
        new-objs
	))
    (define/public (receive objs)
      (include objs #f))
    (define/public (absorb objs)
      (include objs #t))

    (define/public (receive-obj obj)
      (claim obj)
      (for-each (lambda (fn) ((car fn) obj)) _receive-actions)
      (for-each (lambda (fn) ((car fn) obj)) _enter-actions)
      )
    (define/public (absorb-obj obj)
      (claim obj)
      (for-each (lambda (fn) ((car fn) obj)) _absorb-actions)
      (for-each (lambda (fn) ((car fn) obj)) _enter-actions)
      )

    (define (exclude objs abandon?)
      (when (not (list? objs))
	  (set! objs (list objs)))
      (let* ((int+diff (list-intersect+diff _contents objs memq))
	     (int      (car int+diff))
	     (diff     (cadr int+diff)))
	(set! _contents diff) ;what's left over
	(if abandon?
	    (for-each (lambda (o) (abandon-obj o)) int)
	    (for-each (lambda (o) (remove-obj o)) int))
	))
    (define/public (remove objs)
      (exclude objs #f))
    (define/public (abandon objs)
      (unless _loyal
	  (exclude objs #t)))

    (define/public (remove-obj obj)
;(say "depart region " (length _contents))
      (remq-clean-from-malist-val! 'in-region this obj alist)
      (unless (send obj deleted?)
	      (for-each (lambda (fn) ((car fn) obj)) _remove-actions)
	      (for-each (lambda (fn) ((car fn) obj)) _leave-actions)
	      (get-and-rem-from-malist! assq remv obj _restore-abandoned-alist) ; clean up unused abandonment undos
	      (restore-any-obj obj)       ; do removal undos
	      ))
    
    (define/public (abandon-obj obj)
;(say "depart region " (length _contents))
      (unless (or (send obj deleted?) _loyal)
	      (remq-clean-from-malist-val! 'in-region this obj alist)
	      (for-each (lambda (fn) ((car fn) obj)) _abandon-actions)
	      (for-each (lambda (fn) ((car fn) obj)) _leave-actions)
	      (restore-any-obj obj)
	      (restore-abandoned-obj obj)))
    
    ; Updating: --------------------------
    (define/public (update objs)
      (when (not (list? objs))
	  (set! objs (list objs)))
      (for-each (lambda (o) (update-obj o)) objs))

    (define/public (update-obj obj)
      (for-each (lambda (fn) ((car fn) obj)) _update-actions))    
    
    (define/public (finish)
      (for-each (lambda (fn) ((car fn))) _finish-actions))
      
    (define/public (pickup obj) ;object is picked up at start of drag
      (for-each (lambda (fn) ((car fn) obj)) _pickup-actions)
      ;this ensures that obj will be replaced if undo:
      ;Nevermind; delegated to Respond-To-Pickup below
      ;(push!
      ; `(send (get-rgn ,(obj->IDexpr (send this formation))) receive ,(obj->IDexpr obj))
      ; *undo-ops*)
      )
      
    ; Effect lists: ------------------------
    (define/public filter-fn (callback-accessor-functions _filter-fns))
    ; callbacks to restrict contents
    ; each fn returns #t for objects the region accepts

    (define/public leave-actions   (callback-accessor-functions _leave-actions))
    (define/public remove-actions  (callback-accessor-functions _remove-actions))
    (define/public abandon-actions (callback-accessor-functions _abandon-actions))

    (define/public enter-actions   (callback-accessor-functions _enter-actions))
    (define/public receive-actions (callback-accessor-functions _receive-actions))
    (define/public absorb-actions  (callback-accessor-functions _absorb-actions))

    (define/public pickup-actions  (callback-accessor-functions _pickup-actions))

    (define/public update-actions  (callback-accessor-functions _update-actions))
    (define/public finish-actions  (callback-accessor-functions _finish-actions))
))

(define virtual-region%
  (class effect-set%
      (super-instantiate ())
      (send this dynaclass 'virtual-region%)
      (inherit-field _contents)
      (field	(_absorptive #f) ;does regn pick up objs it's dropped on?
		(_greedy #f))    ;when selection dropped on regn, which are picked up?
      (field    (_pane #f))
      (field    (_containment-fn contained-in?))

      (push! this *all-regions*)

      ;==============      
      (define/public containment-fn (get/set _containment-fn))

      (define/public pane
	(case-lambda
	 (() (if _pane
		 (_pane)
		 #f))
	 ((fn) (set! _pane fn))))

      (define/public (formation) #f)

      (define/override (delete)
	(send this abandon _contents)
	(set! *all-regions* (remq this *all-regions*))
	(set! *greedy-regions* (remq this *greedy-regions*))
	(super delete))

      (define/public (refresh-contents) ;assumes region was moved
	(when _absorptive
	    (send this contents (contained-objects (send this pane) _containment-fn))))

      (define/public (refresh-obj-presence obj) ;assumes obj was moved
	(unless (eq? obj (send this formation))
		(let ((now-in (and
			       (send this apply-filters obj)
			       (_containment-fn obj (send this pane))))
		      (was-in (memq obj _contents)))
		  (cond
		   ((and was-in now-in) ;stay
		    (send this update-obj obj))
		   ((and was-in (not now-in)) ;leave
		    (set! _contents (remq obj _contents))
		    (send this remove-obj obj))
		   ((and now-in (not was-in)) ;enter
		    (push! obj _contents)
		    (send this receive-obj obj))))))

      (define/public absorptive
	(case-lambda
	 (() _absorptive)
	 ((val)
	    (if val
		(when (not _absorptive)
		    (let ((obj (send this object)))
		      (when obj
			    (send obj afterposition-callbacks 'add
				  (lambda (o l)
				    (send this refresh-contents)) 'absorption)
				 ;consider adding cbs for resize, reshape also...
			    (send obj post-build-ops 'add
				  (lambda (o label) `(send (get-rgn ,label) refresh-contents)) 'absorption)
			    )))
		(let ((obj (send this object)))
		  (when obj
			(send obj afterposition-callbacks 'remove 'absorption)
			(send obj post-build-ops 'remove #f absorption))))
	    (set! _absorptive val))))

      (define/public greedy
	(case-lambda
	 (() _greedy)
	 ((val) (if val
		    (unless (memq this *greedy-regions*)  ;make greedy
			    (push! this *greedy-regions*))
		    (set! *greedy-regions*                ;un-greedy
			  (remq this *greedy-regions*)))
	  (set! _greedy val))))

      (define/public (sticky) #f)
))

;------ Example ------
(define tinted-windshield%
  (class virtual-region%
    (super-instantiate ())
    (define/override (pane)
      (let ((view (send dynapad bbox)))
	(append (bbw view) (bbne view))))
    (send this enter-actions 'add
	  (lambda (o) (let* ((curr-color (send o pen))
			     (do-fn (lambda (obj)   (send obj pen "cyan")))
			     (undo-fn (lambda (obj) (send obj pen curr-color))))
			(send this undo-on-exit o do-fn undo-fn))))
))
;======================


(define region%
  (class virtual-region%
    (init _obj)
    (super-instantiate ())
    (send this dynaclass 'region%)

    (send this attach-to _obj region-actor-name)

    (field (_decorate-fns null))
    (field (_sticky #f)      ;does regn stick to view?
	   (_retentive #f))  ;do contents move with regn?

    (define/override (formation) ;obj which defines frame
      (send this object))
    (define/override (pane)  ;obj which gets decorated
      (send (send this object) frame))
    (define/public (border)
      (send (send this object) border))

    ; special write adjustments sent to obj
    (send (formation) alternate-build-fn
	  (lambda (obj)
	    (let ((main-part (send obj frame)))
	      `(ic
		(regionize
		 (ic (make-object ,(send main-part dynaclass) dynapad)
		     (position ',(send main-part position))
		     (coords   ',(send main-part coords)))
		 ,(send this dynaclass)
		 ,(send (formation) dynaclass))
		(id ,(send (send this formation) id))))))

    ;include explicit contents list in write
    (send (formation) post-build-ops 'add
	  (lambda (o label) `(send-rgn ,label defer-send
				       '(final-contents
					 ,(export-objs (send this contents)))
				 ))
;	  (lambda (o) `(with rgn (get-actor-named obj region-actor-name)
;			     (send rgn 
;				   refer-when-ready
;				   'final-contents
;				   (list ,@(map obj->id (send this contents))))
;			     ;(send rgn finish)
;			     ))
	  'enumerate-contents)
    
    (send (formation) dependents-fn
	  (lambda (form)
	    (send-rgn form contents)))
;	    (let ((rgn (get-actor-named form region-actor-name)))
;	      (send rgn contents))))

    (define/public (decorate) ;may be overridden to depict _obj specially
      (for-each (lambda (fn) ((car fn))) _decorate-fns))
;      (send (formation) title (symbol->string (send this dynaclass))))

    (define/public decorate-fn
      (callback-accessor-functions _decorate-fns))

    (define/override sticky
      (case-lambda
       (() _sticky)
       ((newsticky) (set! _sticky newsticky)
	(send (send this object) sticky newsticky)
	    ;(if (not newsticky)
	 ;    (send-actor-named (send this object)
	 ;      geo-actor-name refresh-edge-ring))
	(for-each (lambda (obj) (stickify-obj obj newsticky))
		  _contents))))

    (define (seize-obj obj)
      (let ((form (send this formation)))
	(send form add obj)
	(send form separable obj #t)
	(let ((old-owner (get-and-rem-from-malist! assq remq 'last-region-owner obj alist)))
	  (when (and old-owner
		   (not (eq? this (cadr old-owner))))
	      ;steal obj from old-owner
	      (send (cadr old-owner) remove obj)))
	(get-else-push-onto-malist! assq (list 'region-owner this) obj alist)
;	(remote-push! (list 'region-owner this) obj alist)
;	(push-onto-malist-val-always! assq
;				     'region-owner
;				     this obj alist)	
	))

    (define (detach-obj obj)
      (let ((form (send this formation)))
	(send form remove obj)))

    (define (prepare-release-obj obj)
      (detach-obj obj)
      (get-and-rem-from-malist! assq remq 'region-owner obj alist)
      (remote-push! (list 'last-region-owner this) obj alist))

    (define (commit-release-obj obj)
      (detach-obj obj)  
      (get-and-rem-from-malist! assq remq 'region-owner obj alist)
      (get-and-rem-from-malist! assq remq 'last-region-owner obj alist)
      ;un-propogate geon callbacks
      ;(let ((geon (get-actor-named obj geo-actor-name)))
;	(if geon
;	    (send geon detach-callbacks-from-obj obj)))
      )

      (define/public retentive
	(case-lambda
	 (() _retentive)
	 ((bool)
	  (set! _retentive bool)
	  (let* ((form (send this object)))
	    (if bool  ;set retention mechanism
		(begin
		  (send this pickup-actions 'add prepare-release-obj 'retention)
		  (send this update-actions 'add seize-obj  'retention)

	; set filter to prevent grabbing another group's members
		  (send this filter-fn 'add
			(lambda (obj)
			  (let ((owner (assq 'region-owner (send obj alist))))
			    (or (not owner) (memq this (cdr owner)))))
;			  (let* ((owner (get-container obj))
;				 (result
;				  (or (not owner)
;				      (eq? owner (send this formation)))))
;			    (say result)
;			    result))
			'retention)

	; cause future adds/leaves to join/leave formation
		  (send this enter-actions 'add seize-obj 'retention)
		  ;propogate geon callbacks:
		  ;(send this enter-actions 'add
		;	(lambda (o)
		;	  (let ((geon (get-actor-named o geo-actor-name)))
		;	    (if geon
		;		(send geon attach-callbacks-to-obj (get-top-group (send this formation))))))
		;	'retention)

		  (send this leave-actions 'add commit-release-obj 'retention)

	          ; current contents join formation
		  (send form contents (send this visible-contents))
		  (for-each (lambda (o) (send form separable o #t)) (send this visible-contents)))

	; detach retention mechanism
		(begin
		  ; remove current contents
		  (send form contents null)
	          ; cancel future additions
		  (send this enter-actions 'remove #f 'retention)
		  (send this leave-actions 'remove #f 'retention)
		  (send this filter-fn 'remove #f 'retention)
		  ))))))

      (send this filter-fn 'add
	    ;filter to reject regn's own formation or parts of any formation
	    (lambda (o)
	      (let ((form (send this formation)))
		(not (or (eq? o form) (part? o)))))
	    'reject-parts)
      (send this filter-fn 'add
	    (lambda (o)
	      (let ((rgn (get-rgn o)))
		(not (is-a? rgn lens%))))
	    'reject-lenses)

))

(define lens%
; a lens leaves the original content unchanged (hidden behind lens)
;  and applies actions to a copy above lens, which dies when lens is moved
;  away from source obj.
   (class region%
    (init _obj)
    (field (_shadows '())) ;alist mapping source->shadow
;    (field (_layer (make-object layer% dynapad "lens")))
;    (send _layer raise *lens-layer*)
    (super-instantiate (_obj))
    (send this dynaclass 'lens%)

    (define/override (visible-contents) (map cadr _shadows))
    (send _obj layer (send dynapad getvar 'lens-layer))
    (send this absorptive #t)
    ;(send this retentive #t)

    (inherit formation)
    (inherit pane)
    (send (formation) post-build-ops 'remove #f 'enumerate-contents)
				; cancel the explicit enumeration of contents
    (send (formation) dependents-fn
	  (lambda (form) null)) ; cancel listing of contents during write-all

    (define (get-shadow obj)
      (cadr (assq obj _shadows)))

    (define (include-obj obj absorb?)
      (let ((shadow (clone-object obj)))
	;(if _sticky
	;    (stickify-obj obj _sticky))
	;(send shadow layer _layer)
	;(send shadow sticky (send this sticky))
	(send this claim obj)
	(send shadow findable #f)
	(push! (list obj shadow) _shadows)
	(for-each (lambda (fn) ((car fn) shadow))
		  (if absorb? (send this absorb-actions) (send this receive-actions)))
	(for-each (lambda (fn) ((car fn) shadow)) (send this enter-actions))))

    (send this enter-actions 'add
	  (lambda (shadow) (send (send this formation) add shadow)))
    (define/override (absorb-obj obj)  (include-obj obj #t))
    (define/override (receive-obj obj) (include-obj obj #f))

    (define (exclude-obj obj abandon?)
      (remq-clean-from-malist-val! 'in-region this obj alist)
      (let ((shadow (get-shadow obj)))
	(for-each (lambda (fn) ((car fn) shadow))
		  (if abandon? (send this abandon-actions) (send this remove-actions)))
	(for-each (lambda (fn) ((car fn) shadow)) (send this leave-actions))
	(send shadow delete)
	(set! _shadows (remq (assq obj _shadows) _shadows))))

    (define/override (abandon-obj obj) (exclude-obj obj #t))
    (define/override (remove-obj obj)  (exclude-obj obj #f))

    (define/override (update-obj obj)
      (let ((shadow (get-shadow obj)))
	(for-each (lambda (fn) ((car fn) shadow)) (send this update-actions))))

    (define/override (decorate)
      (let* ((obj (send this pane))
	     (bg-color (make-object dynacolor%
				    (send (send obj dynapad) background)))
	     (my-color (send bg-color blend .1 80 170 80))
	     (outline-color (send (send bg-color invert) tcl-color)))
	(send obj fill (send my-color tcl-color))
	;(send obj pen outline-color)
	;(send (send this border) pen outline-color)
	(send obj transparency .7)
	(let ((agent (make-object opaquer% obj)))
	  (send this finish-actions 'add
		(lambda () (send agent hide-soon)))
	  (send (send this formation) afterslide-callbacks 'add
		(lambda (o dx dy) (send agent nudge)) this))
	(super decorate)))
))

(dynaload "actortimer.ss")
(define fader-timer (make-object auto-unsubscribe-timer% dynapad 300))
(send fader-timer start)

(define opaquer%
  (class named-actor%
    (init _obj)
    (field (_start-fading-when 0))
    (field (_finish-fading-when 0))
    (field (_subscribed? #f))
    (field (_fade-delay 500))
    (field (_fade-duration 2000))
    (field (_transparency .7))
    (super-instantiate ())
    (send this attach-to _obj 'opaquer)

    (send _obj bind "<ButtonPress-1>"
	  (lambda (o e) (nudge)))
    (send _obj bind "<KeyPress-space>"
	  (lambda (o e) (nudge)))
;    (let ((wrapper (get-container _obj)))
;      (send wrapper bind "<Enter>"
;	    (lambda (o e) (nudge)))
;      (send wrapper bind "<Leave>"
;	    (lambda (o e) (hide-soon))))

    (define/public (show)
      (send (send this object) transparency _transparency))

    (define/public (fade transp)
      (let ((transp-now (send (send this object) transparency)))
	(when (> transp transp-now)
	    (send (send this object) transparency transp))))

    (define/public (nudge)
      (show) (hide-soon))

    (define/public (hide-soon)
      (let ((now (current-milliseconds)))
	(set! _start-fading-when (+ now _fade-delay))
	(set! _finish-fading-when (+ _start-fading-when _fade-duration))
	(unless _subscribed?
		(send fader-timer subscribe this)
		(set! _subscribed? #t))))

    (define/public (update abstime dtime)
      (cond ((> abstime _finish-fading-when)
	       (send this fade 1)
	       (send fader-timer unsubscribe this)
	       (set! _subscribed? #f))
	    ((> abstime _start-fading-when)
	       (let ((fract (/ (- abstime _start-fading-when) _fade-duration)))
		 (send this fade (lerp fract _transparency 1))))))

    (define/override (delete)
      (send fader-timer unsubscribe this)
      (super delete))
))


(define mutator%
; a mutator permanently affects all contents it's dropped over
;  (unless actions are set to undo)
  (class region%
    (init _obj)
    (super-instantiate (_obj))
    (send this dynaclass 'mutator%)
    (let ((mylayer (make-object layer% dynapad "brush")))
      (send mylayer raise *brush-layer*)
      (send _obj layer mylayer))
    (send this absorptive #t)

    (inherit formation)
    (send (formation) post-build-ops 'remove #f 'enumerate-contents)
				; cancel the explicit enumeration of contents
    (send (formation) dependents-fn
	  (lambda (form) null)) ; cancel listing of contents during write-all


    (send this enter-actions 'add
	  (lambda (o) (push! `(send ,(obj->IDexpr o) position (list ,@(send o position)))
			     *undo-ops*)))
    (send this update-actions 'add
	  (lambda (o) (push! `(send ,(obj->IDexpr o) position (list ,@(send o position)))
			     *undo-ops*)))

    (define/override (decorate)
      (let* ((obj (send this pane))
	     (bg-color (make-object dynacolor%
				    (send (send obj dynapad) background)))
	     (my-color (send bg-color blend .3 180 80 80))
	     (outline-color (send (send bg-color invert) tcl-color)))
	(send obj fill (send my-color tcl-color))
	;(send obj pen outline-color)
	;(send (send this border) pen outline-color)
	(send obj transparency .4)
	(send obj maxsize 1 #t)
	)
      (super decorate))
))

(define tray%
; a tray moves all contents with it when slid, etc.
  (class region%
    (init _obj)
    (super-instantiate (_obj))
    (send this dynaclass 'tray%)
    (send this retentive #t)

    (define/override (decorate)
      (let* ((obj (send this pane))
	     (bg-color (make-object dynacolor%
				    (send (send obj dynapad) background)))
	     (my-color (send bg-color blend .1 80 80 180))
	     (outline-color (send (send bg-color invert) tcl-color)))
	(send obj fill (send my-color tcl-color))
	;(send obj pen outline-color)
	;(send (send this border) pen outline-color)
	;(send obj transparency .9)
	)
      (super decorate))

))

(define fusing-tray%
  (class tray%
    (init _obj)
    (super-instantiate (_obj))
    (send this dynaclass 'fusing-tray%)

    ;Affords fusing, shared by piles and trays
    ; fusing action is handled by formation class
    ; but region needs to keep track of members' sizes.
    ; Recomputes mean on the fly, with e. add or remove
    (field (_num-items 0))
    (field (_mean-item-dim 1)) ;mean max-dimension of content items
    (send this enter-actions 'add
	  (lambda (o)
	    (let ((dim (max (send o width) (send o height))))
	      (increment-mean _mean-item-dim _num-items dim)
	      (sendf this formation use-item-size _mean-item-dim)
	      )))
    (send this leave-actions 'add
	  (lambda (o)
	    (let ((dim (max (send o width) (send o height))))
	      (decrement-mean _mean-item-dim _num-items dim)
	      (sendf this formation use-item-size _mean-item-dim)
	      )))
    (send this finish-actions 'add
	  (lambda () (sendf this formation refresh-fusing-size)))
	    
))

(define sponge%
; a sponge retains all content when moved
;  and acquires any new content it's dropped over
  (class region%
    (init _obj)
    (super-instantiate (_obj))
    (send this dynaclass 'sponge%)
    (send this retentive #t)
    (send this absorptive #t)
))

(define fusing-container%
; a container is a greedy tray; i.e. collects all dropped objs when cursor overlaps
  (class fusing-tray%
    (init _obj)
    (super-instantiate (_obj))
    (send this dynaclass 'fusing-container%)
    (send this greedy #t)

))

;for back-compat only:
(define container% fusing-container%)

#|
; these need more debugging:
(define shelf%
; a shelf stays on a layer above the main layer
  (class fusing-container%
    (init _obj)
    (field (_layer (make-object layer% dynapad "shelf")))
    (send _layer raise *shelf-layer*)
    (super-instantiate (_obj))
    (send this dynaclass 'shelf%)
    (send _obj layer _layer)
    (define/override (decorate)
      (super decorate)
      (send (send this pane) transparency .9))

  (send this leave-actions 'add
     (lambda (obj)
       ;drop to top of main layer
       (send obj layer (send (send (send this object) dynapad) main-layer))
       (send obj raise)))
  
    (send this enter-actions 'add
     (lambda (obj)
       (def curr-layer)
       (def do-fn)
       (def undo-fn)
       (if (send obj selected?)
	   (begin
	     (set! curr-layer (car (send obj layer-stack)))
	     (set! do-fn (lambda (obj) (remote-push! _layer obj layer-stack))))
	   (begin
	     (set! curr-layer (send obj layer))
	     (set! do-fn   (lambda (obj) (send obj layer _layer)))))
       (set! undo-fn (lambda (obj)
		       (if (send obj selected?)
			   (send obj layer-stack ;pop
				 (cdr (send obj layer-stack)))
			   (send obj layer curr-layer))))
       (send this undo-on-exit obj do-fn undo-fn))
     'setlayer)

))
    
(define sticky-shelf%
  (class shelf%
    (init _obj)
    (field (_default-sz 0.15))
    (super-instantiate (_obj))
    (send this dynaclass 'sticky-shelf%)
    (send this sticky #t)
    (define/override (decorate)
      (super decorate)
      (send (send this pane) transparency 1))

    (send this enter-actions 'add
     (lambda (obj)
       (let* ((curr-z  (send obj z))
	      (do-fn   (lambda (obj) (send obj z
					   (/ _default-sz
					      (caddr (send dynapad view))))))
	      (undo-fn (lambda (obj) (send obj z curr-z))))
	 (send this undo-on-exit obj do-fn undo-fn)))
     'sticky-z)
))

(define panel-shelf%
  (class shelf%
    (init _obj)
    (if (not (is-a? _obj minipad-xstrip%))
	(error "Object must be a minipad-xstrip\n"))
    (define/override (retentive . arg) #t) ;panel is already retentive
    (define/override (sticky . arg) #t) ;panel is already sticky
    (super-instantiate (_obj))
    (send this dynaclass 'panel-shelf%)

    (send this enter-actions 'add
     (lambda (obj)
       (send _obj add obj)))
    (send this leave-actions 'add
     (lambda (obj) (send _obj remove obj)))
))
|#

;======= PICK-UP =============
(define (get-pickup-relevant-regions obj)
  (let ((entry (assq 'in-region (send obj alist))))
    (and entry (cdr entry))))

(define (respond-to-pickup dynapad evnt picked-objs)
; first, build alist (reg--> pickup-objs)
  (let ((alist (prepare-rgn/obj-alist picked-objs)))
    (foreach alist (lambda (tuple)
            ; for each reg included in alist, push a refresh onto *undo-ops*
		     (let ((rgn (car tuple)))
		       (foreach (cdr tuple)
				(lambda (o) (send rgn pickup o))))))))

;  (foreach picked-objs
;     (lambda (o)
;       (let ((regs (get-pickup-relevant-regions o)))
;;	     (lyr  (send o layer)))
;	 (if regs
;	     (foreach regs (lambda (r) 
;			     (send r pickup o))))))
;;	 (when (or (eq? lyr (send dynapad main-layer))
;;		 (eq? lyr *shelf-layer*))
;;	       (send o layer *selection-layer*)
;;	       (send o raise)))
;))

(define (prepare-rgn/obj-alist objs)
  (let ((alist null))
    (foreach objs
       (lambda (o)
	 (let ((regs (get-pickup-relevant-regions o)))
	   (when regs
	       (foreach regs (lambda (r)
			       (pushq-onto-malist-val-always! r o alist)))))))
    alist))

(send dynapad beforedrag-callbacks 'add respond-to-pickup)

    ; make sure all regions affected by moving objs
    ; will be restored after an undo
(define *claims-this-round* null)
 ; this is a alist ((reg obj obj...)...) of all additions to regions
 ; during most recent drag

(beforedrag-prepare-undo-callbacks 'add
  (lambda (argPAD evnt objs)
    ; ensure replacement (at end of undo) of objs picked up before drag
    (set! *claims-this-round* null)
    (let ((alist (prepare-rgn/obj-alist objs)))
      (foreach alist (lambda (tuple)
		       (apply ensure-region-claims-after-undo tuple))))))

(define *claims-this-round* null)
 ; this is a alist ((reg obj obj...)...) of all additions to regions
 ; during most recent drag

(afterdrag-prepare-undo-callbacks 'add
  (lambda (argPAD evnt objs)
    ; ensure release (at start of undo) of objs dropped after drag
    (foreach *claims-this-round*
	     (lambda (tuple)
	       (apply ensure-region-unclaims-after-undo tuple)))))

; ========= DROP =============
;this should be implemented differently (at C++ lvl)
;(define (sort-regions-top-down regs)
;  (let* ((reg-objs (map (lambda (r) (send r formation)) regs))
;	 (big-bb (bbunion-objects reg-objs))
;	 (all-objs (send dynapad find 'groupmembers 'overlapping big-bb))
;	 (bottom-up-reg-objs (car (list-intersect+diff all-objs reg-objs memq)))
;	 (bottom-up-regs (map get-rgn bottom-up-reg-objs)))
;    (reverse bottom-up-regs)))
;et voila:
(define (sort-regions-top-down regs)
  (let* ((objs (map (lambda (r) (send r object)) regs))
	 (objs-top-down (reverse (send dynapad order objs))))
    (map get-rgn objs-top-down)))

(define (local-regions argPAD dragset)
  (let ((oldbb (send argPAD getvar 'dragset-bbox))
	(newbb (bbunion-objects dragset)))
    (filter (lambda (r) (let* ((frm (send r pane))
			       (frm-bb (send frm bbox)))
			  (or (bboverlap? frm-bb oldbb)
			      (bboverlap? frm-bb newbb))))
	    *all-regions*)))

(define (respond-to-drop dynapad evnt moved-objs)
  (show-possible-delay dynapad
    (let* ((check-regs (local-regions dynapad moved-objs))) ;*all-regions*))
    ; first check greedy regions
      (let* ((ex (event-x evnt))
	     (ey (event-y evnt))
	     (objs-at-cursor (send dynapad find 'groupmembers 'overlapping (list ex ey ex ey)))
	     (drag-lyr (send dynapad getvar 'drag-layer ))
	     (unmoved-objs-at-cursor
	      (filter (lambda (o) (not (eq? (send o layer) drag-lyr)))
		      objs-at-cursor))
	     (greedy-rgn-objs (map (lambda (rgn) (send rgn formation)) *greedy-regions*))
	     (greedy-objs-at-cursor
	      (filter (lambda (o)
			(and (memq o greedy-rgn-objs)
			     (not (memq o moved-objs))
			     (contains-pt? (send o pane) ex ey)))
		      unmoved-objs-at-cursor))
	     (greedy-rgns-at-cursor
	      (map (lambda (o) (get-actor-named o region-actor-name))
		   greedy-objs-at-cursor)))
      ;(top-greedy-rgn-at-cursor (car (reverse greedy-rgns-at-cursor))))
      ;(say greedy-rgns-at-cursor)
	(foreach (reverse greedy-rgns-at-cursor) ;top->bottom order
		 (lambda (r) 
		 ;(say (send (send r pane) fill)) ;debugging
		   (send r receive moved-objs)
		   (send r finish)))

      ;(say "# Greedies: " (length greedy-rgns-at-cursor))
      ;(say greedy-rgns-at-cursor)

    ; eliminate greedy regs from further checking
	(set! check-regs (list-diff check-regs greedy-rgns-at-cursor memq)))
    ;(say " N x R: " (length moved-objs) (length check-regs))
      (set! check-regs (sort-regions-top-down check-regs))
    ;check if dropped objs have entered/left any regions
      (cross-map (lambda (obj regn) (send regn refresh-obj-presence obj))
		 moved-objs
		 check-regs)
    ;check if any dropped objs *are* regions and apply effects
      (for-each (lambda (obj)
		  (send-actors-named obj region-actor-name refresh-contents))
		moved-objs)

      (for-each (lambda (rgn) (send rgn finish)) check-regs)
      )))

(send dynapad afterdrag-callbacks 'add
      (lambda (eventPAD event draglist)
	;(display "regions responding...")(newline)
	(respond-to-drop eventPAD event draglist)))

;===================================
; syntax:
; (regionize dynaobj regiontype% formtype%)
; or
; (regionize dynaobj '(regiontype% arg0 arg1 ...) '(formtype% arg0 arg1...))
(define regionize
  (case-lambda
   ((obj rgn-type)  (regionize obj rgn-type *default-region-form-type*))
   ((obj rgn-type form-type)
      (unless (or
	       (is-a? obj panel%)
	       (is-a? obj rect%)
	       (is-a? obj image%)
	       (is-a? obj text%)
	       (is-a? obj oval%)
	       (is-a? obj polygon%))
	      (error (format "Can't regionize this class: ~a" obj)))
      ;ensure list args
      (when (not (list? rgn-type))
	  (set! rgn-type (list rgn-type)))
      (when (not (list? form-type))
	  (set! form-type (list form-type)))

      (let* ((form (apply make-object (car form-type) dynapad obj (cdr form-type)))
	     ;(junk (say "top: " (get-topmost-group obj)))
	     (geon  (geodize obj))
	     (rgn-actor (ic (apply make-object (car rgn-type) form (cdr rgn-type))
			    (decorate))))
	(send geon link-to form geo-actor-name) ;creates secondary linking to top-lvl formation
	; main attachment (to lower formation pane) is automatic in geon-create
	;(send rgn-actor refresh-contents)
	;(send rgn-actor finish)
	form))))

