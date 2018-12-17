(dynaload "trees.ss")
(dynaload "tools-lists.ss")

(define (gather-lognames dir treename)
  (let* ((allfiles (directory-list->string dir))
	 (logrexp (regexp (format "~a#*" treename)))
	 (matchfiles (filter (lambda (f) (regexp-match logrexp f))
			     allfiles)))
    matchfiles))

(define (gather-logranges dir treename)
  (let* ((allfiles (directory-list->string dir))
	 (logrexp (regexp (format "~a#(.+)" treename)))
	 (matchids (map (lambda (f)
			  (let ((match (regexp-match logrexp f)))
			    (and match (cadr match))))
			allfiles)))
    (filter (lambda (x) x) matchids)))

(define (find-log-parent logid pair-set)
  (let* ((lognum (logid->lognum logid))
	 (found (first-valid pair-set
			     (lambda (pair) (and (equal? lognum (cadr pair))
						 (car pair))))))
    (and found (cadr found))))

(define (gather-log-transitions dir treename)
; returns list of pairs (pre-logid post-logid)
; reflecting linking between logs
  (let* ((logranges (gather-logranges dir treename))
	 (pairs     (map (lambda (logrange) (logrange->logid+logendnum logrange))
			 logranges))
	 (ids       (map car pairs))
	 (pres      (map (lambda (logid) (find-log-parent logid pairs)) ids)))
    (map list pres ids)))
;    (map (lambda (pre next) (list pre next))
;;	   (list (and pre (format "~a#~a" treename pre)) next))
;	 pres
;	 logids)
;))

;    (for-each (lambda (pre next)
;		(let ((prefilename (and pre (format "~a#~a" treename pre))))
;		  (push-onto-alist-val-always! assoc prefilename next childindex)
;		  (push! (list next prefilename) parentindex)))
;		pres
;		filenames)
;    (list parentindex childindex)))


(define *current-logtree* #f)
(define *current-logbranch* #f)
(define *current-state-marker* #f)

(define (pixels->space px)
  (let ((scale (caddr (send dynapad view))))
    (/ px scale)))

(define logtree%
  (class group%
    (init _dynapad)
    (init-field _dir)
    (init-field _treename)
    (field (_logbranch-alist null))
    (field (_active-path null))

    (super-instantiate (_dynapad))
    (send this findable #f)
    (send this anchor "sw")

    (let ((pairs (sort (lambda (p1 p2) (logid<? (cadr p1) (cadr p2)))
		       (gather-log-transitions _dir _treename))))
      (map (lambda (pair) 
	     (mlet (((preid nextid) pair))
		   (let* ((prevbranch (send this logbranch-with-logid preid))
			  (prevnode (and prevbranch (send prevbranch to-node)))
			  (newobj (make-object visible-logbranch% dynapad this nextid prevnode)))
		     newobj)))
	   pairs))

    (define/public (origin)
      (bbsw (bbstretch (send dynapad bbox) -.05)))

    (define/public treename (get/set _treename))

    (define/public (logbranch-with-logid logid)
      (let ((tuple (assoc logid _logbranch-alist)))
	(and tuple (cadr tuple))))

    (define/override (add lst)
      (if (not (list? lst))
	  (set! lst (list lst)))
      (foreach lst (lambda (branch) (let ((logid (send branch logid)))
				    (push! (list logid branch)
					   _logbranch-alist))))
      (super add lst))

    (define/override (members) (super members))
    (define/override (remove . args) #f)

    (define/override (delete)
      (if (eq? *current-logtree* this)
	  (set! *current-logtree* #f))
      (super delete))

    ;(define/public active-path (get/set _active-path))

    (define/public (refresh-active-path current-branch . future-branches)
      (foreach _active-path (lambda (branch) (send branch exclude-from-active-path)))
      (let ((path-upward (send current-branch path-from-root)))
	(set! _active-path (append path-upward future-branches))
	(foreach _active-path (lambda (branch) (send branch include-in-active-path)))))
))

(define log-treenode%
  (class spatial-treenode%
    (init-field _tree)
    (init-field _prev-branch)
    (field (_next-branches null))

    (define (my-update-fn me dx dy)
      (let ((xy (map + (send _tree origin) (list (pixels->space dx)
						 (pixels->space dy)))))
	(if _prev-branch
	    (send _prev-branch toxy xy))
	(foreach _next-branches (lambda (b) (send b fromxy xy)))))

    (define/public prev-branch
      (case-lambda
       (() _prev-branch)
       ((new) (set! _prev-branch new))))

    (define/public (next-branch new)
      (push! new _next-branches))
    (define/public (next-branches)
      _next-branches)

    (super-instantiate (#f
			my-update-fn
			#f ;horizontal
			20 4))
    ))

(define visible-logbranch%
  (class line%
    (init _dynapad)
    (init-field _tree)
    (init-field _logid)
    (init-field (_from-node #f) (_to-node #f))
    (field (_firststate (let ((idnumstr (logid->lognum _logid)))
			  (and idnumstr (string->number idnumstr)))))
;    (field (_laststate #f))
    (field (_on-active-path? #f))

    (super-instantiate (_dynapad))
    (let ((origin (send _tree origin)))
      (send this coords (append origin origin)))

    (if (not _from-node)
	(set! _from-node (make-object log-treenode% _tree #f)))
    (send _from-node next-branch this)

    (if (not _to-node)
	(begin
	  (set! _to-node (make-object log-treenode% _tree this))
	  (send _from-node add-child-after #f _to-node)))

    (send this sticky #t)
    (send _tree add this)
    (send this findable #f)

    (define/public (logid) _logid)

    (define/public to-node   (get/set _to-node))
    (define/public from-node (get/set _from-node))

    (define/public toxy 
      (case-lambda
       (() (cddr (send this coords)))
       ((xy) (let ((crds (send this coords)))
	       (send this coords (append (list (car crds) (cadr crds)) xy))))))

    (define/public fromxy
      (case-lambda
       (() (let ((crds (send this coords)))
	     (list (car crds) (cadr crds))))
       ((xy) (let ((crds (send this coords)))
	       (send this coords (append xy (cddr crds)))))))

    (define/public (firststate) _firststate)
;    (define/public laststate (get/set _laststate))

    (define/public (prev-branch) (send _from-node prev-branch))

    (define/public (next-branches) (send _to-node next-branches))
;    (define/public (addnext n)
;      (if (null? _nextbranches)
;	  (laststate (send n firststate)))
;      (push! n _nextbranches))

    (define (compute-marker-xy state-id)
      (let* (
;	     (state-span (if (or (not _laststate)
;				 (not _firststate))
;			     0
;			     (- _laststate _firststate)))
	     (crds (send this coords))
;	     (fract (if (or (not state-id) (zero? state-span))
;			0
;			(/ (- state-id (firststate)) state-span)))
	     (undo-stack-len (- (length *undo-stack*) 1))
	     (redo-stack-len (length *redo-stack*))
	     (total-len (+ undo-stack-len redo-stack-len))
	     (fract (if (zero? total-len) 1 (/ undo-stack-len total-len)))

	     (x (lerp fract (car crds) (caddr crds)))
	     (y (lerp fract (cadr crds) (cadddr crds))))
	(list x y)))

    (define/public (hilight)
      (send this pen "white"))
    (define/public (unhilight)
      (if _on-active-path? (lolight) (unlolight)))

    (define/public (lolight)
      ;(say "widen: " _logid)
      (send this pen "grey")
;      (send this penwidth 3))
      (send this penwidth (pixels->space (/ 3 (send this z)))))
    (define/public (unlolight)
      (send this pen "grey")
      (send this penwidth 0))

    (define/public (include-in-active-path)
      (set! _on-active-path? #t)
      ;(remote-push! this _tree active-path)
      (lolight))
    (define/public (exclude-from-active-path)
      (set! _on-active-path? #f)
      ;(send _tree active-path (remq this (send _tree active-path)))
      (unlolight))

    (define/public (update-state state)
;      (include-in-active-path)
      (hilight)
      (if (not *current-state-marker*)
	  (set! *current-state-marker*
		(ic (make-object rect% dynapad)
		    (coords (append (toxy) (map (lambda (x) (+ x (pixels->space 3))) (toxy))))
		    (pen "red")
		    (penwidth -1)
		    (sticky #t)
		    (findable #f))))
;      (laststate (safemax state (laststate)))
      (send *current-state-marker* xy (compute-marker-xy state))
      (send *current-state-marker* raise this)
      )

    (define/public (split newlogid)
      (let* ((oldnode _to-node)
	     (newnode (make-object log-treenode% _tree this))
	     (newbranch (make-object visible-logbranch% (send this dynapad)
				     _tree newlogid newnode oldnode)))
	(set! _to-node newnode)
	(send oldnode prev-branch newbranch)
	(send oldnode insert-parent newnode)
	newbranch))

    (define/public (restore)
      (load-log (send _tree treename) _logid))

    (define/public (path-from-root)
      (let ((prev (prev-branch)))
	(if (not prev)
	    (list this)
	    (append (send prev path-from-root) (list this)))))

    (define/public (setpath)
      (let* ((path (path-from-root))
	     (found (memq *current-logbranch* path))
	     (future-branches (if found (cdr found) null))
	     (future-ids (map (lambda (o) (send o logid)) future-branches)))
	(set! *future-log-path* future-ids)
	(send/apply _tree refresh-active-path *current-logbranch* future-branches)
	(send *current-logbranch* hilight)))
      
    (exclude-from-active-path)
    (send this bind "<Run-Shift-ButtonPress-1>" (lambda (ePAD e) (restore)))
    (send this bind "<Select-Shift-ButtonPress-1>"  (lambda (ePAD e) (restore)))
    (send this bind "<Run-ButtonPress-1>" (lambda (ePAD e) (setpath)))
    (send this bind "<Select-ButtonPress-1>"  (lambda (ePAD e) (setpath)))

))

(define (logid<? logid1 logid2) ;comparison for sorting logids
  (let ((lst1 (logid->lognum+suffix logid1))
	(lst2 (logid->lognum+suffix logid2)))
    (cond ((not lst1) #t) ;#f precedes all else
	  ((not lst2) #f)
	  (else
	   (mlet (((n1 s1 n2 s2) (append lst1 lst2)))
		 (let ((v1 (string->number n1))
		       (v2 (string->number n2)))
		   (if (= v1 v2)
		       (string<? (or s1 "") (or s2 ""))
		       (< v1 v2))))))))

(define (build-log-branches dir treename)
  (send *current-logtree* delete)
  (set! *current-logtree* (make-object logtree% dynapad dir treename))
)

(define-macro (switch-states var new)
 `(begin
    (set! ,var ,new)
    (if *current-logbranch*
	(send *current-logbranch* update-state ,new))))

(define (ensure-logbranch prev-logid this-logid)
  (if (not *current-logtree*)
      (set! *current-logtree*
	    (make-object logtree% dynapad 
			 *current-log-directory*
			 *current-log-treename*)))
  (let* ((prevbranch (send *current-logtree*
			 logbranch-with-logid prev-logid))
	 (prevnode (and prevbranch (send prevbranch to-node)))
	 (thisbranch (or (send *current-logtree*
			     logbranch-with-logid this-logid)
		       (make-object visible-logbranch% dynapad
				    *current-logtree*
				    this-logid prevnode))))
    thisbranch))
		   
(define-macro (switch-logs var new)
 `(let* ((newlogid ,new)
	 (oldlogbranch (send *current-logtree*
			   logbranch-with-logid ,var))
	 (newlogbranch (send *current-logtree*
			   logbranch-with-logid newlogid)))
    (if (not newlogbranch)
	(error "No such log branch: " newlogid))
    (set! ,var newlogid)
    (set! *current-logbranch* newlogbranch)
    (if oldlogbranch (send oldlogbranch unhilight))
    (if newlogbranch (begin
		       (send/apply
			*current-logtree* refresh-active-path
			newlogbranch
			(map (lambda (id) (send *current-logtree*
						logbranch-with-logid id))
			     *future-log-path*))
		       (send newlogbranch update-state *current-state-id*)
		       ))
    ))

(define (split-logbranch newlogid)
  (if *current-logbranch* (send *current-logbranch* split newlogid)))
