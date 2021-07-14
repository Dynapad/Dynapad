#lang racket/base

(require racket/class
         dynapad/base
         dynapad/spd
         dynapad/pad-state
         dynapad/undo-state
         dynapad/misc/misc
         dynapad/layout/bbox
         dynapad/layout/trees
         dynapad/history/log-state
         dynapad/history/logbranch
         dynapad/misc/user-preferences
         dynapad/utils/colors
         dynapad/utils/lerp
         )

(provide logbranch-line%
         visible-logtree%
         )

(define (make-logtree dir name) ;override defn in logbranch.ss
  (make-object visible-logtree% dynapad logbranch-line% dir name))

(define log-treenode%
  (class spatial-treenode%
    (init-field _tree)
    (init-field _prev-branch)
    (field (_next-branches null))

    (define (my-update-fn me dx dy)
      (let ((xy (map + (send _tree origin) (list (pixels->space dx)
                                                 (pixels->space dy)))))
        (when _prev-branch
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

(define visible-logtree%
  (class logtree%
    (init _dynapad)
    (init _branch-class)
    (init _dir)
    (init _name)
    (field (_active-path null))

    (field (_dim-color #f) ;should refresh before use
           (_mid-color #f)
           (_bright-color #f))


    (super-instantiate (_dynapad _branch-class _dir _name))

    (field (_root-node (make-object log-treenode% this #f)))
    (send this anchor "sw")
    (send this layer *logtree-layer*)

    (define/public (origin)
      (bbsw (bbstretch (send dynapad bbox) -.05)))

    (define/public (root-node) _root-node)

    (define/public (refresh-layout)
      (send (root-node) refresh-family))

    (define/override (members) (super members)) ;disable settable members
    (define/override (remove . args) #f) ;disable remove

    (define/override (delete)
      (when (eq? *current-logtree* this)
        (set-*current-logtree*! #f))
      (super delete))

    (define/public (refresh-color-scheme)
      (let* ((bgcolor (make-object dynacolor% (send (send this dynapad) background)))
             (anticolor (send bgcolor invert)))
        (set! _bright-color (send anticolor tcl-color))
        (set! _mid-color (send (send anticolor blend .5 128 128 128) ; -->half-grey
                               tcl-color))
        (set! _dim-color (send (send bgcolor blend .5 128 128 128) tcl-color))
        ))
    (define/public dim-color
      (case-lambda
        (() (when (not _dim-color) (refresh-color-scheme))
            _dim-color)
        ((val) (set! _dim-color val))))
    (define/public mid-color
      (case-lambda
        (() (when (not _mid-color) (refresh-color-scheme))
            _mid-color)
        ((val) (set! _mid-color val))))
    (define/public bright-color
      (case-lambda
        (() (when (not _bright-color) (refresh-color-scheme))
            _bright-color)
        ((val) (set! _bright-color val))))

    (define/public (refresh-active-path)
      (refresh-color-scheme) ;forces periodic refresh to follow changing background
      (let* ((curr-branch (send this current-branch))
             (head (send curr-branch path-from-root))
             (tail *future-log-path*)
             (new-path (append head tail))
             (remove (list-diff _active-path new-path memq))
             (add    (list-diff new-path _active-path memq)))
        (foreach remove (lambda (b) (send b exclude-from-active-path)))
        (foreach add    (lambda (b) (send b include-in-active-path)))
        (set! _active-path new-path)))

    (define/override (clear-active-path)
      (refresh-color-scheme)
      (foreach _active-path (lambda (b) (send b exclude-from-active-path)))
      (set! _active-path null))

    ; build all branches initially
    (say "gathering logfiles...")
    (foreach (send this gather-lognames)
             (lambda (file)
               (or (send this find-branch-matching
                         (lambda (b) (equal? (send b file) file)))
                   (make-object (send this branch-class) this #f file #f #f))))
    ))

(define logbranch-line%
  (class logbranch%
    (init _tree-arg)
    (init _path)
    (init _file)
    (init _startnum)
    (init _logid)
    (init (_endnum #f))
    (init (_no-autolink #f))

    (super-instantiate (_tree-arg _path _file _startnum _logid _endnum))

    (inherit-field _tree)
    (field (_from-node #f) (_to-node #f))
    (field (_on-active-path? #f))

    ; automatically create and attach to line% object
    (let* ((origin (send _tree origin))
           (myline (ic (make-object line% (send _tree dynapad)
                                    (append origin origin))
                       (sticky #t)
                       (findable #f))))
      (send this attach-to myline 'logbranch)
      (send _tree add myline)) ;adds object to logtree (group%)

    (unless _no-autolink
      (say "autolinking... " _file)
      ; automatically find/create and link end nodes
      (let* ((prev-branch (send this get-parent))
             ; the (... get-parent) will recursively generate back to tree root
             ; if parents dont yet exist
             (from (or (and prev-branch (send prev-branch to-node))
                       (send _tree root-node)))
             (to   (make-object log-treenode% _tree this)))
        (from-node from)
        (send _from-node next-branch this)
        (to-node to)
        (send _from-node add-child-after #f _to-node)))

    (define/public (dynapad) (send _tree dynapad))

    (define/public from-node (get/set _from-node))
    (define/public to-node (get/set _to-node))

    (define/public (prev-branch) (send _from-node prev-branch))
    (define/public (next-branches) (send _to-node next-branches))

    (define/public (coords . args)
      (send/apply (send this object) coords args))
    (define/public (pen . args)
      (send/apply (send this object) pen args))
    (define/public (penwidth . args)
      (send/apply (send this object) penwidth args))
    (define/public (z)
      (send (send this object) z))

    (define/override (split-at-entry state-id build-expr)
      (let* ((tail-branch (super split-at-entry state-id build-expr 'no-autolink))
             (oldnode _to-node)
             (newnode (make-object log-treenode% _tree this)))
        (set! _to-node newnode)
        (send tail-branch to-node oldnode)
        (send tail-branch from-node newnode)
        (send newnode next-branch tail-branch)
        (send oldnode prev-branch tail-branch)
        (send oldnode insert-parent newnode)
        tail-branch))

    (define/public toxy
      (case-lambda
        (() (cddr (send this coords)))
        ((xy) (let ((crds (send this coords)))
                (send this coords
                      (append (list (car crds) (cadr crds)) xy))))))

    (define/public fromxy
      (case-lambda
        (() (let ((crds (send this coords)))
              (list (car crds) (cadr crds))))
        ((xy) (let ((crds (send this coords)))
                (send this coords (append xy (cddr crds)))))))

    (define (compute-marker-xy state-id)
      (let* ((crds (send this coords))
             (undo-stack-len (- (length *undo-stack*) 1))
             (redo-stack-len (length *redo-stack*))
             (total-len (+ undo-stack-len redo-stack-len))
             (fract (if (zero? total-len) 1 (/ undo-stack-len total-len)))

             (x (lerp fract (car crds) (caddr crds)))
             (y (lerp fract (cadr crds) (cadddr crds))))
        (list x y)))

    (define/public (hilight)
      ; set appearance of current branch
      (send this pen (send _tree bright-color))
      (let ((obj (send this object)))
        (when obj (send obj raise))))
    (define/public (unhilight)
      (if _on-active-path? (lolight) (unlolight)))

    (define/public (lolight)
      ;set appearance of branches on active path
      (send this pen (send _tree mid-color))
      (send this penwidth (pixels->space (/ 3 (send this z)))))
    (define/public (unlolight)
      (send this pen (send _tree dim-color))
      (send this penwidth (pixels->space (/ 2 (send this z)))))

    (define/override (activate)
      (send _tree refresh-active-path)
      (hilight))
    (define/override (deactivate)
      (unhilight))

    (define/public (path-from-root)
      (let ((prev (prev-branch)))
        (if (not prev)
            (list this)
            (append (send prev path-from-root) (list this)))))

    (define/public (setpath)
      (let* ((branch-path (path-from-root))
             (curr-branch (send _tree current-branch))
             (found (memq curr-branch branch-path)))
        (when found
          (set-*future-log-path*! (cdr found))
          (send _tree refresh-active-path)
          (send curr-branch hilight))))

    (define/public (restore)
      (send _tree clear-active-path)
      (show-possible-delay (send this dynapad)
                           (restore-path (send this path))))

    (define/public (include-in-active-path)
      (set! _on-active-path? #t)
      (let ((obj (send this object)))
        (when obj (send obj raise)))
      (lolight))

    (define/public (exclude-from-active-path)
      (set! _on-active-path? #f)
      (unlolight))

    (exclude-from-active-path)
    (let ((obj (send this object)))
      (send obj bind "<Run-Shift-ButtonPress-1>" (lambda (ePAD e) (restore)))
      (send obj bind "<Select-Shift-ButtonPress-1>"  (lambda (ePAD e) (restore)))
      (send obj bind "<Run-ButtonPress-1>" (lambda (ePAD e) (setpath)))
      (send obj bind "<Select-ButtonPress-1>"  (lambda (ePAD e) (setpath)))
      )

    ))
