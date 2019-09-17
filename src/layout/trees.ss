; tree layout algorithm stolen from:
; http://archive.ncsa.uiuc.edu/SDG/IT94/Proceedings/Searching/doemel/subsection3_2_3.html

(dynaload "actor.ss")

; A set of treenode% actors manage the 2D layout of a tree.
; Each node actor may be attached to an actual dynaobject node or not,
; and updates the position of that node plus maybe its in/out edges, if any


(define treenode%
  ; A treenode% represents only a RELATIVE layout, maintaining topology and
  ;   values of depth and column for each node.
  ; See also spatial-treenode% below
  (class named-actor%
    (init _obj)

    ;(init-field (_link-fn (lambda (from to) #f)))
    ; if provided, (lambda (from to) ...) constructs an arc
    ; from parent's obj to child's obj

    ;(init-field (_relink-fn (lambda (old-parent new-parent child) #f)))
    ; if provided, (lambda (child) ...) updates links from old-parent
    ;  to child when new-parent is inserted between them.
    ; May either relink OP->C as OP->NP and add NP->C
    ; or relink OP->C as NP->C and add OP->NP

    (field (_parent #f)
           ;(_uplink #f)   ;edge-object upward to parent, if any
           (_children null)
           (_depth 0)
           (_column 0))

    (define/public parent (get/set _parent))
    (define/public depth (get/set _depth))
    (define/public column (get/set _column))
    (define/public children (get/set _children))
    ;(define/public uplink (get/set _uplink))

    (define/public (rightmost-descendant)
      (if (null? _children)
          this
          (let ((rightmost-child (list-ref _children (- (length _children) 1))))
            (send rightmost-child rightmost-descendant))))

    (define/public (drop-family)
      (send this depth (+ 1 _depth))
      (foreach _children (lambda (c) (send c drop-family))))

    (define/public (insert-parent new)
      (when _parent
        (let ((my-cell-at-parent (memq this (send _parent children))))
          (set-car! my-cell-at-parent new)))
      (send new depth _depth)
      (send new column _column)
      (send new parent _parent)
      (send new children (list this))
      (send this parent new)
      (send this drop-family))
    ;     (_relink-fn oldparent new this)))

    (define/public (add-child-after sib new)
      (let* ((sib-and-later (memq sib _children))
             (later (if sib-and-later
                        (cdr sib-and-later)
                        _children)))
        (send new parent this)
        (send new depth (+ 1 _depth))
        (if (not sib-and-later)
            (begin ;leftmost child
              (push! new _children)
              (send new column _column)
              (unless (null? later) ; unless only child
                (send this nudge-family-after later)))
            (begin ;right of sib
              (set-mcdr! sib-and-later (cons new later))
              (send new column (+ 2 (send
                                     (send sib rightmost-descendant)
                                     column)))
              (send this nudge-family-after later)))
        ;     (send new uplink (_link-fn (send this object) (send new object)))
        ))

    (define/public (nudge-family-by-2)
      (send this column (+ _column 2))
      (foreach _children (lambda (child) (send child nudge-family-by-2)))
      )

    (define/public (nudge-family-after child)
      (let ((laters (if (list? child)
                        child
                        (cdr (memq child _children)))))
        (foreach laters (lambda (sib) (send sib nudge-family-by-2)))
        (send this column (+ 1 _column))
        (when _parent (send _parent nudge-family-after this))
        ))

    (super-instantiate ())
    (when _obj (send this attach-to _obj 'treenode))
    ))


(define spatial-treenode%
  ; A spatial-treenode% adds a geometry (dx/dy scale and orientation)
  ; and converts relative depth and column of a node into x-y coords
  ; Whenever nodes are added or moved, calls (_update-fn node x y)
  ;  where x and y are the new coords.
  ; _update-fn might, for example, move a dynaobject% node or edge the
  ; spatial-treenode% actor belongs to.
  (class treenode%
    (init _obj)
    (init-field _update-fn)
    ; update-fn (lambda (node x y) ...)
    ; sends reposition to object and other objs (eg edges)
    (init-field _vertical?) ; #t for vertical, #f for horizontal

    (init-field
     _depth-gap      ; if vertical: <0 grows downward, >0 upward
     ;    horizonal: <0 grows leftward, >0 rightward
     _column-gap)    ; if vertical: <0 adds leftward, >0 rightward
    ;    horizontal: <0 adds downward, >0 upward

    (super-instantiate (_obj))
    ;     (send this coords (list (- _ox _rad) (- _oy _rad)
    ;                 (+ _ox _rad) (+ _oy _rad)))

    (define/public (refresh)
      (let ((crds (list
                   (* _depth-gap  (super depth))
                   (* _column-gap (super column)))))
        (when _vertical?
          (set! crds (reverse crds)))
        (apply _update-fn this crds)))

    (define/public (refresh-family)
      (send this refresh)
      (foreach (send this children) (lambda (c) (send c refresh-family))))

    (define/override column
      (case-lambda
        (() (super column))
        ((c) (super column c)
             (refresh))))

    (define/override depth
      (case-lambda
        (() (super depth))
        ((c) (super depth c)
             (refresh))))
    ))

#|
(dynaload "edges.ss")

(define (make-treenode)
  (let* ((obj (make-object oval% dynapad (list -5 -5 5 5)))
         (node (make-object spatial-treenode%
                            obj
                            (lambda (node x y)
                              (send (send node object) xy x y))
                            ;            (lambda (f t) (make-object graph-arc% dynapad f t))
                            ;            (lambda (old-par new-par chld)
                            ;              (send (send chld uplink) from (send new-par object))
                            ;              (send new-par uplink
                            ;                (make-object graph-arc% dynapad
                            ;                     (send old-par object)
                            ;                     (send new-par object))))
                            #t
                            -20 10)))
    node))
|#
