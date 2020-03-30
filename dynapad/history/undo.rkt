#lang racket/base
(require racket/class
         compatibility/mlist
         dynapad/base
         dynapad/copy
         dynapad/ffs
         dynapad/pad-state
         dynapad/import
         dynapad/undo-state
         dynapad/misc/misc ; do-deferred-evals
         dynapad/misc/alist

         dynapad/events/mode ; now stripped down

         dynapad/events/pan
         dynapad/events/draw
         ;dynapad/events/drag ; had to move it all here
         ;dynapad/events/image-events ; woo cycles!
         dynapad/events/reshape
         dynapad/utils/resize
         ;dynapad/events/text  ; seems like this isnt used directly
         ;dynapad/utils/get-user-bbox
         ;dynapad/utils/lasso

         dynapad/history/ids ; this actually works now ?!
         dynapad/history/logs
         )

(provide undoable-delete-all
         restore-set-core
         restore-set
         )

;; cyclical dependencies with mode, select, and event-shared ffs
;; sigh events as well, basically anything that touches the event
;; system and the undo system and menu_functions the tangling is
;; seriously thorough
;; force us to move them here until things can be untangled
;; the culprets are
;; show-possible-delay
;; Set-Select--undoable
;; push-event-mode
;; pop-event-mode
;; push-ops-no-exec  ; which is apparently overwriten in log.rkt sigh

;; events.rkt
;(send arg_PAD modifier 'set "Run")
;(send arg_PAD setvar 'default-mode "Select")
;(make-object event-state% arg_PAD)

;apply bindings
;(bindZoom arg_PAD)
;(bindPan arg_PAD)
;(bindZoom-for-Select arg_PAD)
;(bindHyperlink arg_PAD)
;(bindDrawMode arg_PAD)
;(bindText arg_PAD)
;(bindSelect arg_PAD)
;(bindControlKeys arg_PAD)
;(bindDrag arg_PAD)


(define (false-event-lambda o e) #f)

; gets topmost group which takes events
(define (get-top-group obj)
  (if (and (not (send obj takegroupevents))
           (send obj getgroup))
      (get-top-group (send obj getgroup))
      obj))

; gets topmost group regardless of event handling
(define (get-topmost-group obj)
  (if (send obj getgroup)
      (get-topmost-group (send obj getgroup))
      obj))

;--------------------
; A drag select action may need to "find" different objects in different
; contexts, depending on where the drag select action was initiated.
; The function "choose-appropriate-drag-find-function" can be defined to
; determine which find-function to use.  The basic find function is called
; find-enclosed-objects-standard.
#|
(define find-enclosed-objects #f)

(define find-enclosed-objects-standard
  (lambda (argPAD evnt bb) (send argPAD find 'enclosed bb)))

(define find-enclosed-objects-in-container
  (lambda (argPAD evnt bb)
    (let* ((obj (event-obj evnt)))
      (if (is-a? obj dynapad%)
          (find-enclosed-objects-standard argPAD evnt bb) ;this case shouldn't happen, but does for some reason
          (let* ((outer (get-container obj))
                 (local-objs (send outer contents))
                 (bounded-objs (send argPAD find 'groupmembers 'enclosed bb))
                 (these-objs (list-intersect local-objs bounded-objs memq)))
            these-objs)))))

(define (choose-appropriate-drag-find-function obj)
  (unless (is-a? obj dynapad%)
    (let ((shell (get-container obj))) ;may be #f, ok
      (if (is-a? shell container-form%)
          (set! find-enclosed-objects find-enclosed-objects-in-container)
          (set! find-enclosed-objects find-enclosed-objects-standard)))))

(set! find-enclosed-objects find-enclosed-objects-standard)
|#
;--------------------


(define (buttonpress-on-background? obj)
  (or
   (not obj)
   (is-a? obj dynapad%)
   ;(not (send obj findable))
   ;evaluate some callback list for objects that want to behave as background
   ))

(define basepad-event-binder%
  (class object%
    (init argPAD evs-class)
    (field (_pad argPAD))
    (super-instantiate ())

    (make-object evs-class argPAD)

    ; Undo/Redo
    (let ((undo-lambda (lambda (eventPAD e) (set-currentPAD! eventPAD) (undo)))
          (redo-lambda (lambda (eventPAD e) (set-currentPAD! eventPAD) (redo))))
      (send argPAD bind "<Control-KeyPress-z>"       undo-lambda)
      (send argPAD bind "<Control-KeyPress-y>"       redo-lambda)
      (send argPAD bind "<Control-Shift-KeyPress-Z>" redo-lambda))

    ; Copy/Paste/Cut/Duplicate
    (send argPAD bind "<Control-KeyPress-c>"
          (lambda (eventPAD e) (set-currentPAD! eventPAD) (Copy-Selected)))
    (send argPAD bind "<Control-KeyPress-v>"
          (lambda (eventPAD e) (set-currentPAD! eventPAD) (Paste-From-Copy-Buffer)))
    (send argPAD bind "<Control-KeyPress-x>"
          (lambda (eventPAD e) (set-currentPAD! eventPAD) (Copy-Selected) (send-selected delete)))
    (send argPAD bind "<Control-KeyPress-d>"
          (lambda (eventPAD e) (set-currentPAD! eventPAD) (Copy-Selected)(Paste-From-Copy-Buffer)))
    ; Delete
    (send argPAD bind "<Select-KeyPress-Delete>"
          (lambda (eventPAD e) (Delete-Selected)))
    (send argPAD bind "<Select-KeyPress-BackSpace>"
          (lambda (eventPAD e) (Delete-Selected)))
    (send argPAD bind "<Run-KeyPress-Delete>"
          (lambda (eventPAD e) (Delete-Selected)))
    (send argPAD bind "<Run-KeyPress-BackSpace>"
          (lambda (eventPAD e) (Delete-Selected)))
    ; Deep Delete
    (send argPAD bind "<Select-Shift-KeyPress-Delete>"
          (lambda (eventPAD e) (Deep-Delete-Selected)))
    (send argPAD bind "<Select-Shift-KeyPress-BackSpace>"
          (lambda (eventPAD e) (Deep-Delete-Selected)))
    (send argPAD bind "<Run-Shift-KeyPress-Delete>"
          (lambda (eventPAD e) (Deep-Delete-Selected)))
    (send argPAD bind "<Run-Shift-KeyPress-BackSpace>"
          (lambda (eventPAD e) (Deep-Delete-Selected)))
    ; Grow/shrink
    (send argPAD bind "<Run-KeyPress-b>"
          (lambda (d e) (make-selected-bigger)))
    (send argPAD bind "<Run-KeyPress-s>"
          (lambda (d e) (make-selected-smaller)))
    ; Arrows
    (bindArrows argPAD) ;pan.ss
    ; Dragging
    (bindDrag argPAD) ;drag.ss

    (sendf argPAD evs selector-color "red")   ; i.e. marquee selection rectangle

    ))
;; end events.rkt

;; event-shared.rkt
(define Start-Shift-Select-Event
  (lambda (eventPAD e)
    (set-currentPAD! eventPAD)
    (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
    (sendf eventPAD evs set-sx0sy0 (event-sx e) (event-sy e))
    ;    (choose-appropriate-drag-find-function (event-obj e))
    (sendf eventPAD evs obj0 (event-obj e))
    (push-event-mode eventPAD "BBox")
    #f
    ))


(define Start-Drag/Select-Event
  (lambda (eventPAD e)
    (set-currentPAD! eventPAD)
    (let ((obj (event-obj e)))
      (sendf eventPAD evs set-last-xy (event-x e) (event-y e))
      ;      (sendf eventPAD evs x0 (event-x e))
      ;      (sendf eventPAD evs y0 (event-y e))
      (sendf eventPAD evs set-sx0sy0 (event-sx e) (event-sy e))

      (if (buttonpress-on-background? obj)
          (begin ; begin dragging select rect...
            (Start-Changing-Select--undoable eventPAD)
            (send eventPAD selected null)
            ;        (choose-appropriate-drag-find-function obj)
            (sendf eventPAD evs obj0 obj)
            (push-event-mode eventPAD "BBox")
            )
          (begin ;else begin dragging object(s)
            ;get parent group if it exists. (and then the group above that, etc.)
            (set! obj (get-top-group obj))
            (if (send obj selected?)
                ; dragging selected object(s)
                (Start-Undoable-Drag eventPAD e (send eventPAD selected))
                ; else dragging fresh obj --> unselect
                (begin
                  (Start-Changing-Select--undoable eventPAD)
                  (send eventPAD selected null)
                  (Start-Undoable-Drag eventPAD e (list obj)))))))
    #t;    #f
    ))

(define End-Select-Event
  (lambda (eventPAD e)
    (set-currentPAD! eventPAD)
    (let (;(x (event-x e))
          ;(y (event-y e))
          ;(obj (event-obj e))
          ;(last_x (sendf eventPAD evs lastx))
          ;(last_y (sendf eventPAD evs lasty))
          (selector (sendf eventPAD evs update-selector e)))
      (when selector
        ; end select rect
        (send eventPAD selected
              (sendf eventPAD evs selector-contains))
        ;          (find-enclosed-objects eventPAD e (list last_x last_y x y)))
        (sendf eventPAD evs selector-delete))
      (Done-Changing-Select--undoable eventPAD)
      (if (selection-changed? eventPAD)
          (push-ops-no-exec)
          (abort-undo/redo)) ;ignore if no change
      )
    ;      (begin ; else end bkgnd click
    ;        (Done-Changing-Select--undoable eventPAD)
    ;(if (not (buttonpress-on-background? obj))
    ;    (say "Special case: obj unclick"))
    ;        (if (and (buttonpress-on-background? obj) ;redundant?
    ;             (not (selection-changed? eventPAD)))
    ;        (abort-undo/redo) ;ignore if bg click and no change
    ;        (push-ops-no-exec)))))
    (pop-event-mode eventPAD "BBox")
    #t;    #f
    ))

(define End-Drag-Event
  (lambda (eventPAD evnt)
    (let ((draglist (send eventPAD getvar 'dragset)))
      (store-drag-batch-for-redo eventPAD evnt draglist)
      (Finish-Undoable-Drag eventPAD evnt draglist)
      (Done-Changing-Select--undoable eventPAD)
      (push-ops-no-exec)
      )))

(define End-Shift-Select-Event
  (lambda (eventPAD e)
    (set-currentPAD! eventPAD)
    (let (;(x (event-x e))
          ;(y (event-y e))
          ;(last_x (sendf eventPAD evs lastx))
          ;(last_y (sendf eventPAD evs lasty))
          (obj (event-obj e))
          (selector (sendf eventPAD evs update-selector e)))
      (if (and selector
               (sendf eventPAD evs moved-far-enough? e))
          ; dragged selection rect...
          (begin
            (Start-Changing-Select--undoable eventPAD)
            (map (lambda (o) (if (send o selected?)
                                 (send o unselect)
                                 (send o select)))
                 (sendf eventPAD evs selector-contains))
            ;(send selector contained-objects obj))
            ;         (find-enclosed-objects eventPAD e (list last_x last_y x y)))
            (Done-Changing-Select--undoable eventPAD)
            (if (selection-changed? eventPAD)
                (push-ops-no-exec)
                (abort-undo/redo))
            (sendf eventPAD evs selector-delete))
          ; else single object (or background):
          (begin
            (sendf eventPAD evs selector-delete)
            (let ((obj (event-obj e)))
              (when (and (not (is-a? obj dynapad%)) (send obj findable))
                (set! obj (get-top-group obj))
                (Start-Changing-Select--undoable eventPAD)
                (if (send obj selected?)
                    (send obj unselect)
                    (send obj select))
                (Done-Changing-Select--undoable eventPAD)
                (push-ops-no-exec)
                ))))
      (pop-event-mode eventPAD "BBox")
      #t;    #f
      )))
;; end event-shared.rkt
;; select.rkt
(define (selection-changed? argPAD)
  (let ((old (send argPAD getvar 'old-selection))
        (new (send argPAD selected)))
    (not (equal? old new))))

(define Update-Select-Marquee-With-Motion
  (lambda (eventPAD e)
    (set-currentPAD! eventPAD)
    (sendf eventPAD evs if-not-exists-make-selector-from-lastxy)
    (sendf eventPAD evs update-selector e) ))

(define (bindSelect argPAD modestr)
  ; modestr is either "Select" or "Run"

  (send argPAD bind (format "<~a-ButtonPress-1>" modestr)
        Start-Drag/Select-Event) ;see event-shared.ss

  (send argPAD bind "<BBox-B1-Motion>"
        Update-Select-Marquee-With-Motion)

  (send argPAD bind "<BBox-ButtonRelease-1>"
        End-Select-Event)

  (send argPAD bind (format "<~a-Shift-ButtonPress-1>" modestr)
        Start-Shift-Select-Event)

  (send argPAD bind "<BBox-Shift-B1-Motion>"
        Update-Select-Marquee-With-Motion)

  (send argPAD bind "<BBox-Shift-ButtonRelease-1>"
        End-Shift-Select-Event)
  )
;; end select.rkt
;; mode.rkt
;======= Change Modifier ========
;; end mode.rkt

;; save and delete

; New:
(define undoable-delete-all
  (case-lambda
    ((argPAD) (undoable-delete-all argPAD (saveable-objects argPAD)))
    ((argPAD objs)
     (undoable-delete-objs objs #t))))
;    (foreach objs (lambda (o) (send o delete-all))))))

;--- Load contexts : restore, import ------------

; Importing: remap all ids, keep padid counter
; Restoring: advance padid counter>max, keep all ids

(define (safe-eval expr)
  ;serves as a reentry point in case of errors during eval
  (with-handlers
    ;   ([exn:user? (lambda (exn)
    ([exn:fail? (lambda (exn)
                  (foreach (current-error-ports)
                           (lambda (port)
                             (fprintf port  "Error (~a) in ~a~%" (exn-message exn) expr)))
                  #f)])
    (eval expr)))

(define-syntax restore-set-core
  (syntax-rules ()
    ((_ expr ...)
     (show-possible-delay currentPAD
                          (let* ((objs (filter (lambda (o) (is-a? o object%))
                                               (map safe-eval (list expr ...)))))
                            (do-deferred-evals *id->obj-index*)
                            objs)))))

(define-syntax import-set-core
  (syntax-rules ()
    ((_ expr ...)
     (show-possible-delay currentPAD
                          (let* ((objs (filter (lambda (o) (is-a? o object%))
                                               (map safe-eval (list expr ...))))
                                 ;build temp id->obj map:
                                 (idmap  (filter ;omit objs with no id
                                          (lambda (pair) pair)
                                          (map
                                           (lambda (o) (let ((i (send o wasid)))
                                                         (and i (cons i o))));(list i o))))
                                           objs))))
                            (do-deferred-evals (make-immutable-hash-table idmap))
                            objs)))))

(define (import-path path)
  (use-load-context 'import
                    (load path))  ; file at path should call (load-set ...)
  path)

(define-syntax load-set
  (syntax-rules ()
    ((_ obj ...)
     (if (importing?)
         (import-set-core obj ...)
         (restore-set-core obj ...)))))

;these set load-context if restore/import-path are bypassed
(define-syntax import-set
  (syntax-rules ()
    ((_ expr ...)
     (use-load-context 'import
                       (import-set-core expr ...)))))

(define-syntax restore-set
  (syntax-rules ()
    ((_ expr ...)
     (use-load-context 'restore
                       (restore-set-core expr ...)))))

(define (undo) ;overridden in logs.ss
  (when (not (null? *undo-stack*))
    (let ((do_undo_pair (pop-*undo-stack*!)))
      ; execute undo op
      (restore-set (cadr do_undo_pair))
      (push-*redo-stack*! do_undo_pair)
      )
    )
  )

(define (redo) ;overridden in logs.ss
  (when (not (null? *redo-stack*))
    (let ((do_undo_pair (pop-*redo-stack*!)))
      ; execute redo op
      (restore-set (car do_undo_pair))
      (push-*undo-stack*! do_undo_pair)
      )
    )
  )

(define *beforedrag-prepare-undo-callbacks* null)
; This is NOT equivalent to a dynapad's _beforedrag-callbacks
;  Those are executed every time a drag is replayed off the redo stack;
;  THESE are executed only during the initial user-action,
; and prepare any conditions to restore pre-drag state
; onto the undo-stack

(define beforedrag-prepare-undo-callbacks
  (callback-accessor-functions *beforedrag-prepare-undo-callbacks*))

(define (do-beforedrag-prepare-undo-callbacks argPAD evnt objs)
  (foreach  *beforedrag-prepare-undo-callbacks*
            (lambda (cb) ((car cb) argPAD evnt objs))))

(define *afterdrag-prepare-undo-callbacks* null)
(define afterdrag-prepare-undo-callbacks
  (callback-accessor-functions *afterdrag-prepare-undo-callbacks*))
(define (do-afterdrag-prepare-undo-callbacks argPAD evnt objs)
  (foreach  *afterdrag-prepare-undo-callbacks*
            (lambda (cb) ((car cb) argPAD evnt objs))))
;==============================================
; OBJECT REGISTRATION:
; For redoable-creation,
; created obj must record its new id and reuse that id
;  every time it is re-created.


;Therefore, the do-op (expr on the redo stack)
; cannot contain a raw (make-object...);
; instead, it must contain a (rgstr-obj NEW-id '(build-str...))
;  Use macros below instead of (make-object...)
;  for pushing obj-creation do-ops onto stack.

;Naming conventions:
;pre-hoc:
; (undoable-... <expr>) has not yet eval'd expr (e.g. to create/delete objs)
;post-hoc:
; (undoify- ... <objs>) takes already-created objs, makes creation re/undoable

#|
(define-syntax undoable-make
  (syntax-rules ()
    ((_ cmd ...)
     (let ((newid (fresh-obj-id)))
       (push-ops-and-exec `(ic (,cmd ...) (id ,newid))
                          `(nuke-obj ,newid))))))

(define-syntax undoable-make-set
  ;args are series of object-making expressions
  ; e.g. (undoable-make-set '(make-object...) '(make-object...))
  (syntax-rules ()
    ((_ cmd ...)
     (let* ((do-undo-pairs
             (map (lambda (expr)
                    (let ((newid (fresh-obj-id)))
                      (list `(ic ,expr (id ,newid))
                            `(nuke-obj  ,newid))))
                  `(,cmd ...)))
            (dos (cons 'begin (map car do-undo-pairs)))
            (undos (cons 'begin (map cadr do-undo-pairs))))
       (push-ops-and-exec dos undos)))))
|#
;=============

;-----------------------------------------------------------------
; Example 1 - slide command
;
; (define (gui-slide-objects object_list dx dy)
;   (let ((do-op   (lambda () (send-nd-objs object_list slide dx dy)))
;         (undo-op (lambda () (send-nd-objs object_list slide (- dx) (- dy))))
;         )
;     (push-ops-and-exec do-op undo-op) ))

;-----------------------------------------------------------------
; Example 2 - fill command
;
;(define (gui-fill-objects object_list newcolor)
;  (let* ((color_list (map (lambda (o) (send o fill)) object_list))
;         (undo-op (lambda () (fill-objects-aux object_list color_list)))
;         (do-op   (lambda () (fill-objects-aux object_list newcolor)))
;         )
;    (push-ops-and-exec do-op undo-op) ))
;
;(define (fill-objects-aux object_list color_or_colors)
;  (if (list? color_or_colors)
;    (for-each
;      (lambda (obj clr) (send-nd obj fill clr))
;      object_list
;      color_or_colors)
;
;    (for-each
;      (lambda (o) (send-nd o fill color_or_colors))
;      object_list)))


;-----------------------------------------------------------------
; Example 3 - delete command (move to "trash")
;
; (define (gui-delete-objects object_list)
;   (let ((do-op   (lambda () (delete-objects object_list)))
;         (undo-op (lambda () (undelete-objects object_list)))
;         )
;     (push-ops-and-exec do-op undo-op) ))
;
; (define  main_layer (send dynapad main-layer))
; (define trash_layer (make-object layer% dynapad "trash"))
; (send trash_layer visible #f)
;
; (define (delete-objects object_list)
;   (foreach object_list (lambda (o)
;     (send-nd o layer trash_layer)
;     (send-nd o unselect))))
; (define (undelete-objects object_list)
;   (foreach object_list (lambda (o)
;     (send-nd o layer main_layer)
;     (send-nd o select))))
; (define (empty-trash)
;   (foreach (send trash_layer members) (lambda (o) (send-nd o delete))))

;-----------------------------------------------------------------
; Example 4 - drag operation via mouse input
;

;(define *drag-do-op* #f)
;(define *drag-undo-op* #f)

; This function generates a closure which iterates
; over a list of individual closures.
; Is there a different, better way to do this?
;(define (gen-closure-to-reposition-objects object_list)
;  (define cmds_to_restore
;    (map
;      (lambda (o)
;        (let ((oldpos (send-nd o position)))
;          (lambda () (send-nd o position oldpos))))
;      object_list))
;
;  (new-lambda () (foreach cmds_to_restore (lambda (c) (c))))
;)
;
;(define (gen-closure-to-reposition-objects object_list)
;  (cons 'begin
;    (map (lambda (obj)
;           (let ((oldpos (send-nd obj position)))
;         `(send ,obj position (list ,@oldpos))))
;         object_list)))
;
;(define (store-positions-of-selected-objects-for-undo argPAD)
;  (set! *drag-undo-op* (gen-closure-to-reposition-objects (send argPAD selected))) )
;
;(define (store-positions-of-selected-objects-for-redo-and-push-on-stack argPAD)
;  (set! *drag-do-op* (gen-closure-to-reposition-objects (send argPAD selected)))
;  (push-ops-and-exec *drag-do-op* *drag-undo-op*) )


;(define (store-positions-of-specified-object-for-undo argPAD obj)
;  (set! *drag-undo-op* (gen-closure-to-reposition-objects (list obj))) )

;(define (store-positions-of-specified-object-for-redo-and-push-on-stack argPAD obj)
;  (set! *drag-do-op* (gen-closure-to-reposition-objects (list obj)))
;  (push-ops-and-exec *drag-do-op* *drag-undo-op*) )

;(define (store-positions-of-specified-object-for-redo-and-push-on-stack argPAD obj)
;  (set! *drag-do-op* (gen-closure-to-reposition-objects (list obj)))
;  (push-ops-and-exec *drag-do-op* *drag-undo-op*) )

; The above functions are incorporated into the binding definitions for
; "<Select-ButtonPress-1>" and "<Drag-ButtonRelease-1>"
; (see events.ss and event-binders.ss)

;====== Traps for non-logging =======
(define (load-log . args)
  (error "Must include logs.ss to read log files"))

; some copy stuff tainted by Set-Select--undoable

(define (Paste-From-Copy-Buffer)
  (let* ((newobjects (eval `(import-set ,@*copy_buffer*)))
         (topobjects (filter (lambda (o) (not (send o getgroup))) newobjects)))
    (for-each Offset-Object topobjects)
    ;  (Unselect-Selected--undoable)
    ;  (map (lambda (o)(send o unselect)) (send currentPAD selected))
    ;  (for-each (lambda (o) (send o select)) topobjects)
    (Set-Select--undoable currentPAD topobjects)
    (Copy-Selected) ;recopy new objects to propagate offset
    (undoify-fresh-objs newobjects)
    ))

(define (Copy-Paste-ReSelect)
  ; when triggered (e.g. by Ctrl-Click to select),
  ;  copies selected obj, and makes that the new selection for dragging, etc.
  (Copy-Selected)
  (Paste-From-Copy-Buffer))
(define (using-another-pad--Paste-From-Copy-Buffer argPAD)
  (let ((tmp currentPAD)
        (newobjects '()))
    (set! currentPAD argPAD)
    (set! newobjects (Paste-From-Copy-Buffer))
    ;UNDO HERE?
    (foreach newobjects (lambda (o) (send o unselect)))
    (changemode argPAD "Run")
    (set! currentPAD tmp)
    )
  )

(define (using-another-pad--Copy-Selected argPAD)
  (let ((tmp currentPAD))
    (set! currentPAD argPAD)
    (Copy-Selected)
    ;UNDO HERE?
    (set! currentPAD tmp)
    )
  )
;-----

; === Deleting ===

(define (Delete-Selected)
  (let ((l (filter (lambda (x) (send x deletable?)) (send currentPAD selected))))
    (undoable-delete-objs l)))

(define (Deep-Delete-Selected)
  (let ((l (filter (lambda (x) (send x deletable?)) (send currentPAD selected))))
    (undoable-delete-objs l #t)))

(define (Clear-Workspace . stuff)  ;may be overridden in logs.ss
  (apply undoable-delete-all currentPAD stuff))

(define (Confirm-and-Delete-All)
  (let
      ((l (filter (lambda (x) (send x deletable?))
                  (saveable-objects currentPAD)))) ;include abstract objs
    ;(send currentPAD objects))))
    (cond
      ((null? l)
       (message-box "Delete All" "Nothing to delete" *menubar* '(ok)))
      (else
       (when (eq? 'ok (message-box "Delete" "Delete Everything?" *menubar* '(ok-cancel)))
         ;(undoable-delete-objs l)
         ;(destroy-and-keep-destroying-until-everything-is-gone currentPAD)
         (Clear-Workspace l)
         )))))

;OBSOLETE?
; A quick fix for a policy problem.
; Regions partially ignore the standard delete command,
; so keep sending delete messages until everything is gone.
;(define (destroy-and-keep-destroying-until-everything-is-gone argPAD)
;  (def bound 20)
;  (while (and (> bound 0) (pair? (send argPAD objects)))
;    (for-each (lambda (x) (send x delete)) (send argPAD objects))
;      (-= bound 1)))


;; tainted from drag ... is everything :/

(define (Start-Drag argPAD evnt obj-list)
  (push-event-mode argPAD "Drag")
  (for-each (lambda(x)(send x physics? #f)) obj-list)
  (send argPAD setvar 'dragset obj-list)
  (send argPAD setvar 'dragset-bbox (bbunion-objects obj-list))
  (send argPAD init-drag evnt obj-list)
  (send argPAD do-beforedrag-callbacks evnt obj-list)
  (let ((drag-lyr (send argPAD getvar 'drag-layer))
        (main-lyr (send argPAD main-layer)))
    (when drag-lyr
      (foreach obj-list
               (lambda (o) (when (and (not (send o getgroup))
                                      (eq? (send o layer) main-lyr))
                             (send o raise)
                             (send o layer drag-lyr))))))

  )

(define (Start-Undoable-Drag argPAD evnt obj-list)
  (do-beforedrag-prepare-undo-callbacks argPAD evnt obj-list)
  (Start-Drag argPAD evnt obj-list)
  (store-drag-batch-for-undo argPAD evnt obj-list)
  )

(define (Finish-Undoable-Drag argPAD evnt obj-list)
  (Finish-Drag argPAD evnt obj-list)
  (do-afterdrag-prepare-undo-callbacks argPAD evnt obj-list)
  )


(define (Finish-Drag argPAD evnt obj-list)
  (pop-event-mode argPAD "Drag")
  (for-each (lambda(x)(send x physics? #t)) obj-list)
  (send argPAD do-afterdrag-callbacks evnt obj-list)
  ; maybe drop objs from drag layer:
  (let ((drag-lyr (send argPAD getvar 'drag-layer))
        (main-lyr (send argPAD main-layer)))
    (when drag-lyr
      (foreach obj-list
               (lambda (o) (when (and (not (send o getgroup))
                                      (eq? (send o layer) drag-lyr))
                             (send o layer main-lyr)
                             (send o raise))))))
  )



; drag-batch is for replicating gui-style drag operations. (see undo.ss)
(define (drag-batch eventPAD evnt obj-list dest-list)
  (Start-Drag eventPAD evnt obj-list)
  ; NOTE: pick-up and drop events both currently use same evnt
  ; ideally should include both events as separate args
  ;  (for-each (lambda (o psn)
  ;             (send o position psn))
  ;            obj-list dest-list)
  ;  (Finish-Drag eventPAD evnt obj-list))
  (animate-batch-then-do obj-list dest-list 600
                         (lambda () (Finish-Drag eventPAD evnt obj-list)))
  )
;WARNING:
;  this version animates moves, but is non-blocking:
;  returns control to user before animation and Finish-Drag are complete;
;  therefore preemptive actions which don't wait for anim to finish
;  may corrupt workspace state.


;(define (undrag-batch eventPAD evnt

(define (bindDrag argPAD)
  (send argPAD bind "<Drag-B1-Motion>"
        (lambda (eventPAD e) (set-currentPAD! eventPAD)
                (let* ((last_x (sendf eventPAD evs lastx))
                       (last_y (sendf eventPAD evs lasty))
                       (x (event-x e))
                       (y (event-y e))
                       )
                  (when (and last_x last_y)
                    (let ((dx (- x last_x))(dy (- y last_y)))
                      (send eventPAD do-duringdrag e)
                      (for-each
                       (lambda (o) (send o slide dx dy))
                       (send eventPAD getvar 'dragset) )))
                  (sendf eventPAD evs set-last-xy x y)
                  )))

  ;  (send argPAD bind "<Drag-ButtonRelease-1>"
  ;        End-Drag-Event)
  (send argPAD bind "<Drag-ButtonRelease-1>"
        (lambda (eventPAD e)
          (when (member "Drag" (send eventPAD eventModeStack))
            (End-Drag-Event eventPAD e))))

  )

#|
; example of drag callback
(send argPAD afterdrag-callbacks 'add
      (lambda (eventPAD event screendistance draglist)
        (if (> screendistance 10)
            (printf "~a objects moved.~%" (length draglist))
            ;else
            (printf "not a complete move.~%"))))
|#

;; from ids

(define *debug-deferred-exprs* null) ;delete this later
(define (do-deferred-evals idmap)
  (set! *debug-deferred-exprs* (deferred-exprs)) ;delete this later
  (mmap
   ;(lambda (e) (printf "~a~%" e) (eval e))
   eval
   (mmap (lambda (expr) (import-expr-with-objs expr idmap))
         (order-by-phase (deferred-exprs)))))

