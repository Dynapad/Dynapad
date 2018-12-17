(require (lib "class.ss"))
(dynaload "undo-hooks.ss")
(dynaload "menu_functions.ss")

(dynaload "event-state.ss")
(dynaload "event-shared.ss")

(dynaload "mode.ss")


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

(dynaload "pan.ss")
(dynaload "drag.ss")
(dynaload "select.ss")
(dynaload "image-events.ss")
(dynaload "reshape.ss")
(dynaload "resize.ss")
(dynaload "text.ss")

(dynaload "get-user-bbox.ss")
(dynaload "lasso.ss")

(define basepad-event-binder%
  (class object%
    (init argPAD evs-class)
    (field (_pad argPAD))
    (super-instantiate ())

    (make-object evs-class argPAD)

     ; Undo/Redo
     (let ((undo-lambda (lambda (eventPAD e) (set! currentPAD eventPAD) (undo)))
	   (redo-lambda (lambda (eventPAD e) (set! currentPAD eventPAD) (redo))))
       (send argPAD bind "<Control-KeyPress-z>"       undo-lambda)
       (send argPAD bind "<Control-KeyPress-y>"       redo-lambda)
       (send argPAD bind "<Control-Shift-KeyPress-Z>" redo-lambda))

     ; Copy/Paste/Cut/Duplicate
     (send argPAD bind "<Control-KeyPress-c>"
	   (lambda (eventPAD e) (set! currentPAD eventPAD) (Copy-Selected)))     
     (send argPAD bind "<Control-KeyPress-v>"
	   (lambda (eventPAD e) (set! currentPAD eventPAD) (Paste-From-Copy-Buffer)))
     (send argPAD bind "<Control-KeyPress-x>"
	   (lambda (eventPAD e) (set! currentPAD eventPAD) (Copy-Selected) (send-selected delete)))     
     (send argPAD bind "<Control-KeyPress-d>"
	   (lambda (eventPAD e) (set! currentPAD eventPAD) (Copy-Selected)(Paste-From-Copy-Buffer)))
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
