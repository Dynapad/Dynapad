
(define (selection-changed? argPAD)
  (let ((old (send argPAD getvar 'old-selection))
    (new (send argPAD selected)))
    (not (equal? old new))))

; For one-step selection changes:
(define (Set-Select--undoable argPAD set)
  (when (not (list? set)) (set! set (list set)))
    (Start-Changing-Select--undoable argPAD)
    (send argPAD selected set)
    (Done-Changing-Select--undoable argPAD set))

; For two-step (i.e. button down-->up) changes:
(define (Start-Changing-Select--undoable argPAD . oldset)
  (let ((old (if (null? oldset)
         (send argPAD selected)
         (car oldset))))
    (send argPAD setvar 'old-selection old)
    (store-selection-for-undo old)))

(define (Done-Changing-Select--undoable argPAD . newset)
  (let ((new (if (null? newset)
         (send argPAD selected)
         (car newset))))
    (store-selection-for-redo new)))

(define Update-Select-Marquee-With-Motion
  (lambda (eventPAD e)
    (set! currentPAD eventPAD)
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
