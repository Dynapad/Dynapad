#lang racket/base

(require racket/class
         dynapad/spd ; XXX breaks debugability
         dynapad/pad-state
         dynapad/misc/misc
         dynapad/layout/bbox
         (only-in dynapad/events/image-events set-image-hires-list *list_of_hirez_images*)
         dynapad/events/zoom-pushpull)

; enable automatic hires when hit zoom limit

;requires event-binders-new.ss

(access-hit-near-zoomlimit-callbacks
 'add
 (lambda (argPAD evnt)
   (let ((target (event-obj evnt)))
     (when (and (has-method? target 'hires?)
                ; is target big enough in view?
                (let* ((padbb (send argPAD bbox))
                       (imgbb (send target bbox)))
                  (<= .8 (max (/ (bbwidth imgbb) (bbwidth padbb))
                              (/ (bbheight imgbb) (bbheight padbb)))))
                ; already hires?
                (not (memq target *list_of_hirez_images*)))
       (show-possible-delay (send target dynapad)
                            (set-image-hires-list (list target)))
       )))
 'hit-near-zoomlimit)
