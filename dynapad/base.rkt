#lang racket/base

; FIXME tab completion while running this causes everything to freeze

(provide dynapad
         ; things needed for calls to eval
         import-set
         ObjID

         load-log
         start-state
         max-padid
         set-max-padid
         load-set

         created-by
         visit-state
         visit-start
         change-view
         ;#;
         importing?
         ic
         defer-send
         #;
         do-deferred-evals
         (all-from-out dynapad/image)
         (all-from-out dynapad/pad-state)
         (all-from-out dynapad/undo-state)
         (all-from-out dynapad/objects)
         (all-from-out dynapad/utils/graph)
         (all-from-out dynapad/events/text)
         (all-from-out racket/class) ; needed for some top-level calls to send
         )

; things that need to be at the top level for reasons I don't entirely understand
; (require dynapad/pad-state) for the event struct for oval

(require
 (only-in racket/class send make-object)
 (only-in racket/gui/base
          make-eventspace
          current-eventspace
          )
 ;compatibility/mlist
 #;
 (only-in dynapad/base
          make-new-dynapad)
 dynapad/objects ; all provided forms need to be available here
 ; otherwise various calls to eval will fail with
 ; undefined errors
 dynapad/pad-state
 dynapad/dynapad-c-api

 ; used in top level evals
 (only-in dynapad/copy import-set)
 (only-in dynapad/history/undo load-set)
 (only-in dynapad/history/ids
          ObjID
          max-padid
          set-max-padid
          )
 (only-in dynapad/history/logs
          load-log
          start-state
          )

 (only-in dynapad/history/log-state
          created-by
          visit-state
          visit-start
          change-view
          )
 ;#; ; not the whole issue here it seems
 (only-in dynapad/import
          importing?)
 #;
 (only-in dynapad/history/deferred-evals do-deferred-evals)
 (only-in dynapad/misc/misc ic)
 dynapad/image
 (only-in dynapad/ffs defer-send)

 (only-in dynapad/utils/graph graph-edge% graph-arc%)
 (only-in dynapad/events/text text%)
 (only-in dynapad/undo-state ; needed for restore-log-branch
          newview
          oldview
          oldbb
          newbb  
          zoomtarget
          zoomobject)
 )

(current-eventspace (make-eventspace))

(define (announce-module-loading name) #f)
(define (update-progress pct) #f)
; anytime after here, once progress.ss is included, loading modules (if so coded)
; will announce themselves and generate "..." progress bar to stdout

#; ; no longer needed I think
(current-library-collection-paths
 (cons
  (build-path *dynapad-directory* "dynapad-collects")
  (current-library-collection-paths)))

;-----------------------------------------------------------------
(require ;dynapad/utils/alias
         ;dynapad/utils/formation
         ;dynapad/utils/lambdas
         ;dynapad/events/events
         dynapad/history/undo  ; events bits were moved to undo for now
         dynapad/history/log-state
         (only-in dynapad/history/logs
                  start-new-history)
         ;dynapad/layout/bbox
         ;dynapad/layout/arrange
         ;dynapad/misc/command-shortcuts ; may want to reprovide these or set them up for #lang dynapad
         dynapad/misc/user-preferences
         dynapad/menu/menubar
         dynapad-collects/misc/pathhack)

;(dynaload "alias.ss")               ; allows aliasing of pathnames
;(dynaload "events.ss")              ; basic events plus draw-application events)
;(dynaload "bbox.ss")                ; bounding box utilities
;(dynaload "arrange.ss")             ; utilities for layout and arrangement of objects
;arrangeimages is now obsolete, replaced by import-dirs.ss
;(dynaload "arrangeimages.ss")       ; module for load all images from a directory
;(dynaload "formation.ss")           ; base mechanism for defining "rich objects"
;(dynaload "lambdas.ss")
;(require new-lambda)
;(dynaload "command-shortcuts.ss")   ; shortcut functions (mostly for programmers)
;(dynaload "user-preferences.ss")    ; a mechanism for saving characteristics of the dynapad environment.

;-----------------------------------------------------------------

;; this has to be done here because other stages of init
;; e.g. log-state.rkt expects the pads to already exist
;; at compile time, this is probably a design flaw
(void
 (set-initial-pad-state! (make-new-dynapad ".dynapad"))
 (load-and-apply-user-preferences dynapad)
 (init-menubar)  ; menubar setup
 (init-logtree-layer)
 (start-new-history)  ; set up logging for undo
 )

;(define *popup-menus-enabled?* #f)  ; -> menu_popup
;(define *menubar* #f) -> menubar
(define enable-marquee-and-lasso #f)
;(when *use-menubar* (dynaload "menubar.ss"))

(when *window-geometry* (send dynapad winfo *window-geometry*))

; load applications after dynapad has started
#; ; old functionality, probably needs to be restored
(let ((load-on-startup "./apps.ss"))
  (if (file-exists? load-on-startup)
      (load load-on-startup)
      ;else
      (when (getenv "DYNAHOME")
        (set! load-on-startup (build-path->string (getenv "DYNAHOME") "apps.ss"))
        (when (file-exists? load-on-startup)
          (load load-on-startup))
        )
      )
  )

