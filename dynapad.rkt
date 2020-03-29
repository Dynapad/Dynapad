#lang racket/base

;(require lang/plt-pretty-big)
(require racket/gui/base)
(require compatibility/mlist)
(require dynapad/dynapad-c-api)


(current-eventspace (make-eventspace))

(define (announce-module-loading name) #f)
(define (update-progress pct) #f)
; anytime after here, once progress.ss is included, loading modules (if so coded)
; will announce themselves and generate "..." progress bar to stdout

(define *home-directory* #f)
(define *dynapad-directory* (current-load-relative-directory))

(current-library-collection-paths
 (cons
  (build-path *dynapad-directory* "collects")
  (current-library-collection-paths)))

;-----------------------------------------------------------------
(require (lib "pathhack.ss" "misc"))
(load-relative "src/misc/dynaload.ss")

(dynaload "alias.ss")               ; allows aliasing of pathnames
(dynaload "events.ss")              ; basic events plus draw-application events)
(dynaload "bbox.ss")                ; bounding box utilities
(dynaload "arrange.ss")             ; utilities for layout and arrangement of objects
;arrangeimages is now obsolete, replaced by import-dirs.ss
;(dynaload "arrangeimages.ss")       ; module for load all images from a directory
(dynaload "formation.ss")           ; base mechanism for defining "rich objects"
(dynaload "lambdas.ss")
;(require new-lambda)
(dynaload "command-shortcuts.ss")   ; shortcut functions (mostly for programmers)
(dynaload "user-preferences.ss")    ; a mechanism for saving characteristics of the dynapad environment.

(define *list-of-all-dynapads* '())

(define (make-new-dynapad stringname)
  (let ((newPAD (make-object dynapad% stringname)))
    (createModes newPAD)
    (send newPAD bindtags "specific")
    (send newPAD focus)

    (set! *list-of-all-dynapads*
          (append *list-of-all-dynapads* (list newPAD)))

    newPAD
    )
  )

;-----------------------------------------------------------------

(define dp1 (make-new-dynapad ".dynapad"))

; define some variable aliases
(define dynapad  dp1) ; dynapad means "current pad"

(define currentPAD  dp1) ; temporary


(load-and-apply-user-preferences dynapad)


(define *popup-menus-enabled?* #f)
(define *menubar* #f)
(define enable-marquee-and-lasso #f)
(when *use-menubar*
  (dynaload "menubar.ss"))

(define (main-menu-title obj) ;probably overridden by application module
  (if obj "Object Menu" "Dynapad Menu"))

(when *window-geometry* (send dynapad winfo *window-geometry*))


; load applications after dynapad has started
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

