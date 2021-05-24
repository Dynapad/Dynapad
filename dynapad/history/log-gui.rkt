#lang racket/base
(require (only-in racket/class send make-object)
         (only-in racket/gui/base
                  horizontal-pane%
                  check-box%)
         dynapad/menu/menu_popup
         dynapad/menu/menu-state
         (only-in dynapad/menu/wxmenu
                  add-checkable-menu-item
                  add-menu-separator)
         dynapad/history/log-state
         dynapad/pad-state
         (only-in dynapad/misc/user-preferences *use-menubar*)
         (only-in dynapad/history/logbranch current-logtree)
         )

; ---------GUI controls------------
(define (show-history-tree arg)
  (when arg
    (send (current-logtree) refresh-layout))
  (send *logtree-layer* visible arg)
  (and showhistorybox (send showhistorybox set-value arg)))
(define (toggle-history-tree)
  (let ((shown? (send *logtree-layer* visible)))
    (show-history-tree (not shown?))))
(define showhistorypane (and *use-menubar*
                             (make-object horizontal-pane% *menubar*))) ;may be used by showlogs.ss

(define showhistorybox
  (and showhistorypane
       (make-object check-box% "Show history tree" showhistorypane
                    (lambda (button evnt) (toggle-history-tree)))))
(show-history-tree #f)

;add to popup menu
(append-mainmenu-constructor
 (lambda (menu obj)
   (add-menu-separator menu)
   (add-checkable-menu-item menu "Show history tree"
                            (lambda (i) (toggle-history-tree))
                            (send *logtree-layer* visible))))

;=================================================================
