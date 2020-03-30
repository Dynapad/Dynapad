#lang racket/base

(require racket/class
         dynapad/base
         dynapad/undo-state
         dynapad/pad-state
         dynapad/misc/misc
         dynapad/utils/lerp
         dynapad/history/logbranch
         dynapad/history/log-state
         dynapad/history/showlogs
         )

(define *current-state-marker* #f)
;red dot showing current state on log tree

(define (infer-logbead-position)
  (let* ((r (length *redo-stack*))
         (u (- (length *undo-stack*) 1))
         (r+u (+ r u)))
    (if (zero? r+u)
        1
        (/ u r+u))))

; these override defns in logs.ss to update position of *current-state-marker*
(define (enter-midstate new)
  (set-current-state-id new)
  (send (current-logbranch) lerp-marker (infer-logbead-position)))
(define (enter-firststate new)
  (set-current-state-id new)
  (send (current-logbranch) lerp-marker 0))
(define (enter-laststate new)
  (set-current-state-id new)
  (send (current-logbranch) lerp-marker 1))

(define logbranch-beaded-line%
  (class logbranch-line%
    (init _tree-arg)
    (init _path)
    (init _file)
    (init _startnum)
    (init _logid)
    (init (_endnum #f))
    (init (_no-autolink #f))

    (super-instantiate (_tree-arg _path _file _startnum _logid _endnum _no-autolink))

    (define/public (lerp-marker fract)
      (let* ((crds (send this coords))
             (toxy (cddr crds))
             (x (lerp fract (car crds) (caddr crds)))
             (y (lerp fract (cadr crds) (cadddr crds))))
        (when (not *current-state-marker*)
          (set! *current-state-marker*
                (ic (make-object rect% dynapad)
                    (coords (append toxy
                                    (map (lambda (x)
                                           (+ x (pixels->space 3))) toxy)))
                    (pen "red")
                    (penwidth -1)
                    (sticky #t)
                    (findable #f)
                    (layer *logtree-layer*)
                    )))
        (send *current-state-marker* xy x y)
        (send *current-state-marker* raise (send this object))
        ))
    ))

(define visible-beaded-logtree%
  (class visible-logtree%
    (init _dynapad _branch-class _dir _name)
    (super-instantiate (_dynapad _branch-class _dir _name))

    (define/override (refresh-layout)
      (super refresh-layout)
      (send (send this current-branch) lerp-marker (infer-logbead-position)))
    ))

(define (make-logtree dir name) ;override defn in logbranch.ss
  (make-object visible-beaded-logtree% dynapad logbranch-beaded-line% dir name))

