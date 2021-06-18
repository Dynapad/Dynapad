#lang racket/base

(provide slowgrow
         addbinding
         )

(require racket/class
         racket/list
         racket/file
         (only-in racket/gui/base
                  make-eventspace
                  current-eventspace
                  )
         (only-in racket/path file-name-from-path)
         compatibility/mlist
         compatibility/defmacro
         dynapad/libdynapad-wrapper
         dynapad/ffs
         dynapad/base ; dynapad%
         dynapad/bind
         dynapad/pad-state
         dynapad/misc/misc
         dynapad/misc/alist
         dynapad/layout/bbox  ; sigh of course this wasn't in the original spec
         ;dynapad/history/ids  ; see if we really need this ... cycles
         ;                     ; export-objs
         dynapad/utils/hilights
         collects/misc/pathhack  ; build-path->string
         )

;;; (load-relative-extension (path-replace-suffix "build/libdynapad" (system-type 'so-suffix)))

(define padthread
  (let
      ((new-es (make-eventspace)))

    (parameterize
        ((current-eventspace new-es))
      (thread
       (lambda ()
         (wish)
         (display "bye"))))))

(define (describe-all-callbacks obj)
  (map (lambda (name mth) (say name (eval `(send ,obj ,mth))))
       (list "delete" "afterdelete"
             "slide" "afterslide"
             "position" "afterposition"
             "scale" "afterscale"
             "width" "afterwidth"
             "height" "afterheight"
             "select"
             "reposition" "resize"
             )
       (list 'delete-callbacks 'afterdelete-callbacks
             'slide-callbacks 'afterslide-callbacks
             'position-callbacks 'afterposition-callbacks
             'scale-callbacks 'afterscale-callbacks
             'width-callbacks 'afterwidth-callbacks
             'height-callbacks 'afterheight-callbacks
             'select-callbacks
             'reposition-callbacks 'resize-callbacks
             )))

(define slowgrow
  (case-lambda
    ((obj slowness)
     ;slowness 0--> sticky z (constant size)
     ;slowness 1--> normal zooming
     ;        >1--> faster than view
     (slowgrow obj slowness
               (or (not slowness)
                   (let ((padz (send (send obj dynapad) getzoom)))
                     (* (send obj z)
                        (expt padz slowness))))))
    ((obj slowness home-sz)
     (slowgrow obj slowness home-sz 0))
    ;      (Obj-z = relative size; Apparent size = obj-z * pad-z)
    ; Need 3 params to describe a line plus threshold.
    ;
    ;
    ; obj-z   `
    ;          `
    ;           `       /
    ;            `     / - - slowness (slope)
    ;             `.  /
    ;               `/  <- - home-sz (y-intercept):
    ;  obj          /:`.      = rel. sz = apparent sz at zoom=1
    ;apparent      / :  ` .
    ;  size  _____/  :  <- -`-.- - - min-sz
    ;                :          `  .
    ;   0            1                 `  -   _
    ;      out  <--zoom-->  in                    "    -    .
    ;
    ((obj slowness home-sz min-sz)
     ;home-sz is obj's z when dynapad zoom=1
     ; min-sz sets minimum size (fraction of home-sz)
     (if (not slowness)
         (send obj renderscript #f) ;normal zooming
         ;else slowed zooming:
         (let* ((mypad (send obj dynapad))
                (set-z-script
                 (lambda (o)
                   (let* ((padz (send mypad getzoom))
                          (newz (* home-sz (expt padz (- slowness 1))))
                          (minz (/ min-sz padz)))
                     (send o z (max minz newz))))))
           ;(send obj z (/ home-sz (send mypad getzoom)))
           (send obj renderscript
                 (lambda (o)
                   (set-z-script o)
                   (send o renderitem)))
           ;run set-z-script now, to set the correct intial z
           (set-z-script obj)
           )))))


; for text labels, try (slowgrow label .3 2 .5)


(define panel%
  (class dynaobject%
    (inherit-field _dynapad)
    (init dynaptr (initcoords '(0 0 100 100)) (initmembers #f))
    (set! _dynapad dynaptr)
    (inherit-field cptr selected)
    (inherit dynaclass)
    (override writeoptions fix delete anchor re-anchor)
    (public members add remove save divisible coords fill)
    (field (savemembers null))

    (define anchor (case-lambda
                     (() (super anchor))
                     ((newanchor) (super anchor "sw")) ))

    (define (re-anchor newanchor) (super re-anchor "sw"))

    (define (fill color) (sch_panelsetfill (send this get-cptr) color))

    (define coords (case-lambda
                     (() (send this bbox))
                     ((crds)
                      (send this width (bbwidth crds))
                      (send this height (bbheight crds))
                      (send this xy (car crds) (cadr crds)) )))

    (define members
      (case-lambda
        (() (sch_members cptr))
        ((newmembers)
         (sch_members cptr (map (lambda (o) (send o get-cptr)) newmembers))
         (send this update-any-hilights))))

    (define save
      (case-lambda
        (() (map (lambda (o) (send o padid)) (members)))
        ((newmembers) (set! savemembers newmembers))))

    (define (fix alist)
      (for-each (lambda (o) (add (cadr (assq o alist)))) savemembers)
      (super fix alist))

    (define (add l)
      (let ((l (if (list? l) l (list l))))
        (for-each (lambda (o) (sch_addmember cptr (send o get-cptr))) l))
      (send this update-any-hilights))

    (define (remove l)
      (let ((l (if (list? l) l (list l))))
        (for-each (lambda (o) (sch_removemember cptr (send o get-cptr))) l))
      (send this update-any-hilights))

    (define (writeoptions)
      `(,@(super writeoptions)
        (save ',(save)) ;;; Should be same as group%
        (coords ',(coords))
        (divisible ,(divisible))))

    (define (delete)
      (for-each (lambda (o) (remove o) (send o delete)) (members))
      (super delete))

    (define divisible
      (case-lambda
        (() (sch_divisible cptr))
        ((bool) (sch_divisible cptr bool))))

    (define/override (select . args)
      (if (null? args) (super select)
          (when (not selected)
            (set! selected (make-object select% _dynapad this (car args))))))

    (super-instantiate (_dynapad (sch_makepanel (send _dynapad get-cptr) this)))
    (dynaclass 'panel%)
    (coords initcoords)
    (when initmembers (members initmembers))))

(define (addbinding PAD evnt fnc)
  (send PAD bind evnt (cons fnc (send PAD bind evnt))))

(define bind_to_tag
  (case-lambda
    ((pad tag) (sch_bind_to_tag (send pad get-cptr) tag))
    ((pad tag event) (sch_bind_to_tag (send pad get-cptr) tag event))
    ((pad tag event callback)
     (let ((bindings (bind_to_tag pad tag event)))
       (cond
         ((not callback) (set! bindings null)) ; grandfather #f to mean null
         ((list? callback) (set! bindings callback))
         (else (set! bindings (list callback))))
       (sch_bind_to_tag
        (send pad get-cptr)
        tag
        event
        bindings)))))
