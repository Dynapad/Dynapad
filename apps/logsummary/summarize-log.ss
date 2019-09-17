(load-relative "logmunge.ss")
(require (lib "string.ss"))

(define *summary-port* #f)
(define (summarize-log logpath)
  (let* ((dir  (path-only logpath))
     (file (file-name-from-path logpath))
     (newfile (format "summary-~a" file))
     (newpath (build-path->string dir newfile)))
    (when (file-exists? logpath)
      (set! *summary-port*
        (open-output-file newpath #:mode 'text #:exists 'replace))
      (load logpath))
    (when *summary-port*
      (close-output-port *summary-port*)
      (set! *summary-port* #f))
    ))

(define (Select-Logfile-to-Summarize)
  (let ((log (Select-File-Dialog 'load "/home/Share/dsbauer/subjects/")))
    (and log
     (summarize-log log))))
      
(if *popup-menus-enabled?*
    (append-mainmenu-constructor
     (lambda (mb obj)
       (add-menu-separator mb)
       (add-menu-item mb "Summarize Log..." Select-Logfile-to-Summarize)
       )))

(define (fsay . args)
  (when *summary-port*
    (for-each (lambda (x) (fprintf *summary-port* "~a " x)) args)
    (fprintf *summary-port* "~%"))
  (apply say args))

(define (current-error-ports) ;override version in ids.ss
  (let ((stderr (list (current-error-port))))
    (if *summary-port*
    (cons *summary-port* stderr)
    stderr)))

(define (dxy->compass dx dy)
  (let ((ang (make-object geo-angle% dx dy)))
    (send ang compass-pt)))

(define (log-b b x)
  (/ (log x) (log b)))
(define (log-10 x)
  (log-b 10 x))

(define (round-to-d-digits n d) ;sorta works
  (let* ((s (sign n))
     (digits (round (log-10 (abs n))))
     (fac (inexact->exact (expt 10 (- digits d))))
     (a (round (/ n fac))))
    (* fac a)))
(define (round-to-d-digits num x)
   (let ([y (expt 10 x)])
     (number->string (/ (exact->inexact (round (* num y))) y))))

(define (start-state . args)
  (set-load-context 'restore)
  (for-each (lambda (a) (eval a)) args))

(define *tape-block* #f)
(define (tape-synch state min sec)
  (set! *tape-block* (make-object tape-timeblock%
                  (reltime 0 0)
                  (reltime 0 0)
                  (reltime min sec)
                  state)))

(define (report-zoom z)
  (format "z=~a" (round-to-d-digits z 3)))

(define (change-view type state . args)
  (let ((msg #f))
    (set! msg
      (case type
        ((startpan) #f)
        ((endpan) (format "pan ~a->~a"
                  (dxy->compass *delta-viewx* *delta-viewy*)
                  (drag-magnitude *delta-viewx* *delta-viewy*)))
;                  (round-to-d-digits *delta-viewx* 3)
;                  (round-to-d-digits *delta-viewy* 3)))
        ((startzoom)
           (set! *zoom-finished?* #f)
           #f)
        ((zoom-in)
           (set! *zoom-finished?* #t)
           (format "zoom into ~a: ~a" (replace-obj-with-name *zoom-target-id*)
               (report-zoom *last-zoom*)
               ))
        ((zoom-back-in)
           (set! *zoom-finished?* #t)
           "zoom back in")
        ((zoom-back-out)
           (set! *zoom-finished?* #t)
           "zoom back out")
        ((endzoom)
         (if *zoom-finished?*
         #f
         (if *zoomed-in?*
             (format "zoom in:  ~a" (report-zoom *last-zoom*))
             (format "zoom out: ~a" (report-zoom *last-zoom*)))))
        (else #f)
        ))
    (when msg
      (print-tape-time state)
      (fsay msg))
    ))

(define *last-zoom* #f)
(define *last-viewx* #f)
(define *last-viewy* #f)
(define *zoomed-in?* #f)
(define *delta-viewx* #f)
(define *delta-viewy* #f)

(define *zoom-finished?* #t)
(define *zoom-target-id* #f)

(define (oldview x y z) (set! *last-zoom* z)
  (set! *last-viewx* x)
  (set! *last-viewy* y))
(define (oldbb . args) #f)
(define (newview x y z) (set! *zoomed-in?* (< *last-zoom* z))
            (set! *last-zoom* z)
            (set! *delta-viewx* (- x *last-viewx*))
            (set! *delta-viewy* (- y *last-viewy*))
            (set! *last-viewx* x)
            (set! *last-viewy* y)
  )
(define (newbb . args) #f)
(define (zoomtarget id) (set! *zoom-target-id* id))
(define (zoomobject . args) #f)

(define (log-continues) (revert-load-context))

(define (change-state state do-op undo-op)
  (let ((expr #f))
    (cond ((cset! expr (depth-first-searchq do-op 'drag-batch))
       (apply summarize-drag state undo-op expr))
;       (eval `(summarize-drag ,@expr)))
      ((cset! expr (depth-first-searchq do-op 'regionize))
;       (eval `(summarize-regionize ,@expr)))
       ;l(apply summarize-regionize expr))
       (summarize-regionize state do-op))
      ((cset! expr (depth-first-searchq do-op 'make-pile-with-objs))
       (summarize-regionize state do-op))
      ((depth-first-searchq do-op 'samedir-relation%)
       (summarize-import state do-op))
      ((depth-first-searchq do-op 'delete)
       (summarize-delete state do-op))
      ((depth-first-searchq do-op 'bbox) ;resize
       (summarize-resize state do-op))
      ; since move, etc have already been checked, now check for selection only
      ((cset! expr (depth-first-searchq do-op 'selected))
       (summarize-selection state expr))
      (else #f)))
)

(define (print-tape-time state)
  (let ((rt (send *tape-block* state->reltime state)))
    (when *summary-port*
      (fprintf *summary-port* "~a " (send rt write)))
    (display (format "~a " (send rt write)))))

(define (summarize-resize state do-op)
  (let ((clause (depth-first-searchq do-op 'objid)))
    (print-tape-time state)
    (fsay "resize:" (replace-obj-with-name (safe-eval clause)))))

(define (summarize-selection state expr)
  ; expr has form ('selected (list ...))
  (let ((names (replace-idlist-with-names (cadr expr))))
    (print-tape-time state)
    (fsay "select: " names)))

(define (summarize-delete state do-op)
  (let ((clause (depth-first-searchq do-op 'objid)))
    (print-tape-time state)
    (fsay "delete:" (replace-obj-with-name (safe-eval clause)))))

(define (summarize-regionize state do-op)
  (let* ((expr (depth-first-searchq do-op 'ic))
     (newobj (eval expr)))
    (print-tape-time state)
    (fsay "make tool:" (replace-obj-with-name newobj))))

(define (summarize-import state do-op)
  (print-tape-time state)
  (fsay "import directory...")
  (eval do-op)
)

(define (replace-obj-with-name o) ;o may also be an id
  (if (number? o)
      (set! o (safe-eval `(objid ,o))))
  (and o
       (cond ((is-a? o pdf-portrait%)
          (format "~a[~a]" (file-name-from-path (send o url))
              (title->inits (send o title))))
         ((is-a? o titled-frame-container%)
          (format "~a-~a" (send (get-rgn o) dynaclass) (send o id)))
         (else o))))
  
(define (replace-idlist-with-names ids) ;ids has form '(list (objid N) ...)
  (let* ((objs (or (safe-eval ids) null))
     (names (map replace-obj-with-name objs)))
    names))

(define (drag-magnitude dx dy)
  (round-to-d-digits (/ (max (abs dx) (abs dy)) 125) 1))

(define (summarize-drag state undo-op drag-batch dynapad event ids dests)
  (let* ((undrag-expr (depth-first-searchq undo-op 'position))
     (orig-posns (cadr (memq 'position undrag-expr)))
     (dx (- (cadadr dests) (cadr orig-posns)))
     (dy (- (caddr (cadr dests)) (caddr orig-posns)))
     (names (replace-idlist-with-names ids)))
    (unless (null? names)
        (print-tape-time state)
        (apply fsay (format "drag ~a->~a:" (dxy->compass dx dy)
                (drag-magnitude dx dy)
                )
           names))))

(define first-letter-rexp  (regexp "^[A-Za-z]"))
(define (title->inits title)
;makes a string of the first letter of e. word in title
  (if (not title) ""
      (let* ((words (regexp-split " +" title))
         (inits (map (lambda (word)
               (let ((match (regexp-match first-letter-rexp word)))
                 (and match (car match))))
             words))
         (keeps (list-head (filter identity inits) 8)))
    (apply string-append keeps))))
    

;(define-syntax summarize-drag
;  (syntax-rules ()
;    ((_ drag-batch dynapad fake-evt (sth ...))
;     (list sth ...))))

;(define (summarize-regionize regionize junk-obj rgn-type form-type)
;  (fsay "regionize: " rgn-type))

(define (visit-state when state)
  (print-tape-time when)
  (print-tape-time state)
  (fsay "<-- undo/redo"))

#|
(define dummy-region%
  (class linkable-obj%
    (init-field _type)
    (super-instantiate ())
    (define/public name (get/set _type))))

(define-macro (regionize base rgntype formtype)
  `(let ((rgn
      (make-object dummy-region% ',rgntype)))
     (fsay "make region:" ',rgntype)
     rgn))

(define samedir-relation%
  (class linkable-obj%
    (field (_name #f))
    (super-instantiate ())
    (define/public name (get/set _name))
))

(define dummy-pdf%
  (class linkable-obj%
    (init-field _url)
    (field (_name (file-name-from-path _url)))

    (define/override (refer-when-ready . args) #f)
    (define/public (name) _name)
    (super-instantiate ())      
))

(define (make-pdf-at-position url x y z)
  (make-object dummy-pdf% url))
|#
