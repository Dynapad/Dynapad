#lang racket/base

; LOG FILE NOTES
;
; While manipulating workspace(s), user creates "states" whose
; causal relations are tree-like:
;
;        /--3                     Visitation sequence: 0,1,2,3,2,4,5,6,7,5,8...
; 0--1--2       /--6--7
;        \--4--5
;               \--8...
;
; Branch nodes (e.g. 2,5) appear when user loads an old state (2,5)
;  and modifies it in a new direction (4,8).
; Log files record all states, even those "abandoned" (via undo, etc),
;  and can reconstruct them by starting from the closest preceding branch node
;  and iterating through each update until the desired state is reached.
; There is a separate log file for each segment of a tree (i.e. node->node/leaf)
;  (e.g. 0->2, 2->3, 2->5).
; The start state of each log (i.e. each node: 0,2,5) is an explicitly saved state
;  that can be rebuilt directly.

;------------------------------------------------------
;LOG FILE FORMAT:
;<log-file>     --> <prev-log> <start-state> (<change-state> | <visit-state>)+
;<prev-log>     --> (prev-log     <state-ID> <branch-suffix>) <bytes>\n
;<start-state>  --> (start-state  <state-ID> <do-op> <do-op>...) <bytes>\n
;<change-state> --> (change-state <state-ID> (<do-op>...) (<undo-op>...)) <bytes>\n
;<visit-state>  --> (visit-state  <state-ID> <when>) <bytes>\n

; <bytes> is the number of bytes of the previous s-expression, used
;  to rewind to an entry's start.  Every entry requires two (read..)s to
;  advance to the next entry, one for entry itself, one for (read).
; A <state-ID> is unique to each state,
;   as a referent (in <change-state> or a reference (in <visit-state>),
;   and is also the time that state was first created:
;   <seconds> or <seconds.milliseconds>  (relative to origin date, 1/1/03)
; In a <prev-log>, the <state-ID> and <branch-suffix> identify the log
;   which precedes this one (providing the history of this starting state)
; <when> is a timestamp of the same format, although it does not
;   reference another <state-ID>
; <do-op>s and <undo-op>s are scheme commands to be eval'd to reach the
;   next/previous state
;-----------------------------------------------------
;LOG FILE NAMES:
; All log files are named:
;<tree-name>@<start-state-ID>  (eg "project@12.34")
;  or
;<tree-name>@<start-state-ID><branch-suffix>  (eg "project@12.34a","project@12.34b")
;
; The <tree-name> is the user-assigned workspace name (or "untitledN")
; The <start-state-ID> is the <state-ID> of the log's starting state (see above)
; A <branch-suffix> is applied for all logs starting at branch points (see below).
;------------------------------------------------------
;BRANCHING EXAMPLE:  (fN: states stored in log file fN)
;
;f0:(0)--1--2--3        Create state at time 3
;           ^
;f0:(0)--1--2--3--4        Create state at time 4
;          ^
;f0:(0)--1--2--3--4        Undo x2: view state 2
;        ^
;f0:(0)--1--2--3--4        Diverge from 2: create state at time 5
;        \_5
;          ^
;Log files update thus:
;f0:(0)--1--2    f2a:(2)--3--4    Truncate f0; move tail to f2a;
;         f2b:(2)--5    Begin new log f2b: start state 2, change->5
;                         ^
;f2a:(2)--3--4            Load state 4
;            ^
;f2a:(2)--3--4--6        Continue extending this branch...
;        ^
;
;------------------------------------------------------
;LOG LINKING:
; A log's parent/predecessor is identified in the <prev-log> entry
;   (actual filename is <tree-name>@<state-ID><suffix>)
; A log's children/successors are implicit: they have filenames with <state-ID>
;   equal to the <state-ID> of the last <change-state> entry
;-----------------------------------------------------
;STATE INDEX:
; An index file for each tree (named "<tree-name>.index")
;  summarizes which states are in which logs:
;  (index-states <from> <to> <file-idx>) <bytes>\n
;  ...
; A given state-ID (or timestamp between states) can be matched to
;  the corresponding <from>-<to> range in the index, and then to the <file-idx>
; <file-idx> is "<state-ID><suffix>?";
;   the corresponding filename is "<tree-name>.<file-idx>"
; Note that the index is redundant; it can always be rebuilt from the set of logs.
; A given <state-ID> will never be found in a <file-idx> with a higher number.
;-----------------------------------------------------
;SAVE FILES:
;  The user's actual "save file" (which can be moved anywhere)
;  merely refers to a log:
;  (load-state <tree-name> <file-idx>)
;  By default, <treename> is the name under which the tree is first saved
;======================================================

(provide #; ;didn't work :/
         (all-from-out undo-state)
         do-deferred-evals
         restore-set-core
         restore-set
         ensure-keyframe
         start-new-history

         ; needed for save and restore
         load-log
         start-state
         )

(require (only-in racket/class send is-a? object%)
         dynapad/seval
         racket/string
         compatibility/defmacro
         ;mzlib/process
         dynapad/ffs
         (only-in dynapad/import
                  importing?
                  #;
                  deferred-exprs
                  #;
                  order-by-phase)
         dynapad/pad-state
         dynapad/save
         (only-in dynapad/spd
                  show-possible-delay)
         dynapad/misc/misc
         dynapad/misc/progress
         #;
         (only-in dynapad/misc/tools-lists
                  mmap)
         ;dynapad/history/undo ; cycle
         dynapad/undo-state
         #;
         (only-in dynapad/history/ids import-expr-with-objs)
         dynapad/history/logbranch
         ;dynapad/history/showlogs
         dynapad/history/logbead
         dynapad/history/log-state
         (only-in dynapad/history/deferred-evals
                  do-deferred-evals)
         (only-in dynapad/misc/command-shortcuts
                  ; FIXME this is not supposed to to being used in the core impl here
                  get-hostname)
         
         (only-in dynapad/menu/wxmenu
                  add-menu-item
                  add-menu-separator)
         ; edges -> menubar -> menu_functions -> logs ; previous cycle now fixed by lifting out graph
         (only-in dynapad/utils/graph graph-edge% graph-arc%) ; needed for restore-log-branch
         (only-in dynapad/events/text text%) ; needed for restore-log-branch
         collects/misc/pathhack
         (for-syntax racket/base)
         )

#; ; not relevant in the compiled module context
(announce-module-loading "logging...")

;Utilities for reading/writing workspace logs

;(dynaload "logbranch.rkt")
#;
(update-progress .6)

;OPTIONAL: comment out to exclude...
; visible logs:
;(dynaload "showlogs.rkt")  ;requires logbranch.ss
; state-marker
;(dynaload "logbead.rkt")   ;requires showlogs.ss
#;
(update-progress .8)

#|
(define (start-new-log treename prev-log-idx state-id build-msg)
  ;  (set! *prev-logid* prev-log-idx)
  (if treename
      (set! *current-log-treename* treename))
  (unless *current-log-treename*
    (set! *current-log-treename* (default-log-treename)))
  (let ((newid  (get-unique-logid state-id)))
    (ensure-logbranch prev-log-idx newid)
    (switch-logs *current-read-logid* newid))
  (set! *current-write-logid* *current-read-logid*)
  (prepare-write-log)
  (send (current-logbranch)
        log-startstate-entry state-id build-msg)
  ;  *current-write-logid*
  )
|#

; these are subsumed by new (start-state...) syntax:
(define-macro (my-logtreename)
  `(let* ((fullpath (this-expression-source-path))) ;see command-shortcuts.ss
     (let-values (((dir file dir?) (split-path->string fullpath)))
       (and (not dir?)
            (logfile->treename file)))))

(define-macro (my-logtreedir)
  `(this-expression-source-directory))

(define (restore-log-branch state-id logdir treename logid build-expr)
  (if (importing?) ;if importing, just build objects
      (eval build-expr)
      ;else restore history normally:
      (begin
        (ensure-current-logtree logdir treename)
        ;(if *heed-start-state?*
        ;    (send (current-logtree) clear-active-path))
        (send (current-logtree)
              ensure-current-branch logid) ; redundant arg: state-id)
        (send (current-logtree) reset-maxid)
        (when *heed-start-state?* ;may be already in (or beyond) state, no need to build
          (set-*future-log-path*! null)
          (enter-firststate state-id)
          (send (current-logbranch)
                log-visitstart-entry (make-timestamp-ID) state-id)
          ; this is where logs are evaled if you run into undefined
          ; errors that are coming from here check whether they have
          ; been imported at the top level AND in the file the defines
          ; the macro AND in the file that uses and expands the macro
          (eval build-expr))   ;build starting state
        (set-*heed-start-state?*! #t))) ;activate in future
  )

;(define (restore-log-branch . args)
;  (apply say args))

(define restore-log-branch-convert-args
  (case-lambda
    ((logpath state-id logdir treename logid build-expr)
     (if (not logpath)
         ; use given args
         (restore-log-branch state-id logdir treename logid build-expr)
         ; else extract from logpath
         (let* ((dir+file (logpath->logdir+logfile logpath))
                (logdir (and dir+file (car dir+file)))
                (logfile (cadr dir+file))
                (treename (logfile->treename logfile))
                (logid (logfile->logid logfile)))
           (restore-log-branch state-id logdir treename logid build-expr))))
    ; variable args needed for back-compat:
    ((logpath state-id treename logid build-expr)
     (restore-log-branch-convert-args
      logpath state-id #f treename logid build-expr))
    ((logpath state-id logid build-expr)
     (restore-log-branch-convert-args
      logpath state-id #f #f logid build-expr))
    ))

;for debugging:
;(define (restore-log-branch-convert-args logpath stateid logid . args)
;  (say logpath stateid logid))
;(define (change-state . args) #f)
;(define (view-state . args) #f)
;(define (change-view . args) #f)
;(define (visit-state . args) #f)

; (start-start...) overrides warning-generator version in ids.ss
(define-syntax (start-state stx)
  (syntax-case stx ()
    [(_ whatever ...) ;don't parse args here, just get syntax context (mypath)
     ; and pass along to restore-log-branch-convert-args
     (with-syntax ((mypath (datum->syntax stx (syntax-source stx))))
       ; in order to get eval to work we had to read from a file which
       ; remove the syntax location, load preserves it but can't eval the forms
       (syntax (restore-log-branch-convert-args
                (and (not (equal? "STDIN" mypath)) mypath) ;"STDIN"-->#f
                whatever ...)))]
    ))

(define (tape-synch . args) #t) ;used by logsummary app

(define (export-expr expr) expr)
;in case exprs must be modified before including in undo/redo frame...

(define (create-frame do-expr undo-expr)
  ; assumes there are no states ahead on *redo-stack*; extend stack/log
  (let ((state-id (make-timestamp-ID)))
    ;(eval do-expr)   ;perform operation
    ;would be more robust to eval do-expr after all else,
    ;but may have slow response time
    ;replace all object references with '(objid <id>)
    (set! do-expr (export-expr do-expr))
    (set! undo-expr (export-expr undo-expr))
    (push-*undo-stack*! (make-undo/redo-frame state-id do-expr undo-expr))
    (send (current-logbranch)
          log-changestate-entry state-id do-expr undo-expr);write to log
    (enter-laststate state-id)
    ))

(define (at-logbranch-start?)
  (or (null? *undo-stack*)
      (null? (cdr *undo-stack*))))
(define (at-logbranch-end?)
  (null? *redo-stack*))
(define (at-log-leaf?)
  (and (at-logbranch-end?) (not *log-continues?*)))

(define push-ops-no-exec
  (case-lambda
    (()
     (let ((do-expr (maybe-wrap *redo-ops*))
           (undo-expr (maybe-wrap *undo-ops*)))
       (unless (and (null? do-expr) (null? undo-expr))
         (unless (at-log-leaf?)
           ; future states exist...
           (let ((build-expr #f))
             (cond
               ((null? (cdr *undo-stack*))
                ;at start node (i.e. stack bottom):
                (set! build-expr (cadar *undo-stack*))) ;use build-expr saved in redo-expr slot
               ((null? *redo-stack*)
                ;at end of continued branch (= start node of next log)
                (set! build-expr (buildstr-prev-state undo-expr)))
               (else
                ;at intermediate node:
                (set! build-expr (buildstr-prev-state undo-expr))
                (send (current-logbranch) split-at-entry *current-state-id* build-expr)))
             ; ...so fork new branch
             (let ((newbranch
                    (send (current-logtree) new-branch *current-state-id* build-expr)))
               (set-*future-log-path*! null)
               (send (current-logtree) activate-branch newbranch))))
         ;either way, add new frame
         (create-frame do-expr undo-expr)
         (set-*undo-ops*! null)
         (set-*redo-ops*! null)
         )
       ))
    ((do-expr undo-expr)
     (push-*redo-ops*! do-expr)
     (push-*undo-ops*! undo-expr)
     (push-ops-no-exec))
    ))

(define (push-ops-and-exec do-expr undo-expr)
  (eval do-expr)
  (push-ops-no-exec do-expr undo-expr))


;;; from undo
;; from ids

(define-syntax restore-set-core
  (syntax-rules ()
    ((_ expr ...)
     (let* ((objs (filter (lambda (o) (is-a? o object%))
                          (map safe-eval (list expr ...)))))
       (do-deferred-evals *id->obj-index*)
       objs)
     #; ; XXX show-possible-delay kills backtraces
     (show-possible-delay currentPAD
                          (let* ((objs (filter (lambda (o) (is-a? o object%))
                                               (map safe-eval (list expr ...)))))
                            (do-deferred-evals *id->obj-index*)
                            objs)))))

(define-syntax restore-set
  (syntax-rules ()
    ((_ expr ...)
     (use-load-context 'restore
                       (restore-set-core expr ...)))))

;;; end from undo

(define (restore-undo-frame frame) ;may be overridden
  (restore-set (caddr frame)))
(define (restore-redo-frame frame)
  (restore-set (cadr frame)))

(define (restore-path-no-delete path)
  (use-load-context 'restore
                    (clear-undo/redo)
                    (load path))
  ) ;MAYBE WRONG-- when did this go awry?

(define (load-next-log)
  (let* ((nextbranch (pop-*future-log-path*!))
         (nextpath (send nextbranch path)))
    ;(display (format "loading next log ~a...~%" nextpath))
    (send (current-logtree) activate-branch nextbranch)
    ;    (switch-logs (current-logbranch) nextbranch)
    ;    (send *current-logbranch* close); just to be safe
    (set-*heed-start-state?*! #f) ;skips start-state entry when loading
    (restore-path-no-delete nextpath)    ; load/exec the log; changes *undo-stack*
    (forward-frame))) ;exec first redo

(define (forward-frame)
  (if (null? *redo-stack*)  ;if N or P
      (if (null? *future-log-path*) ;no future frames; maybe another stack?
          #f  ;FUTURE: if *log-continues?*, may also proceed by searching for next log
          (load-next-log))
      ;else: F or PF (some future frames)...
      (let ((frame (pop-*redo-stack*!)))
        (push-*undo-stack*! frame)  ;--> PF
        (enter-midstate (car frame))
        (send (current-logbranch)
              log-visitstate-entry (make-timestamp-ID) *current-state-id*)
        ;    (eval (cadr frame)))))
        (restore-redo-frame frame)))) ;exec redo-expr

(define redo forward-frame)

(define (load-prev-log)
  (let ((prevbranch (send (current-logbranch) get-parent)))
    (if prevbranch
        (let ((prevpath (send prevbranch path)))
          (push-*future-log-path*! (current-logbranch))
          ;      (switch-logs (current-logbranch) prevbranch)
          ;(display (format "loading prev log ~a...~%" prevpath))
          (send (current-logtree) activate-branch prevbranch)
          (set-*heed-start-state?*! #f)
          (restore-path-no-delete prevpath)    ; load/exec the log; changes *undo-stack*
          ;     (display (format "past-stack= ~a~%" *undo-stack*))
          (backward-frame)) ;invoke new last frame of new *undo-stack*
        #f))) ;bottom of stack, can't rewind

(define (backward-frame)
  ;  (display "back-frame")(newline)cadrable
  (let* ((frame    (car *undo-stack*)))
    ; (undo-expr (caddr frame)))
    (when (not (null? (cdr *undo-stack*)))  ;never pop last frame
      ; last frame calls load-prev-log
      (pop-*undo-stack*!) ;pop this frame
      (push-*redo-stack*! frame)
      (enter-midstate (caar *undo-stack*))
      (send (current-logbranch)
            log-visitstate-entry (make-timestamp-ID) *current-state-id*))
    ;    (eval undo-expr)))
    (restore-undo-frame frame))) ;exec undo-expr

;(define (backward-frame)
;;  (display "back-frame")(newline)cadrable
;  (let* ((frame    (pop! *undo-stack*)) ;--> N,M,S
;     (undo-expr (caddr frame)))
;    (when (not (null? *undo-stack*))  ;when M
;      (push! frame *redo-stack*)
;      (set! *current-state-id* (caar *undo-stack*))
;      (log-visitstate-entry (make-timestamp-ID) *current-state-id*))
;    (restore-set undo-expr)))

(define undo backward-frame)

(define buildstr-this-state Save-All-To-String)

(define (buildstr-prev-state undo-expr)
  ;creates buildstr of state after applying undo-expr
  ;  (append (buildstr-this-state) (list (export-expr undo-expr))))
  (Format-SaveAll-Expr-Into-String
   (append (list 'begin)
           (list (Save-All-To-Expr))
           (list undo-expr))))

;(define (activate-branch branch)
;  ;assumes already in branch start state
;;  (unless (= *current-state-id* (send branch startnum))
;;      (error "Cannot activate branch from current state id:" *current-state-id*))
;  (reset-stacks *current-state-id*)
;  (switch-logs (current-logbranch) branch))

(define (start-new-history)
  (delete-all currentPAD)
  (ensure-current-logtree #f)
  (let* ((starttime (make-timestamp-ID))
         (newbranch ; this goes straight to the log, not to memory ?
          (send (current-logtree) new-branch starttime '(list #t))))
    (send (current-logtree) activate-branch newbranch)
    (enter-firststate starttime)
    ))

(define (Clear-Workspace stuff) ;overrides Clear-Workspace in menu_functions.ss
  (start-new-history))

;(define (load-log treename logid)
;; filename is relative to log directory
;  (unless (importing?)
;      (begin
;        (if (current-logtree)
;        (send (current-logtree) delete))
;        (set! (current-logtree)
;          (make-object logtree% dynapad *logbranch-class* #f treename))))
;  (let ((branch
;     (make-object *logbranch-class* (current-logtree) #f #f #f logid)))
;    (unless (importing?) (set! *current-logbranch* branch))
;    (load (send branch path))))

(define load-log
  (case-lambda
    ((treename logid)
     (unless (importing?)
       (ensure-current-logtree treename)
       (send (current-logtree) ensure-current-branch logid))
     (mimic-old-load (send (current-logbranch) path)))
    ((hname treename logid)
     (unless (equal? hname (get-hostname))
       (error (format "Log hostname (~a) doesn't match this host" hname)))
     (load-log treename logid))))

(define (ensure-keyframe)
  (unless (at-logbranch-start?) ;ready for saving if already at branch start
    ;   ((at-logbranch-end?)
    ;      (activate-branch
    ;       (send (current-logtree) new-branch
    ;         *current-state-id* (buildstr-this-state))))
    (send (current-logtree) activate-branch
          (send (current-logbranch) split-at-entry
                *current-state-id* (buildstr-this-state)))
    (enter-laststate *current-state-id*)
    )
  )

; === NOTES ===
; The system can be characterized as a State-Machine:
; Four states of position in current branch:
;    N(ew)    S(tart)
;   no past,  no past,
;   no fut.   future
;
;    E(nd)    M(iddle)
;    past,     past,
;   no fut.   future
;
; Five transitions:
;  u(ndo)
;  r(edo)
;  a(ct)
;  s(ave)
;  l(oad)
;
;u:
;N and prevlog? --> undo, loadprev, M
;N else --> N
;E --> undo, M
;S and prevlog? --> undo, loadprev, M
;S else --> S
;
;r:
;E and nextlog? --> loadnext, S
;E else --> E
;M and morefuture --> redo, M
;M else --> redo, E
;S --> redo, M
;
;a:
;N --> push, E
;E --> push, E
;S --> fork (=split, branch, view-new), N
;M --> fork, N
;
;s:
;N --> link, N
;E --> append, view-new, N
;M --> split, view-tail, S
;S --> link, S
;
;l:
; any --> load, N or S
