#lang racket/base

(require racket/list
         dynapad/misc/tools-lists
         dynapad/utils/actor
         collects/misc/pathhack
         )

;(dynaload "actor.ss")
;(dynaload "tools-lists.ss") ; need (first-valid...)
;(require (lib "list.ss"))
;(require (lib "etc.ss")) ;need (second/third/fourth/etc ...)

; ============= Name-part generation, conversion  =============
; Nomenclature:
; treename  = "tree"
; lognum    = 123.456 (= stateid of first/last state in log)
; logsuffix = "x"
; logendnum = <lognum>
; logid     = "<lognum>" or "<lognum><logsuffix>"
; logrange  = "<logid>-" or "<logid>-<logendnum>"
; logfile   = "<treename>#<logrange>" (e.g. "tree#123.456a-125")
; logpath   = ".../<logfile>"

(define logid-pattern "([0-9.]+)([a-z]*)")
(define logrange-pattern (format "((~a)-([0-9.]*))" logid-pattern))
(define logfile-rexp (regexp (format "([^#]+)#~a" logrange-pattern)))
; --> (all treename logrange logid lognum suffix logendnum)
(define logrange-rexp (regexp logrange-pattern))
; --> (all logrange logid lognum suffix logendnum)
(define logid-rexp (regexp logid-pattern))
; --> (logid lognum suffix)


(define (logpath->logdir+logfile logpath)
  (let-values (((dir file dir?) (split-path->string logpath)))
    (when dir? (error "Logpath" logpath "is a directory"))
    (list dir file)))

(define logrange->logfile
  (case-lambda
    ((logrange) (logrange->logfile *current-log-treename* logrange))
    ((treename logrange) (format "~a#~a" treename logrange))))
(define lograngeparts->logrange
  (case-lambda
    ((logid) (format "~a-" logid))
    ((logid logendnum) (format "~a-~a" logid logendnum))
    ((lognum suffix logendnum)
     (format "~a~a-~a" lognum suffix logendnum))))
(define (logrange->logid+logendnum logrange)
  (let ((match (regexp-match logrange-rexp logrange)))
    (and match (list (third match) (sixth match)))))
(define (logfile->lognum+suffix logfile)
  ;returns #f if failure or list (lognum suffix)
  (let ((match (regexp-match logfile-rexp logfile)))
    (and match (list (fifth match) (sixth match)))))
(define (logfile->lognum logfile)
  (let ((match (regexp-match logfile-rexp logfile)))
    (and match (fifth match))))
(define (logfile->logid logfile)
  (let ((match (regexp-match logfile-rexp logfile)))
    (and match (fourth match))))
(define (logfile->logrange logfile)
  (let ((match (regexp-match logfile-rexp logfile)))
    (and match (third match))))
(define (logfile->treename logfile)
  (let ((match (regexp-match logfile-rexp logfile)))
    (and match (second match))))
(define (logfile->logendnum logfile)
  (let* ((match (regexp-match logfile-rexp logfile))
         (logendnum (and match (seventh match))))
    (when (equal? logendnum "")
      (set! logendnum #f))
    logendnum))
(define (logfile->suffix logfile)
  (let* ((match (regexp-match logfile-rexp logfile))
         (suffix (and match (sixth match))))
    ;    (if (equal? suffix "")
    ;    (set! suffix #f))
    suffix))

(define (logpath->logdir+treename logpath)
  (let-values (((dir file dir?) (split-path->string logpath)))
    (and (not dir?)
         (list dir (logfile->treename file)))))

(define (logid->lognum logid)
  (let ((match (regexp-match logid-rexp logid)))
    (and match (second match))))
(define (logid->lognum+suffix logid)
  (let ((match (regexp-match logid-rexp logid)))
    (and match (cdr match))))
(define (lognum+suffix->logid lognum suffix)
  (format "~a~a" lognum suffix))

(define *suffix-alphabet* (list "" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                                "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(define (default-log-directory)
  (setup-dynapad-subdirectory "logs"))

;============ Management classes ================
(define logtree%
  ; manages set of connected logs
  (class group% ; group% so that subclasses (i.e. visible-logtree%)
    ;  can contain dynaobject%s
    (init _dynaptr)
    (init-field _branch-class)
    (init-field _directory)
    (init-field _treename)
    (field (_branches null))
    (field (_current-logbranch #f))
    (field (_trust-cache? #t))
    (super-instantiate (_dynaptr))
    (send this findable #f)

    (define/public (branch-class) _branch-class)

    (define/public (directory)
      (or _directory (cset! _directory (default-log-directory))))

    (define/public (treename)
      (or _treename
          (cset! _treename
                 ;generates "log<N+1>" from highest "logN#xxx" in saves directory
                 (let* ((name-rexp (regexp "log([0-9]+)\\#"))
                        (filenames (directory-list->string (directory)))
                        (untitled-nums
                         (map (lambda (name)
                                (let ((match (regexp-match name-rexp name)))
                                  (if match (string->number (cadr match)) #f)))
                              filenames))
                        (nums-only (filter (lambda (n) n) untitled-nums))
                        (next-num (if (null? nums-only) 0 (+ 1 (apply max nums-only))))
                        (next-name (format "log~a" next-num)))
                   next-name))))

    (define/public (find-branch-matching match-fn)
      (let ((found (first-valid _branches match-fn)))
        (and found (caar found))))

    (define/public (add-branch new)
      (push! new _branches))
    (define/public (branches) _branches)

    (define/public (verify-name dir name)
      (if (and
           (equal? name _treename)
           (equal? dir  _directory))
          this #f))

    (define/public (new-branch stateid build-expr . endnum-etc)
      ;open/write new branch log from existing start node
      ;build-expr is an expression used in (start-state...) entry
      ; to build initial log state
      (let ((newbranch (apply make-object (branch-class)
                              this #f #f stateid #f endnum-etc)))
        (send newbranch initialize-log stateid build-expr)
        newbranch))

    (define/public (ensure-current-branch logid)
      (let* ((branch
              (or ;current branch has correct logid
               (and _current-logbranch
                    (send _current-logbranch verify-logid logid))
               ;search existing branches
               (find-branch-matching (lambda (b) (equal? logid (send b logid))))
               ;else create correct logbranch
               (make-object (branch-class) this #f #f #f logid))))
        (activate-branch branch)))

    (define/public current-branch (get/set _current-logbranch))

    (define/public (activate-branch branch)
      (when _current-logbranch
        (send _current-logbranch deactivate))
      (current-branch branch)
      (let* ((startid  (send branch startnum))
             (startnum (and startid (ensure-number startid))))
        (reset-stacks startnum))
      (send branch activate)
      branch)

    (define/public (clear-active-path) #t) ;hook for override by subclass

    (define/public (gather-lognames)
      (let* ((allfiles (directory-list->string (send this directory)))
             (logrexp (regexp (string-append
                               ;normally rexp "^~a#" should be enough,
                               (format "^~a#" (send this treename))
                               ;but ensure last char isnt ~ to exclude emacs autosave files
                               ".*[^~]$")))
             (matchfiles (filter (lambda (f) (regexp-match logrexp f))
                                 allfiles)))
        matchfiles))

    ; ---------- Maxid-cacheing -------------------
    (define obj-id-clause-rexp (regexp "[(]id ([0-9]+)[)]"))

    (define (get-log-maxid logfilename)
      ; scans logfile for pattern "(id N)"; returns highest N found or #f
      (when (relative-path? logfilename)
        (set! logfilename (build-path->string (send this directory) logfilename)))
      (and (file-exists? logfilename)
           (let* ((port (open-input-file logfilename 'text))
                  (line #f)
                  (hits null)
                  (ids null))
             (while (not (eq? eof (cset! line (read-line port))))
               (set! hits (append hits (regexp-match* obj-id-clause-rexp line))))
             (close-input-port port)
             (set! ids (map (lambda (sexpr)
                              (string->number (cadr (regexp-match obj-id-clause-rexp sexpr))))
                            hits))
             (and (not (null? ids))
                  (apply max ids)))
           ))

    (define/public get-tree-maxid
      ; finds highest "(id N)" of any logfile in tree
      (case-lambda
        ((logfile-names)
         (display "Refreshing max id...")
         (let* ((maxids (map get-log-maxid logfile-names))
                (keeps (filter identity maxids))
                (newmax (and (not (null? keeps))
                             (apply max keeps))))
           (say newmax)
           newmax))
        (() (get-tree-maxid (send this gather-lognames)))))

    (define/public (get-cache-filename)
      (build-path->string (directory) (format ".~a" (treename))))

    (define/public trust-cache?
      (case-lambda
        (() _trust-cache?)
        ((arg) (if arg
                   (when (not _trust-cache?)
                     (cache-maxid))
                   (when _trust-cache?
                     (uncache-maxid))))))

    (define/public cache-maxid ;create file
      (case-lambda
        (() (cache-maxid (get-cache-filename)))
        ((filename)
         (let ((outport (open-output-file filename #:exists 'replace)))
           (fprintf outport "(set-max-padid ~a)" *id-counter*)
           (close-output-port outport)
           (set! _trust-cache? #t)
           ))))

    (define/public uncache-maxid ;delete file
      (case-lambda
        (() (uncache-maxid (get-cache-filename)))
        ((filename)
         (set! _trust-cache? #f)
         (and (file-exists? filename)
              (delete-file filename)))))

    (define/public (reset-maxid)
      ;check cached file (.treename): if more recent than all logs, trust it;
      ; otherwise recompute tree maxid from scratch
      (let* ((mydir (directory))
             (cachefile (get-cache-filename)))
        ;         (cachetime (and (file-exists? cachefile)
        ;                  (file-or-directory-modify-seconds cachefile)))
        ;         (logtimes (map file-or-directory-modify-seconds logpaths))
        ;         (maxtime (if (null? logtimes) 0
        ;              (apply max logtimes))))
        (if (and (trust-cache?)
                 (file-exists? cachefile))
            (load cachefile)
            ;else no/stale cache or
            (let* ((logfiles (send this gather-lognames))
                   (logpaths (map (lambda (file) (build-path->string (directory) file)) logfiles))
                   (newmax (get-tree-maxid logpaths)))
              (when newmax
                (set-max-padid newmax)
                (cache-maxid cachefile))))))

    ))

;(define (new-padid)   ;overrides version in ids.ss
;  (if (current-logtree)
;      (send (current-logtree) trust-cache? #f))
;  (++_ *id-counter*))

(define (make-logtree dir name) ;may be overridden in showlogs.ss, etc
  (make-object logtree% dynapad logbranch% dir name))

(define *current-logtree* #f)
(define (current-logtree) *current-logtree*)
(define ensure-current-logtree
  (case-lambda
    ((dir name)
     (or (and *current-logtree* (send *current-logtree* verify-name dir name))
         (begin ;wrong tree or no tree
           (when *current-logtree* (send *current-logtree* delete))
           (cset! *current-logtree* (make-logtree dir name)))))
    ((name) (ensure-current-logtree
             (default-log-directory)
             name))))

(define (current-logbranch)
  (and *current-logtree*
       (send *current-logtree* current-branch)))

(define logbranch%
  ; corresponds to and manages a single logbranch file
  (class named-actor%
    (init-field _tree) ;should be non-#f
    (init-field _path) ; any (but not all) other args may be #f
    (init-field _file)
    (init-field _startnum)
    (init _logid)

    (init-field (_endnum #f))
    (field (_suffix #f))

    (field (_write-port #f)
           (_read-port #f))

    (super-instantiate ())
    (send (logtree) add-branch this)

    (when _logid
      ; match logid to existing file to fill in endnum
      (set! _file (get-filename-matching
                   (format "^~a#~a-.*$" (treename) _logid))))

    (define/public (verify-logid id)
      (if (equal? id (logid)) this #f))
    ; ==== Name Management =======

    (define (clear-dependents)
      (set! _file #f)
      (set! _path #f))

    (define/public (logtree) _tree)

    (define/public (directory)
      (send (logtree) directory))

    (define/public (treename)
      (send (logtree) treename))

    (define/public (path)
      (or _path
          (cset! _path (build-path->string (directory) (file)))))

    (define/public (file)
      (or _file
          (begin (clear-dependents)
                 (cset! _file (format "~a#~a~a-~a"
                                      (treename) (startnum)
                                      (or (suffix) "")
                                      (or (endnum) ""))))))

    (define/public startnum
      (case-lambda
        (() (or _startnum
                (and _file (cset! _startnum (logfile->lognum _file)))
                (error "Logbranch _startnum is #f: "
                       (memq this (send _tree branches)))))
        ((num) (clear-dependents)
               (cset! _startnum num))))

    ;    (define (generate-unique-suffix dir treename lognum)
    ;       ;returns suffix which makes filepath unique
    ;      (let ((suffixes *suffix-alphabet*)
    ;        (try-name #f))
    ;    (while (and (cset! try-name
    ;               (logrange->logfile
    ;                treename
    ;                (lograngeparts->logrange lognum (car suffixes) "")))
    ;            (not (null? (ls-dir dir try-name))))
    ;           (set! suffixes (cdr suffixes))
    ;           (if (null? suffixes)
    ;           (error "Out of fileindex suffixes!\n")))
    ;    (car suffixes)))

    (define (generate-unique-suffix dir treename lognum)
      ;returns suffix which makes filepath unique
      (let* ((construct-fn
              (lambda (sfx-lst) (if (null? sfx-lst)
                                    (begin (error "Out of fileindex suffixes!\n") #f)
                                    (logrange->logfile
                                     treename
                                     (lograngeparts->logrange lognum (car sfx-lst) "")))))
             (exists-fn (lambda (name) (not (null? (ls-dir dir name)))))
             (seed (construct-unique-filename-seed construct-fn exists-fn cdr
                                                   *suffix-alphabet*)))
        (and seed (car seed))))

    (define/public suffix
      (case-lambda
        (() (or _suffix
                (and _file (cset! _suffix (logfile->suffix _file)))
                (suffix (generate-unique-suffix (directory) (treename) (startnum)))))
        ((num) (clear-dependents)
               (cset! _suffix num))))

    (define/public (logid)
      (format "~a~a" (startnum) (suffix)))

    (define/public endnum
      (case-lambda
        (() (or _endnum
                (and _file (cset! _endnum (logfile->logendnum _file)))))
        ((num) (let ((old-path (path)))
                 (clear-dependents)
                 (cset! _endnum num)
                 ; auto-rename file if exists
                 (when (and (file-exists? old-path)
                            (not (equal? old-path (path))))
                   (rename-file-or-directory old-path (path)))
                 num))))

    (define/public (precedes? otherbranch)
      ; comparison function: ancestors always precede descents
      (let ((val1 (ensure-number (startnum)))
            (val2 (ensure-number (send otherbranch startnum)))
            (sfx1 (or (suffix) ""))
            (sfx2 (or (send otherbranch suffix) "")))
        (if (= val1 val2)
            (string<? sfx1 sfx2)
            (< val1 val2))))

    (define/public (get-filename-matching pattern)
      (let ((found (ls-dir (directory) pattern)))
        (case (length found)
          ((0) #f)
          ((1) (car found))
          (else (error "Multiple logs match pattern" pattern)))))

    (define/public (get-parent)
      (let* ((start (startnum)))
        ;first check other branches in _tree
        (or (send _tree find-branch-matching
                  (lambda (branch) (equal? start (send branch endnum))))
            ;else go to directories, use filename to locate
            (let* ((pattern (format "^~a#.*-~a$" (treename) start))
                   (found-filename (get-filename-matching pattern)))
              (and found-filename
                   (make-object (send _tree branch-class) _tree #f found-filename #f #f))))
        ))

    ; ======== Reading/Writing ===========

    (define/public (prepare-for-write)
      (when _read-port (close)) ;(error "Log already open for reading:" (path))) ;for debug
      (unless _write-port
        (set! _write-port (open-output-file (path) #:mode 'text #:exists 'append))))
    (define/public (prepare-for-read)
      (when _write-port (close));(error "Log already open for writing:" (path))) ;for debug
      (unless _read-port
        (set! _read-port (open-input-file (path) 'text))))

    (define/public (close)
      (when _write-port (begin
                          (close-output-port _write-port)
                          (set! _write-port #f)))
      (when _read-port (begin
                         (close-input-port _read-port)
                         (set! _read-port #f))))

    (define/public (write-to-log string)
      (prepare-for-write)
      (fprintf _write-port string))
    (define (write-log-entry msg)
      (prepare-for-write)
      (let* ((fpos (file-position _write-port))
             (junk (if (string? msg)
                       (fprintf _write-port msg)
                       (write msg _write-port)))
             (len  (- (file-position _write-port) fpos)))
        ;write length field
        (fprintf _write-port (format "~a~%" len))))

    (define (rewind-entry)
      ; Assumes log position is at ^: (entry...)^N, where N is length of (entry...)
      ; moves log position to ^(entry...) N and returns file position of that point
      (let ((start-pos (file-position _read-port))
            (jump-bytes (read _read-port)))
        (if (number? jump-bytes)
            (let ((target (- start-pos jump-bytes)))
              (file-position _read-port target)
              target)
            (begin ;if failed, restore start-pos
              (file-position _read-port start-pos)
              #f))))

    (define (skip-entry)
      ;assumes at file open at start of entry
      (read _read-port) ;read this entry
      (read _read-port) ;read bytes
      (read-char _read-port) ;read newline
      )

    (define (copy-log-tail to-logbranch)
      ;copies *current-log-file*, from current point to end, into file of to-logbranch
      ; assumes both files open and positioned correctly
      (let ((expr #f))
        (while (not (eq? eof (cset! expr (read-line _read-port))))
          (send to-logbranch write-to-log (format "~a~%" expr)))))

    (define (read-log-until quit-fn)
      ;leaves log at start of first entry for which quit-fn is #t,
      ; or at eof and returns #f if not found
      (let ((expr  (read _read-port)))
        (cond ((eq? expr eof) #f)
              ((quit-fn expr) (rewind-entry))
              (else (begin
                      (read _read-port) ;read length field, advancing to next entry
                      (read-log-until quit-fn))))))

    (define (seek-log-entry-with-ID state-id)
      (prepare-for-read)
      (read-log-until
       (lambda (expr) (and (list? expr)
                           (not (null? (cdr expr)))
                           (let ((found-id (cadr expr)))
                             (and (number? found-id)
                                  (= found-id state-id)))))))

    (define/public (log-changestate-entry state-ID do undo)
      (write-log-entry (list 'change-state state-ID `',do `',undo
                             `(max-padid ,*id-counter*)))
      ;(send (current-logtree) trust-cache? #t)
      (send (current-logtree) cache-maxid)
      )

    (define/public (log-changeview-entry panorzoom view . args)
      (write-log-entry (apply list 'change-view panorzoom
                              (make-timestamp-ID) view args)))

    (define/public (log-visitstate-entry when state-ID)
      (write-log-entry (list 'visit-state when state-ID)))

    (define/public (log-visitstart-entry when state-ID)
      (write-log-entry (list 'visit-start when state-ID
                             (send dynapad winid)
                             (get-username)
                             (get-hostname))))


    (define/public (log-startstate-entry state-ID build-expr)
      (if (string? build-expr)
          (write-log-entry (format
                            "(start-state ~a ~s '~a)"
                            state-ID (logid) build-expr))
          (write-log-entry (list 'start-state
                                 state-ID
                                 ;'(my-logtreedir) '(my-logtreename)
                                 ;(treename)
                                 (logid) `',build-expr)))
      (write-log-entry (list 'created-by
                             state-ID
                             (send dynapad winid)
                             (get-username)
                             (get-hostname)))
      )

    (define/public (initialize-log start-state-id build-msg)
      (log-startstate-entry start-state-id build-msg)
      (send (current-logtree) trust-cache? #t)
      )

    (define (split-here stateid build-expr . more)
      ;Assumes _read-port is open for reading and file position is correct
      ;  (i.e. start of first entry to replant)
      ;Returns tail branch after split point
      (let* ((tail-pos (file-position _read-port)) ;save current file posn
             (curr-end (endnum)) ; transfer current filename endnum to new tail branch
             (tail-branch
              (send/apply _tree new-branch stateid build-expr curr-end more)))

        ;transplant tail of current log
        (copy-log-tail tail-branch)
        (send tail-branch close)
        (send this close)

        ;truncate current log
        (send this endnum stateid)
        ; NOTE: consider using (truncate-file...) from PLT lib os.ss
        (sch_truncatefile (path) tail-pos)
        ;cap it off
        (write-log-entry '(log-continues))
        (send this close)

        tail-branch
        ))



    (define/public (split-at-entry  state-id build-expr . more)
      ; Assumes state-id being sought != log start state (else error)
      ; forks log (THIS) corresponding to *current-logid*:
      ;  i.e. all entries after state-id (if any) moved to one new log (A)
      ;       and second new log (B) is started
      ; Returns file-idx of log preceding B -- which is usually THIS,
      ;  but may be prev-THIS if state-id is THIS's start state
      (when (seek-log-entry-with-ID state-id) ;auto-rewinds to start of entry
        ;go to next entry:
        (skip-entry)
        ;now at split point...
        (apply split-here state-id build-expr more)))

    (define/public (activate) #t) ;these may be overridden by logbranch-line%
    (define/public (deactivate) #t)

    ))

(define (log-changeview-entry panorzoom view . args)
  (let ((br (current-logbranch)))
    (when br (send/apply br log-changeview-entry panorzoom view args))))
