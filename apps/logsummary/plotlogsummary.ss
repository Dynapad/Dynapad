(load-relative "logmunge.ss")
(require (lib "pregexp.ss"))
(require (lib "string.ss"))

(def *dragevent-codes* null) ;alist: codes->categories
(def *dragevent-rows* null) ;alist: category->rows
(def *tags-by-row* null) ; alist of text% objects for e. row
(def ysep 15)
(def *all-devts* null)
(def *time-labels* null)

(define time-label%
  (class text%
    (init-field _reltime)
    (init (inittext #f) (initposition #f) (initfont #f) (initanchor #f))

    (field (_x (send (send _reltime minus *basetime*) total)))
    (define/public (x) _x)

    (let ((usetext (or inittext
               (send _reltime write)))
      (x _x)
      (y -10)
      (z 0.8))
      (super-instantiate (dynapad usetext (list x y z) #f "n"))
      )
))

(define base-devt% ;dragevent%
  (class object%
    (init-field _basetime)
    (init _min _sec)
    (field (_myclass #f)
           (_time (make-object reltime% _min _sec))
       (_blots null)
       (_blotclass oval%)
;       (_row 0)
       (_color "ffffff")
       )
    (super-instantiate ())

    ;(define/public row (get/set _row))
    (define/public myclass (get/set _myclass))
    (define/public (myrows)
      (let ((tuple (assq (myclass) *dragevent-rows*)))
    (and tuple (cdr tuple))))

    (define/public color (get/set _color))
    (define/public (blot) _blot)
    (define/public (relsecs) (send (send _time minus _basetime)
                   total))

    (define/public (delete)
      (foreach _blots
           (lambda (b) (send b delete))))
    (define/public (plot)
      (foreach _blots
           (lambda (b) (send b delete)))
      (let* ((x (relsecs))
         (ys (map (lambda (r) (* r ysep))
              (send this myrows)))
         (last-label (if (null? *time-labels*)
                 #f
                 (car *time-labels*)))
         (last-x (and last-label
              (send last-label x)))
         (threshold (if (not last-x)
                0
                (* 60 (quotient (+ last-x 70) 60))))
         )
    (if (> x threshold)
        (push! (make-object time-label% _time) *time-labels*))
    (set! _blots
          (map
           (lambda (y)
         (ic (make-object _blotclass dynapad)
             (penwidth 1)
             (coords x y x y)
             (pen _color)))
           ys))
    _blots))
))

(define-macro (def-dragevent code class super color . rows)
  `(begin
     (define ,class
       (class ,super
          (init _basetime _min _sec)
          (super-instantiate (_basetime _min _sec))
          (send this myclass ,class)
          (if ,color (send this color ,color))
          ))
     (push! (list ,class ,@rows)  *dragevent-rows*)
     (push! (list ,code ,class) *dragevent-codes*)))





(def-dragevent #f err-devt% base-devt% "pink")
(def-dragevent 'E:oc E:oc-devt% err-devt% #f 3)
(def-dragevent 'E:co E:co-devt% err-devt% #f 3)
(def-dragevent 'E:di E:di-devt% err-devt% #f 3)
(def-dragevent 'E:zd E:zd-devt% err-devt% #f 1)
(def-dragevent 'E:rd E:rd-devt% err-devt% #f 2)
(def-dragevent 'E:dr E:dr-devt% err-devt% #f 3)
(def-dragevent 'E:rP E:rP-devt% err-devt% #f 2)
(def-dragevent 'E:sd E:sd-devt% err-devt% #f 1)
(def-dragevent 'E:g  E:g-devt%  err-devt% #f 0)
(def-dragevent 'E:ext E:ext-devt% err-devt% #f 5)

(def-dragevent 'E:ps E:ps-devt% err-devt% #f 1)
(def-dragevent 'E:dp E:dp-devt% err-devt% #f 3)
(def-dragevent 'E:pz E:pz-devt% err-devt% #f 1)

(def-dragevent #f confused-devt% base-devt% "red")
(def-dragevent 'E:sc E:sc-devt% confused-devt% #f 1)
(def-dragevent 'E:ds E:ds-devt% confused-devt% #f 1)

(def-dragevent #f view-devt% base-devt% "yellow")
(def-dragevent 'V:C V:C-devt% view-devt% #f 3)
(def-dragevent 'V:Cz V:Cz-devt% view-devt% #f 3)

(def-dragevent #f ok-devt% base-devt% "green")
(def-dragevent 'Cat Cat-devt% ok-devt% #f 4)
(def-dragevent 'Ext Ext-devt% ok-devt% #f 5)
(def-dragevent 'Cnsl Cnsl-devt% ok-devt% "blue" 5)

(def-dragevent 'Zoom zoom-devt% base-devt% "grey" 8)
(def-dragevent 'Pan  pan-devt%  base-devt% "white" 3)
(def-dragevent 'make make-devt% base-devt% "orange" 6)

(def-dragevent 'pile% pile-devt% base-devt% "grey"  10)
(def-dragevent 'container% container-devt% base-devt% "blue"  10)
(def-dragevent 'lens% lens-devt% base-devt% "green"  10)




;=====
(define *basetime* (make-object reltime% 59 15))
(define (register-evnt m s . codes)
  ;(say codes)
  (map (lambda (c)
     (when (string? c)
           (string-lowercase! c)
           (set! c (string->symbol c)))
     (let* ((tuple (assoc c *dragevent-codes*))
        (cls   (and tuple (cadr tuple)))
        (devt  (and cls (make-object cls *basetime* m s))))
(say c tuple)
       (when devt
         (push! devt *all-devts*)
         (send devt plot))
       devt))
       codes))

(define (clear-all-devts)
  (foreach *all-devts*
       (lambda (d) (send d delete)))
  (set! *all-devts* null))

(def tags-rexp "^([0-9]+):([0-9]+).*<(([a-zA-Z:]+[?,; ]*)+)>.*$")
(def otherstuff-rexp
"^([0-9]+):([0-9]+) *(drag|zoom|pan|make|delete) [^%]+(pile%|lens%|container%)?[^%]+$")
;(def dragtool-rexp "^([0-9]

(define (scan-log-line line)
  (let* (;(tags null) ;accumulates embedded tags
     (tag-matches   (regexp-match tags-rexp line))
     (tag-str (and tag-matches (fourth tag-matches)))
     (tags (and tag-str (pregexp-split "[ ?,;]" tag-str)))

     (other-matches (regexp-match otherstuff-rexp line))
     (verb-str (and other-matches (fourth other-matches)))
     (obj-str  (and other-matches (fifth other-matches)))
     ;(min (and matches (cadr tag-matches)))
     ;(sec (and matches (caddr tag-matches)))
     )
    (if tags
        ;(say min sec codes)
      (apply register-evnt
         (string->number (cadr tag-matches)) ;min
         (string->number (caddr tag-matches));sec
         tags)
      ;else simple verb
      (when other-matches
        (register-evnt
         (string->number (cadr other-matches)) ;min
         (string->number (caddr other-matches));sec
         verb-str obj-str))
      )))


(define plot-logsummary
  (case-lambda
   (() (plot-logsummary (Select-File-Dialog 'load "/home/Share/dsbauer/subjects/Beate/")))
   ((path)
    (when (and path
           (file-exists? path))
      (let ((port (open-input-file path 'text))
        (line #f))
        (while (not (eq? eof (cset! line (read-line port))))
           (scan-log-line line))
        (close-input-port port))))))