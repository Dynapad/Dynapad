(define (gesture->color gesture)
  (cond
    ((string-ci=? gesture "T1")    "#ffa4d2")  ;pink
    ((string-ci=? gesture "T1sq")  "#ff0000")  ;red
    ((string-ci=? gesture "T1tw")  "#ff9144")  ;orange
    ((string-ci=? gesture "T1cl")  "#ff36ff")  ;purple
    ((string-ci=? gesture "AS")    "#ffff00")  ;yellow
    ((string-ci=? gesture "ASpep") "#ffff9f")  ;light yellow
    ((string-ci=? gesture "B")     "#00ffff")  ;sky blue
    ((string-ci=? gesture "SC")    "#00c4c4")  ;sea green
    ((string-ci=? gesture "NB")    "#9cff9c")  ;light green
    (else
      (printf "no color for ~a" gesture)
      "black")))

(define (min:sec->sec time)
  (let*
    ((rexp "(.*):(.*)")
     (parsed (cdr (regexp-match rexp time)))
     (min (string->number (car parsed)))
     (sec (string->number (cadr parsed))))
    (+ (* 60 min) sec)))

(define entry%
  (class rect%
    (init-field _pad _ts _te _lh _rh _other _speaker _text)
    (field (_tssec (min:sec->sec _ts)) (_tesec (min:sec->sec _te)))
    (field (_rhrect #f) (_lhlabel #f) (_rhlabel #f))
    (inherit penwidth fill position)
    (define/public (ts) _ts)
    (define/public (te) _te)
    (define/public (lh) _lh)
    (define/public (rh) _rh)
    (define/public (other) _other)
    (define/public (speaker) _speaker)
    (define/public (text) _text)
    (define/public (tssec) _tssec)
    (define/public (tesec) _tesec)
    (super-instantiate(_pad (list (* 10 _tssec) 0 (* 10 _tesec) 40)))
    (penwidth 1)
    (fill (gesture->color _lh))
    (set! _lhlabel (make-object text% _pad _lh))
    (send _lhlabel anchor "c")
    (send _lhlabel position (position))
    (when (not (equal? _rh ""))
      (set! _rhrect (make-object rect% _pad (list (* 10 _tssec) 40 (* 10 _tesec) 80)))
      (send _rhrect penwidth 1)
      (send _rhrect fill (gesture->color _rh))
      (set! _rhlabel (make-object text% _pad _rh))
      (send _rhlabel anchor "c")
      (send _rhlabel position (send _rhrect position)))))

(define transcript%
  (class object%
    (init-field _path)
    (define/public (path) _path)
    (define/public (entries) _entries)
    (define (proc port)
      (let ((line (read-line port)))
        (cond
          ((eq? line eof) ())
          (else
            (cons
          (let*
                ((rexp "(.*)\t(.*)\t(.*)\t(.*)\t(.*)\t(.*)\t(.*)")
                 (parsed (cdr (regexp-match rexp line))))
                (apply make-object entry% dynapad parsed))
              (proc port))))))
    (field (_entries (call-with-input-file _path proc)))
    (super-instantiate())))

;(make-object transcript% "/tmp/Amaya-example.txt")
