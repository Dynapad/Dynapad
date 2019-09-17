(module xpdf mzscheme

  (require (lib "process.ss"))

  (provide xpdf/pdfimages xpdf/pdftops xpdf/pdfinfo xpdf/pdftotext
    xpdf/verbose)

  (define xpdf/verbose
    (let ((_verbose #f))
      (case-lambda
        (() _verbose)
    ((bool) (set! _verbose bool)))))

  (define pdfimages (find-executable-path "pdfimages" #f))
  (if (not pdfimages) (error "can't find pdfimages"))
  (define (xpdf/pdfimages . args)
    (if (null? args) #f
      (apply system* pdfimages args)))

  (define pdftops (find-executable-path "pdftops" #f))
  (if (not pdftops) (error "can't find pdftops"))
  (define (xpdf/pdftops . args)
    (if (null? args) #f
      (apply system* pdftops args)))

  (define pdfinfo (find-executable-path "pdfinfo" #f))
  (if (not pdfinfo) (error "can't find pdfinfo"))
  ; DIFFERENT FROM man pdfinfo, additional argument is output file
  ; to be consistent with other xpdf commands
  (define (xpdf/pdfinfo . args)
    (cond
      ((null? args) #f)
      ((< (length args) 2)
        (if (xpdf/verbose)
      (printf "xpdf/pdfinfo: too few arguments ~a~%" args))
    #f)
      (else
        (let*
      ((ofile (car (reverse args)))
       (out (open-output-file ofile))
       (args (reverse (cdr (reverse args))))
       (l (apply process*/ports out #f #f pdfinfo args))
           (stdout (list-ref l 0))
           (stdin  (list-ref l 1))
           (pid    (list-ref l 2))
           (stderr (list-ref l 3))
           (proc (list-ref l 4))
           (result #f))
          (proc 'wait)
      (set! result (proc 'status))
          (close-output-port out)
          (close-output-port stdin)
          (close-input-port stderr)
          result))))

  (define pdftotext (find-executable-path "pdftotext" #f))
  (if (not pdftotext) (error "can't find pdftotext"))
  (define (xpdf/pdftotext . args)
    (if (null? args) #f
      (apply system* pdftotext args))))
