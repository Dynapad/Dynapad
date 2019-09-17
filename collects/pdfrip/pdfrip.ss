(module pdfrip mzscheme

  (require (lib "list.ss"))
  (require (lib "file.ss"))
  (require "../pdfrip/misc.ss")
  (require "../xpdf/xpdf.ss")
  (require "../thumbify/thumbify.ss")
  (require "../imagemagick/imagemagick.ss")
  (require "../misc/pathhack.ss")

  (provide pdfrip pdfripdir pdfriptree
    pdfrip/force pdfrip/verbose
    make-pdfrip
    ripdir ripdir? ripokay?
    pdf?)

  (define (pdf? path)
    (and
      (string-ci=? (suf path) "pdf")
      (not (eq? (string-ref (basename path) 0) #\.))))

  ; should I check (pdf? path)
  (define (ripdir path)
    (build-path->string (dirname path) (string-append "." (basename path))))

  (define (ripdir? path)
    (and
      (string-ci=? (suf path) "pdf")
      (eq? (string-ref (basename path) 0) #\.)))

  (define (make-ripdir path)
    (let ((ripdir (ripdir path)))
      (if (not (directory-exists? ripdir))
        (make-directory ripdir))))

  (define pdfrip/force
    (let ((_force #f))
      (case-lambda
        (() _force)
    ((bool) (set! _force bool)))))

  (define pdfrip/verbose
    (let ((_verbose #f))
      (case-lambda
        (() _verbose)
    ((bool) (set! _verbose bool)))))

  (define (ripokay? path)
    (let*
      ((ripdir (ripdir path))
       (firstpage (build-path->string ripdir "firstpage.jpg"))
       (largest (build-path->string ripdir "largest.jpg")))
      (and (file-exists? firstpage) (file-exists? largest))))

  (define 1k 1024)
  (define 100k (* 100 1k))

  ; remove leading and trailing spaces
  (define (remove-space s)
    (let*
      ((r (regexp "^ *(.*[^ ]) *$"))
       (l (regexp-match r s)))
       (if l (cadr l) "")))

  (define (make-pdfrip path ripdir)
    (if (not (directory-exists? ripdir))
      (error "ripdir ~a doesn't exist" ripdir))
    (let
      ((path (path->complete-path path))
       (odir (current-directory))
       (largest #f))
      ; ============== CHANGE current-directory ==============
      (current-directory ripdir)

      (xpdf/pdfimages path "image")
      (for-each
        (lambda(x)
      (cond
        ((and (pbm? x) (< (file-size x) 1k)) (delete-file x))
        ((and (pbm? x) (> (file-size x) 100k)) (delete-file x))
        ((and (ppm? x) (< (file-size x) 1k)) (delete-file x))
        (else
          (im/convert x (string-append (basename x #f) ".jpg"))
          (delete-file x))))
    (filter (lambda(x) (or (pbm? x) (ppm? x))) (directory-list)))

    ; I already removed any 0 length pbm/ppm
    ; Do I need to check the jpg for 0 length?

    (for-each
      (lambda(x)
        (thumbify x)
        (cond
          ((not largest)
            (set! largest (list x (file-size x))))
          ((> (file-size x) (cadr largest))
            (set! largest (list x (file-size x))))))
      (filter jpg? (directory-list)))

    ; Check for largest being 0 size?

    (when largest
      (let ((jpg "largest.jpg"))
        (copy-file (car largest) jpg)
        (thumbify jpg)))

    (let
      ((ps "firstpage.ps")
       (jpg "firstpage.jpg"))
          (xpdf/pdftops "-f" "1" "-l" "1" path ps)
      (cond
        ((not (file-exists? ps)) #f)
        ((= (file-size ps) 0)
          (delete-file ps))
        (else
          (im/convert ps jpg)
          (delete-file ps)
          (thumbify jpg))))

    (xpdf/pdfinfo path "info.txt")

    (xpdf/pdftotext path "text")

    ; extract title from text
    ; if copying from pdf is not allowed, then text will not exist
    (if (file-exists? "text")
      (let
        ((in (open-input-file "text"))
         (line #f))
        (file-seek in 0)
        (set! line (read-line in))
        (close-input-port in)
        (if (eof-object? line) (set! line ""))
        (set! line (remove-space line))
        (let ((out (open-output-file "title")))
          (display line out)
          (close-output-port out))))

    ; extract author from text
    ; if copying from pdf is not allowed, then text will not exist
    (if (file-exists? "text")
      (let
        ((in (open-input-file "text"))
         (line #f))
        (file-seek in 2)
        (set! line (read-line in))
        (close-input-port in)
        (if (eof-object? line) (set! line ""))
        (set! line (remove-space line))
        (let ((out (open-output-file "author")))
          (display line out)
          (close-output-port out))))

      ; ============== RESTORE current-directory ==============
      (current-directory odir)))

  (define (pdfrip path)
    (cond
      ((not (file-exists? path))
        (if (pdfrip/verbose) (printf "~a is not a file~%" path))
    #f)
      ((not (pdf? path))
        (if (pdfrip/verbose) (printf "~a is not a pdf~%" path))
    #f)
      ((and (directory-exists? (ripdir path)) (not (pdfrip/force)))
        (list path #f))
      (else
     (if (directory-exists? (ripdir path))
       (delete-directory/files (ripdir path)))
     (make-ripdir path)
     (make-pdfrip path (ripdir path))
     (if (pdfrip/verbose) (printf "~a~%" path))
     (list path #t))))

  (define (pdfripdir dir)
    (map
      (lambda(x) (pdfrip (build-path->string dir x)))
      (filter pdf?  (directory-list->string dir))))

  (define (pdfriptree dir)
    (cons
      (pdfripdir dir)
      (map
        (lambda(x) (pdfriptree (build-path->string dir x)))
    (filter
      (lambda(x)
        (and
          (directory-exists? (build-path->string dir x))
          (not (string=? x "thumbs"))
          (not (string=? x "bad"))
          (not (ripdir? x))))
      (directory-list->string dir))))))
