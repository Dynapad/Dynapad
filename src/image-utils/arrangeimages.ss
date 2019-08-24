; This file is obsolete; import operations subsumed by import-dirs.ss

(dynaload "tools-lists.ss")

; grandfather in exising image collections
; when saved, they'll become image% objects
(define workspaceimage%
  (class image%
    (super-instantiate())))

(define (gifjpg-list dir)
  (filter
    (lambda(x) (sch_imagep (build-path->string dir x)))
    (directory-list->string dir)))

(define (hires-list dir)
  (filter
    (lambda (x)
      (cond
       ;((regexp-match thumb_rexp x) #f)
	((sch_imagep (build-path->string dir x)) x)
	(else #f)))
    (sort (lambda (a b)
         (or (< (string-length a) (string-length b))
             (string<? a b)))
      (directory-list->string dir))))

(define (hires-list-by-size dir)
 (let ((img-files (filter
		   (lambda (f) (sch_imagep (build-path->string dir f)))
		   (directory-list->string dir))))
   (sort (lambda (a b)
	   (> (file-size (build-path->string dir a)) (file-size (build-path->string dir b))))
	 img-files)))

; image-dir-callbacks allow functions to be called after (arrangedir dir).
; callbacks take two args:  dir-fullpath and imagelist
(define *image-dir-callbacks* null)
(define (push-image-dir-callback fn) (push! fn *image-dir-callbacks*))
(define (append-image-dir-callback fn) (endpush! *image-dir-callbacks* fn))
(define (remove-image-dir-callback fn)
  (set! *image-dir-callbacks* (remq fn *image-dir-callbacks*)))

; the optional RE argument provides a filter mechanism
; "s.*\\.jpg" --> files that start with s and have suffix ".jpg"
(define (arrangedir dir . RE)
  (if (not (directory-exists? dir))
    (begin (error "arrangeimages: directory doesn't exist " dir) #f)
    (let ((filelist (hires-list dir))
          (images '()))

      (when (not (null? RE))
        (set! RE (regexp (string-append "^" (car RE) "$"))) ;; --> ^...$ force exact match
        (set! filelist (filter (lambda (f) (regexp-match RE f)) filelist)))

      (set! images (arrangeimages (map (lambda (file) (build-path->string dir file)) filelist)))

      (for-each (lambda (fn) (fn dir images)) *image-dir-callbacks*)

      images)))

(define *current-z-plane* #f)
(define (set-load-z newz) (set! *current-z-plane* newz))

(define (arrangeimages files)
  (let* ((images null)
         (vu (send dynapad view))
         (view_x (car vu))
         (view_y (cadr vu))
         (zfac (/ 1.0 (caddr vu)))
         (x view_x)
         (y view_y)
         (numrows (sqrt (length files)))
         )
    (when *current-z-plane* (set! zfac *current-z-plane*))
    (for-each
      (lambda (file)
	(when (sch_imagep file)
          (set! images
            (append images (list
              (make-object image% dynapad file (list x y zfac)))))
	  (set! x (+ x (* 200 zfac)))
          (when (> (- x view_x) (* (* 200 numrows) zfac))
            (set! x view_x)
            (set! y (- y (* 200 zfac))))))
      files)
    ;undo hook replaces: images))
    (undoify-fresh-objs images)))

(define (freeimages images)
  (map
    (lambda (image)
      (send image delete))
    images))

;-----------------------------------------------------------------
(define (get-subdirs dir)
  (filter
    (lambda (x) (directory-exists? x))
    (map (lambda (d) (build-path->string dir d)) (directory-list->string dir))))

(define (arrangedir-and-subdirs dir . RE)
  (if (not (directory-exists? dir))
    (begin (error "arrangeimages: directory doesn't exist " dir) #f)
    (let ((filelist (hires-list dir))
          (dirlist (get-subdirs dir))
         )
      (set! filelist (map (lambda (file) (build-path->string dir file)) filelist))
      (foreach dirlist (lambda (subdir)
        (set! filelist (append filelist
          (map (lambda (file) (build-path->string subdir file)) (hires-list subdir))
        ))
      ))
      (when (not (null? RE))
        (set! RE (regexp (string-append "^" (car RE) "$")))
        (set! filelist (filter (lambda (f) (regexp-match RE f)) filelist)))
      (arrangeimages filelist) )))
;-----------------------------------------------------------------

(define *default_directory* #f)
(define (Arrange-Images)
  (let ((dir (get-directory "Arrange directory" #f *default_directory* null)))
    (when dir
      (set! *default_directory* dir)
      (show-possible-delay dynapad
        (arrangedir-and-center dir #t))
      )))

(define (arrangedir-and-center dir . returnflag)
  (let ((image-list (arrangedir dir)))
    (when image-list (send dynapad center image-list 1000 #t))
    (if (null? returnflag) (length image-list) image-list)
    ))

;-----------------------------------------------------------------

(define pdf%
  (class image%
    (init initpad initpath (initposition #f))
    (inherit hirespath dynaclass position writeoptions)
    (field (_path #f) (_name #f) (_dotdir #f) (_author #f) (_title #f)
      (_firstpage #f) (_images '()) (_largest #f)
      (_composite #f) (_composites '()) (_text #f))

    (override write)

    (define/public (path)
      _path)
    (define/public (name)
      _name)
    (define/public (dotdir)
      _dotdir)
    (define/public (author)
      _author)
    (define/public (title)
      _title)
    (define/public (firstpage)
      _firstpage)
    (define/public (images)
      _images)
    (define/public (largest)
      _largest)
    (define/public (composite)
      _composite)
    (define/public (composites)
      _composites)
    (define/public (text)
      _text)

    (define (write)
      `(let ((obj (make-object ,(dynaclass) dynapad ,(path))))
         (send* obj
           ,@(writeoptions))
	 obj))

    (set! _path (path->complete-path initpath))
    (let-values (((base name dir?) (split-path->string _path)))
      (set! _name name)
      (set! _dotdir (build-path->string base (string-append "." name))))

    (define (dotpath name)
      (let ((x (build-path->string _dotdir name)))
        (if (file-exists? x) x #f)))

    (set! _firstpage (dotpath "firstpage.jpg"))
    (set! _largest (dotpath "largest.jpg"))
    (set! _composite (dotpath "composite.jpg"))
    (set! _text (dotpath "text.jpg"))

    (define image_regexp (regexp "image-[0-9][0-9][0-9].[jJ][pP][gG]"))

    (when (directory-exists? _dotdir)
    (set! _images
      (map (lambda(x) (dotpath x))
        (filter (lambda(file) (regexp-match image_regexp file))
          (sort string<? (directory-list->string _dotdir)))))
    )

    (define composite_regexp (regexp "composite-[0-9][0-9][0-9].[jJ][pP][gG]"))

    (when (directory-exists? _dotdir)
    (set! _composites
      (map (lambda(x) (dotpath x))
        (filter (lambda(file) (regexp-match composite_regexp file))
          (sort string<? (directory-list->string _dotdir)))))
    )

    (super-instantiate(initpad))
    (when initposition
      (position initposition))
    (when _composite (hirespath _composite))
    (dynaclass 'pdf%)))

(define pdf_rexp (regexp "(.*[/\\])?(.*)(\\.([pP][dD][fF]))$"))
(define *pdf_rexp* pdf_rexp)
(define *image_rexp* (regexp "\\.((jpe?g)|(JPE?G)|(gif)|(GIF))$"))

(define (pdf-list dir)
  (filter
    (lambda (x)
      (and
        (regexp-match pdf_rexp x)
	(file-exists? (build-path->string dir x))))
    (sort (lambda (a b)
         (or (< (string-length a) (string-length b))
             (string<? a b)))
      (directory-list->string dir))))

(define (make-pdf-at-position file x y zfac)
 (make-object pdf% dynapad file (list x y zfac)))

(define (arrangepdfs files)
  (set! files
    (filter
      (lambda (file) (and (regexp-match pdf_rexp file) (file-exists? file)))
      files))
  (let* ((pdfs null)
         (vu (send dynapad view))
         (view_x (car vu))
         (view_y (cadr vu))
         (zfac (/ 1.0 (caddr vu)))
         (x view_x)
         (y view_y)
         (numrows (sqrt (length files)))
         )
    (when *current-z-plane* (set! zfac *current-z-plane*))
    (for-each
      (lambda (file)
        (set! pdfs
          (append pdfs (list
            (make-pdf-at-position file x y zfac))))
	(set! x (+ x (* 200 zfac)))
        (when (> (- x view_x) (* (* 200 numrows) zfac))
          (set! x view_x)
          (set! y (- y (* 200 zfac)))))
      files)
    pdfs))

(define (arrangepdfdir dir)
  (if (not (directory-exists? dir))
    (begin (error "arrangepdfs: directory doesn't exist " dir) #f)
    (let ((pdfs (pdf-list dir)))

      (set! pdfs (arrangepdfs
        (map (lambda (file) (build-path->string dir file)) pdfs)))

      (for-each (lambda (fn) (fn dir pdfs)) *image-dir-callbacks*)

      pdfs)))

(define (arrangepdfdir-and-center dir . returnflag)
  (let ((pdf-list (arrangepdfdir dir)))
    (when pdf-list (send dynapad center pdf-list 1000 #t))
    (if (null? returnflag) (length pdf-list) pdf-list)
    ))

(define *pdf_default_directory* #f)

(define (Arrange-Pdfs)
  (let ((dir (get-directory "Arrange directory" #f *pdf_default_directory* null))
        (pdflist #f))
    (when dir
      (set! *pdf_default_directory* dir)
      (send dynapad cursor 3)
      (set! pdflist
        (arrangepdfdir-and-center dir #t)
      )
      (gui-update-mode dynapad) ; reset cursor to mode-specific appearance
      (undoify-fresh-objs  pdflist)
      )))
