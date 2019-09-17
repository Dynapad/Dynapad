(module composite mzscheme
  (require "../pdfrip/pdfrip.ss")
  (require "../pdfrip/misc.ss")
  (require "../imagemagick/imagemagick.ss")
  (require "../thumbify/thumbify.ss")
  (require "../misc/pathhack.ss")
  (require (lib "list.ss"))

  (provide
    tile2x2 largest_rightside largest_bottomhalf
    composite-NNN composite-NNN? composite-NNNs
    composite.jpg
    composite compositedir)

  (define (image-NNN? x)
    (let ((rexp (regexp "image-[0-9][0-9]*.jpg")))
      (regexp-match rexp (basename x))))

  (define (image-NNNs path)
    (let ((dir (ripdir path)))
      (map (lambda(x) (build-path->string dir x))
        (filter image-NNN? (directory-list->string dir)))))

  (define (firstpage path)
    (build-path->string (ripdir path) "firstpage.jpg"))

  (define (largest path)
    (build-path->string (ripdir path) "largest.jpg"))

  (define (tile2x2 path)
    (let*
      ((dir (ripdir path))
       (composite (composite.jpg path))
       (images (image-NNNs path))
       (firstpage (firstpage path))
       (tile (build-path->string dir "tile.jpg")))
      (if (> (length images) 4)
        (set! images
      (reverse (list-tail (reverse images) (- (length images) 4)))))
      (if (and (not (null? images)) (file-exists? firstpage))
        (apply im/montage
      (append
        (append (list "-geometry" "600x800" "-tile" "2x2") images)
        (list tile))))
      (cond
        ((and (file-exists? firstpage) (file-exists? tile))
      (im/montage "-geometry" "600x800" firstpage tile composite))
    ((file-exists? firstpage)
      (im/montage "-geometry" "600x800" firstpage composite))
    ((file-exists? tile)
      (copy-file-replace tile composite)))
      (if (file-exists? tile)
        (delete-file tile))))

  (define (largest_rightside path)
    (let
      ((composite (composite.jpg path))
       (firstpage (firstpage path))
       (largest (largest path)))
      (cond
        ((and (file-exists? firstpage) (file-exists? largest))
      (im/montage "-geometry" "600x800" firstpage largest composite))
    ((file-exists? firstpage)
      (copy-file-replace firstpage composite))
    ((file-exists? largest)
      (copy-file-replace largest composite)))))
    

  (define (largest_bottomhalf path)
    (let
      ((composite (composite.jpg path))
       (firstpage (firstpage path))
       (largest (largest path)))
      (cond
        ((and (file-exists? firstpage) (file-exists? largest))
      (im/composite "-geometry" "600x800" largest firstpage composite))
    ((file-exists? firstpage)
      (copy-file-replace firstpage composite))
    ((file-exists? largest)
      (copy-file-replace largest composite)))))

  (define funcs (list
    tile2x2
    largest_rightside
    largest_bottomhalf))

  (define %0wd
    (case-lambda
      ((n) (%0wd n 3))
      ((n w)
        (let loop
          ((s (format "~a" n)))
          (cond
            ((>= (string-length s) w) s)
            (else (loop (string-append "0" s))))))))

  (define (composite.jpg path)
    (build-path->string (ripdir path) (format "composite.jpg")))

  (define composite-NNN
    (case-lambda
      ((path) (composite-NNN path 0))
      ((path n)
         (build-path
       (ripdir path)
       (format "composite-~a.jpg" (%0wd n))))))

  (define (composite-NNN? x)
    (let ((rexp (regexp "composite-[0-9][0-9]*.jpg")))
      (regexp-match rexp (basename x))))

  (define (composite-NNNs path)
    (let ((dir (ripdir path)))
      (map (lambda(x) (build-path->string dir x))
        (filter composite-NNN? (directory-list->string dir)))))

  (define (composite path)
    (let loop
      ((funcs funcs)
       (n 0))
      (cond
        ((null? funcs) ())
    ((file-exists? (composite-NNN path n))
      (loop (cdr funcs) (+ n 1)))
    (else
      ((car funcs) path)
          (if (file-exists? (composite.jpg path))
            (begin
              (printf "~a~%" (composite-NNN path n))
          (rename-file-or-directory
            (composite.jpg path)
        (composite-NNN path n))
          (thumbify (composite-NNN path n)))
              (printf "failed to create ~a~%" (composite-NNN path n)))
      (loop (cdr funcs) (+ n 1)))))
    (if (and
          (file-exists? (composite-NNN path 0))
          (not (file-exists? (composite.jpg path))))
      (begin
        (copy-file-replace (composite-NNN path 0) (composite.jpg path))
        (thumbify (composite.jpg path)))))

  (define (compositedir dir)
    (for-each
      (lambda(x)
        (composite (build-path->string dir x)))
      (filter pdf? (directory-list->string dir)))))
