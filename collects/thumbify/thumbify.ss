(module thumbify mzscheme

  ; (require (file "thumbify.ss"))
  ; (thumbify "a/b/c.jpg") or (thumbify "a/b/c.jpg" 150)
  ; (thumbifydir "a/b") or (thumbifydir "a/b" 150)
  ; (thumbifytree "a/b") or (thumbifytree "a/b" 150)
  ;   recurses down the tree
  ; thumbify only thumbifies files with image suffixes (see sufs below)
  ; thumbify only thumbifies files that have no thumbs, unless thumbify/force is #t
  ; default resolution is 125 (see RES below)
  ; thumbify creates thumbs/res directory, if it doesn't exist

  (provide thumbify thumbifydir thumbifytree
    thumbdir thumbdir-exists? make-thumbdir
    thumb thumb-exists? make-thumb
    thumbify/force thumbify/verbose
    image?)

  (require (lib "list.ss"))
  (require (lib "process.ss"))
  (require "../imagemagick/imagemagick.ss")
  (require "../misc/pathhack.ss")

  (define RES 125)

  (define sufs (list "jpg" "gif" "png" "bmp" "tif" "pbm" "ppm" "pnm"))

  (define image-suf?
    (case-lambda
      ((x) (image-suf? x sufs))
      ((x sufs)
        (cond
      ((null? sufs) #f)
      ((string-ci=? x (car sufs)) #t)
      (else (image-suf? x (cdr sufs)))))))
      
  (define pathsuf-rexp (regexp "(.*)\\.(.*)"))

  (define (image? path)
    (let
      ((l (regexp-match pathsuf-rexp path)))
      (cond
        ((not (list? l)) #f)
    (else (image-suf? (list-ref l 2))))))

  (define dirname
    (case-lambda
      (() (dirname "."))
      ((path)
        (let*-values
          (((dir name dir?) (split-path->string path))
           ((dir) (if (eq? dir 'relative) "." dir)))
          dir))))

  (define basename
    (case-lambda
      (() (basename "."))
      ((path)
        (let*-values
          (((dir name dir?) (split-path->string path)))
      (if dir? "" name)))))

  ; I know, this will be confusing
  ; thumbsdir is <imagedir>/thumbs and only used internally
  ; thumbdir  is <imagedir>/thumbs/res
  (define thumbsdir
    (case-lambda
      (() (thumbsdir "."))
      ((path)
        (build-path->string (dirname path) "thumbs"))))

  (define (thumbsdir? path)
    (string=? (basename path) "thumbs"))

  (define thumbsdir-exists?
    (case-lambda
      (() (thumbsdir-exists? "."))
      ((path)
        (directory-exists? (thumbsdir path)))))

  ; See above
  (define thumbdir
    (case-lambda
      (() (thumbdir "." RES))
      ((path) (thumbdir path RES))
      ((path res)
        (build-path->string (thumbsdir path) (number->string res)))))

  (define thumbdir-exists?
    (case-lambda
      (() (thumbdir-exists? "." RES))
      ((path) (thumbdir-exists? path RES))
      ((path res)
    (directory-exists? (thumbdir path res)))))

  (define make-thumbdir
    (case-lambda
      (() (make-thumbdir "." RES))
      ((path) (make-thumbdir path RES))
      ((path res)
        (let*
      ((dir (dirname path))
           (thumbs (thumbsdir path))
           (thumbs/res (build-path->string thumbs (number->string res))))
          (if (not (directory-exists? thumbs))
            (make-directory thumbs))
          (if (not (directory-exists? thumbs/res))
            (make-directory thumbs/res))))))

  (define thumb
    (case-lambda
      ((path) (thumb path RES))
      ((path res)
    (cond
      ((not (image? path)) #f)
      (else
            (let*
          ((name (basename path))
           (l (regexp-match pathsuf-rexp name))
           (base (list-ref l 1))
           (suf (list-ref l 2)))
           (build-path->string
             (thumbdir path res)
             (format "~a-~a.~a" base res suf))))))))

  (define thumb-exists?
    (case-lambda
      ((path) (thumb-exists? path RES))
      ((path res)
    (cond
      ((not (image? path)) #f)
          (else (file-exists? (thumb path res)))))))

  (define thumbify/force
    (let ((_force #f))
      (case-lambda
        (() _force)
    ((bool) (set! _force bool)))))

  (define thumbify/verbose
    (let ((_verbose #f))
      (case-lambda
        (() _verbose)
    ((bool) (set! _verbose bool)))))

  (define make-thumb
    (case-lambda
      ((path thumbpath) (make-thumb path thumbpath RES))
      ((path thumbpath res)
    (im/resize path thumbpath res))))

  (define thumbify
    (case-lambda
      ((path) (thumbify path RES))
      ((path res)
        (cond
      ((not (file-exists? path))
            (if (thumbify/verbose) (printf "~a is not a file~%" path))
        #f)
      ((not (image? path))
        (if (thumbify/verbose) (printf "~a is not a image~%" path))
        #f)
      ((and (thumb-exists? path res) (not (thumbify/force)))
        (list path res #f))
      (else
        (if (thumb-exists? path res)
          (delete-file (thumb path res)))
        (make-thumbdir path res)
        (make-thumb path (thumb path res) res)
        (if (thumbify/verbose) (printf "~a~%" path))
        (list path res #t))))))

  (define thumbifydir
    (case-lambda
      (() (thumbifydir "." RES))
      ((dir) (thumbifydir dir RES))
      ((dir res)
    (cond
      ((not (directory-exists? dir))
            (if (thumbify/verbose) (printf "~a is not a directory~%" dir))
        #f)
      (else
            (map
          (lambda(x)
        (thumbify (build-path->string dir x) res))
          (filter image? (directory-list->string dir))))))))

  (define thumbifytree
    (case-lambda
      (() (thumbifytree "." RES))
      ((dir) (thumbifytree dir RES))
      ((dir res)
        (cond
        ((not (directory-exists? dir))
            (if (thumbify/verbose) (printf "~a is not a directory~%" dir))
        #f)
        (else
        (cons
          (thumbifydir dir res)
              (map
                (lambda(x)
                (thumbifytree (build-path->string dir x) res))
              (filter
          (lambda(x)
            (let ((path (build-path->string dir x)))
              (and (directory-exists? path)
                   (not (thumbsdir? path)))))
          (directory-list->string dir))))))))))
