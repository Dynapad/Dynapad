(module pathhack mzscheme
  (provide
    directory-list->string
    build-path->string
    find-system-path->string
    split-path->string
    explode-path->string
)

(require (lib "file.ss"))

; work around for v299, path is now type and not string
(define (directory-list->string arg)
  (let ((l (directory-list arg)))
    (if (list? l)
      (map (lambda(p) (if (path? p) (path->string p) p)) l)
      l)))

; work around for v299, path is now type and not string
(define (build-path->string . args)
  (let ((p (apply build-path args)))
    (if (path? p) (path->string p) p)))

; work around for v299, path is now type and not string
(define (find-system-path->string arg)
  (let ((p (find-system-path arg)))
    (if (path? p) (path->string p) p)))

; work around for v299, path is now type and not string
(define (split-path->string arg)
  (let-values (((dir name dir?) (split-path arg)))
    (values
      (if (path? dir) (path->string dir) dir)
      (if (path? name) (path->string name) name)
      dir?)))

; work around for v299, path is now type and not string
(define (explode-path->string arg)
  (let ((l (explode-path arg)))
    (if (list? l)
      (map (lambda(p) (if (path? p) (path->string p) p)) l)
      l)))

)
