(module movebad mzscheme
  (require (lib "file.rkt"))
  (require (lib "list.rkt"))
  (require "../pdfrip/pdfrip.rkt")
  (require "../pdfrip/composite.rkt")
  (require "../pdfrip/import.rkt")
  (require "../misc/pathhack.rkt")

  (provide movebaddir movebadtree)

  (define (movebaddir dir)
    (movebad dir)
    )

  (define (movebadtree dir)
    (movebaddir dir)
    (for-each
     (lambda(x)
       (cond
         ((string=? x "thumbs") #f)
         ((string=? x "bad") #f)
         ((ripdir? x) #f)
         (else (movebadtree (build-path->string dir x)))))
     (filter
      (lambda(x) (directory-exists? (build-path->string dir x)))
      (directory-list->string dir)))))
