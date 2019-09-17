(module movebad mzscheme
  (require (lib "file.ss"))
  (require (lib "list.ss"))
  (require "../pdfrip/pdfrip.ss")
  (require "../pdfrip/composite.ss")
  (require "../pdfrip/import.ss")
  (require "../misc/pathhack.ss")

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
