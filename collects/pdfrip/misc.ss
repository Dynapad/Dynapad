(module misc mzscheme

  (require "../misc/pathhack.ss")

  (provide
    dirname basename
    suf jpg? pbm? ppm?
    copy-file-replace file-seek)

  (define dirname
    (case-lambda
      (() (dirname "."))
      ((path)
        (let*-values
          (((dir name dir?) (split-path->string path))
           ((dir) (if (eq? dir 'relative) "." dir)))
          dir))))

  (define pathsuf-rexp (regexp "(.*)\\.(.*)"))

  (define (suf path)
    (let ((l (regexp-match pathsuf-rexp path)))
    (if l
      (list-ref l 2)
      "")))

  (define (pbm? path)
    (string-ci=? (suf path) "pbm"))

  (define (ppm? path)
    (string-ci=? (suf path) "ppm"))

  (define (jpg? path)
    (string-ci=? (suf path) "jpg"))

  (define basename
    (case-lambda
      ((path) (basename path #t))
      ((path suf?)
        (let*-values
          (((dir name dir?) (split-path->string path))
	   ((l) (regexp-match pathsuf-rexp name)))
	  (cond
	    (dir? "")
	    (suf? name)
	    (else (cadr l)))))))
	  
  (define (copy-file-replace src dest)
    (if (file-exists? dest)
      (delete-file dest))
    (copy-file src dest))

  (define (file-seek port seek)
    (file-position port 0)
    (let loop
      ((line 0))
      (cond
        ((= line seek) #t)
        ((eof-object? (read-line port)) #f)
        (else
         (loop (+ line 1)))))))
