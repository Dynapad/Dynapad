(module import mzscheme
  (require (lib "file.ss"))
  (require (lib "list.ss"))
  (require "../pdfrip/pdfrip.ss")
  (require "../pdfrip/composite.ss")
  (require "../misc/pathhack.ss")

  (provide importdir importtree
    flatten remove-all-but-pdf movebad flatten-and-importtree)

  (define flatten
    (case-lambda
      ((dir) (flatten dir dir))
      ((dir dest)
        (let ((port (open-output-file (build-path->string dest "map") 'append)))
	  (flatten dir dest port)
	  (close-output-port port)))
      ((dir dest port)
        (for-each
          (lambda(x)
            (cond
	      ((string=? x "thumbs") #f)
	      ((string=? x "bad") #f)
	      ((ripdir? x) #f)
	      ((string=? x "map") #f)
	      ((pdf? x)
	        (cond
	          ((string=? dir dest) #f)
		  ((not (file-exists? (build-path->string dest x)))
	            ;(printf "move ~a to ~a~%"
		    ;  (build-path->string dir x)
		    ;  (build-path->string dest x))
		    (fprintf port "~a ~a~%"
		      x
		      (build-path->string dir x))
		    (rename-file-or-directory
		      (build-path->string dir x)
		      (build-path->string dest x)))
		  (else
	            (let loop
		      ((newx x))
	              (cond
		        ((file-exists? (build-path->string dest newx))
		          (printf "rename to ~a~%" (string-append "X" newx))
		          (loop (string-append "X" newx)))
		        (else
	                  ;(printf "move ~a to ~a~%"
		          ;  (build-path->string dir x)
		          ;  (build-path->string dest newx))
		          (fprintf port "~a ~a~%"
			    newx
		            (build-path->string dir x))
			  (rename-file-or-directory
			    (build-path->string dir x)
			    (build-path->string dest newx))))))))
	      ((directory-exists? (build-path->string dir x))
	        (flatten (build-path->string dir x) dest))))
          (directory-list->string dir)))))

  (define (remove-all-but-pdf dir)
    (for-each
      (lambda(x)
        (cond
	  ((string=? x "thumbs") #f)
	  ((string=? x "bad") #f)
	  ((ripdir? x) #f)
	  ((string=? x "map") #f)
	  ((pdf? x) #f)
	  (else
	    (printf "removing ~a~%" (build-path->string dir x))
	    (delete-directory/files (build-path->string dir x)))))
      (directory-list->string dir)))

  (define (movebad dir)
    (if (not (directory-exists? (build-path->string dir "bad")))
      (make-directory (build-path->string dir "bad")))
    (for-each
      (lambda(x)
        (when (not (ripokay? (build-path->string dir x)))
	  (printf "pdfrip of ~a failed~%" (build-path->string dir x))
	  (rename-file-or-directory
	    (build-path->string dir x)
	    (build-path->string dir "bad" x))
	  (if (directory-exists? (ripdir (build-path->string dir x)))
	    (rename-file-or-directory
	    (ripdir (build-path->string dir x))
	    (build-path->string dir "bad" (ripdir x))))))
      (filter pdf? (directory-list->string dir))))

  (define (importdir dir)
    (pdfripdir dir)
    (movebad dir)
    ;(compositedir dir))
    )

  (define (importtree dir)
    (importdir dir)
    (for-each
      (lambda(x)
	(cond
	  ((string=? x "thumbs") #f)
	  ((string=? x "bad") #f)
	  ((ripdir? x) #f)
	  (else (importtree (build-path->string dir x)))))
      (filter
        (lambda(x) (directory-exists? (build-path->string dir x)))
	(directory-list->string dir))))

  (define (flatten-and-importtree dir)
    (flatten dir)
    (remove-all-but-pdf dir)
    (importdir dir)))
