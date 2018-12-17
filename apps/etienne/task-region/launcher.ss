(require (lib "process.ss" "mzlib"))

(define launcher%
  (class object%
    (super-instantiate ())

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Internal functions
    (define/private (execute return command)
      (define p (process command))
      (define i (car    p))
      (define o (cadr   p))
      (define s (cadddr p))
      (define r (list))

      (if (not return)
	  (do ((line "" (read-line i)))
	      ((eq? line eof))
	    (push! line r)))

      (close-input-port  i)
      (close-output-port o)
      (close-input-port  s)

      r)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods
    (define/public (web-browser url)
      (execute #t (format "konqueror ~a" url)))
    )
  )