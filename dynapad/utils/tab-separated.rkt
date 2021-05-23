
(require (lib "pregexp.rkt"))

(define (read-tab-separated-file path)
  (define results ())
  (if (not (file-exists? path))
      (error "file not found")
      ;else
      (let ((port (open-input-file path))
            (rexp "")
            (done #f)
            (textline "")
            )
        (while (not done)
          (set! textline (read-line port))
          (when (not (eq? textline eof))
            (push!
             (pregexp-split "\t" textline)
             results
             )
            )
          (set! done (eq? textline eof))
          )
        )
      )
  results
  )


