;;Why doesn't drsecheme have string-index?

(define (string-index str char)
  (if (not (string? str))
      (error "First argument to string-index must be a string."))
  (if (not (char? char))
      (error "Second argument to string-index must be a char."))
 
  (let ((index  0)
        (length (string-length str)))
   
    ;; manually expanded while loop
    (letrec ((test? (lambda ()
                      (and (< index length)
                           (not (equal? (string-ref str index) char)))))
            
             (body  (lambda ()
                      (if (test?)
                          (begin
                            (set! index (+ index 1))
                            (body))
                          void))))
      (body))
   
    (if (= index length)
        #f
        index)))
