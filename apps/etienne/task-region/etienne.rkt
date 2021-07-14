(define (join d l)
  (if (> (length l) 1)
      (string-append (car l) d (join d (cdr l)))
      (if (null? l) "" (car l))))