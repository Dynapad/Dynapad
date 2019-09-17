(require (lib "process.ss"))

(define (diff l1 l2)
  (cond ((null? l1) '())
        ((member (car l1) l2) (diff (cdr l1) l2))
        (else (append (list (car l1)) (diff (cdr l1) l2)))))

(define (overlap l1 l2)
  (if (null? l1)
      ()
      (if (memq (car l1) l2)
          (append (list (car l1)) (overlap (cdr l1) l2))
          (overlap (cdr l1) l2))))

(define (execute return command)
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

(define (decv->hexv v)
  (cond ((and (>= v 0) (<= v 9)) (number->string v))
        ((= v 10) "A")
        ((= v 11) "B")
        ((= v 12) "C")
        ((= v 13) "D")
        ((= v 14) "E")
        ((= v 15) "F")))

(define (dec->hex n)
  (let ((d (floor (modulo n 16)))
        (r (floor (/ n 16))))
    (if (<= r 0)
        (decv->hexv d)
        (string-append (dec->hex r) (decv->hexv d)))))

(define (join d l)
  (if (> (length l) 1)
      (string-append (car l) d (join d (cdr l)))
      (if (null? l) "" (car l))))
