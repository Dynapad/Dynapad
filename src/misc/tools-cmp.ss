(define (cmp-nums a b)
  (cond ((< a b) -1)
        ((> a b) 1)
        (else 0)))

(define (safe-cmp-nums a b) ; handles #f
  (cond ((not a) -1)
	((not b) 1)
        ((< a b) -1)
        ((> a b) 1)
        (else 0)))
 
(define (cmp-strs a b)
  (cond ((string<? a b) -1)
        ((string>? a b) 1)
        (else 0)))

(define (safe-cmp-strs a b) ; handles #f
  (cond ((not a) -1)
	((not b) 1)
	((string<? a b) -1)
        ((string>? a b) 1)
        (else 0)))

(define (cmp-str-prefix a b)
  (cond ((string-find? a b) 0)
        ((string<? a b) -1)
        (else 1)))

(define (cmp-num-pairs a b)
  (if (eqv? (car a) (car b))
      (cmp-nums (cadr a) (cadr b))
      (cmp-nums (car a) (car b)))) 