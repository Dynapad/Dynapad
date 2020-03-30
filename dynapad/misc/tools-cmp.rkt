#lang racket/base

(require (only-in racket/string string-prefix?))

(provide cmp-nums)

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

; not entirely clear what the behavior is supposed to be
; and string-find? is defined nowhere
; I _think_ it returns a total ordering on all strings
; based on prefix comparison so if a < b then -1 < 0
; so string-prefix? -> 0 would make sense
; however this is not symmetric, what if b is a prefix of a?
(define (cmp-str-prefix a b)
  (cond ((string-prefix? a b) 0)
        ((string<? a b) -1)
        (else 1)))

(define (cmp-num-pairs a b)
  (if (eqv? (car a) (car b))
      (cmp-nums (cadr a) (cadr b))
      (cmp-nums (car a) (car b))))
