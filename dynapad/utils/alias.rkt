#lang racket/base

(require dynapad/misc/misc)

(provide *pdf-viewer-app*
         *url-alias-alist*
         alias-url
         )

; TODO provide a way to set this ...
(define *pdf-viewer-app* "xpdf") ;set your own in .padsetup

(define *url-alias-alist* null)
(define (alias-url from to)
  (let ((found (assoc from *url-alias-alist*)))
    (if found
        (if (not (equal? (cadr found) to))
            (error "URL alias already exists: " found)
            #f)
        (begin
          (push! (list from to) *url-alias-alist*)
          #t))))