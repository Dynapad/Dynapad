#lang racket/base

(provide append-mainmenu-constructor
         include-application-mainmenu-constructors 
         )

(define *application-mainmenu-constructors* null)
;list of fns to generate app-specific submenus

(define (append-mainmenu-constructor fn)
  (set! *application-mainmenu-constructors*
        (append *application-mainmenu-constructors* (list fn))))

(define (include-application-mainmenu-constructors popmenu object)
  (for-each (lambda (fn) (fn popmenu object)) *application-mainmenu-constructors*))

