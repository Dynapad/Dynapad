(require (lib "file.ss"))

(load-relative "dir-browser.ss")
(load-relative "icons.ss")
(dynaload "launcher.ss")
(load-relative "thermometer.ss")

;; --Global Objects --
(define *launcher* (make-object launcher%)) ;Adds bindings to file icons
(define thermometer (make-object thermometer%)) ;Adds temp. to file icons

;; -- Global settings --
(set! *cowboy-zoom-on-objects* #f)


;; -- Global functions --
(define (directory-list-ignore-hidden directory)
  (filter
   (lambda (item)
     (not (is-hidden? item)))
     (directory-list->string directory)))

(define (is-hidden? item)
  (char=? (car (string->list item)) #\.))


;; From ints 0 to 32767
(define (generate-random-pos . args)
  (define zfac (if (null? args) #f (car args)))
  (let* ((max (expt 2 15))
     (xp (random max))
     (yp (random max))
     (zp (if zfac zfac (random max))))
    (list xp yp zp)))
