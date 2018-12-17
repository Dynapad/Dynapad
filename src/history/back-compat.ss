; This whole file is ugly hacky code just to support Dan's thesis log files.


(define titled-resizable-frame-container% resizable-dissolving-fusing-frame-container%)
(define simple-linear-projector% linear-projector%)
(define linear-projector% labeled-linear-projector%)

(define brush% mutator%)

; this is just a dummy class which redirects 'member calls
;  to the appropriate samedir-clique
(define samedir-relation%
  (class linkable-obj%
    (field (_name ""))

    (super-instantiate ())
    (abstract-objects-callbacks 'add
       (lambda () (list this)) this) ;includes this in things to be saved

    (define/public (delete-all)
      (send this delete))
    (define/public (write-all) null)

    ;(define (id) (set! _id id))
    (define/public (name str) (set! _name str))
    (define/public (members op obj)
      (unless (equal? op 'add) (error "back-compat.ss found unknown msg: " op))
      (send (samedir-relation) add-objs-with-key obj _name)

      ;what the hell; use this opportunity to put it into a duplicate-tracker also
      (track-duplicates-of-objs obj)
      )

))

;(with ... (ic () ...) ... (send obj refer- "#grp" 'members ''add obj)