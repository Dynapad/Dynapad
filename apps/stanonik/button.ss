(require (lib "class.ss"))
(load-relative "workspace.ss")

(define button%
  (class oval%
    (init pad)
    (inherit findable sticky coords bind layer dynaclass)
    (rename (super-slide slide) (super-position position))

    (define/override (slide . args)
      (let ((s (sticky)))
        (sticky #f)
        (super-slide . args)
        (sticky s)))

    (define/override (position . args)
      (let ((s (sticky)))
        (sticky #f)
        (super-position . args)
        (sticky s)))

    ; so even if left findable, not written
    (define/override (write)
      #f)

    ; hmm, might need to delete button?
    (define/override (delete)
      #f)

    (super-instantiate (pad))
    (dynaclass 'button%)
    (layer (make-object layer% pad "button"))
    (sticky #t)
    (coords '(0 0 25 25))
    (findable #f)

    (bind "<Mod1-ButtonPress-1>"
      (lambda(o e)
        (findable (if (findable) #f #t))))))

(define r (make-object button% dynapad))
(send r position '(-100 100 1))
(send r fill "red")
(send r bind "<ButtonPress-1>"
  (lambda(o e)
    (changemode dynapad "Run")
    #f))

(define g (make-object button% dynapad))
(send g position '(-50 100 1))
(send g fill "green")
(send g bind "<ButtonPress-1>"
  (lambda(o e)
    (changemode dynapad "Select")
    #f))
