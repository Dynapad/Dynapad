(dynaload "actor.ss")

(define dt-actor%
  (class actor%
    (super-instantiate ())
    (field (down-lambda (lambda always #f))
           (move-lambda (lambda always #f))
           (up-lambda (lambda always #f)))
    (public down move up down-function move-function up-function)

    (define (down . args) (apply down-lambda args))
    (define (move . args) (apply move-lambda args))
    (define (up . args) (apply up-lambda args))

    (define down-function (get/set down-lambda))
    (define move-function (get/set move-lambda))
    (define up-function (get/set up-lambda))
  )
)
