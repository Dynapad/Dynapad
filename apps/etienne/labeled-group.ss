
(define labeled-group%
  (class container-form%
    (init __dynapad)
    (init __label)

    (inherit dynaclass)
    (inherit-field _dynapad)

    (super-instantiate (__dynapad))
    (dynaclass 'labeled-group%)
    (set! _dynapad __dynapad)

    (name-part _label label)
    (label __label)

    (bind (label) "<Run-ButtonRelease-1>" (lambda (o e) (send this single-click)))

    (send (label) faderange 0.2)
    (send (label) maxsize '(0.7 #t))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Click events
    (define/public (single-click)
      ;; center on the object
      (send dynapad center _label 500))
      ;; make all the contents take group events
      ;; (for-each (lambda (item) (send item takegroupevents #t)) (send this contents)))

    ;; Override add so it automatically makes objects
    ;;   take group events and lower them behind the label
    (rename (super-add add))
    (define/override (add object)
      (super-add object)
      (send object takegroupevents #f)
      (send object raise (label)))

    )
  )

