
(define control-panel%
  (class container-form%
    (init dynaptr)

    (field (_orientation 'h))
    (field (_ax           0))
    (field (_ay           0))
    (field (_limiting     32))

    (inherit dynaclass)
    (super-instantiate (dynaptr))
    (dynaclass 'control-panel%)

    (name-part _panel panel)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Instantiation stuff
    ;;
    ;; Make the panel.
    (panel (make-object rect% dynaptr (list _ax _ay _ax _ay)))
    (send (panel) fill "green")
    (send (panel) transparency 0.75)

    ;; Draw the items along the control panel
    (define/public (align-content)
      (define x _ax)
      (define y _ay)

      (for-each
       (lambda (item)
         (define w (send item width))
         (define h (send item height))
         ;;
         ;; place the item
         (if (eq? _orientation 'h)
             (send item bbox (list x y (+ x w) (+ y h)))
             (send item bbox (list (- x w) (- y h) x y)))
         ;;
         ;; update our coordinates
         (if (eq? _orientation 'h)
             (set! x (+ x w))
             (set! y (- y h))))
       (send this contents)))

    ;; Returns the width of the control panel
    (define/public (content-length dim)
      (define length 0)
      (for-each
       (lambda (item)
         (if (eq? dim 'h)
             (set! length (+ length (send item width)))
             (set! length (+ length (send item height)))))
       (send this contents))
      length)

    ;; Draws the underlying control panel
    (define/public (draw-panel)
      (define w (if (eq? _orientation 'h) (content-length 'h) _limiting))
      (define h (if (eq? _orientation 'v) (content-length 'v) _limiting))

      (if (eq? _orientation 'h)
          (send (panel) bbox (list _ax _ay (+ _ax w) (+ _ay h)))
          (send (panel) bbox (list _ax _ay (- _ax w) (- _ay h)))))

    (rename (super-add add))
    (define/override (add item)
      (define itemw (send item width))
      (define itemh (send item height))
      ;;
      ;; Scale the item before you add it to the panel.
      (if (<= itemw itemh)
          (send item scale (/ _limiting itemw))
          (send item scale (/ _limiting itemh)))
      ;;
      ;; Add it to the contents.
      (super-add item)
      (refresh))

    (define/public (vertical)
      (set! _orientation 'v)
      (refresh))

    (define/public (horizontal)
      (set! _orientation 'h)
      (refresh))

    (define/public (setanchor x y)
      (set! _ax x)
      (set! _ay y)
      (refresh))

    (define/public (getanchor)
      (list _ax _ay))

    (define/public (refresh)
      (draw-panel)
      (align-content))

    )
  )

