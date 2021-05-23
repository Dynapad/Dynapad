
(define launcher%
  (class object%
    (super-instantiate ())

    (field (_editor      "/usr/bin/emacs"))
    (field (_webbrowser  "/usr/bin/konqueror"))
    (field (_imageviewer "/usr/bin/display"))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods
    (define/public (web-browser url)
      (execute #t (format "~a ~a" _webbrowser url)))

    (define/public (edit-file file)
      (execute #t (format "~a ~a" _editor file)))

    (define/public (view-image file)
      (execute #t (format "~a ~a" _imageviewer file)))
    )
  )