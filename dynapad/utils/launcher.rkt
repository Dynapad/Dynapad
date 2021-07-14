(dynaload "system-command.rkt")

; originally from etienne

(define launcher%
  (class object%
    (super-instantiate ())

    (field (_editorlist      (list "emacs" "vi")))
    (field (_webbrowserlist  (list "konqueror" "mozilla" "netscape" )))
    (field (_imageviewerlist (list "display" "xv")))
    (field (_pdfviewerlist   (list "acroread")))
    (field (_psviewerlist    (list "gv" "ghostscript")))

    (field (_ext_type_alist

            '(("txt"  text)

              ("ss"   code)
              ("cpp"  code)
              ("c"    code)
              ("h"    code)

              ("htm"  webpage)
              ("html" webpage)

              ("jpg"  image) ("Jpg"  image) ("JPG"  image)
              ("gif"  image) ("Gif"  image) ("GIF"  image)
              ("png"  image) ("Png"  image) ("PNG"  image)

              ("pdf"  pdf) ("PDF"  'pdf)

              ("ps"  postscript) ("PS"   postscript)
              ("eps" postscript) ("EPS"  postscript)

              ("o"    noapp)
              ("db"   noapp)

              ("exe"  exe)
              )))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Methods

    (define/public (get-file-type path_or_url)
      ; first check for url -- "http" or "www"
      ;   'url
      ; second check if file is executable
      ;   'exe
      ; otherwise, assume local file to view
      (let* ((ext (filename-extension path_or_url))
             (file-type (assoc ext _ext_type_alist)))
        (if file-type (cadr file-type) #f)))

    (define/public (open-application-for path)
      (let ((file-type (get-file-type path)))
        (case file-type
          ('text     (edit-file   path))
          ('code     (edit-file   path))
          ('image    (view-image  path))
          ('pdf      (view-pdf    path))
          ('url      (web-browser path))
          ('webpage  (view-html   path))
          ('exe      (open-exe    path))
          ('noapp    #f)
          (else      (edit-file path)) ;;assume text
          )))


    (define/public (web-browser url)
      (system-application (format "~a ~a" (car _webbrowserlist) url)))

    (define/public (open-exe executable)
      (system-application (format "~a" executable)))


    (define/public (edit-file file)
      (system-application (format "~a ~a" (car _editorlist) file)))

    (define/public (view-image file)
      (system-application (format "~a ~a" (car _imageviewerlist) file)))

    (define/public (view-html file)
      (system-application (format "~a ~a" (car _webbrowserlist) file)))

    (define/public (view-pdf file)
      (system-application (format "~a ~a" (car _pdfviewerlist) file)))

    (define/public (view-ps  file)
      (system-application (format "~a ~a" (car _psviewerlist) file)))

    )
  )

