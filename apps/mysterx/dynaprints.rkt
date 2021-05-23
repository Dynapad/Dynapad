(load-relative "C:/cygwin/home/hollan/dynapad/dynapad.rkt")

(require (lib "mysterx.rkt" "mysterx"))

(define ie (cci/progid "InternetExplorer.Application"))
(com-set-property! ie "Visible" #t)

(com-invoke ie "Navigate" "http://hci.ucsd.edu")

(define url%
  (class oval%
    (init-field _ie)
    (field (_url #f))
    (inherit fill)
    (define/public (get) _url)
    (define/public (goto)
      (com-invoke _ie "Navigate" _url))
    (super-instantiate(dynapad '(0 0 20 20)))
    (fill "blue")
    (set! _url (com-get-property _ie "LocationURL"))))

(define url-dx 0)
(define url-dy 0)

(define urls ())

(define (ie-document-complete doc url)
  (let ((new (make-object url% ie)))
    (send new slide url-dx url-dy)
    (set! url-dx (+ url-dx 10))
    (set! url-dy (+ url-dy 10))
    (send new bind "<Run-Double-ButtonPress>"
          (lambda(o e)
            (send (event-obj e) goto)))
    (set! urls (append urls (list new)))))

(com-register-event-handler ie "DocumentComplete" ie-document-complete)
