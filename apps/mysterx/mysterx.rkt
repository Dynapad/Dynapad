(require (lib "mysterx.ss" "mysterx"))

(define ie (cci/progid "InternetExplorer.Application"))
(com-set-property! ie "Visible" #t)

(com-invoke ie "Navigate" "http://hci.ucsd.edu")
(com-get-property ie "LocationURL")

(com-register-event-handler ie "DocumentComplete" (lambda(doc url) (printf "~a~%" url)))
;(com-unregister-event-handler ie "DocumentComplete")
