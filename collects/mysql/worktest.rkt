(load-relative "workspace.rkt")
(define w (make-object workspace%))
(printf "~a~%" (send w names))
(printf "hi~%")
(printf "~a~%" (send w names "jims_images"))

