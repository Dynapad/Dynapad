(dynaload "event-binders-new.ss")

(dynaload "draw.ss")
(dynaload "hyperlink.ss")

(define newpaddraw-event-binder%
  (class newpad-event-binder%

     (init argPAD evs-class)
     (super-instantiate (argPAD evs-class))
     (bindHyperlink argPAD) ;hyperlink.ss
     (bindDrawMode argPAD) ;draw.ss
     (bindTextMode argPAD) ;text.ss
))