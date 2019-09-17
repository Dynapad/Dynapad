(define blob-projector%
  (class projector%
    (init _rgn _cells)

    (super-instantiate (_rgn _cells))
))
   
(define spiral-projector%
  (class blob-projector%
     (init-field _rgn)
     (init-field _param)
     (inherit-field _cells)

     (super-instantiate (_rgn (make-object cell-row% _param)))

     (define/override (render)
       (let ((target (send _rgn boundary))
         (stuff (send _cells map-cells
              (lambda (o) (send o obj)))))
;     (say target stuff)
     (unless (null? stuff)
         (arrange-in-spiral-onto-object-no-scaling stuff target))

     (send _rgn contents stuff) ;forces recompute of fencepost positions
                                ; for pile auto-wrap
     (send _rgn finish)
     ))
))

;(define (get-webthumb-date thumb)
  

;(define web-time-param
;  (make-object quantitative-parameter%
;    get-webthumb-date
;    cmp-nums
;    identity-fn))


;(make-object spiral-projector% _rgn web-time-param)
;(make-object spiral-projector% reg date-parameter)

