; this assumes both event-binders-new.ss and logs.ss are already loaded

(access-hit-near-zoomlimit-callbacks
 'add
 (lambda (argPAD evnt)
   (log-changeview-entry ''zoom-in
                         (cons 'newview (send argPAD view))
                         (cons 'newbb (send argPAD bbox))
                         (cons 'zoomtarget
                               (if *last-zoom-targets*
                                   (map (lambda (o) (send o id)) *last-zoom-targets*)
                                   (list #f)))
                         (list 'zoomobject
                               (let ((obj (event-obj evnt)))
                                 (if (is-a? obj dynapad%)
                                     #f
                                     (send obj id)))))))

(access-hit-far-zoomlimit-callbacks
 'add
 (lambda (argPAD evnt)
   (log-changeview-entry ''zoom-out
                         (cons 'newview (send argPAD view))
                         (cons 'newbb (send argPAD bbox)))))

(access-hit-midfar-zoomlimit-callbacks
 'add
 (lambda (argPAD evnt)
   (log-changeview-entry ''zoom-back-out
                         (cons 'newview (send argPAD view))
                         (cons 'newbb (send argPAD bbox)))))

(access-hit-midnear-zoomlimit-callbacks
 'add
 (lambda (argPAD evnt)
   (log-changeview-entry ''zoom-back-in
                         (cons 'newview (send argPAD view))
                         (cons 'newbb (send argPAD bbox)))))

(access-beforezoom-callbacks
 'add
 (lambda (argPAD)
   (log-changeview-entry ''startzoom
                         (cons 'oldview (send argPAD view))
                         (cons 'oldbb (send argPAD bbox)))))

(access-afterzoom-callbacks
 'add
 (lambda (argPAD)
   (log-changeview-entry ''endzoom
                         (cons 'newview (send argPAD view))
                         (cons 'newbb (send argPAD bbox)))))

(access-beforepan-callbacks
 'add
 (lambda (argPAD)
   (log-changeview-entry ''startpan
                         (cons 'oldview (send argPAD view))
                         (cons 'oldbb (send argPAD bbox)))))

(access-afterpan-callbacks
 'add
 (lambda (argPAD)
   (log-changeview-entry ''endpan
                         (cons 'newview (send argPAD view))
                         (cons 'newbb (send argPAD bbox)))))

(define (oldview . args) #f)
(define (newview . args) #f)
(define (oldbb . args) #f)
(define *last-view-bbox* #f)
(define (newbb . args) (set! *last-view-bbox* args))
(define (zoomtarget . args) #f)
(define (zoomobject . args) #f)